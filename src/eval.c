// TODO: Validate that a DimIndex, UnitIndex etc. is actually within range
// during lookup
#include "eval.h"

#include <assert.h>

#define NOB_STRIP_PREFIX
#include "nob.h"
// TODO: Find a better solution to this (migrate to Nob's String_View?)
#undef sv_eq

#include "parser.h"
#include "sv.h"

/// The maximum depth the interpreter will go to while trying to find an
/// expression for a derived unit in terms of its dimensions fundamental unit
#define UNIT_CAST_MAX_DEPTH 1024

// TODO: Implement better pretty printing for some of these values
// (e.g. print the name of the dimension/unit instead of just the index)
static void unit_dump(FILE *f, Unit u)
{
    fprintf(f, "`" SV_FMT "` ", SV_ARG(u.name));
    if (u.expr.type != EXPR_TYPE_NONE) {
        fprintf(f, "= ");
        expr_print(f, u.expr);
    }
    fprintf(f, "(%zu)", u.dim);
}

static void dim_dump(FILE *f, Dim d)
{
    fprintf(f, "`" SV_FMT "` (%zu)", SV_ARG(d.name), d.fundamental_unit);
}

static void val_dump(FILE *f, Value v)
{
    fprintf(f, "%f (%zu)", v.num, v.unit);
}

static void var_dump(FILE *f, Var v)
{
    fprintf(f, "`" SV_FMT "` (%zu)", SV_ARG(v.name), v.dim);
    if (v.initialised) {
        fprintf(f, " = ");
        val_dump(f, v.value);
    }
}

void dump_context(FILE *f, EvalContext *ctx)
{
    fprintf(f, "EvalContext {\n");
    if (ctx->units.count > 0) {
        fprintf(f, "\tunits: [");
        // TODO: Print index as well
        unit_dump(f, ctx->units.items[0]);
        for (size_t i = 1; i < ctx->units.count; i++) {
            fprintf(f, ", ");
            unit_dump(f, ctx->units.items[i]);
        }
        fprintf(f, "],\n");
    }
    if (ctx->dims.count > 0) {
        fprintf(f, "\tdims: [");
        // TODO: Print index as well
        dim_dump(f, ctx->dims.items[0]);
        for (size_t i = 1; i < ctx->dims.count; i++) {
            fprintf(f, ", ");
            dim_dump(f, ctx->dims.items[i]);
        }
        fprintf(f, "],\n");
    }
    if (ctx->vars.count > 0) {
        fprintf(f, "\tvars: [");
        // TODO: Print index as well
        var_dump(f, ctx->vars.items[0]);
        for (size_t i = 1; i < ctx->vars.count; i++) {
            fprintf(f, ", ");
            var_dump(f, ctx->vars.items[i]);
        }
        fprintf(f, "],\n");
    }
    fprintf(f, "}\n");
}

EvalContext new_context(void)
{
    EvalContext ctx = {0};
    // TODO: Disallow the user (on the lexer/parser level) to name identifiers
    // with these "special" names
    Dim dimensionless = {
        .name = SV("<dimensionless>"),
    };
    da_append(&ctx.dims, dimensionless);

    Unit unitless = {
        .name = SV("<unitless>"),
        .dim  = 0,
        .expr = {0},
    };
    da_append(&ctx.units, unitless);

    return ctx;
}

/// If a dimension with name `name` exists in `ctx`, returns true and sets
/// `*resp` (if resp != NULL) to the index of the corresponding dimension. If
/// resp == NULL, simply returns whether the dimension exists. If the dimension
/// does not exist, returns false and does not modify `*resp`.
static bool try_resolve_dim_by_name(EvalContext *ctx, StringView name,
                                    DimIndex *resp)
{
    DimIndex idx;
    for (idx = 0; idx < ctx->dims.count; idx++) {
        if (sv_eq(name, ctx->dims.items[idx].name)) {
            if (resp != NULL) {
                *resp = idx;
            }
            return true;
        }
    }

    return false;
}

/// If a unit with name `name` exists in `ctx`, returns true and sets
/// `*resp` (if resp != NULL) to the index of the corresponding unit. If
/// resp == NULL, simply returns whether the unit exists. If the unit
/// does not exist, returns false and does not modify `*resp`
static bool try_resolve_unit_by_name(EvalContext *ctx, StringView name,
                                     UnitIndex *resp)
{
    UnitIndex idx;
    for (idx = 0; idx < ctx->units.count; idx++) {
        if (sv_eq(name, ctx->units.items[idx].name)) {
            if (resp != NULL) {
                *resp = idx;
            }
            return true;
        }
    }

    return false;
}

/// If a var with name `name` exists in `ctx`, returns true and sets
/// `*resp` (if resp != NULL) to the index of the corresponding var. If
/// resp == NULL, simply returns whether the var exists. If the unit
/// does not exist, returns false and does not modify `*resp`
static bool try_resolve_var_by_name(EvalContext *ctx, StringView name,
                                    VarIndex *resp)
{
    VarIndex idx;
    for (idx = 0; idx < ctx->vars.count; idx++) {
        if (sv_eq(name, ctx->vars.items[idx].name)) {
            if (resp != NULL) {
                *resp = idx;
            }
            return true;
        }
    }

    return false;
}

static DimIndex resolve_dim_by_name(EvalContext *ctx, StringView name)
{
    DimIndex res;
    if (!try_resolve_dim_by_name(ctx, name, &res)) {
        fprintf(stderr, "ERROR: Could not resolve dim `" SV_FMT "`.\n",
                SV_ARG(name));
        fprintf(stderr, "context: ");
        dump_context(stderr, ctx);
        exit(1);
    }
    return res;
}

static UnitIndex resolve_unit_by_name(EvalContext *ctx, StringView name)
{
    UnitIndex res;
    if (!try_resolve_unit_by_name(ctx, name, &res)) {
        fprintf(stderr, "ERROR: Could not resolve unit `" SV_FMT "`.\n",
                SV_ARG(name));
        fprintf(stderr, "context: ");
        dump_context(stderr, ctx);
        exit(1);
    }
    return res;
}

static VarIndex resolve_var_by_name(EvalContext *ctx, StringView name)
{
    VarIndex res;
    if (!try_resolve_var_by_name(ctx, name, &res)) {
        fprintf(stderr, "ERROR: Could not resolve var `" SV_FMT "`.\n",
                SV_ARG(name));
        fprintf(stderr, "context: ");
        dump_context(stderr, ctx);
        exit(1);
    }
    return res;
}

static Value value_binop(Value left, Value right, Op op)
{
    switch (op) {
    case OP_MULT: {
        // TODO: Add compound units
        // This would possibly be better implemented by separating out the
        // notion of a "unit" (which can be simple or compound, and would
        // probably contain a dynamic array of pairs (f, p) where f is a
        // fundamnetal unit and p is its power) and a "fundamental unit" (a unit
        // specific to a dimension)
        if (left.unit != 0 && right.unit != 0) {
            assert(0 && "Compound units are not yet implemented");
        }
        return (Value) {
            .num  = left.num * right.num,
            .unit = left.unit == 0 ? right.unit : left.unit,
        };
    }

    case OP_DIV: {
        // TODO: Add inverse units

        if (right.unit != 0) {
            assert(0 && "Inverse and compound units are not yet implemented");
        }

        return (Value) {
            .num  = left.num / right.num,
            .unit = left.unit,
        };
    }

    default:
        fprintf(stderr, "ERROR: Don't know how to perform operation: ");
        op_print(stderr, op);
        fputc('\n', stderr);
        exit(1);
    }
}

Value val_print(EvalContext *ctx, Value value)
{
    int printed = printf("%f", value.num);
    if (value.unit != 0) {
        printed +=
            printf(" " SV_FMT, SV_ARG(ctx->units.items[value.unit].name));
    }
    printed += printf("\n");
    return (Value) {
        .num  = (double) printed,
        .unit = 0,
    };
}

Value eval_expr(EvalContext *ctx, Expr expr)
{
    switch (expr.type) {
    case EXPR_TYPE_NUM: {
        double num     = expr.as.num.val;
        UnitIndex unit = (sv_is_empty(expr.as.num.unit))
                             ? UNITLESS
                             : resolve_unit_by_name(ctx, expr.as.num.unit);

        return (Value) {
            .num  = num,
            .unit = unit,
        };
    }

    case EXPR_TYPE_BINOP: {
        Value left  = eval_expr(ctx, *expr.as.binop.left);
        Value right = eval_expr(ctx, *expr.as.binop.right);

        return value_binop(left, right, expr.as.binop.op);
    }

    case EXPR_TYPE_VAR: {
        VarIndex idx = resolve_var_by_name(ctx, expr.as.var.name);
        return ctx->vars.items[idx].value;
    }

    case EXPR_TYPE_ASSIGN: {
        VarIndex idx = resolve_var_by_name(ctx, expr.as.assign.lhs);
        Value rhs    = eval_expr(ctx, *expr.as.assign.rhs);

        DimIndex lhs_dim = ctx->vars.items[idx].dim;
        DimIndex rhs_dim = ctx->units.items[rhs.unit].dim;
        if (lhs_dim != rhs_dim) {
            fprintf(stderr,
                    "ERROR: Cannot assign to var `" SV_FMT
                    "`: LHS dim (`" SV_FMT "`) != RHS dim (`" SV_FMT "`).\n",
                    SV_ARG(expr.as.assign.lhs),
                    SV_ARG(ctx->dims.items[lhs_dim].name),
                    SV_ARG(ctx->dims.items[rhs_dim].name));
            exit(1);
        }

        ctx->vars.items[idx].value = rhs;
        return rhs;
    }

    case EXPR_TYPE_FUNCALL: {
        if (!sv_eq(expr.as.funcall.fun, SV("print"))) {
            // TODO: Implement general function calling
            assert(0 && "TODO: Only the `print` function is implemented");
        } else {
            if (expr.as.funcall.args.count == 0) {
                fprintf(stderr,
                        "ERROR: `print`: No argument provided (expected 1).\n");
                exit(1);
            }
            if (expr.as.funcall.args.count > 1) {
                fprintf(stderr,
                        "ERROR: `print`: Too many arguments provided (got %zu, "
                        "expected 1).\n",
                        expr.as.funcall.args.count);
                exit(1);
            }

            Value input = eval_expr(ctx, expr.as.funcall.args.items[0]);
            return val_print(ctx, input);
        }
    }

    case EXPR_TYPE_UNITCAST: {
        UnitIndex target_id =
            resolve_unit_by_name(ctx, expr.as.unit_cast.target);
        Unit target = ctx->units.items[target_id];

        Value val   = eval_expr(ctx, *expr.as.unit_cast.value);
        Unit source = ctx->units.items[val.unit];
        if (source.dim != target.dim) {
            fprintf(stderr,
                    "ERROR: Cannot cast a value of dimension `" SV_FMT
                    "` to unit `" SV_FMT "` of dimension `" SV_FMT "`.\n",
                    SV_ARG(ctx->dims.items[source.dim].name),
                    SV_ARG(target.name),
                    SV_ARG(ctx->dims.items[target.dim].name));
            exit(1);
        }
        Dim dim = ctx->dims.items[source.dim];

        // TODO: Support units that aren't just pure multiplication (e.g.
        // Fahrenheit/Celsius)
        double conv_factor = 1;

        if (target.expr.type != EXPR_TYPE_NONE) {
            Value fun_to_target = eval_expr(ctx, target.expr);
            for (int i = 0; fun_to_target.unit != dim.fundamental_unit; i++) {
                if (i >= UNIT_CAST_MAX_DEPTH) {
                    fprintf(
                        stderr,
                        "ERROR: Exceeded max depth of %d while trying to cast "
                        "to unit `" SV_FMT "`.\n",
                        UNIT_CAST_MAX_DEPTH, SV_ARG(target.name));
                    exit(1);
                }

                Value next =
                    eval_expr(ctx, ctx->units.items[fun_to_target.unit].expr);

                fun_to_target = (Value) {
                    .num  = fun_to_target.num * next.num,
                    .unit = next.unit,
                };
            }
            assert(fun_to_target.unit == dim.fundamental_unit);

            conv_factor /= fun_to_target.num;
        }

        if (source.expr.type != EXPR_TYPE_NONE) {
            Value source_to_fun = eval_expr(ctx, source.expr);
            for (int i = 0; source_to_fun.unit != dim.fundamental_unit; i++) {
                if (i >= UNIT_CAST_MAX_DEPTH) {
                    fprintf(
                        stderr,
                        "ERROR: Exceeded max depth of %d while trying to cast "
                        "from unit `" SV_FMT "`.\n",
                        UNIT_CAST_MAX_DEPTH, SV_ARG(source.name));
                    exit(1);
                }

                Value next =
                    eval_expr(ctx, ctx->units.items[source_to_fun.unit].expr);

                source_to_fun = (Value) {
                    .num  = source_to_fun.num * next.num,
                    .unit = next.unit,
                };
            }
            assert(source_to_fun.unit == dim.fundamental_unit);

            conv_factor *= source_to_fun.num;
        }

        return (Value) {
            .num  = val.num * conv_factor,
            .unit = target_id,
        };
    }

    case EXPR_TYPE_PAREN: {
        return eval_expr(ctx, *expr.as.paren.inner);
    }

    default:
        fprintf(stderr, "ERROR: Don't know how to evaluate expression: ");
        expr_print(stderr, expr);
        fputc('\n', stderr);
        exit(1);
    }
}

void eval_stmt(EvalContext *ctx, Stmt stmt)
{
    switch (stmt.type) {
    case STMT_TYPE_NONE:
        return;

    case STMT_TYPE_DIM_DECL: {
        Dim dim = {
            .name             = stmt.as.dim_decl.name,
            .fundamental_unit = UNITLESS,
        };

        if (try_resolve_dim_by_name(ctx, dim.name, NULL)) {
            // TODO: Better (located) error reporting
            fprintf(stderr,
                    "ERROR: A dimension with name `" SV_FMT
                    "` already exists.\n",
                    SV_ARG(dim.name));
            fprintf(stderr, "context: ");
            dump_context(stderr, ctx);
            exit(1);
        }
        da_append(&ctx->dims, dim);
        return;
    }

    case STMT_TYPE_UNIT_DECL: {
        DimIndex dim = resolve_dim_by_name(ctx, stmt.as.unit_decl.dim);

        Unit unit = {
            .name = stmt.as.unit_decl.name,
            .dim  = dim,
            // TODO: Renamet his quantity to `as.unit_decl.expr` as well for
            // consistency
            .expr = stmt.as.unit_decl.value,
        };
        if (try_resolve_unit_by_name(ctx, unit.name, NULL)) {
            // TODO: Better (located) error reporting
            fprintf(stderr,
                    "ERROR: A unit with name `" SV_FMT "` already exists.\n",
                    SV_ARG(unit.name));
            fprintf(stderr, "context: ");
            dump_context(stderr, ctx);
            exit(1);
        }

        if (unit.expr.type == EXPR_TYPE_NONE) {
            Dim *dim = &ctx->dims.items[unit.dim];
            if (dim->fundamental_unit != UNITLESS) {
                fprintf(
                    stderr,
                    "ERROR: Dimension `" SV_FMT
                    "` already has a fundamental unit `" SV_FMT
                    "`. The unit `" SV_FMT
                    "` must be expressed in terms of the fundamental unit.\n",
                    SV_ARG(dim->name),
                    SV_ARG(ctx->units.items[dim->fundamental_unit].name),
                    SV_ARG(unit.name));
                exit(1);
            }

            dim->fundamental_unit = ctx->units.count;
        }

        da_append(&ctx->units, unit);

        return;
    }

    case STMT_TYPE_VAR_DECL: {
        DimIndex dim = resolve_dim_by_name(ctx, stmt.as.var_decl.dim);

        Var var = {
            .name = stmt.as.var_decl.name,
            .dim  = dim,
        };
        if (try_resolve_var_by_name(ctx, var.name, NULL)) {
            fprintf(stderr,
                    "ERROR: A variable with name `" SV_FMT
                    "` already exists.\n",
                    SV_ARG(var.name));
            fprintf(stderr, "context: ");
            dump_context(stderr, ctx);
            exit(1);
        }

        if (stmt.as.var_decl.value.type == EXPR_TYPE_NONE) {
            var.initialised = false;
        } else {
            Value value     = eval_expr(ctx, stmt.as.var_decl.value);
            var.initialised = true;
            var.value       = value;
        }
        da_append(&ctx->vars, var);
        return;
    };

    case STMT_TYPE_EXPR: {
        eval_expr(ctx, stmt.as.expr);
        return;
    }

    default:
        fprintf(stderr, "ERROR: Don't know how to evaluate statement: ");
        stmt_print(stderr, stmt);
        fputc('\n', stderr);
        exit(1);
    }
}
