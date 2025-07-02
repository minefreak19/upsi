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
static void simple_unit_dump(FILE *f, SimpleUnit u)
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

static void compound_unit_dump(FILE *f, CompoundUnit u)
{
    for (size_t i = 0; i < u.elems_count; i++) {
        fprintf(f, "(%zu, %" POWER_FMT ")", u.elems[i].unit, u.elems[i].power);
    }
}

static void val_dump(FILE *f, Value v)
{
    fprintf(f, "%f ", v.num);
    compound_unit_dump(f, v.unit);
}

static void var_dump(FILE *f, Var v)
{
    fprintf(f, "`" SV_FMT "`", SV_ARG(v.name));
    if (v.initialised) {
        fprintf(f, " = ");
        val_dump(f, v.value);
    }
}

void dump_context(FILE *f, EvalContext *ctx)
{
    fprintf(f, "EvalContext {\n");
    if (ctx->simple_units.count > 0) {
        fprintf(f, "\tunits: [");
        // TODO: Print index as well
        simple_unit_dump(f, ctx->simple_units.items[0]);
        for (size_t i = 1; i < ctx->simple_units.count; i++) {
            fprintf(f, ", ");
            simple_unit_dump(f, ctx->simple_units.items[i]);
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
    EvalContext ctx   = {0};
    Dim dimensionless = {
        .name = SV("(dimensionless)"),
    };
    da_append(&ctx.dims, dimensionless);

    SimpleUnit unitless = {
        .name = SV("(unitless)"),
        .dim  = 0,
        .expr = {0},
    };
    da_append(&ctx.simple_units, unitless);

    return ctx;
}

void free_context(EvalContext *ctx)
{
    for (size_t i = 0; i < ctx->simple_units.count; i++) {
        expr_free(ctx->simple_units.items[i].expr);
    }
    free(ctx->simple_units.items);
    // Nothing to do for dimensions or variables
    // TODO: This will change once compound dimensions are added
    free(ctx->dims.items);
    free(ctx->vars.items);
    *ctx = (EvalContext) {0};
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
                                     SimpleUnitIndex *resp)
{
    SimpleUnitIndex idx;
    for (idx = 0; idx < ctx->simple_units.count; idx++) {
        if (sv_eq(name, ctx->simple_units.items[idx].name)) {
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

static SimpleUnitIndex resolve_unit_by_name(EvalContext *ctx, StringView name)
{
    SimpleUnitIndex res;
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
        for (size_t ru = 0; ru < right.unit.elems_count; ru++) {
            for (size_t lu = 0; lu < left.unit.elems_count; lu++) {
                if (left.unit.elems[lu].unit == right.unit.elems[ru].unit) {
                    left.unit.elems[lu].power += right.unit.elems[ru].power;
                    goto cont;
                }
            }

            if (left.unit.elems_count + 1 >= COMPOUND_UNIT_CAP) {
                fprintf(stderr,
                        "ERROR: Exceeded max cap of %d simple units in "
                        "compound unit.\n",
                        COMPOUND_UNIT_CAP);
                exit(1);
            }
            left.unit.elems[left.unit.elems_count++] = right.unit.elems[ru];
        cont:
            continue;
        }

        left.num *= right.num;

        return left;
    }

    case OP_DIV: {
        // a / b = a * (1/b)
        right.num = 1 / right.num;
        for (size_t i = 0; i < right.unit.elems_count; i++) {
            right.unit.elems[i].power = -right.unit.elems[i].power;
        }

        return value_binop(left, right, OP_MULT);
    }

    default:
        fprintf(stderr, "ERROR: Don't know how to perform operation: ");
        op_print(stderr, op);
        fputc('\n', stderr);
        exit(1);
    }
}

static int simple_unit_pow_print(EvalContext *ctx, SimpleUnitPow sup)
{
    int printed = 0;
    printed += printf(SV_FMT, SV_ARG(ctx->simple_units.items[sup.unit].name));
    if (sup.power != 1) {
        printed += printf("^%" POWER_FMT, sup.power);
    }
    return printed;
}

Value val_print(EvalContext *ctx, Value value)
{
    // TODO: Implement ability to print values and units to strings instead of
    // directly to stdout
    int printed = printf("%f", value.num);
    if (value.unit.elems_count > 0) {
        printed += printf(" ");
        printed += simple_unit_pow_print(ctx, value.unit.elems[0]);
        for (size_t i = 1; i < value.unit.elems_count; i++) {
            printed += printf(" * ");
            printed += simple_unit_pow_print(ctx, value.unit.elems[i]);
        }
    }
    printed += printf("\n");
    return (Value) {
        .num  = (double) printed,
        .unit = {0},
    };
}

static bool compound_unit_is_fundamental(EvalContext *ctx, CompoundUnit unit)
{
    if (unit.elems_count != 1 || unit.elems[0].power != 1) {
        fprintf(stderr, "%zu\n", unit.elems_count);
        assert(0 &&
               "TODO: compound_unit_is_fundamental is not implemented for "
               "actually compound units");
    }

    Dim dim = ctx->dims.items[ctx->simple_units.items[unit.elems[0].unit].dim];
    return dim.fundamental_unit == unit.elems[0].unit;
}

static bool compound_unit_is_castable(EvalContext *ctx, CompoundUnit a,
                                      CompoundUnit b)
{
    for (DimIndex dim = 0; dim < ctx->dims.count; dim++) {
        Power power_a = 0;
        for (size_t j = 0; j < a.elems_count; j++) {
            if (ctx->simple_units.items[a.elems[j].unit].dim == dim) {
                power_a += a.elems[j].power;
            }
        }

        Power power_b = 0;
        for (size_t j = 0; j < b.elems_count; j++) {
            if (ctx->simple_units.items[b.elems[j].unit].dim == dim) {
                power_b += b.elems[j].power;
            }
        }

        if (power_a != power_b) return false;
    }

    return true;
}

Value eval_expr(EvalContext *ctx, Expr expr)
{
    switch (expr.type) {
    case EXPR_TYPE_NUM: {
        double num = expr.as.num.val;
        // TODO: Support compound unit literals
        SimpleUnitIndex sui = (sv_is_empty(expr.as.num.unit))
                                  ? UNITLESS
                                  : resolve_unit_by_name(ctx, expr.as.num.unit);

        return (Value) {
            .num = num,
            .unit =
                {
                    .elems[0] =
                        {
                            .unit  = sui,
                            .power = 1,
                        },
                    .elems_count = sui == UNITLESS ? 0 : 1,
                },
        };
    }

    case EXPR_TYPE_BINOP: {
        Value left  = eval_expr(ctx, *expr.as.binop.left);
        Value right = eval_expr(ctx, *expr.as.binop.right);

        return value_binop(left, right, expr.as.binop.op);
    }

    case EXPR_TYPE_VAR: {
        VarIndex idx = resolve_var_by_name(ctx, expr.as.var.name);
        Var var      = ctx->vars.items[idx];
        if (!var.initialised) {
            fprintf(stderr,
                    "ERROR: Attempt to read uninitialised variable `" SV_FMT
                    "`.\n",
                    SV_ARG(var.name));
            exit(1);
        }
        return var.value;
    }

    case EXPR_TYPE_ASSIGN: {
        VarIndex idx = resolve_var_by_name(ctx, expr.as.assign.lhs);
        Value rhs    = eval_expr(ctx, *expr.as.assign.rhs);

        // TODO: Better error message here once compound dimensions are
        // figured out
        if (ctx->vars.items->initialised &&
            !compound_unit_is_castable(ctx, ctx->vars.items[idx].value.unit,
                                       rhs.unit)) {
            fprintf(stderr,
                    "ERROR: Cannot assign to var `" SV_FMT
                    "`: Dimensionality mismatch.\n",
                    SV_ARG(expr.as.assign.lhs));
            exit(1);
        }

        ctx->vars.items[idx].value = rhs;
        ctx->vars.items[idx].initialised = true;
        return rhs;
    }

    case EXPR_TYPE_FUNCALL: {
        if (sv_eq(expr.as.funcall.fun, SV("print"))) {
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
        } else if (sv_eq(expr.as.funcall.fun, SV("dump"))) {
            if (expr.as.funcall.args.count == 0) {
                dump_context(stdout, ctx);
                return (Value) {0};
            }
            if (expr.as.funcall.args.count > 1) {
                fprintf(stderr,
                        "ERROR: `dump`: Too many arguments provided (got %zu, "
                        "expected 1).\n",
                        expr.as.funcall.args.count);
                exit(1);
            }

            Expr inner = expr.as.funcall.args.items[0];
            if (inner.type == EXPR_TYPE_VAR) {
                var_dump(stdout, ctx->vars.items[resolve_var_by_name(
                                     ctx, inner.as.var.name)]);
            }

            Value input = eval_expr(ctx, inner);
            printf("value: ");
            return val_print(ctx, input);
        } else {
            fprintf(stderr,
                    "ERROR: TODO: Function calls are not implemented yet.\n");
            fprintf(stderr,
                    "NOTE: Only the following standard functions are "
                    "implemented: \n");
            fprintf(
                stderr,
                "\tprint - prints a value (intended for regular printing)\n");
            fprintf(stderr,
                    "\tdump  - dumps a value/variable (intended for debugging "
                    "the interpreter)\n");
            exit(1);
        }
    }

    case EXPR_TYPE_UNITCAST: {
        SimpleUnitIndex target_id =
            resolve_unit_by_name(ctx, expr.as.unit_cast.target);
        SimpleUnit target = ctx->simple_units.items[target_id];

        Value val = eval_expr(ctx, *expr.as.unit_cast.value);
        if (val.unit.elems_count > 1 ||
            (val.unit.elems_count == 1 && val.unit.elems[0].power != 1)) {
            fprintf(stderr,
                    "TODO: Dimension checking and unit casting for compound "
                    "units is not yet implemented.\n");
            exit(1);
        }
        SimpleUnit source = ctx->simple_units.items[val.unit.elems[0].unit];
        if (source.dim != target.dim) {
            fprintf(stderr,
                    "ERROR: Cannot cast a value of dimension `" SV_FMT
                    "` to unit `" SV_FMT "` of dimension `" SV_FMT "`.\n",
                    SV_ARG(ctx->dims.items[source.dim].name),
                    SV_ARG(target.name),
                    SV_ARG(ctx->dims.items[target.dim].name));
            exit(1);
        }

        // TODO: Support units that aren't just pure multiplication (e.g.
        // Fahrenheit/Celsius)
        double conv_factor = 1;

        if (target.expr.type != EXPR_TYPE_NONE) {
            Value fun_to_target = eval_expr(ctx, target.expr);
            for (int i = 0;
                 !compound_unit_is_fundamental(ctx, fun_to_target.unit); i++) {
                if (i >= UNIT_CAST_MAX_DEPTH) {
                    fprintf(stderr,
                            "ERROR: Exceeded max depth of %d while trying "
                            "to cast "
                            "to unit `" SV_FMT "`.\n",
                            UNIT_CAST_MAX_DEPTH, SV_ARG(target.name));
                    exit(1);
                }

                assert(fun_to_target.unit.elems_count == 1 &&
                       fun_to_target.unit.elems[0].power == 1);
                Value next = eval_expr(
                    ctx,
                    ctx->simple_units.items[fun_to_target.unit.elems[0].unit]
                        .expr);

                fun_to_target = (Value) {
                    .num  = fun_to_target.num * next.num,
                    .unit = next.unit,
                };
            }
            assert(compound_unit_is_fundamental(ctx, fun_to_target.unit));

            conv_factor /= fun_to_target.num;
        }

        if (source.expr.type != EXPR_TYPE_NONE) {
            Value source_to_fun = eval_expr(ctx, source.expr);
            for (int i = 0;
                 !compound_unit_is_fundamental(ctx, source_to_fun.unit); i++) {
                if (i >= UNIT_CAST_MAX_DEPTH) {
                    fprintf(stderr,
                            "ERROR: Exceeded max depth of %d while trying "
                            "to cast "
                            "from unit `" SV_FMT "`.\n",
                            UNIT_CAST_MAX_DEPTH, SV_ARG(source.name));
                    exit(1);
                }

                assert(source_to_fun.unit.elems_count == 1 &&
                       source_to_fun.unit.elems[0].power == 1);
                Value next = eval_expr(
                    ctx,
                    ctx->simple_units.items[source_to_fun.unit.elems[0].unit]
                        .expr);

                source_to_fun = (Value) {
                    .num  = source_to_fun.num * next.num,
                    .unit = next.unit,
                };
            }
            assert(compound_unit_is_fundamental(ctx, source_to_fun.unit));

            conv_factor *= source_to_fun.num;
        }

        return (Value) {
            .num = val.num * conv_factor,
            .unit =
                {
                    .elems_count = 1,
                    .elems[0] =
                        {
                            .unit  = target_id,
                            .power = 1,
                        },
                },
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

        SimpleUnit unit = {
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
                    "` must be expressed in terms of the fundamental "
                    "unit.\n",
                    SV_ARG(dim->name),
                    SV_ARG(ctx->simple_units.items[dim->fundamental_unit].name),
                    SV_ARG(unit.name));
                exit(1);
            }

            dim->fundamental_unit = ctx->simple_units.count;
        }

        da_append(&ctx->simple_units, unit);

        return;
    }

    case STMT_TYPE_VAR_DECL: {
        Var var = {
            .name = stmt.as.var_decl.name,
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
