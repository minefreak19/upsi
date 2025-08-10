// TODO: Validate that a DimIndex, UnitIndex etc. is actually within range
// during lookup
#include "eval.h"

#include <assert.h>
#include <math.h>
#include <string.h>

#define NOB_STRIP_PREFIX
#include "nob.h"
// TODO: Find a better solution to this (migrate to Nob's String_View?)
#undef sv_eq

#include "parser.h"
#include "sv.h"

/// The maximum depth the interpreter will go to while trying to find an
/// expression for a derived unit in terms of its dimensions fundamental unit
#define UNIT_CAST_MAX_DEPTH 1024

// `simple` in this sense means uni-dimensional
static inline bool named_unit_is_simple(EvalContext *ctx, NamedUnit unit)
{
    if (unit.val.num == 0) return true;
    if (unit.val.unit.is_anonymous) {
        if (unit.val.unit.compound.elems_count > 1) return false;
        // I'm not sure whether this assertion is necessary
        assert(unit.val.unit.compound.elems_count > 0);
        if (unit.val.unit.compound.elems[0].power > 1) return false;
    } else {
        return named_unit_is_simple(
            ctx, ctx->named_units.items[unit.val.unit.named]);
    }
    return true;
}

static bool named_unit_is_fundamental(NamedUnit unit)
{
    return unit.val.num == 0;
}

static CompoundUnit compound_unit_copy(CompoundUnit cu)
{
    CompoundUnit res = {0};
    res.elems_count  = cu.elems_count;
    memcpy(res.elems, cu.elems, cu.elems_count * sizeof(cu.elems[0]));
    return res;
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
                                     NamedUnitIndex *resp)
{
    NamedUnitIndex idx;
    for (idx = 0; idx < ctx->named_units.count; idx++) {
        if (sv_eq(name, ctx->named_units.items[idx].name)) {
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
        eval_context_dump(stderr, ctx);
        exit(1);
    }
    return res;
}

static NamedUnitIndex resolve_unit_by_name(EvalContext *ctx, StringView name)
{
    NamedUnitIndex res;
    if (!try_resolve_unit_by_name(ctx, name, &res)) {
        fprintf(stderr, "ERROR: Could not resolve unit `" SV_FMT "`.\n",
                SV_ARG(name));
        fprintf(stderr, "context: ");
        eval_context_dump(stderr, ctx);
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
        eval_context_dump(stderr, ctx);
        exit(1);
    }
    return res;
}

/// Forcibly coerces a NamedUnitIndex to a CompoundUnit, either by unwrapping
/// the value of the unit (in the case of named compound units), or (in the case
/// of a simple unit) by wrapping it in a CompoundUnit
static CompoundUnit coerce_named_unit_index_to_compound(EvalContext *ctx,
                                                        NamedUnitIndex nui)
{
    if (nui == UNITLESS) return (CompoundUnit) {0};

    NamedUnit unit = ctx->named_units.items[nui];
    if (named_unit_is_simple(ctx, unit)) {
        return (CompoundUnit) {
            .elems_count = 1,
            .elems[0] =
                {
                    .unit  = nui,
                    .power = 1,
                },
        };
    }

    assert(unit.val.unit.is_anonymous);
    return compound_unit_copy(unit.val.unit.compound);
}

static Unit resolve_unit_from_expr(EvalContext *ctx, Expr expr)
{
    switch (expr.type) {
    case EXPR_TYPE_VAR: {
        NamedUnitIndex nui = resolve_unit_by_name(ctx, expr.as.var.name);
        return (Unit) {
            .is_anonymous = false,
            .named        = nui,
        };
    }

    case EXPR_TYPE_BINOP: {
        Unit left  = resolve_unit_from_expr(ctx, *expr.as.binop.left);
        Unit right = resolve_unit_from_expr(ctx, *expr.as.binop.right);

        if (!left.is_anonymous) {
            left.compound =
                coerce_named_unit_index_to_compound(ctx, left.named);
            left.is_anonymous = true;
        }
        if (!right.is_anonymous) {
            right.compound =
                coerce_named_unit_index_to_compound(ctx, right.named);
            right.is_anonymous = true;
        }

        switch (expr.as.binop.op) {
        case OP_MULT: {
            assert(left.is_anonymous);
            assert(right.is_anonymous);
            for (size_t ru = 0; ru < right.compound.elems_count; ru++) {
                for (size_t lu = 0; lu < left.compound.elems_count; lu++) {
                    if (left.compound.elems[lu].unit ==
                        right.compound.elems[ru].unit) {
                        left.compound.elems[lu].power +=
                            right.compound.elems[ru].power;
                        goto mult_cont;
                    }
                }

                if (left.compound.elems_count + 1 >= COMPOUND_UNIT_CAP) {
                    fprintf(stderr,
                            "ERROR: Exceeded max cap of %d distinct simple "
                            "units in "
                            "compound unit.\n",
                            COMPOUND_UNIT_CAP);
                    exit(1);
                }
                left.compound.elems[left.compound.elems_count++] =
                    right.compound.elems[ru];
            mult_cont:
                continue;
            }

            return left;
        }

        case OP_DIV: {
            assert(left.is_anonymous);
            assert(right.is_anonymous);
            for (size_t ru = 0; ru < right.compound.elems_count; ru++) {
                for (size_t lu = 0; lu < left.compound.elems_count; lu++) {
                    if (left.compound.elems[lu].unit ==
                        right.compound.elems[ru].unit) {
                        left.compound.elems[lu].power -=
                            right.compound.elems[ru].power;
                        goto div_cont;
                    }
                }

                if (left.compound.elems_count + 1 >= COMPOUND_UNIT_CAP) {
                    fprintf(stderr,
                            "ERROR: Exceeded max cap of %d distinct simple "
                            "units in "
                            "compound unit.\n",
                            COMPOUND_UNIT_CAP);
                    exit(1);
                }
                assert(named_unit_is_simple(
                    ctx,
                    ctx->named_units.items[right.compound.elems[ru].unit]));
                left.compound.elems[left.compound.elems_count++] =
                    (SimpleUnitPow) {
                        .power = -right.compound.elems[ru].power,
                        .unit  = right.compound.elems[ru].unit,
                    };
            div_cont:
                continue;
            }

            return left;
        }

        default:
            fprintf(stderr, "ERROR: Illegal operation `");
            op_print(stderr, expr.as.binop.op);
            fprintf(stderr, "` in unit expression.\n");
            exit(1);
        }
    }

    case EXPR_TYPE_PAREN:
        return resolve_unit_from_expr(ctx, *expr.as.paren.inner);

    default:
        fprintf(stderr, "ERROR: Can't resolve compound unit from expr: ");
        expr_print(stderr, expr);
        fprintf(stderr, ".\n");
        exit(1);
    }
}

/// Intended to only be used for compound dimensions
static Dim resolve_dim_from_expr(EvalContext *ctx, Expr expr)
{
    switch (expr.type) {
    case EXPR_TYPE_VAR: {
        return (Dim) {
            .name        = SV(""),
            .is_compound = true,
            .as.compound =
                {
                    .elems_count = 1,
                    .elems[0] =
                        {
                            .dim   = resolve_dim_by_name(ctx, expr.as.var.name),
                            .power = 1,
                        },
                },
        };
    }

    case EXPR_TYPE_BINOP: {
        Dim left  = resolve_dim_from_expr(ctx, *expr.as.binop.left);
        Dim right = resolve_dim_from_expr(ctx, *expr.as.binop.right);

        assert(left.is_compound);
        assert(right.is_compound);

        switch (expr.as.binop.op) {
        case OP_MULT: {
            for (size_t rd = 0; rd < right.as.compound.elems_count; rd++) {
                for (size_t ld = 0; ld < left.as.compound.elems_count; ld++) {
                    if (left.as.compound.elems[ld].dim ==
                        right.as.compound.elems[rd].dim) {
                        left.as.compound.elems[ld].power +=
                            right.as.compound.elems[rd].power;
                        goto mult_cont;
                    }
                }

                if (left.as.compound.elems_count + 1 >= COMPOUND_DIM_CAP) {
                    fprintf(stderr,
                            "ERROR: Exceeded max cap of %d distinct simple "
                            "dimensions in "
                            "compound dimension.\n",
                            COMPOUND_DIM_CAP);
                    exit(1);
                }
                left.as.compound.elems[left.as.compound.elems_count++] =
                    right.as.compound.elems[rd];
            mult_cont:
                continue;
            }

            return left;
        }

        case OP_DIV: {
            for (size_t ru = 0; ru < right.as.compound.elems_count; ru++) {
                for (size_t lu = 0; lu < left.as.compound.elems_count; lu++) {
                    if (left.as.compound.elems[lu].dim ==
                        right.as.compound.elems[ru].dim) {
                        left.as.compound.elems[lu].power -=
                            right.as.compound.elems[ru].power;
                        goto div_cont;
                    }
                }

                if (left.as.compound.elems_count + 1 >= COMPOUND_DIM_CAP) {
                    fprintf(stderr,
                            "ERROR: Exceeded max cap of %d distinct simple "
                            "dimensions in "
                            "compound dimension.\n",
                            COMPOUND_DIM_CAP);
                    exit(1);
                }
                left.as.compound.elems[left.as.compound.elems_count].power =
                    -right.as.compound.elems[ru].power;
                left.as.compound.elems[left.as.compound.elems_count].dim =
                    right.as.compound.elems[ru].dim;
                left.as.compound.elems_count++;
            div_cont:
                continue;
            }

            return left;
        }

        default:
            fprintf(stderr, "ERROR: Illegal operation `");
            op_print(stderr, expr.as.binop.op);
            fprintf(stderr, "` in dimension expression.\n");
            exit(1);
        }
    }

    case EXPR_TYPE_PAREN:
        return resolve_dim_from_expr(ctx, *expr.as.paren.inner);

    default:
        fprintf(stderr, "ERROR: Can't resolve compound unit from expr: ");
        expr_print(stderr, expr);
        fprintf(stderr, ".\n");
        exit(1);
    }
}

static Value value_binop(EvalContext *ctx, Value left, Value right, Op op)
{
    switch (op) {
    case OP_MULT: {
        if (!left.unit.is_anonymous) {
            left.unit.compound =
                coerce_named_unit_index_to_compound(ctx, left.unit.named);
            left.unit.is_anonymous = true;
        }
        if (!right.unit.is_anonymous) {
            right.unit.compound =
                coerce_named_unit_index_to_compound(ctx, right.unit.named);
            right.unit.is_anonymous = true;
        }

        for (size_t ru = 0; ru < right.unit.compound.elems_count; ru++) {
            for (size_t lu = 0; lu < left.unit.compound.elems_count; lu++) {
                if (left.unit.compound.elems[lu].unit ==
                    right.unit.compound.elems[ru].unit) {
                    left.unit.compound.elems[lu].power +=
                        right.unit.compound.elems[ru].power;
                    goto cont;
                }
            }

            if (left.unit.compound.elems_count + 1 >= COMPOUND_UNIT_CAP) {
                fprintf(
                    stderr,
                    "ERROR: Exceeded max cap of %d distinct simple units in "
                    "compound unit.\n",
                    COMPOUND_UNIT_CAP);
                exit(1);
            }
            left.unit.compound.elems[left.unit.compound.elems_count++] =
                right.unit.compound.elems[ru];
        cont:
            continue;
        }

        left.num *= right.num;

        // TODO: A proper way of cleaning up units (extracted into function and
        // called everywhere we mess with units)
        if (left.unit.is_anonymous && left.unit.compound.elems_count == 1 &&
            left.unit.compound.elems[0].power == 1) {
            left.unit = (Unit) {
                .is_anonymous = false,
                .named        = left.unit.compound.elems[0].unit,
            };
        }
        return left;
    }

    case OP_DIV: {
        if (!left.unit.is_anonymous) {
            left.unit.compound =
                coerce_named_unit_index_to_compound(ctx, left.unit.named);
            left.unit.is_anonymous = true;
        }
        if (!right.unit.is_anonymous) {
            right.unit.compound =
                coerce_named_unit_index_to_compound(ctx, right.unit.named);
            right.unit.is_anonymous = true;
        }

        // a / b = a * (1/b)
        right.num = 1 / right.num;
        for (size_t i = 0; i < right.unit.compound.elems_count; i++) {
            right.unit.compound.elems[i].power =
                -right.unit.compound.elems[i].power;
        }

        return value_binop(ctx, left, right, OP_MULT);
    }

    default:
        fprintf(stderr, "ERROR: Don't know how to perform operation: ");
        op_print(stderr, op);
        fputc('\n', stderr);
        exit(1);
    }
}

static int named_unit_pow_print(EvalContext *ctx, SimpleUnitPow sup)
{
    int printed = 0;
    printed += printf(SV_FMT, SV_ARG(ctx->named_units.items[sup.unit].name));
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
    if (!value.unit.is_anonymous) {
        printed += printf(
            " " SV_FMT, SV_ARG(ctx->named_units.items[value.unit.named].name));
    } else {
        assert(value.unit.compound.elems_count > 0);
        printed += printf(" ");
        printed += named_unit_pow_print(ctx, value.unit.compound.elems[0]);
        for (size_t i = 1; i < value.unit.compound.elems_count; i++) {
            printed += printf(" * ");
            printed += named_unit_pow_print(ctx, value.unit.compound.elems[i]);
        }
    }
    printed += printf("\n");
    return (Value) {
        .num  = (double) printed,
        .unit = {0},
    };
}

/// Checks if a compound unit has any units that are not fundamental
static bool compound_unit_is_fundamental(EvalContext *ctx, CompoundUnit unit)
{
    for (size_t i = 0; i < unit.elems_count; i++) {
        Dim dim =
            ctx->dims.items[ctx->named_units.items[unit.elems[i].unit].dim];
        // Since the compound unit should only be composed of simple units,
        // pointing to simple dimensions
        assert(!dim.is_compound);
        if (dim.as.simple.fundamental_unit != unit.elems[i].unit) return false;
    }
    return true;
}

static bool unit_is_fundamental(EvalContext *ctx, Unit unit)
{
    return unit.is_anonymous
               ? compound_unit_is_fundamental(ctx, unit.compound)
               : named_unit_is_fundamental(ctx->named_units.items[unit.named]);
}

static bool compound_unit_is_castable(EvalContext *ctx, CompoundUnit a,
                                      CompoundUnit b)
{
    // Skip DIMLESS
    for (DimIndex dim = 1; dim < ctx->dims.count; dim++) {
        Power power_a = 0;
        for (size_t j = 0; j < a.elems_count; j++) {
            if (ctx->named_units.items[a.elems[j].unit].dim == dim) {
                power_a += a.elems[j].power;
            }
        }

        Power power_b = 0;
        for (size_t j = 0; j < b.elems_count; j++) {
            if (ctx->named_units.items[b.elems[j].unit].dim == dim) {
                power_b += b.elems[j].power;
            }
        }

        if (power_a != power_b) return false;
    }

    return true;
}

static bool unit_is_castable(EvalContext *ctx, Unit a, Unit b)
{
    if (!a.is_anonymous) {
        a.compound     = coerce_named_unit_index_to_compound(ctx, a.named);
        a.is_anonymous = true;
    }
    if (!b.is_anonymous) {
        b.compound     = coerce_named_unit_index_to_compound(ctx, b.named);
        b.is_anonymous = true;
    }

    return compound_unit_is_castable(ctx, a.compound, b.compound);
}

static double conversion_factor_to_fundamental_simple(EvalContext *ctx,
                                                      NamedUnit unit)
{
    assert(named_unit_is_simple(ctx, unit));
    if (named_unit_is_fundamental(unit)) return 1;

    double conv_factor = unit.val.num;
    assert(conv_factor != 0);  // Unit should not be fundamental

    if (unit.val.unit.is_anonymous) {
        fprintf(stderr,
                "ERROR: Assertion failed: Expected compound unit, got: ");
        named_unit_dump(stderr, ctx, unit);
        fprintf(stderr, "\n");
        exit(1);
    }
    assert(!unit.val.unit.is_anonymous);
    NamedUnit current_unit = ctx->named_units.items[unit.val.unit.named];

    for (int i = 0; !named_unit_is_fundamental(current_unit); i++) {
        if (i >= UNIT_CAST_MAX_DEPTH) {
            fprintf(stderr,
                    "ERROR: Exceeded max depth of %d while trying "
                    "to cast to/from unit `" SV_FMT "`.\n",
                    UNIT_CAST_MAX_DEPTH, SV_ARG(unit.name));
            exit(1);
        }

        assert(named_unit_is_simple(ctx, current_unit));
        Value next = current_unit.val;
        assert(!next.unit.is_anonymous);

        conv_factor *= next.num;
        current_unit = ctx->named_units.items[next.unit.named];
    }
    return conv_factor;
}

static double conversion_factor_to_fundamental_compound(EvalContext *ctx,
                                                        CompoundUnit unit)
{
    double conv_factor = 1;
    for (size_t i = 0; i < unit.elems_count; i++) {
        double simple_factor = conversion_factor_to_fundamental_simple(
            ctx, ctx->named_units.items[unit.elems[i].unit]);
        conv_factor *= pow(simple_factor, unit.elems[i].power);
    }

    return conv_factor;
}

static double conversion_factor_to_fundamental(EvalContext *ctx, Unit unit)
{
    if (unit.is_anonymous) {
        return conversion_factor_to_fundamental_compound(ctx, unit.compound);
    }

    if (named_unit_is_simple(ctx, ctx->named_units.items[unit.named])) {
        return conversion_factor_to_fundamental_simple(
            ctx, ctx->named_units.items[unit.named]);
    }

    return ctx->named_units.items[unit.named].val.num *
           conversion_factor_to_fundamental_compound(
               ctx, coerce_named_unit_index_to_compound(ctx, unit.named));
}

Value eval_expr(EvalContext *ctx, Expr expr)
{
    switch (expr.type) {
    case EXPR_TYPE_NUM: {
        double num = expr.as.num.val;
        // TODO: Support compound unit literals
        NamedUnitIndex nui = (sv_is_empty(expr.as.num.unit))
                                 ? UNITLESS
                                 : resolve_unit_by_name(ctx, expr.as.num.unit);

        return (Value) {
            .num = num,
            .unit =
                {
                    .is_anonymous = false,
                    .named        = nui,
                },
        };
    }

    case EXPR_TYPE_BINOP: {
        Value left  = eval_expr(ctx, *expr.as.binop.left);
        Value right = eval_expr(ctx, *expr.as.binop.right);

        return value_binop(ctx, left, right, expr.as.binop.op);
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

        // TODO: Better error message on dimensionality mismatch (specify the
        // names of both dimensions)
        if (ctx->vars.items[idx].initialised &&
            !unit_is_castable(ctx, ctx->vars.items[idx].value.unit, rhs.unit)) {
            fprintf(stderr,
                    "ERROR: Cannot assign to var `" SV_FMT
                    "`: Dimensionality mismatch.\n",
                    SV_ARG(expr.as.assign.lhs));
            exit(1);
        }

        ctx->vars.items[idx].value       = rhs;
        ctx->vars.items[idx].initialised = true;
        return rhs;
    }

    case EXPR_TYPE_FUNCALL: {
        if (sv_eq(expr.as.funcall.fun, SV("print"))) {
            if (expr.as.funcall.args.count == 0) {
                fprintf(stderr,
                        "ERROR: `print`: No argument provided "
                        "(expected 1).\n");
                exit(1);
            }
            if (expr.as.funcall.args.count > 1) {
                fprintf(stderr,
                        "ERROR: `print`: Too many arguments provided "
                        "(got %zu, "
                        "expected 1).\n",
                        expr.as.funcall.args.count);
                exit(1);
            }

            Value input = eval_expr(ctx, expr.as.funcall.args.items[0]);
            return val_print(ctx, input);
        } else if (sv_eq(expr.as.funcall.fun, SV("dump"))) {
            if (expr.as.funcall.args.count == 0) {
                eval_context_dump(stdout, ctx);
                return (Value) {0};
            }
            if (expr.as.funcall.args.count > 1) {
                fprintf(stderr,
                        "ERROR: `dump`: Too many arguments provided "
                        "(got %zu, "
                        "expected 1).\n",
                        expr.as.funcall.args.count);
                exit(1);
            }

            Expr inner = expr.as.funcall.args.items[0];
            if (inner.type == EXPR_TYPE_VAR) {
                var_dump(
                    stdout, ctx,
                    ctx->vars
                        .items[resolve_var_by_name(ctx, inner.as.var.name)]);
            }

            Value input = eval_expr(ctx, inner);
            printf("value: ");
            return val_print(ctx, input);
        } else {
            fprintf(stderr,
                    "ERROR: TODO: Function calls are not implemented "
                    "yet.\n");
            fprintf(stderr,
                    "NOTE: Only the following standard functions are "
                    "implemented: \n");
            fprintf(stderr,
                    "\tprint - prints a value (intended for regular "
                    "printing)\n");
            fprintf(stderr,
                    "\tdump  - dumps a value/variable (intended for "
                    "debugging "
                    "the interpreter)\n");
            exit(1);
        }
    }

    case EXPR_TYPE_UNITCAST: {
        Value val   = eval_expr(ctx, *expr.as.unit_cast.value);
        Unit source = val.unit;
        Unit target = resolve_unit_from_expr(ctx, *expr.as.unit_cast.target);

        if (!unit_is_castable(ctx, source, target)) {
            fprintf(stderr, "ERROR: Cannot cast a value of unit `");
            unit_dump(stderr, ctx, source);
            fprintf(stderr, "` to unit `");
            unit_dump(stderr, ctx, target);
            fprintf(stderr, "`: Dimensionality mismatch.\n");
            exit(1);
        }

        // TODO: Support units that aren't just pure multiplication
        // (e.g. Fahrenheit/Celsius)
        double conv_factor = 1;
        if (!unit_is_fundamental(ctx, target)) {
            conv_factor /= conversion_factor_to_fundamental(ctx, target);
        }
        if (!unit_is_fundamental(ctx, source)) {
            conv_factor *= conversion_factor_to_fundamental(ctx, source);
        }

        return (Value) {
            .num  = val.num * conv_factor,
            .unit = target,
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
        Dim dim = {0};
        if (stmt.as.dim_decl.expr.type != EXPR_TYPE_NONE) {
            dim = resolve_dim_from_expr(ctx, stmt.as.dim_decl.expr);
            assert(dim.is_compound);
            assert(dim.as.compound.elems_count != 0);
            assert(dim.as.compound.elems_count > 1 ||
                   dim.as.compound.elems[0].power > 1);
        }

        dim.name = stmt.as.dim_decl.name;

        if (try_resolve_dim_by_name(ctx, dim.name, NULL)) {
            // TODO: Better (located) error reporting
            fprintf(stderr,
                    "ERROR: A dimension with name `" SV_FMT
                    "` already exists.\n",
                    SV_ARG(dim.name));
            fprintf(stderr, "context: ");
            eval_context_dump(stderr, ctx);
            exit(1);
        }

        da_append(&ctx->dims, dim);
        return;
    }

    case STMT_TYPE_UNIT_DECL: {
        DimIndex dim = resolve_dim_by_name(ctx, stmt.as.unit_decl.dim);

        NamedUnit unit = {
            .name = stmt.as.unit_decl.name,
            .dim  = dim,
            .val  = stmt.as.unit_decl.value.type != EXPR_TYPE_NONE
                        ? eval_expr(ctx, stmt.as.unit_decl.value)
                        : (Value) {0},
        };
        if (try_resolve_unit_by_name(ctx, unit.name, NULL)) {
            // TODO: Better (located) error reporting
            fprintf(stderr,
                    "ERROR: A unit with name `" SV_FMT "` already exists.\n",
                    SV_ARG(unit.name));
            fprintf(stderr, "context: ");
            eval_context_dump(stderr, ctx);
            exit(1);
        }

        // Trying to define a fundamental unit
        if (named_unit_is_fundamental(unit)) {
            Dim *dim = &ctx->dims.items[unit.dim];
            if (dim->is_compound) {
                // TODO: This doesn't actually verify that the concerned
                // dimension has an implicit fundamental unit (i.e. it could be
                // possible that one of the component units has no fundamental
                // unit, or that the user intends that dimension's unit to be
                // implicitly defined through the definition of this fundamental
                // unit
                // TODO: Compute fundamental unit for compound dimensions
                fprintf(
                    stderr,
                    "ERROR: Dimension `" SV_FMT
                    "` is a compound (derived) dimension and thus already has "
                    "an implicit fundamental unit. The unit `" SV_FMT
                    "` should be expressed in terms of simpler units.\n",
                    SV_ARG(dim->name), SV_ARG(unit.name));
                exit(1);
            }

            // TODO: Add assertions/errors that prevent a dimension without an
            // instantiated fundamental unit from being used in variables
            if (dim->as.simple.fundamental_unit != UNITLESS) {
                fprintf(stderr,
                        "ERROR: Simple dimension `" SV_FMT
                        "` already has a fundamental unit `" SV_FMT
                        "`. The unit `" SV_FMT
                        "` must be expressed in terms of the fundamental "
                        "unit.\n",
                        SV_ARG(dim->name),
                        SV_ARG(ctx->named_units
                                   .items[dim->as.simple.fundamental_unit]
                                   .name),
                        SV_ARG(unit.name));
                exit(1);
            }

            dim->as.simple.fundamental_unit = ctx->named_units.count;
        }

        da_append(&ctx->named_units, unit);

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
            eval_context_dump(stderr, ctx);
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

EvalContext eval_context_new(void)
{
    EvalContext ctx   = {0};
    Dim dimensionless = {
        .name = SV("(dimensionless)"),
    };
    da_append(&ctx.dims, dimensionless);

    NamedUnit unitless = {
        .name = SV("(unitless)"),
        .dim  = 0,
        .val  = {0},
    };
    da_append(&ctx.named_units, unitless);

    return ctx;
}

void eval_context_destroy(EvalContext *ctx)
{
    free(ctx->named_units.items);
    free(ctx->dims.items);
    free(ctx->vars.items);
    *ctx = (EvalContext) {0};
}

void dim_dump(FILE *f, EvalContext *ctx, Dim d)
{
    if (d.is_compound) {
        // Otherwise, it should be a simple dimension
        assert(d.as.compound.elems_count > 0);

        fprintf(f, "`" SV_FMT "` = [", SV_ARG(d.name));
        fprintf(f, "(`" SV_FMT "`, %" POWER_FMT ")",
                SV_ARG(ctx->dims.items[d.as.compound.elems[0].dim].name),
                d.as.compound.elems[0].power);
        for (size_t i = 1; i < d.as.compound.elems_count; i++) {
            fprintf(f, ", (`" SV_FMT "`, %" POWER_FMT ")",
                    SV_ARG(ctx->dims.items[d.as.compound.elems[i].dim].name),
                    d.as.compound.elems[i].power);
        }
        fprintf(f, "]");
    } else {
        fprintf(
            f, "`" SV_FMT "` (`" SV_FMT "`)", SV_ARG(d.name),
            SV_ARG(ctx->named_units.items[d.as.simple.fundamental_unit].name));
    }
}

void named_unit_dump(FILE *f, EvalContext *ctx, NamedUnit u)
{
    fprintf(f, "`" SV_FMT "` ", SV_ARG(u.name));
    fprintf(f, "(`" SV_FMT "`)", SV_ARG(ctx->dims.items[u.dim].name));
    if (!named_unit_is_fundamental(u)) {
        fprintf(f, " = ");
        val_dump(f, ctx, u.val);
    }
}

void compound_unit_dump(FILE *f, EvalContext *ctx, CompoundUnit u)
{
    for (size_t i = 0; i < u.elems_count; i++) {
        fprintf(f, "(`" SV_FMT "`, %" POWER_FMT ")",
                SV_ARG(ctx->named_units.items[u.elems[i].unit].name),
                u.elems[i].power);
    }
}

void unit_dump(FILE *f, EvalContext *ctx, Unit u)
{
    u.is_anonymous ? compound_unit_dump(f, ctx, u.compound)
                   : named_unit_dump(f, ctx, ctx->named_units.items[u.named]);
}

void val_dump(FILE *f, EvalContext *ctx, Value v)
{
    fprintf(f, "%f ", v.num);
    unit_dump(f, ctx, v.unit);
}

void var_dump(FILE *f, EvalContext *ctx, Var v)
{
    fprintf(f, "`" SV_FMT "`", SV_ARG(v.name));
    if (v.initialised) {
        fprintf(f, " = ");
        val_dump(f, ctx, v.value);
    }
}

// TODO: Improve debugging apparatus
void eval_context_dump(FILE *f, EvalContext *ctx)
{
    fprintf(f, "EvalContext {\n");
    if (ctx->named_units.count > 0) {
        fprintf(f, "\tunits: [\n");
        fprintf(f, "\t\t[0] = ");
        named_unit_dump(f, ctx, ctx->named_units.items[0]);
        for (size_t i = 1; i < ctx->named_units.count; i++) {
            fprintf(f, ",\n");
            fprintf(f, "\t\t[%zu] = ", i);
            named_unit_dump(f, ctx, ctx->named_units.items[i]);
        }
        fprintf(f, "\n\t],\n");
    }
    if (ctx->dims.count > 0) {
        fprintf(f, "\tdims: [\n");
        fprintf(f, "\t\t[0] = ");
        dim_dump(f, ctx, ctx->dims.items[0]);
        for (size_t i = 1; i < ctx->dims.count; i++) {
            fprintf(f, ",\n");
            fprintf(f, "\t\t[%zu] = ", i);
            dim_dump(f, ctx, ctx->dims.items[i]);
        }
        fprintf(f, "\n\t],\n");
    }
    if (ctx->vars.count > 0) {
        fprintf(f, "\tvars: [\n");
        fprintf(f, "\t\t[0] = ");
        var_dump(f, ctx, ctx->vars.items[0]);
        for (size_t i = 1; i < ctx->vars.count; i++) {
            fprintf(f, ",\n");
            fprintf(f, "\t\t[%zu] = ", i);
            var_dump(f, ctx, ctx->vars.items[i]);
        }
        fprintf(f, "\n\t],\n");
    }
    fprintf(f, "}\n");
}

