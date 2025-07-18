#ifndef EVAL_H_
#define EVAL_H_

#include <inttypes.h>

#include "parser.h"

/*
 * The definitions in this file are in a somewhat strange and disorganised
 * order, because different structures here have weird dependency relations with
 * others. It might be possible to clean up the definitions in the future.
 */
typedef size_t SimpleUnitIndex, DimIndex, VarIndex;

/// Dimensionless and Unitless are always first in the evaluation context
#define DIMLESS ((DimIndex) 0)
#define UNITLESS ((SimpleUnitIndex) 0)

typedef int32_t Power;
#define POWER_FMT PRIi32

// TODO: Remove COMPOUND_DIM_CAP and COMPOUND_UNIT_CAP (make these arrays
// dynamic)
// TODO: Support for anonymous dimensions

/// Maximum number of distinct SimpleDims that can be composed in a compound
/// unit
#define COMPOUND_DIM_CAP 128

typedef struct {
    StringView name;
    bool is_compound;
    union {
        struct {
            SimpleUnitIndex fundamental_unit;
        } simple;

        struct {
            size_t elems_count;
            struct {
                DimIndex dim;
                Power power;
            } elems[COMPOUND_DIM_CAP];
        } compound;
    } as;
} Dim;

/// Shouldn't be used directly; encodes a simple unit raised to a particular
/// power, as will be needed for variables with arbitrarily complex units
typedef struct {
    SimpleUnitIndex unit;
    Power power;
} SimpleUnitPow;

/// Maximum number of distinct SimpleUnits that can be composed in a compound
/// unit
#define COMPOUND_UNIT_CAP COMPOUND_DIM_CAP

// TODO: The current implementation of compound units is memory intensive when
// many values have the same compound unit. Can this be improved?
typedef struct {
    SimpleUnitPow elems[COMPOUND_UNIT_CAP];
    size_t elems_count;
} CompoundUnit;

typedef struct {
    double num;
    CompoundUnit unit;
} Value;

typedef struct {
    StringView name;
    DimIndex dim;

    // This should be (Value) {0} for a fundamental unit, and the computed value
    // in terms of a different unit otherwise
    Value val;
} SimpleUnit;

typedef struct {
    StringView name;

    // TODO: Can this flag be NaN-boxed into `value`?
    bool initialised;
    Value value;
} Var;

typedef struct {
    // TODO: Turn this into a stack of scopes

    /// dims.items[0] is special and always refers to the "dimensionless"
    /// dimension
    struct {
        Dim *items;
        size_t count;
        size_t capacity;
    } dims;

    /// units.items[0] is special and always refers to the "unitless" unit
    struct {
        SimpleUnit *items;
        size_t count;
        size_t capacity;
    } simple_units;

    struct {
        Var *items;
        size_t count;
        size_t capacity;
    } vars;
} EvalContext;

EvalContext new_context(void);
void free_context(EvalContext *ctx);
Value eval_expr(EvalContext *ctx, Expr expr);
void eval_stmt(EvalContext *ctx, Stmt stmt);
void dump_context(FILE *f, EvalContext *ctx);

#endif  // EVAL_H_
