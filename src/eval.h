#ifndef EVAL_H_
#define EVAL_H_

#include <inttypes.h>

#include "parser.h"

/*
 * The definitions in this file are in a somewhat strange and disorganised
 * order, because different structures here have weird dependency relations with
 * others. It might be possible to clean up the definitions in the future.
 */
typedef size_t NamedUnitIndex, DimIndex, VarIndex;

/// Dimensionless and Unitless are always first in the evaluation context
#define DIMLESS ((DimIndex) 0)
#define UNITLESS ((NamedUnitIndex) 0)

typedef int32_t Power;
#define POWER_FMT PRIi32

// TODO: Remove COMPOUND_DIM_CAP and COMPOUND_UNIT_CAP (make these arrays
// dynamic)
// TODO: Support for anonymous dimensions

/// Maximum number of distinct simple dims that can be composed in a compound
/// dimension
#define COMPOUND_DIM_CAP 128

typedef struct {
    StringView name;
    bool is_compound;
    union {
        struct {
            NamedUnitIndex fundamental_unit;
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
    /// This should always point to a simple unit
    NamedUnitIndex unit;
    Power power;
} SimpleUnitPow;

/// Maximum number of distinct simple units that can be composed in a compound
/// unit
#define COMPOUND_UNIT_CAP COMPOUND_DIM_CAP

typedef struct {
    SimpleUnitPow elems[COMPOUND_UNIT_CAP];
    size_t elems_count;
} CompoundUnit;

// NOTE: a (Unit) {0} should always create a clearly UNITLESS unit, rather than
// some invalid state or edge case
typedef struct {
    bool is_anonymous;
    union {
        CompoundUnit compound;  // if is_anonymous is true
        NamedUnitIndex named;
    };
} Unit;

typedef struct {
    double num;
    Unit unit;
} Value;

typedef struct {
    StringView name;
    DimIndex dim;

    // This should be (Value) {0} for a fundamental unit, and the computed value
    // in terms of a different unit otherwise
    Value val;
} NamedUnit;

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
        NamedUnit *items;
        size_t count;
        size_t capacity;
    } named_units;

    struct {
        Var *items;
        size_t count;
        size_t capacity;
    } vars;
} EvalContext;

EvalContext eval_context_new(void);
void eval_context_destroy(EvalContext *ctx);

Value eval_expr(EvalContext *ctx, Expr expr);
void eval_stmt(EvalContext *ctx, Stmt stmt);

void dim_dump(FILE *f, EvalContext *ctx, Dim d);
void named_unit_dump(FILE *f, EvalContext *ctx, NamedUnit u);
void compound_unit_dump(FILE *f, EvalContext *ctx, CompoundUnit u);
void unit_dump(FILE *f, EvalContext *ctx, Unit u);
void val_dump(FILE *f, EvalContext *ctx, Value v);
void var_dump(FILE *f, EvalContext *ctx, Var v);
void eval_context_dump(FILE *f, EvalContext *ctx);

#endif  // EVAL_H_
