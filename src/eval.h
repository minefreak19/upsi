#ifndef EVAL_H_
#define EVAL_H_

#include "parser.h"

typedef size_t UnitIndex, DimIndex, VarIndex;

/// Dimensionless and Unitless are always first in the evaluation context
#define DIMLESS ((DimIndex) 0)
#define UNITLESS ((UnitIndex) 0)

typedef struct {
    StringView name;
    UnitIndex fundamental_unit;
} Dim;

typedef struct {
    StringView name;
    DimIndex dim;

    // This should have type EXPR_TYPE_NONE for a fundamental unit, and should
    // otherwise be a mathematical expression in terms of a different unit. The
    // evaluator should error if the units don't match, and a check should be
    // put in place so that it doesn't end up going in circles in the event of a
    // cyclic definition of units
    Expr expr;
} Unit;

typedef struct {
    double num;
    UnitIndex unit;
} Value;

typedef struct {
    StringView name;

    // TODO: Do we need to keep track of this?
    DimIndex dim;
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
        Unit *items;
        size_t count;
        size_t capacity;
    } units;

    struct {
        Var *items;
        size_t count;
        size_t capacity;
    } vars;
} EvalContext;

EvalContext new_context(void);
void free_context(EvalContext *ctx);
void eval_stmt(EvalContext *ctx, Stmt stmt);
void dump_context(FILE *f, EvalContext *ctx);

#endif  // EVAL_H_
