#ifndef EVAL_H_
#define EVAL_H_

#include "parser.h"

typedef size_t SimpleUnitIndex, DimIndex, VarIndex;

/// Dimensionless and Unitless are always first in the evaluation context
#define DIMLESS ((DimIndex) 0)
#define UNITLESS ((SimpleUnitIndex) 0)

typedef struct {
    StringView name;
    SimpleUnitIndex fundamental_unit;
} Dim;

typedef struct {
    StringView name;
    DimIndex dim;

    // This should have type EXPR_TYPE_NONE for a fundamental unit, and should
    // otherwise be a mathematical expression in terms of a different unit. 
    Expr expr;
} SimpleUnit;

typedef struct {
    double num;
    SimpleUnitIndex unit;
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
void eval_stmt(EvalContext *ctx, Stmt stmt);
void dump_context(FILE *f, EvalContext *ctx);

#endif  // EVAL_H_
