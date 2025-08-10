#ifndef PARSER_H_
#define PARSER_H_

#include "lexer.h"

// TODO: Document (& enforce?) the particular subset of expressions that can be
// used to represent units
// TODO: Add a way to automatically add metric prefixes to a unit through an
// annotation
/*
 * GRAMMAR (recursive descent)
 * =======
 *
 * program = [statement]
 *
 * statement = dimDecl | unitDecl | varDecl | expr ";"
 *
 * dimDecl = "dim" NAME ("=" expr)? ";"
 *
 * unitDecl = "unit" NAME ":" NAME ("=" expr)? ";"
 *
 * varDecl = "let" NAME ("=" expr)? ";"
 *
 * expr = assignmentExpr
 *
 * assignmentExpr = NAME "=" expr | unitCastExpr
 *
 * unitCastExpr = geomExpr ("in" expr)?
 *
 * geomExpr = funcallExpr (("*" | "/") geomExpr)?
 *
 * funcallExpr = NAME "(" [expr ","]? ")" | primaryExpr
 *
 * primaryExpr = "(" expr ")" | NUM NAME? | NAME
 */

// TODO: Add file locations to Expr and Stmt for error reporting in interpreter
typedef enum {
    EXPR_TYPE_NONE = 0,

    EXPR_TYPE_NUM,
    EXPR_TYPE_VAR,
    EXPR_TYPE_PAREN,
    EXPR_TYPE_BINOP,
    EXPR_TYPE_ASSIGN,
    EXPR_TYPE_FUNCALL,
    // TODO: Work out whether this should only be a part of `print` semantics,
    // since we intend variables of the same dimension to be automatically
    // assigned to each other without explicit casting.
    EXPR_TYPE_UNITCAST,

    EXPR_TYPE__COUNT,
} ExprType;

typedef enum {
    OP_NONE = 0,

    OP_MULT,
    OP_DIV,

    OP__COUNT,
} Op;

typedef struct Exprs {
    struct Expr *items;
    size_t count;
    size_t capacity;
} Exprs;

typedef struct Expr {
    ExprType type;
    union {
        struct {
            double val;
            /// Unitless quantities should have unit = SV("")
            StringView unit;
        } num;

        struct {
            StringView name;
        } var;

        struct {
            struct Expr *inner;
        } paren;

        struct {
            struct Expr *left;
            struct Expr *right;
            Op op;
        } binop;

        // TODO: Is assignment really just a binary operation? Can this be
        // achieved by implementing pattern matching?
        struct {
            StringView lhs;
            struct Expr *rhs;
        } assign;

        struct {
            StringView fun;

            Exprs args;
        } funcall;

        struct {
            struct Expr *value;

            /// This is either a name or an algebraic representation of the
            /// relevant compound unit
            struct Expr *target;
        } unit_cast;
    } as;
} Expr;

typedef enum {
    STMT_TYPE_NONE = 0,

    STMT_TYPE_DIM_DECL,
    STMT_TYPE_UNIT_DECL,
    STMT_TYPE_VAR_DECL,
    STMT_TYPE_EXPR,

    STMT_TYPE__COUNT
} StmtType;

typedef struct {
    StmtType type;
    union {
        struct {
            StringView name;

            Expr expr; 
        } dim_decl;

        struct {
            StringView dim;
            StringView name;

            Expr value;
        } unit_decl;

        // TODO: Does this mean that variables and units are equivalent?
        struct {
            StringView name;

            Expr value;
        } var_decl;

        Expr expr;
    } as;
} Stmt;

void op_print(FILE *f, Op op);
void expr_print(FILE *f, Expr expr);
void expr_free(Expr expr);
void stmt_print(FILE *f, Stmt stmt);
void stmt_free(Stmt stmt);

typedef struct {
    Lexer lexer;
} Parser;

Expr parse_expr(Parser *self);
Stmt parse_stmt(Parser *self);

#endif  // PARSER_H_
