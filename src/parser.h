#ifndef PARSER_H_
#define PARSER_H_

#include "lexer.h"

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
 * dimDecl = "dim" NAME ";"
 *
 * unitDecl = "unit" NAME ":" NAME ("=" expr)? ";"
 *
 * varDecl = "let" NAME ":" NAME ("=" expr)? ";"
 *
 * expr = assignmentExpr
 *
 * assignmentExpr = NAME "=" expr | geomExpr
 *
 * geomExpr = funcallExpr (("*" | "/") expr)?
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
            // TODO: Declare dimensions in terms of other dimensions
            StringView name;
        } dim_decl;

        struct {
            StringView dim;
            StringView name;

            Expr value;
        } unit_decl;

        // TODO: Does this mean that variables and units are equivalent?
        struct {
            StringView dim;
            StringView name;

            Expr value;
        } var_decl;

        Expr expr;
    } as;
} Stmt;

void stmt_print(FILE *f, Stmt stmt);

#define PARSER_EXPRS_CAP 256

typedef struct {
    Lexer lexer;

    /// Buffer of expressions that outputted Exprs might have pointers to
    // TODO: Is there a better way to implement this?
    Expr exprs[PARSER_EXPRS_CAP];
    size_t exprs_count;
} Parser;

Expr parse_expr(Parser *self);
Stmt parse_stmt(Parser *self);

#endif  // PARSER_H_
