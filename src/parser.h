#ifndef PARSER_H_
#define PARSER_H_

#include "lexer.h"

/*
 * GRAMMAR
 * =======
 *
 * program = [statement]
 *
 * statement = dimDecl | unitDecl
 *
 * dimDecl = "dim" NAME ";"
 *
 * unitDecl = "unit" NAME ":" NAME ("=" expr)? ";"
 *
 * expr = geomExpr
 *
 * geomExpr = primaryExpr ("*" | "/") expr
 *
 * primaryExpr = "(" expr ")" | NUM NAME? | NAME
 */

typedef enum {
    EXPR_TYPE_NONE = 0,

    EXPR_TYPE_NUM,
    EXPR_TYPE_PAREN,
    EXPR_TYPE_BINOP,

    EXPR_TYPE__COUNT,
} ExprType;

typedef enum {
    OP_NONE = 0,

    OP_MULT,
    OP_DIV,

    OP__COUNT,
} Op;

typedef struct Expr {
    ExprType type;
    union {
        struct {
            double val;
            /// Unitless quantities should have unit = SV("")
            StringView unit;
        } num;

        struct {
            struct Expr *inner;
        } paren;

        struct {
            struct Expr *left;
            struct Expr *right;
            Op op;
        } binop;
    } as;
} Expr;

typedef enum {
    STMT_TYPE_NONE = 0,

    STMT_TYPE_DIM_DECL,
    STMT_TYPE_UNIT_DECL,

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
