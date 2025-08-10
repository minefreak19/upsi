#include "parser.h"

#include <assert.h>
#include <stdlib.h>

#define NOB_STRIP_PREFIX
#include "lexer.h"
#include "nob.h"

static_assert(
    TOK_TYPE__COUNT == 17,
    "Exhaustive definition of op_from_tok_type with respect to TokenType's");
static_assert(OP__COUNT == 3,
              "Exhaustive definition of op_from_tok_type with respect to Op's");
static Op op_from_tok_type(TokenType type)
{
    switch (type) {
    case TOK_TYPE_STAR:
        return OP_MULT;
    case TOK_TYPE_SLASH:
        return OP_DIV;
    default:
        return OP_NONE;
    }
}

// TODO: Lookup table of humanised names of token types for error messages,
// allowing expected_str to be NULL if not needed to be specified
static void ensure_tok_type(Token tok, TokenType type, const char *expected_str)
{
    if (tok.type != type) {
        loc_print(stderr, tok.loc);
        fprintf(stderr, ": ERROR: Expected %s\n", expected_str);
        exit(1);
    }
}

static Expr *parser_save_expr(Parser *self, Expr expr)
{
    // TODO: Add an optional Arena to Parser for allocating expressions
    (void) self;

    Expr *exprp = malloc(sizeof(Expr));
    if (!exprp) {
        fprintf(stderr, "ERROR: Not enough memory.\n");
        abort();
    }
    *exprp = expr;
    return exprp;
}

static Expr parse_primary_expr(Parser *self)
{
    Token tok = lex_token(&self->lexer);

    if (tok.type == TOK_TYPE_LPAREN) {
        Expr inner    = parse_expr(self);
        Token closing = lex_token(&self->lexer);
        // TODO: Report location of opening parenthesis here
        ensure_tok_type(closing, TOK_TYPE_RPAREN, "closing parenthesis");

        return (Expr) {
            .type           = EXPR_TYPE_PAREN,
            .as.paren.inner = parser_save_expr(self, inner),
        };
    } else if (tok.type == TOK_TYPE_NUM) {
        Expr res = {
            .type        = EXPR_TYPE_NUM,
            .as.num.val  = tok.numval,
            .as.num.unit = SV(""),
        };

        Token next = peek_token(self->lexer);
        if (next.type == TOK_TYPE_NAME) {
            lex_token(&self->lexer);
            res.as.num.unit = next.name;
        }

        return res;
    } else if (tok.type == TOK_TYPE_NAME) {
        return (Expr) {
            .type        = EXPR_TYPE_VAR,
            .as.var.name = tok.name,
        };
    }

    loc_print(stderr, tok.loc);
    fprintf(stderr, ": ERROR: Unexpected primary expression\n");
    exit(1);
}

// TODO: Add tuples
static Expr parse_funcall_expr(Parser *self)
{
    // Similar trick as in parse_assignment_expr() which avoids needing to peek
    // 2 tokens ahead

    Expr primExpr = parse_primary_expr(self);
    Token next    = peek_token(self->lexer);
    if (primExpr.type != EXPR_TYPE_VAR || next.type != TOK_TYPE_LPAREN) {
        return primExpr;
    }
    lex_token(&self->lexer);
    next = peek_token(self->lexer);

    if (next.type == TOK_TYPE_RPAREN) {
        lex_token(&self->lexer);
        return (Expr) {
            .type = EXPR_TYPE_FUNCALL,
            .as.funcall =
                {
                    .fun  = primExpr.as.var.name,
                    .args = {0},
                },
        };
    }

    Exprs args = {0};
    while (true) {
        Expr arg = parse_expr(self);
        da_append(&args, arg);

        next = peek_token(self->lexer);
        if (next.type == TOK_TYPE_COMMA) {
            lex_token(&self->lexer);
            next = peek_token(self->lexer);
        } else if (next.type == TOK_TYPE_RPAREN) {
            lex_token(&self->lexer);
            break;
        } else {
            loc_print(stderr, next.loc);
            fprintf(
                stderr,
                ": ERROR: Expected ',' or ')' after argument to function\n");
            exit(1);
        }
    }

    return (Expr) {
        .type = EXPR_TYPE_FUNCALL,
        .as.funcall =
            {
                .fun  = primExpr.as.var.name,
                .args = args,
            },
    };
}

static Expr parse_geom_expr(Parser *self)
{
    Expr left = parse_funcall_expr(self);

    Token next = peek_token(self->lexer);
    Op op      = op_from_tok_type(next.type);
    if (op == OP_NONE) {
        return left;
    }

    lex_token(&self->lexer);
    Expr right = parse_geom_expr(self);

    return (Expr) {
        .type = EXPR_TYPE_BINOP,
        .as.binop =
            {
                .op    = op,
                .left  = parser_save_expr(self, left),
                .right = parser_save_expr(self, right),
            },
    };
}

static Expr parse_unit_cast_expr(Parser *self)
{
    Expr value = parse_geom_expr(self);

    Token next = peek_token(self->lexer);
    if (next.type != TOK_TYPE_KEYWORD_IN) {
        return value;
    }
    lex_token(&self->lexer);

    Expr unit = parse_expr(self);

    return (Expr) {
        .type = EXPR_TYPE_UNITCAST,
        .as.unit_cast =
            {
                .value  = parser_save_expr(self, value),
                .target = parser_save_expr(self, unit),
            },
    };
}

static Expr parse_assignment_expr(Parser *self)
{
    // The implementation here deviates from the grammar slightly so as to not
    // rely on peeking more than 1 token ahead (which would be required to check
    // for both a variable name and an `=`).

    Expr lhs = parse_unit_cast_expr(self);

    Token next = peek_token(self->lexer);
    if (lhs.type == EXPR_TYPE_VAR && next.type == TOK_TYPE_EQ) {
        lex_token(&self->lexer);
        Expr rhs = parse_expr(self);
        return (Expr) {
            .type = EXPR_TYPE_ASSIGN,
            .as.assign =
                {
                    .lhs = lhs.as.var.name,
                    .rhs = parser_save_expr(self, rhs),
                },
        };
    } else {
        return lhs;
    }
}

Expr parse_expr(Parser *self) { return parse_assignment_expr(self); }

/// These functions assume that the corresponding keyword has already been
/// consumed.
static Stmt parse_dim_decl(Parser *self)
{
    Stmt res = {0};
    res.type = STMT_TYPE_DIM_DECL;

    Token name = lex_token(&self->lexer);
    ensure_tok_type(name, TOK_TYPE_NAME, "name of dimension");

    res.as.dim_decl.name = name.name;

    Token next = lex_token(&self->lexer);
    if (next.type == TOK_TYPE_SEMICOLON) return res;
    ensure_tok_type(next, TOK_TYPE_EQ,
                    "`=` or semicolon after dimension declaration");
    res.as.dim_decl.expr = parse_expr(self);

    Token semi = lex_token(&self->lexer);
    ensure_tok_type(semi, TOK_TYPE_SEMICOLON,
                    "a semicolon after dimension declaration");

    return res;
}

static Stmt parse_unit_decl(Parser *self)
{
    Stmt res = {0};
    res.type = STMT_TYPE_UNIT_DECL;

    Token unit_name = lex_token(&self->lexer);
    ensure_tok_type(unit_name, TOK_TYPE_NAME, "name of unit");
    res.as.unit_decl.name = unit_name.name;

    Token colon = lex_token(&self->lexer);
    ensure_tok_type(colon, TOK_TYPE_COLON, "a colon");

    Token dim_name = lex_token(&self->lexer);
    ensure_tok_type(dim_name, TOK_TYPE_NAME, "name of unit dimension");
    res.as.unit_decl.dim = dim_name.name;

    Token tok = lex_token(&self->lexer);
    if (tok.type == TOK_TYPE_SEMICOLON) return res;

    ensure_tok_type(tok, TOK_TYPE_EQ, "`;` or `=` after unit declaration");
    res.as.unit_decl.value = parse_expr(self);

    Token semi = lex_token(&self->lexer);
    ensure_tok_type(semi, TOK_TYPE_SEMICOLON,
                    "semicolon after unit definition");

    return res;
}

static Stmt parse_var_decl(Parser *self)
{
    Token name = lex_token(&self->lexer);
    ensure_tok_type(name, TOK_TYPE_NAME, "variable name");

    Token next = lex_token(&self->lexer);
    Expr value = {0};
    if (next.type == TOK_TYPE_EQ) {
        value = parse_expr(self);
        next  = lex_token(&self->lexer);
    }

    ensure_tok_type(next, TOK_TYPE_SEMICOLON,
                    "a semicolon after variable declaration");

    return (Stmt) {
        .type = STMT_TYPE_VAR_DECL,
        .as.var_decl =
            {
                .name  = name.name,
                .value = value,
            },
    };
}

Stmt parse_stmt(Parser *self)
{
    Token tok = peek_token(self->lexer);

    switch (tok.type) {
    case TOK_TYPE_NONE:
        lex_token(&self->lexer);
        return (Stmt) {
            .type = STMT_TYPE_NONE,
        };

    case TOK_TYPE_KEYWORD_DIM:
        // TODO: Refactor this so that the consumption of the keyword token
        // happens in the corresponding function
        lex_token(&self->lexer);
        return parse_dim_decl(self);

    case TOK_TYPE_KEYWORD_UNIT:
        lex_token(&self->lexer);
        return parse_unit_decl(self);

    case TOK_TYPE_KEYWORD_LET:
        lex_token(&self->lexer);
        return parse_var_decl(self);

    case TOK_TYPE_SEMICOLON:
        // Semicolon at the start of an expr implies an empty statement and can
        // be ignored
        lex_token(&self->lexer);
        return parse_stmt(self);

    default: {
        // (don't consume tok, as it's needed for the expression parser)
        Expr expr = parse_expr(self);
        ensure_tok_type(lex_token(&self->lexer), TOK_TYPE_SEMICOLON,
                        "semicolon after expression statement");

        return (Stmt) {
            .type    = STMT_TYPE_EXPR,
            .as.expr = expr,
        };
    }
    }
}

static_assert(OP__COUNT == 3,
              "Exhaustive definition of op_print() with respect to Op's");
void op_print(FILE *f, Op op)
{
    switch (op) {
    case OP_NONE: {
        fprintf(f, "None");
    } break;

    case OP_MULT: {
        fprintf(f, "Mult");
    } break;

    case OP_DIV: {
        fprintf(f, "Div");
    } break;

    default:
        assert(0 && "Unreachable");
    }
}

static_assert(
    EXPR_TYPE__COUNT == 8,
    "Exhaustive definition of expr_print() with respect to ExprType's");
void expr_print(FILE *f, Expr expr)
{
    switch (expr.type) {
    case EXPR_TYPE_NONE: {
        fprintf(f, "None");
    } break;

    case EXPR_TYPE_NUM: {
        fprintf(f, "Num(%f", expr.as.num.val);
        if (!sv_is_empty(expr.as.num.unit)) {
            fprintf(f, " `" SV_FMT "`", SV_ARG(expr.as.num.unit));
        }
        fputc(')', f);
    } break;

    case EXPR_TYPE_VAR: {
        fprintf(f, "Var(`" SV_FMT "`)", SV_ARG(expr.as.var.name));
    } break;

    case EXPR_TYPE_PAREN: {
        fprintf(f, "Paren(");
        expr_print(f, *expr.as.paren.inner);
        fprintf(f, ")");
    } break;

    case EXPR_TYPE_BINOP: {
        fprintf(f, "BinOp(");
        expr_print(f, *expr.as.binop.left);
        fprintf(f, ", ");
        op_print(f, expr.as.binop.op);
        fprintf(f, ", ");
        expr_print(f, *expr.as.binop.right);
        fprintf(f, ")");
    } break;

    case EXPR_TYPE_ASSIGN: {
        fprintf(f, "Assign(");
        fprintf(f, "`" SV_FMT "`", SV_ARG(expr.as.assign.lhs));
        fprintf(f, ", ");
        expr_print(f, *expr.as.assign.rhs);
        fprintf(f, ")");
    } break;

    case EXPR_TYPE_FUNCALL: {
        fprintf(f, "Funcall(`" SV_FMT "`", SV_ARG(expr.as.funcall.fun));
        if (expr.as.funcall.args.count > 0) {
            fprintf(f, ", args = [");
            expr_print(f, expr.as.funcall.args.items[0]);
            for (size_t i = 1; i < expr.as.funcall.args.count; i++) {
                fprintf(f, ", ");
                expr_print(f, expr.as.funcall.args.items[i]);
            }
            fprintf(f, "]");
        }
        fputc(')', f);
    } break;

    case EXPR_TYPE_UNITCAST: {
        fprintf(f, "UnitCast(target = ");
        expr_print(f, *expr.as.unit_cast.target);
        fprintf(f, ", value = ");
        expr_print(f, *expr.as.unit_cast.value);
        fputc(')', f);
    } break;

    default:
        assert(0 && "Unreachable");
    }
}

static_assert(
    STMT_TYPE__COUNT == 5,
    "Exhaustive definition of stmt_print() with respect to StmtType's");
void stmt_print(FILE *f, Stmt stmt)
{
    switch (stmt.type) {
    case STMT_TYPE_NONE: {
        fprintf(f, "None");
    } break;

    case STMT_TYPE_DIM_DECL: {
        fprintf(f, "DimDecl(`" SV_FMT "`)", SV_ARG(stmt.as.dim_decl.name));
    } break;

    case STMT_TYPE_UNIT_DECL: {
        fprintf(f, "UnitDecl(name = `" SV_FMT "`, dim = `" SV_FMT "`",
                SV_ARG(stmt.as.unit_decl.name), SV_ARG(stmt.as.unit_decl.dim));
        if (stmt.as.unit_decl.value.type != EXPR_TYPE_NONE) {
            fprintf(f, ", value = ");
            expr_print(f, stmt.as.unit_decl.value);
        }
        fputc(')', f);
    } break;

    case STMT_TYPE_VAR_DECL: {
        fprintf(f, "VarDecl(name = `" SV_FMT "`",
                SV_ARG(stmt.as.var_decl.name));
        if (stmt.as.var_decl.value.type != EXPR_TYPE_NONE) {
            fprintf(f, ", value = ");
            expr_print(f, stmt.as.var_decl.value);
        }
        fputc(')', f);
    } break;

    case STMT_TYPE_EXPR: {
        fprintf(f, "Expr(");
        expr_print(f, stmt.as.expr);
        fputc(')', f);
    } break;

    default:
        assert(0 && "Unreachable");
    }
}

void expr_free(Expr expr)
{
    switch (expr.type) {
    case EXPR_TYPE_PAREN:
        expr_free(*expr.as.paren.inner);
        free(expr.as.paren.inner);
        return;

    case EXPR_TYPE_BINOP:
        expr_free(*expr.as.binop.left);
        expr_free(*expr.as.binop.right);
        free(expr.as.binop.left);
        free(expr.as.binop.right);
        return;

    case EXPR_TYPE_ASSIGN:
        expr_free(*expr.as.assign.rhs);
        free(expr.as.assign.rhs);
        return;

    case EXPR_TYPE_FUNCALL: {
        Exprs args = expr.as.funcall.args;
        for (size_t i = 0; i < args.count; i++) {
            expr_free(args.items[i]);
        }
        free(args.items);
        return;
    }

    case EXPR_TYPE_UNITCAST:
        expr_free(*expr.as.unit_cast.value);
        expr_free(*expr.as.unit_cast.target);
        free(expr.as.unit_cast.value);

    case EXPR_TYPE_NONE:
    case EXPR_TYPE_NUM:
    case EXPR_TYPE_VAR:
    case EXPR_TYPE__COUNT:
        return;
    }
}

void stmt_free(Stmt stmt)
{
    switch (stmt.type) {
    case STMT_TYPE_NONE:
        return;

    case STMT_TYPE_DIM_DECL: 
        expr_free(stmt.as.dim_decl.expr);
        break;

    case STMT_TYPE_UNIT_DECL: 
        expr_free(stmt.as.unit_decl.value);
        break;

    case STMT_TYPE_VAR_DECL:
        expr_free(stmt.as.var_decl.value);
        break;

    case STMT_TYPE_EXPR: 
        expr_free(stmt.as.expr);
        break;

    case STMT_TYPE__COUNT:
        assert(0 && "Unreachable");
    }
}
