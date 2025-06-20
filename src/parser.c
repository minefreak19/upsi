#include "parser.h"

#include <assert.h>
#include <stdlib.h>

#include "lexer.h"

// TODO: Should Parser have a full list of tokens and a cursor, rather than
// relying on the Lexer to continually supply tokens?

static_assert(
    TOK_TYPE__COUNT == 15,
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
    if (self->exprs_count >= PARSER_EXPRS_CAP) {
        assert(0 && "Tried to allocate too many expressions!");
    }

    self->exprs[self->exprs_count++] = expr;
    return &self->exprs[self->exprs_count - 1];
}

// TODO: These functions can probably accept the next token if it's already
// peeked, thus saving redoing some of the lexing work
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

static Expr parse_geom_expr(Parser *self)
{
    Expr left = parse_primary_expr(self);

    Token next = peek_token(self->lexer);
    Op op      = op_from_tok_type(next.type);
    if (op == OP_NONE) {
        return left;
    }

    lex_token(&self->lexer);
    Expr right = parse_expr(self);

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

static Expr parse_assignment_expr(Parser *self)
{
    // The implementation here deviates from the grammar slightly so as to not
    // rely on peeking more than 1 token ahead (which would be required to check
    // for both a variable name and an `=`).

    Expr lhs = parse_geom_expr(self);

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

    Token semi = lex_token(&self->lexer);
    ensure_tok_type(semi, TOK_TYPE_SEMICOLON, "a semicolon");

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
    if (tok.type == TOK_TYPE_SEMICOLON) {
        return res;
    } else if (tok.type == TOK_TYPE_EQ) {
        Expr expr = parse_expr(self);

        res.as.unit_decl.value = expr;
        return res;
    } else {
        loc_print(stderr, tok.loc);
        fprintf(stderr,
                ": ERROR: Expected `;` or `=` after unit declaration\n");
        exit(1);
    }

    return res;
}

static Stmt parse_var_decl(Parser *self)
{
    Token name = lex_token(&self->lexer);
    ensure_tok_type(name, TOK_TYPE_NAME, "variable name");

    Token colon = lex_token(&self->lexer);
    ensure_tok_type(colon, TOK_TYPE_COLON,
                    "`:` followed by dimension of variable");

    Token dim = lex_token(&self->lexer);
    ensure_tok_type(dim, TOK_TYPE_NAME, "name of dimension of variable");

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
                .dim   = dim.name,
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
    EXPR_TYPE__COUNT == 6,
    "Exhaustive definition of expr_print() with respect to ExprType's");
void expr_print(FILE *f, Expr expr)
{
    switch (expr.type) {
    case EXPR_TYPE_NONE: {
        fprintf(f, "None");
    } break;

    case EXPR_TYPE_NUM: {
        fprintf(f, "Num(%f `" SV_FMT "`)", expr.as.num.val,
                SV_ARG(expr.as.num.unit));
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
        fprintf(f, "VarDecl(name = `" SV_FMT "`, dim = `" SV_FMT "`",
                SV_ARG(stmt.as.unit_decl.name), SV_ARG(stmt.as.unit_decl.dim));
        if (stmt.as.unit_decl.value.type != EXPR_TYPE_NONE) {
            fprintf(f, ", value = ");
            expr_print(f, stmt.as.unit_decl.value);
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
