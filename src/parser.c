#include "parser.h"

#include <assert.h>
#include <stdlib.h>

#include "lexer.h"

// TODO: Should Parser have a full list of tokens and a cursor, rather than
// relying on the Lexer to continually supply tokens?

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

static Expr parse_primary_expr(Parser *self) 
{
    Token tok = lex_token(&self->lexer); 

    if (tok.type == TOK_TYPE_NUM) {
        Expr res = {
            .type = EXPR_TYPE_NUM, 
            .as.num.val = tok.numval,
            .as.num.unit = SV(""),
        };

        Token next = peek_token(self->lexer);
        if (next.type == TOK_TYPE_NAME) {
            lex_token(&self->lexer); 
            res.as.num.unit = next.name; 
        }

        return res; 
    }

    // TODO: Add an ExprType for variables
    loc_print(stderr, tok.loc); 
    fprintf(stderr, ": ERROR: Unexpected primary expression\n"); 
    exit(1);
}

static Expr parse_expr(Parser *self)
{
    Token next = peek_token(self->lexer);

    if (next.type == TOK_TYPE_LPAREN) {
        lex_token(&self->lexer); 
        Expr inner    = parse_expr(self);
        Token closing = lex_token(&self->lexer);
        // TODO: Report location of opening parenthesis here
        ensure_tok_type(closing, TOK_TYPE_RPAREN, "closing parenthesis");

        return (Expr) {
            .type           = EXPR_TYPE_PAREN,
            .as.paren.inner = parser_save_expr(self, inner),
        };
    }

    return parse_primary_expr(self); 
}

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

Stmt parse_stmt(Parser *self)
{
    Token tok = lex_token(&self->lexer);

    switch (tok.type) {
    case TOK_TYPE_KEYWORD_DIM:
        return parse_dim_decl(self);

    case TOK_TYPE_KEYWORD_UNIT:
        return parse_unit_decl(self);

    case TOK_TYPE_SEMICOLON:
        // Semicolon at the start of an expr implies an empty statement and can
        // be ignored

        // TODO: Evaluate whether this means the language can be
        // semicolon-optional
        return parse_stmt(self);

    default:
        token_print(stderr, tok);
        fprintf(stderr, ": Unexpected start of statement\n");
        exit(1);
    }
}

static_assert(
    EXPR_TYPE__COUNT == 3,
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

    case EXPR_TYPE_PAREN: {
        fprintf(f, "Paren(");
        expr_print(f, *expr.as.paren.inner);
        fprintf(f, ")");
    } break;

    default:
        assert(0 && "Unreachable");
    }
}

static_assert(
    STMT_TYPE__COUNT == 3,
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

    default:
        assert(0 && "Unreachable");
    }
}
