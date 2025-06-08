#include "lexer.h"

#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "sv.h"

static_assert(TOK_TYPE__COUNT == 11,
              "Exhaustive definition of TOK_NAMES wrt TokenType's");
static const StringView TOK_NAMES[TOK_TYPE__COUNT] = {
    [TOK_TYPE_KEYWORD_DIM]  = SV("dim"),
    [TOK_TYPE_KEYWORD_UNIT] = SV("unit"),

    [TOK_TYPE_SEMICOLON] = SV(";"),
    [TOK_TYPE_COLON]     = SV(":"),
    [TOK_TYPE_EQ]        = SV("="),
};

bool tok_is_keyword(Token tok)
{
    return TOK_TYPE__KW_START <= tok.type && tok.type < TOK_TYPE__KW_END;
}

bool tok_is_symb(Token tok)
{
    return TOK_TYPE__SYMB_START <= tok.type && tok.type < TOK_TYPE__SYMB_END;
}

bool is_symb(char c)
{
    // TODO: For any potential multi-character symbols, should this function
    // return true for all characters or only the first character of each
    // symbol?
    for (int i = TOK_TYPE__SYMB_START; i < TOK_TYPE__SYMB_END; i++) {
        if (c == TOK_NAMES[i].text[0]) return true;
    }
    return false;
}

bool try_lex_symb(Lexer *self, Token *resp)
{
    if (self->cur >= self->text_len) {
        return false;
    }

    for (int type = TOK_TYPE__SYMB_START; type < TOK_TYPE__SYMB_END; type++) {
        StringView name = TOK_NAMES[type];

        if (strncmp(&self->text[self->cur], name.text, name.len) == 0) {
            resp->type = type;
            self->cur += name.len;
            return true;
        }
    }
    // No symbol matches

    return false;
}

bool try_lex_word(Lexer *self, StringView *resp)
{
    if (self->cur >= self->text_len) {
        return false;
    }
    // TODO: Optimise this. I'm sure this isn't the cleanest or more efficient
    // way to collect a word
    StringView res = {
        .text = &self->text[self->cur],
        .len  = 0,
    };
    while (self->cur < self->text_len) {
        char c = self->text[self->cur];
        if (isspace(c) || is_symb(c)) {
            *resp = res;
            return true;
        }
        self->cur++;
        res.len++;
    }

    *resp = res;
    return true;
}

long char_to_digit(char c)
{
    assert(isdigit(c));
    return c - '0';
}

Token lex_number(Lexer *self)
{
    assert(isdigit(self->text[self->cur]));

    int64_t num = 0;
    while (self->cur < self->text_len && isdigit(self->text[self->cur])) {
        num *= 10;
        num += char_to_digit(self->text[self->cur]);

        self->cur++;
    }

    if (self->cur < self->text_len && self->text[self->cur] == '.') {
        if (self->cur + 1 >= self->text_len ||
            !isdigit(self->text[self->cur + 1])) {
            // TODO: Add a proper error reporting mechanism
            fprintf(
                stderr,
                "ERROR: Could not lex number - trailing `.' without further "
                "digits\n");
            exit(1);
        }

        self->cur++;

        double fracpart = 0;
        double factor   = 0.1;
        while (isdigit(self->text[self->cur])) {
            fracpart += factor * char_to_digit(self->text[self->cur]);
            factor /= 10.0;

            self->cur++;
        }

        return (Token) {
            .type     = TOK_TYPE_FLOAT,
            .floatval = ((double) num + fracpart),
        };
    } else {
        return (Token) {
            .type   = TOK_TYPE_INT,
            .intval = num,
        };
    }
}

// TODO: This currently assumes the input is a sequence of completely valid
// tokens. This might not be a valid assumption - or maybe errors are something
// we want to deal with in the parser. Time will tell.
Token lex_token(Lexer *self)
{
    Token res = {0};
    while (self->cur < self->text_len && isspace(self->text[self->cur]))
        self->cur++;

    if (self->cur >= self->text_len) {
        return res;
    }

    if (try_lex_symb(self, &res)) return res;
    // TODO: Support negative numbers in lexer
    if (isdigit(self->text[self->cur])) {
        return lex_number(self);
    }

    StringView word;
    if (try_lex_word(self, &word)) {
        for (int kw = TOK_TYPE__KW_START; kw < TOK_TYPE__KW_END; kw++) {
            if (sv_eq(word, TOK_NAMES[kw])) {
                res.type = kw;
                return res;
            }
        }
        // No keyword matches

        res.type = TOK_TYPE_NAME;
        res.name = word;
        return res;
    }

    assert(0 && "could not lex text");
}

static_assert(
    TOK_TYPE__COUNT == 11,
    "Exhaustive definition of token_print with respect to TokenType's");
void token_print(FILE *f, Token tok)
{
    if (tok_is_keyword(tok)) {
        fprintf(f, "KW(" SV_FMT ")", SV_ARG(TOK_NAMES[tok.type]));
    } else if (tok_is_symb(tok)) {
        fprintf(f, "SYMB(" SV_FMT ")", SV_ARG(TOK_NAMES[tok.type]));
    } else {
        switch (tok.type) {
        case TOK_TYPE_NONE: {
            fprintf(f, "NONE");
        } break;

        case TOK_TYPE_NAME: {
            fprintf(f, "NAME(" SV_FMT ")", SV_ARG(tok.name));
        } break;

        case TOK_TYPE_INT: {
            fprintf(f, "INT(%" PRIi64 ")", tok.intval);
        } break;

        case TOK_TYPE_FLOAT: {
            fprintf(f, "FLOAT(%f)", tok.floatval);
        } break;

        default: {
            fprintf(stderr,
                    "%s:%d (%s): ERROR: Unreachable - found token with illegal "
                    "type %d\n",
                    __FILE__, __LINE__, __func__, tok.type);
            exit(1);
        }
        }
    }
}

Lexer lexer_from_cstr(const char *text)
{
    return (Lexer) {.text = text, .text_len = strlen(text), .cur = 0};
}

