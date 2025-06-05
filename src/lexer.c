#include "lexer.h"

#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "sv.h"

bool is_symb(char c)
{
    const char *symbols = ";:=";
    for (; *symbols; symbols++) {
        if (c == *symbols) return true;
    }
    return false;
}

bool try_lex_symb(Lexer *self, Token *resp)
{
    if (self->cur >= self->text_len) {
        return false;
    }

    Token res = {0};
    switch (self->text[self->cur]) {
    case ';': {
        res.type = TOK_TYPE_SEMICOLON;
        self->cur++;
        *resp = res;
        return true;
    }
    case ':': {
        res.type = TOK_TYPE_COLON;
        self->cur++;
        *resp = res;
        return true;
    }
    case '=': {
        res.type = TOK_TYPE_EQ;
        self->cur++;
        *resp = res;
        return true;
    }
    default: {
        return false;
    }
    }
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

    long num = 0;
    while (self->cur < self->text_len && isdigit(self->text[self->cur])) {
        num *= 10;
        num += char_to_digit(self->text[self->cur]);

        self->cur++;
    }

    return (Token) {
        .type   = TOK_TYPE_INT,
        .intval = num,
    };
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
        // TODO: can this fail?
        return lex_number(self);
    }

    StringView word;
    if (try_lex_word(self, &word)) {
        // TODO: Automate this somehow (with a table of keywords)
        // This might require adding a union to Token
        if (sv_eq(word, SV("dim"))) {
            res.type = TOK_TYPE_KEYWORD_DIM;
            return res;
        } else if (sv_eq(word, SV("unit"))) {
            res.type = TOK_TYPE_KEYWORD_UNIT;
            return res;
        } else {
            res.type = TOK_TYPE_NAME;
            res.name = word;
            return res;
        }
    }

    assert(0 && "could not lex text");
}

// TODO: Implement a more flexible way to show tokens
void token_print(FILE *f, Token tok)
{
    switch (tok.type) {
    case TOK_TYPE_NONE: {
        fprintf(f, "NONE");
    } break;

    case TOK_TYPE_KEYWORD_DIM: {
        fprintf(f, "KW(dim)");
    } break;

    case TOK_TYPE_KEYWORD_UNIT: {
        fprintf(f, "KW(unit)");
    } break;

    case TOK_TYPE_SEMICOLON: {
        fprintf(f, "SEMICOLON");
    } break;

    case TOK_TYPE_COLON: {
        fprintf(f, "COLON");
    } break;

    case TOK_TYPE_EQ: {
        fprintf(f, "EQ");
    } break;

    case TOK_TYPE_NAME: {
        fprintf(f, "NAME(" SV_FMT ")", SV_ARG(tok.name));
    } break;

    case TOK_TYPE_INT: {
        fprintf(f, "INT(%ld)", tok.intval);
    } break;
    }
}

Lexer lexer_from_cstr(const char *text)
{
    return (Lexer) {.text = text, .text_len = strlen(text), .cur = 0};
}

