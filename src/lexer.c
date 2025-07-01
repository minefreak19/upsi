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

static_assert(TOK_TYPE__COUNT == 17,
              "Exhaustive definition of TOK_NAMES wrt TokenType's");
// ClangFormat keeps trying to put these into multiple columns, when I want
// one per line
// clang-format off
static const StringView TOK_NAMES[TOK_TYPE__COUNT] = {
    [TOK_TYPE_KEYWORD_DIM]  = SV("dim"),
    [TOK_TYPE_KEYWORD_UNIT] = SV("unit"),
    [TOK_TYPE_KEYWORD_LET]  = SV("let"),
    [TOK_TYPE_KEYWORD_IN]   = SV("in"),

    [TOK_TYPE_SEMICOLON] = SV(";"),
    [TOK_TYPE_COLON]     = SV(":"),
    [TOK_TYPE_COMMA]     = SV(","),
    [TOK_TYPE_EQ]        = SV("="),
    [TOK_TYPE_STAR]      = SV("*"),
    [TOK_TYPE_SLASH]     = SV("/"),
    [TOK_TYPE_LPAREN]    = SV("("),
    [TOK_TYPE_RPAREN]    = SV(")"),
};
// clang-format on

static inline bool tok_is_keyword(Token tok)
{
    return TOK_TYPE__KW_START <= tok.type && tok.type < TOK_TYPE__KW_END;
}

static inline bool tok_is_symb(Token tok)
{
    return TOK_TYPE__SYMB_START <= tok.type && tok.type < TOK_TYPE__SYMB_END;
}

/// Returns true if the character passed in can be the first character of a
/// symbol (i.e. this character can mark the end of a word)
static bool is_symb(char c)
{
    for (int i = TOK_TYPE__SYMB_START; i < TOK_TYPE__SYMB_END; i++) {
        if (c == TOK_NAMES[i].text[0]) return true;
    }
    return false;
}

// TODO: Handle different types of line endings properly (LF/CRLF/CR)
static void lexer_advance(Lexer *self, size_t n)
{
    for (size_t i = 0; i < n; i++) {
        if (self->text[self->cur] == '\n') {
            self->loc.line++;
            self->loc.col = 0;
        }
        self->loc.col++;
        self->cur++;
    }
}

static inline void lexer_trim_left(Lexer *self)
{
    while (self->cur < self->text_len && isspace(self->text[self->cur])) {
        lexer_advance(self, 1);
    }
}

static bool try_lex_symb(Lexer *self, Token *resp)
{
    if (self->cur >= self->text_len) {
        return false;
    }

    for (int type = TOK_TYPE__SYMB_START; type < TOK_TYPE__SYMB_END; type++) {
        StringView name = TOK_NAMES[type];

        if (strncmp(&self->text[self->cur], name.text, name.len) == 0) {
            resp->type = type;
            lexer_advance(self, name.len);
            return true;
        }
    }
    // No symbol matches

    return false;
}

static bool try_lex_word(Lexer *self, StringView *resp)
{
    if (self->cur >= self->text_len) {
        return false;
    }
    StringView res = {
        .text = &self->text[self->cur],
        .len  = 0,
    };
    while (self->cur < self->text_len) {
        char c = self->text[self->cur];
        if (isspace(c) || is_symb(c)) {
            if (res.len == 0) return false;

            *resp = res;
            return true;
        }
        lexer_advance(self, 1);
        res.len++;
    }

    if (res.len == 0) return false;

    *resp = res;
    return true;
}

static inline long char_to_digit(char c)
{
    assert(isdigit(c));
    return c - '0';
}

static Token lex_number(Lexer *self)
{
    assert(isdigit(self->text[self->cur]));

    // Save loc at the beginning of the number
    FileLoc loc = self->loc;

    int64_t num = 0;
    while (self->cur < self->text_len && isdigit(self->text[self->cur])) {
        num *= 10;
        num += char_to_digit(self->text[self->cur]);

        lexer_advance(self, 1);
    }

    // TODO: Clean up this code
    if (self->cur < self->text_len && self->text[self->cur] == '.') {
        if (self->cur + 1 >= self->text_len ||
            !isdigit(self->text[self->cur + 1])) {
            loc_print(stderr, self->loc);
            fprintf(
                stderr,
                ": ERROR: Could not lex number - trailing `.' without further "
                "digits\n");
            exit(1);
        }

        lexer_advance(self, 1);

        double fracpart = 0;
        double factor   = 0.1;
        while (isdigit(self->text[self->cur])) {
            fracpart += factor * char_to_digit(self->text[self->cur]);
            factor /= 10.0;

            lexer_advance(self, 1);
        }

        return (Token) {
            .type   = TOK_TYPE_NUM,
            .numval = ((double) num + fracpart),
            .loc    = loc,
        };
    } else {
        return (Token) {
            .type   = TOK_TYPE_NUM,
            .numval = num,
            .loc    = loc,
        };
    }
}

Token lex_token(Lexer *self)
{
    lexer_trim_left(self);

    // Skip comment, if any
    if (self->cur + 1 < self->text_len && self->text[self->cur] == '/' &&
        self->text[self->cur + 1] == '/') {
        while (self->text[self->cur] != '\n') {
            lexer_advance(self, 1);
        }
        return lex_token(self);
    }

    Token res = {0};
    res.loc   = self->loc;

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

// TODO: Add some sort of buffer that stores peeked tokens, to prevent repeating
// the same computation multiple times
Token peek_token(Lexer self) { return lex_token(&self); }

inline void loc_print(FILE *f, FileLoc loc)
{
    fprintf(f, SV_FMT ":%" PRIu32 ":%" PRIu32, SV_ARG(loc.file), loc.line,
            loc.col);
}

static_assert(
    TOK_TYPE__COUNT == 17,
    "Exhaustive definition of token_print with respect to TokenType's");
void token_print(FILE *f, Token tok)
{
    loc_print(f, tok.loc);
    fprintf(f, ": ");
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

        case TOK_TYPE_NUM: {
            fprintf(f, "FLOAT(%f)", tok.numval);
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

inline Lexer lexer_from_cstr(const char *text)
{
    return (Lexer) {
        .loc =
            {
                .file = SV(""),
                .col  = 1,
                .line = 1,
            },
        .text     = text,
        .text_len = strlen(text),
        .cur      = 0,
    };
}

