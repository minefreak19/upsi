#ifndef LEXER_H_
#define LEXER_H_

#include <stdio.h>

#include "sv.h"

typedef enum {
    TOK_TYPE_NONE = 0,

    TOK_TYPE_KEYWORD_DIM,
    TOK_TYPE_KEYWORD_UNIT,

    TOK_TYPE_NAME,

    TOK_TYPE_SEMICOLON,
    TOK_TYPE_COLON,
} TokenType;

typedef struct {
    TokenType type;
    StringView name;
} Token;

typedef struct {
    const char *text;
    size_t text_len;
    size_t cur;
} Lexer;

bool is_symb(char c);
bool try_lex_symb(Lexer *self, Token *resp);
bool try_lex_word(Lexer *self, StringView *resp);
Token lex_token(Lexer *self);
void token_print(FILE *f, Token tok);
Lexer lexer_from_cstr(const char *text);

#endif  // LEXER_H_
