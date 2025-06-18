#ifndef LEXER_H_
#define LEXER_H_

#include <stdint.h>
#include <stdio.h>

#include "sv.h"

typedef struct {
    StringView file;
    uint32_t line; 
    uint32_t col; 
} FileLoc; 

typedef enum {
    TOK_TYPE_NONE = 0,

    TOK_TYPE__KW_START,
    TOK_TYPE_KEYWORD_DIM = TOK_TYPE__KW_START,
    TOK_TYPE_KEYWORD_UNIT,
    TOK_TYPE__KW_END,

    TOK_TYPE__SYMB_START,
    TOK_TYPE_SEMICOLON = TOK_TYPE__SYMB_START,
    TOK_TYPE_COLON,
    TOK_TYPE_EQ,
    TOK_TYPE_STAR,
    TOK_TYPE_SLASH,
    TOK_TYPE_LPAREN,
    TOK_TYPE_RPAREN,
    TOK_TYPE__SYMB_END,

    TOK_TYPE_NAME,
    // TODO: Do we need INT and FLOAT to be separate?
    TOK_TYPE_INT,
    TOK_TYPE_FLOAT,

    TOK_TYPE__COUNT,
} TokenType;

typedef struct {
    FileLoc loc;
    TokenType type;
    union {
        StringView name;
        int64_t intval;
        double floatval;
    };
} Token;

typedef struct {
    const char *text;
    size_t text_len;
    size_t cur;
    FileLoc loc;
} Lexer;

Token lex_token(Lexer *self);
void loc_print(FILE *f, FileLoc loc);
void token_print(FILE *f, Token tok);
Lexer lexer_from_cstr(const char *text);

#endif  // LEXER_H_
