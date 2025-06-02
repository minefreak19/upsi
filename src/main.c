#include <assert.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>

#include "lexer.h"

#define SV_IMPLEMENTATION
#include "sv.h"

static struct {
    const char *program_name;
    const char *source_file;
} args;

char *slurp_file_to_cstr(const char *path)
{
    FILE *f = fopen(path, "r");
    if (f == NULL) {
        fprintf(stderr, "ERROR: Could not open file %s: %s\n", path,
                strerror(errno));
        exit(1);
    }
    if (fseek(f, 0, SEEK_END) < 0) {
        fprintf(stderr, "ERROR: Could not seek in file %s: %s\n", path,
                strerror(errno));
        exit(1);
    }
    long len = ftell(f);
    if (len < 0) {
        fprintf(stderr, "ERROR: Could not get length for file %s: %s\n", path,
                strerror(errno));
        exit(1);
    }
    // TODO: Maybe put this in an Arena so we don't leak memory here, even
    // though for now this function is only called once
    char *res = malloc(len + 1);  // null byte
    if (fseek(f, 0, SEEK_SET) < 0) {
        fprintf(stderr, "ERROR: Could not seek in file %s: %s\n", path,
                strerror(errno));
        exit(1);
    }
    if ((long) fread(res, 1, len, f) < len) {
        fprintf(stderr, "ERROR: Could not read file %s: %s\n", path,
                strerror(errno));
        exit(1);
    }
    res[len] = '\0';
    return res;
}

void usage(FILE *f)
{
    fprintf(f, "Usage: %s <source_file>\n", args.program_name);
}

int main(int argc, char **argv)
{
    (void) argc;
    args.program_name = *argv++;
    assert(args.program_name &&
           "TODO: This program is expected to only be called from the command "
           "line");

    args.source_file = *argv++;
    if (args.source_file == NULL) {
        fprintf(
            stderr,
            "ERROR: Too few arguments. Source filename must be specified.\n");
        usage(stderr);
        exit(1);
    }

    const char *text = slurp_file_to_cstr(args.source_file);

    Lexer lexer = lexer_from_cstr(text);

    for (Token tok = lex_token(&lexer); tok.type != TOK_TYPE_NONE;
         tok       = lex_token(&lexer)) {
        token_print(stdout, tok);
        putc('\n', stdout);
    }
    return 0;
}
