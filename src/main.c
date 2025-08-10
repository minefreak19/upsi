#include <assert.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>

#include "eval.h"
#include "lexer.h"
#include "parser.h"

#define SV_IMPLEMENTATION
#include "sv.h"

static struct {
    const char *program_name;
    const char *source_file;
    enum {
        MODE_NORMAL = 0,
        MODE_DEBUG,
        MODE_DEBUG_PARSER,
    } mode;
} args = {0};

/// Expects caller to `free()` the returned cstr
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
    fprintf(f, "Usage: %s <source_file> [mode]\n", args.program_name);
    fprintf(f, "Modes: \n");
    fprintf(f,
            "\t--debug - Standard debugging mode. Interprets code while "
            "printing debug info every statement.\n");
    fprintf(f,
            "\t--debug-parser - Parser debugging mode. Only parses statements "
            "(printing debug info every statement), without interpreting.\n");
    fprintf(f,
            "If no mode is specified ('normal mode'), interprets code without "
            "printing debug information.\n");
}

int main(int argc, char **argv)
{
    (void) argc;
    args.program_name = *argv++;
    assert(args.program_name &&
           "This program is expected to only be called from the command "
           "line");

    args.source_file = *argv++;
    if (args.source_file == NULL) {
        fprintf(
            stderr,
            "ERROR: Too few arguments. Source filename must be specified.\n");
        usage(stderr);
        exit(1);
    }

    for (char *arg = *argv++; arg; arg = *argv++) {
        if (strcmp(arg, "--debug") == 0) {
            if (args.mode != MODE_NORMAL) {
                fprintf(stderr,
                        "WARN: Overwriting previously specified mode to enter "
                        "into debug mode.\n");
            }
            args.mode = MODE_DEBUG;
        } else if (strcmp(arg, "--debug-parser") == 0) {
            if (args.mode != MODE_NORMAL) {
                fprintf(stderr,
                        "WARN: Overwriting previously specified mode to enter "
                        "into parser debug mode.\n");
            }
            args.mode = MODE_DEBUG_PARSER;
        } else {
            fprintf(stderr, "ERROR: Unknown argument `%s`.\n", arg);
            usage(stderr);
            exit(1);
        }
    }

    const char *text = slurp_file_to_cstr(args.source_file);

    Lexer lexer    = lexer_from_cstr(text);
    lexer.loc.file = sv_from_cstr(args.source_file);

    Parser parser = {
        .lexer = lexer,
    };

    EvalContext ctx = eval_context_new();

    // TODO: Rewrite the basic control flow into a large switch-case to reduce
    // complexity
    if (args.mode == MODE_DEBUG) eval_context_dump(stdout, &ctx);
    for (Stmt stmt = parse_stmt(&parser); stmt.type != STMT_TYPE_NONE;
         stmt_free(stmt), stmt = parse_stmt(&parser)) {
        if (args.mode == MODE_DEBUG || args.mode == MODE_DEBUG_PARSER) {
            loc_print(stdout, stmt.loc);
            printf(": Parsed stmt: ");
            stmt_print(stdout, stmt);
            printf("\n");
        }
        if (args.mode == MODE_NORMAL || args.mode == MODE_DEBUG)
            eval_stmt(&ctx, stmt);
        if (args.mode == MODE_DEBUG) eval_context_dump(stdout, &ctx);
    }

    eval_context_destroy(&ctx);

    free((void *) text);
    return 0;
}
