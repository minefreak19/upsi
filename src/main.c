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
    bool debug;
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

    for (char *arg = *argv++; arg; arg = *argv++) {
        if (strcmp(arg, "--debug") == 0) {
            args.debug = true;
        }
    }

    const char *text = slurp_file_to_cstr(args.source_file);

    Lexer lexer    = lexer_from_cstr(text);
    lexer.loc.file = sv_from_cstr(args.source_file);

    Parser parser = {
        .lexer = lexer,
    };

    EvalContext ctx = new_context();

    if (args.debug) dump_context(stdout, &ctx);
    for (Stmt stmt = parse_stmt(&parser); stmt.type != STMT_TYPE_NONE;
         stmt      = parse_stmt(&parser)) {
        eval_stmt(&ctx, stmt);
        if (args.debug) dump_context(stdout, &ctx);
    }

    // TODO: It will probably be necessary to specify a fundamental unit for
    // each dimension at time of dimension declaration

    free((void *) text);
    return 0;
}
