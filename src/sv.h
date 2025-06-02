#ifndef SV_H_
#define SV_H_

#include <stdbool.h>
#include <stddef.h>

typedef struct {
    const char *text;
    size_t len;
} StringView;

#define SV(cstr) ((StringView) {.text = (cstr), .len = sizeof(cstr) - 1})

#define SV_FMT "%.*s"
// TODO: Is there a more proper way to appease the compiler without having to
// cast to int?
#define SV_ARG(sv) ((int) (sv).len), ((sv).text)

bool sv_eq(StringView a, StringView b);

#endif  // SV_H_

#ifdef SV_IMPLEMENTATION
#include <string.h>

bool sv_eq(StringView a, StringView b)
{
    return a.len == b.len && strncmp(a.text, b.text, a.len) == 0;
}

#endif  // SV_IMPLEMENTATION
