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
#define SV_ARG(sv) ((int) (sv).len), ((sv).text)

StringView sv_from_cstr(const char *cstr);
bool sv_eq(StringView a, StringView b);
bool sv_is_empty(StringView sv);

#endif  // SV_H_

#ifdef SV_IMPLEMENTATION
#include <string.h>

StringView sv_from_cstr(const char *cstr)
{
    return (StringView) {
        .text = cstr,
        .len  = strlen(cstr),
    };
}

bool sv_eq(StringView a, StringView b)
{
    return a.len == b.len && strncmp(a.text, b.text, a.len) == 0;
}

bool sv_is_empty(StringView sv) {
    return sv.len == 0;
}

#endif  // SV_IMPLEMENTATION
