#ifndef CINTRE_COMMON_H
#define CINTRE_COMMON_H

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define countof(__a) (sizeof(__a)/sizeof*(__a))

#define ref * const
#define cref const* const

#include "dyarr.h"
typedef dyarr(char) buf;
typedef struct { char const* ptr; size_t len; } bufsl;
#define bufmt(x) (unsigned)(x).len, (x).ptr

#define exitf(...) (notif(__VA_ARGS__), exit(EXIT_FAILURE))
#define notif(...) (fprintf(stderr, __VA_ARGS__), fputc(10, stderr))

buf read_all(char const* file) {
    buf r = {0};
    FILE* f = fopen(file, "rb");
    if (!f) return r;
    if (0 != fseek(f, 0, SEEK_END)) exitf("Could not read file %s", file);
    r.ptr = malloc(r.len = r.cap = ftell(f));
    if (!r.ptr) exitf("OOM");
    fseek(f, 0, SEEK_SET);
    fread(r.ptr, 1, r.len, f);
    fclose(f);
    return r;
}

#define search_namespace(n, ns) for (size_t k = 0; k < ns.len; k++) if (!dyarr_cmp(&ns.ptr[k].name, &n))

#endif // CINTRE_COMMON_H
