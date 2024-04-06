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

#define search_namespace(n, ns) for (size_t k = 0; k < ns.len; k++) if (!dyarr_cmp(&ns.ptr[k].name, &n))

#endif // CINTRE_COMMON_H
