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
#define bufis(x, c) (strlen((c)) == (x).len && !memcmp(c, (x).ptr, strlen((c))))

#ifdef LOC_NOTIF
# define _HERE_STR(__ln) #__ln
# define _HERE_XSTR(__ln) _HERE_STR(__ln)
# define HERE "(" __FILE__ ":" _HERE_XSTR(__LINE__) ") "
#else
# define HERE
#endif

#define exitf(...) (notif(__VA_ARGS__), exit(EXIT_FAILURE))
#define notif(...) (fprintf(stderr, HERE __VA_ARGS__), fputc(10, stderr))

#define report_lex_locate(ls, ...) (                                    \
    fprintf(stderr, "\x1b[1m[%s:%zu]\x1b[m %.*s \x1b[1m##\x1b[m ",  \
            (ls)->file, (ls)->line, bufmt(llne((ls)))),             \
    notif(__VA_ARGS__))

#define search_namespace(n, ns) for (size_t k = 0; k < ns.len; k++) if (!dyarr_cmp(&ns.ptr[k].name, &n))

#endif // CINTRE_COMMON_H
