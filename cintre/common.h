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
typedef ct_dyarr(char) ct_buf;
typedef struct { char const* ptr; size_t len; } ct_bufsl;
#define bufmt(x) (unsigned)(x).len, (x).ptr
#define bufis(x, c) (strlen((c)) == (x).len && !memcmp(c, (x).ptr, strlen((c))))

#ifdef LOC_NOTIF
# define _HERE_STR(__ln) #__ln
# define _HERE_XSTR(__ln) _HERE_STR(__ln)
# define HERE "(" __FILE__ ":" _HERE_XSTR(__LINE__) ") "
#else
# define HERE
#endif

#ifdef TRY_EXITF
# include <setjmp.h>
static jmp_buf _try_jmp_buf = {0};
static bool _try_jmp_flg = false;
# define try if ((_try_jmp_flg = !setjmp(_try_jmp_buf)))
# define exitf(...) (notif(__VA_ARGS__), _try_jmp_flg ? longjmp(_try_jmp_buf, 1) : exit(EXIT_FAILURE))
#else
# define try if (1)
# define exitf(...) (notif(__VA_ARGS__), exit(EXIT_FAILURE))
#endif
#define notif(...) (fflush(stdout), fprintf(stderr, HERE __VA_ARGS__), fputc(10, stderr))

#define report_lex_locate(ls, ...) (                                \
    fflush(stdout),                                                 \
    fprintf(stderr, "\x1b[1m[%s:%zu]\x1b[m %.*s \x1b[1m##\x1b[m ",  \
            (ls)->file, (ls)->line, bufmt(ct_llne((ls)))),             \
    notif(__VA_ARGS__))

// used in lexer and preparer
// TODO: change things so it uses the tbd `dyarr_sortedsearch`, both on search and push
#define search_namespace(n, ns) for (size_t k = 0; k < ns.len; k++) if (!dyarr_cmp(&ns.ptr[k].name, &n))

#define isidstart(__c) (('a' <= ((__c)|32) && ((__c)|32) <= 'z') || '_' == (__c))
#define isidcont(__c) (('a' <= ((__c)|32) && ((__c)|32) <= 'z') || ('0' <= (__c) && (__c) <= '9') || '_' == (__c))

struct ct_declaration;
struct ct_expression;
struct ct_adpt_type;
struct ct_adpt_item;
struct ct_slot;
struct ct_bytecode;
struct ct_run_state;

void ct_print_decl(FILE ref strm, struct ct_declaration cref decl);
void ct_print_expr(FILE ref strm, struct ct_expression cref expr, unsigned const depth);
void ct_print_type(FILE ref strm, struct ct_adpt_type cref ty, bool const top);
void ct_print_code(FILE ref strm, struct ct_bytecode const code);
void ct_print_item(FILE ref strm, struct ct_adpt_item cref it, char cref stack, unsigned const depth);
void ct_print_tops(FILE ref strm, struct ct_run_state cref rs, struct ct_adpt_item cref items, size_t const count);
void ct_print_slot(FILE ref strm, struct ct_slot cref slt);

#endif // CINTRE_COMMON_H
