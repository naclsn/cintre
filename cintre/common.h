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

#define _dyarr_allocfail  (exitf("OOM"), NULL)
#include "dyarr.h"
//typedef dyarr(char) buf;
//typedef struct { char const* ptr; size_t len; } bufsl;
//#define bufmt(x) (unsigned)(x).len, (x).ptr
//#define bufis(x, c) (strlen((c)) == (x).len && !memcmp(c, (x).ptr, strlen((c))))

typedef size_t tokt;
#define tokn(__at) (ls->tokens.ptr+(__at))
static inline char const* quoted(char cref txt)
{
    static char quo[2048];
    size_t lex_strquo(char* const quoted, size_t const size, char const* const unquoted, size_t const length);
    return lex_strquo(quo, sizeof quo, txt, strlen(txt)) ? quo : txt;
}

static inline char const* lexerline(char cref file, size_t const line)
{
    static char lne[2048];
    size_t lex_getline(char* const res, size_t const size, char const* const file, size_t const line);
    return lex_getline(lne, sizeof lne, file, line), lne;
}

#define report_lex_locate(ls, ...) (                                         \
    fflush(stdout),                                                          \
    fprintf(stderr, "\x1b[1m[%s:%zu]\x1b[m %s \x1b[1m##\x1b[m ",             \
            (ls)->work.ptr+(ls)->sources.ptr[(ls)->sources.len-1].file,      \
            (ls)->sources.ptr[(ls)->sources.len-1].line,                     \
            lexerline(                                                       \
                (ls)->work.ptr+(ls)->sources.ptr[(ls)->sources.len-1].file,  \
                (ls)->sources.ptr[(ls)->sources.len-1].line)),               \
    notif(__VA_ARGS__))


#define on_lex_preprocerr(ls, err) (          \
    report_lex_locate(ls, "Error: %s", err),  \
    exitf("Stopping at preprocessor error"))

static inline void* mallox(size_t n)
{
    void ref r = malloc(n);
    if (!r) exitf("OOM");
    return r;
}

#define isidstart(__c) (('a' <= ((__c)|32) && ((__c)|32) <= 'z') || '_' == (__c))
#define isidcont(__c) (('a' <= ((__c)|32) && ((__c)|32) <= 'z') || ('0' <= (__c) && (__c) <= '9') || '_' == (__c))

struct lex_state;
struct declaration;
struct expression;
struct statement;
struct adpt_type;
struct adpt_item;
struct slot;
struct bytecode;
struct run_state;

void print_decl(FILE ref strm, struct lex_state cref ls, struct declaration cref decl);
void print_expr(FILE ref strm, struct lex_state cref ls, struct expression cref expr, unsigned const depth);
void print_cxpr(FILE ref strm, struct lex_state cref ls, struct expression cref expr);
void print_stmt(FILE ref strm, struct lex_state cref ls, struct statement cref stmt, unsigned const depth);
void print_type(FILE ref strm, struct adpt_type cref ty, bool const top);
void print_code(FILE ref strm, struct bytecode const code);
void print_item(FILE ref strm, struct adpt_item cref it, char cref stack, unsigned const depth);
void print_tops(FILE ref strm, struct run_state cref rs, struct adpt_item cref items, size_t const count);
void print_slot(FILE ref strm, struct slot cref slt);
void print_lexr(FILE ref strm, struct lex_state cref ls);

#endif // CINTRE_COMMON_H
