/// C lexer with inbuilt preprocessor; example:
/// ```c
/// lex_state ls = {0};                    // $ cpp
/// lex_define(&ls, "_XOPEN_SOURCE", "1"); //     -D_XOPEN_SOURCE=1
/// lex_include(&ls, "lib/include");       //     -Ilib/include
/// lex_entry(&ls, "./main.c");            //     ./main.c
///
/// size_t k;
/// // EOF is an empty token ("", so starts with '\0')
/// while (k = lext(ls), ls.tokens.ptr[k]) {
///     printf("%s:%zu: %s\n",
///             // current file name as C-string
///             ls.work.ptr+ls.sources.ptr[ls.sources.len-1].file,
///             // current line number, 1-based
///             ls.sources.ptr[ls.sources.len-1].line,
///             // returned token `k` as C-string
///             ls.tokens.ptr+k);
/// }
///
/// lex_free(&ls);
/// ```
///
/// Modified for cintre (removed .._IMPL.. and use CINTRE_..); see licensed
/// version https://github.com/naclsn/mopfiles/blob/main/c-things/utils/lexer.h
/// (if it's still there...)

#ifndef CINTRE_LEXER_H
#define CINTRE_LEXER_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "common.h"

typedef struct lex_state lex_state;

/// add to defined macros (-D)
void lex_define(lex_state* const ls, char const* const name, char const* const value);
/// remove from defined macros (-U)
void lex_undef(lex_state* const ls, char const* const name);
/// add to include paths (-I)
void lex_incdir(lex_state* const ls, char const* const path);
/// set the entry file, lexer ready to go
void lex_entry(lex_state* const ls, FILE* const stream, char const* const file);
/// go back (at most) `count` tokens (only reason it'd do less is if it found the begining since last `lex_entry`)
void lex_rewind(lex_state* const ls, size_t const count);
/// clear and delete everything (do not hold on to tokens!)
void lex_free(lex_state* const ls);
/// compute a preprocessor expression
long lex_eval(lex_state* const ls, char const* const expr);
/// next token, move forward (your char* is at `ls->tokens.ptr+return`, don't hold on to the ptr but to the offset)
size_t lext(lex_state* const ls);

/// make `"a\nb"` (including the quotes and the backslash) from `{0x61, 0x0a, 0x62}`
size_t lex_strquo(char* const quoted, size_t const size, char const* const unquoted, size_t const length);
/// make `{0x61, 0x0a, 0x62}` from `"a\nb"` (including the quotes and the backslash)
size_t lex_struqo(char* const unquoted, size_t const size, char const* const quoted);
/// convenience function for: open file, seek to line, read it into buffer
size_t lex_getline(char* const res, size_t const size, char const* const file, size_t const line);

#define arry(...) struct { __VA_ARGS__* ptr; size_t len, cap; }

struct lex_state {
    arry(struct lex_macro {
        size_t name; // in ls->work
        size_t value; // in ls->work
        size_t length; // of value (can be multiple \0-separated tokens)
        int params; // -1 if object-like
        int marked; // 0 if can expand
    }) macros;
    arry(char const*) paths;

    arry(struct lex_source {
        FILE* stream;
        size_t file; // in ls->work
        size_t line; // 1 based
    }) sources;
    char gotc; // private

    // these flags are private mainly because they can be set/reset unpredicably
    char nlend, // private; count '\n' as soft EOF (eg. in #define ..)
         noxid, // private; disable expanding identifiers
         nomrg, // private; disable merging string literal
         nodir; // private; disable '#' from starting a directive

    char const* cstream; // (essentially) private; will read chars from this until '\0' instead of the current source's stream

    arry(char) work; // (mostly) private; file names, macro values, temporaries, ...

    size_t ahead; // (preferably) private; tokens[len-ahead .. len] is already tokenized (eg. from a macro expansion)
    arry(char) tokens; // \0 \0 <token1> \0 <token2> \0 ... you can look back all you want, but maybe don't interpret anything that's ahead
};

#define frry(__a) (free((__a)->cap ? (__a)->ptr : NULL), (__a)->ptr = NULL, (__a)->len = (__a)->cap = 0)
#define push(__a) ((__a)->len < (__a)->cap || (((__a)->cap || !(__a)->ptr) && ((__a)->ptr = realloc((__a)->ptr, ((__a)->cap+= (__a)->cap+8)*sizeof*(__a)->ptr))) ? (__a)->ptr+(__a)->len++ : (exit(EXIT_FAILURE), (__a)->ptr))
#define grow(__a, __k, __n) _lex_arrygrow((char**)&(__a)->ptr, &(__a)->len, &(__a)->cap, sizeof*(__a)->ptr, (__k), (__n))
#define last(__a) ((__a)->ptr+(__a)->len-1)
#define each(__a) for (void* const _ptr = (__a)->ptr,* const _end = (__a)->ptr+(__a)->len; _end == (__a)->ptr ? (__a)->ptr = _ptr, 0 : 1; ++(__a)->ptr)
static char* _lex_arrygrow(char** const ptr, size_t* const len, size_t* const cap, size_t const s, size_t const k, size_t const n) {
    if (!n) return *ptr+k*s;
    size_t const nlen = *len+n;
    if (*cap < nlen && !(cap && (*ptr = realloc(*ptr, (*cap = nlen)*s)))) exit(EXIT_FAILURE);
    if (k < *len) memmove(*ptr+(k+n)*s, *ptr+k*s, (*len-k)*s);
    return *len = nlen, *ptr+k*s;
}

// everything between / * and * / or // and newline, 0-terminated, no newline
#ifndef on_lex_comment
#define on_lex_comment(ls, cstr) (void)cstr
#endif
// the text given in <>, 0-terminated
#ifndef on_lex_sysinclude
#define on_lex_sysinclude(ls, cstr) (void)cstr
#endif
// the text given in "", 0-terminated
#ifndef on_lex_missinclude
#define on_lex_missinclude(ls, cstr) (void)cstr
#endif
// everything after the '#' exclusive, 0-terminated, no newline
#ifndef on_lex_unknowndir
#define on_lex_unknowndir(ls, cstr) (void)cstr
#endif
// everything after the 'error' exclusive, 0-terminated, no newline
#ifndef on_lex_preprocerr
#define on_lex_preprocerr(ls, cstr) (void)cstr
#endif

// ---

#define curr  (ls->sources.ptr+ls->sources.len-1)
#define endd  (ls->cstream?! *ls->cstream : feof(curr->stream) || ferror(curr->stream))

#define isnum(__c) ('0' <= (__c) && (__c) <= '9')
#define isidh(__c) (('a' <= ((__c)|32) && ((__c)|32) <= 'z') || '_' == (__c))
#define isidt(__c) (isidh((__c)) || isnum((__c)))

#define blankchrs " \t\v\f"
#define nlchrs "\r\n"

static char _lex_getc(lex_state* const ls);

static void _lex_getdelim(lex_state* const ls, char const d) {
    char c;
    while (d != (c = _lex_getc(ls)) && !endd) *push(&ls->work) = c;
}

static void _lex_comment(lex_state* const ls) {
    do {
        _lex_getdelim(ls, '*');
        char const c = _lex_getc(ls);
        if ('/' == c) break;
        *push(&ls->work) = '*';
        *push(&ls->work) = c;
    } while (!endd);
}

static char _lex_getc(lex_state* const ls) {
    if (ls->cstream) {
        char const c = *ls->cstream++;
        return c ? c : EOF;
    }

    char c = ls->gotc ? ls->gotc : fgetc(curr->stream);
    ls->gotc = '\0';
    curr->line+= '\n' == c;
#   define case2(c1, c2, ret) if (c1 == c) { if (c2 == (c = fgetc(curr->stream))) return ret; else ungetc(c, curr->stream), c = c1; }
    case2('\\', '\n', (++curr->line, _lex_getc(ls)));
    case2('<', '%', '{');
    case2('%', '>', '}');
    case2('<', ':', '[');
    case2(':', '>', ']');
    case2('%', ':', '#');
    size_t const comment_at = ls->work.len;
    case2('/', '*', (
        _lex_comment(ls),
        *push(&ls->work) = '\0',
        ls->work.len = comment_at,
        (on_lex_comment(ls, (ls->work.ptr+comment_at))),
        ' '
    ));
    case2('/', '/', (
        _lex_getdelim(ls, '\n'),
        *push(&ls->work) = '\0',
        ls->work.len = comment_at,
        --curr->line, (on_lex_comment(ls, (ls->work.ptr+comment_at))), ++curr->line,
        '\n'
    ));
#   undef case2
    return c;
}

static void _lex_ungetc(lex_state* const ls, char const c) {
    if (ls->cstream) {
        --ls->cstream;
        return;
    }

    ls->gotc = c;
    curr->line-= '\n' == c;
}

static void _lex_fclose(lex_state* const ls) {
    ls->gotc = '\0';
    if (stdin != curr->stream) fclose(curr->stream);
    curr->stream = NULL;

    // yyy: when the last stream is closed, it is kept in the list so the user
    // can get a sane `.file` and `.line` in the same way; ig overall it makes
    // it so `.line` is not outside the file even after it is closed
    --curr->line;

    if (1 < ls->sources.len) --ls->sources.len;
}

static size_t _lex_simple(lex_state* const ls) {
    char c;
#   define next() (c = _lex_getc(ls))
#   define accu() (*push(&ls->tokens) = next())
#   define undo() (_lex_ungetc(ls, ls->tokens.ptr[ls->tokens.len-1]), ls->tokens.ptr[ls->tokens.len-1] = '\0')

    size_t const token_at = ls->tokens.len;
    char const c0 = accu();

    switch (c0) {
    case '"':
    case '\'':
        do if ('\\' == accu()) accu(), accu();
        while (c0 != c && !endd);
        break;

    case '-':
    case '+': case '&': case '|': case '<': case '>': case '#':
    case '*': case '/': case '%': case '^': case '=': case '!':
        accu();
        switch (c0) {
        case '-':
            if ('>' == c) break;
            else
            // fall through
        case '+': case '&': case '|': case '<': case '>': case '#':
            if (c0 == c) {
                if ('<' == c || '>' == c) {
                    if ('=' == next()) *push(&ls->tokens) = '=';
                    else _lex_ungetc(ls, c);
                }
                break;
            } else
            // fall through
        case '*': case '/': case '%': case '^': case '=': case '!':
            if ('=' == c) break;
            else
            // fall through
        default:
            undo();
            return token_at;
        }
        break;

    default:
        if (isidh(c0)) {
            do accu();
            while (isidt(c));
            undo();
            return token_at;
        }

        if (isnum(c0) || '.' == c0) {
            if ('.' == c0 && (accu(), !isnum(c))) {
                if ('.' == c && '.' == accu()) break;
                undo();
                return token_at;
            }

            char const* dgts = "0123456789";
            int fp = 1;
            if ('.' != c0 && '0' == c) switch (accu()|32) {
            case 'b': dgts = "01";       fp = 0; accu(); break;
            case 'o': dgts = "01234567"; fp = 0; accu(); break;
            case 'x': dgts = "0123456789abcdef"; accu(); break;
            }

            while (strchr(dgts, c|32) || '\'' == c) accu();
            if (fp) {
                if ('.' != c0 && '.' == c) {
                    do accu();
                    while (strchr(dgts, c|32) || '\'' == c);
                }
                if ('e' == (c|32) || 'p' == (c|32)) {
                    accu();
                    if ('-' == c || '+' == c) c = '\'';
                    do accu();
                    while (strchr("0123456789", c|32) || '\'' == c);
                }
            }
            while ('f' == c || 'l' == c || 'u' == c) accu();

            undo();
            return token_at;
        }

        if (c0 < ' ' || '~' < c0) {
            ls->tokens.ptr[ls->tokens.len-1] = '\0';

            // maybe
            char const* const blanks = ls->nlend ? blankchrs : nlchrs blankchrs;
            while (strchr(blanks, c = _lex_getc(ls)));
            if ('\n' == c) return token_at;
            _lex_ungetc(ls, c);

            return endd ? token_at : _lex_simple(ls);
        }
    }

#   undef undo
#   undef accu
#   undef next

    *push(&ls->tokens) = '\0';
    return token_at;
}

static size_t _lex_ahead(lex_state* const ls) {
    size_t const token_at = ls->tokens.len - ls->ahead;
    size_t const token_len = strlen(ls->tokens.ptr+token_at);

    if (0x1a == ls->tokens.ptr[token_at]) {
        char const* const name = ls->tokens.ptr+token_at+1;
        ls->ahead-= token_len+1;
        for (size_t k = 0; k < ls->macros.len; ++k) if (!strcmp(ls->work.ptr+ls->macros.ptr[k].name, name)) {
            ls->macros.ptr[k].marked = 0;
            grow(&ls->tokens, token_at+token_len+1, -token_len-1);
            return lext(ls);
        }

        // should be unreachable
        ls->ahead = 0;
        return lext(ls);
    }

    ls->ahead-= token_len+1;
    return token_at;
}

static long _lex_eval_top(lex_state* const ls);

static long _lex_eval_one(lex_state* const ls) {
    size_t const token_at = lext(ls);
    char const* const token = ls->tokens.ptr+token_at;

    switch (*token) {
        union { long l; char b[sizeof(long)]; } r;
    case'\'': return r.l = 0, lex_struqo(r.b, sizeof r.b, token), r.l; // xxx: little endian
    case '+': return +_lex_eval_one(ls);
    case '-': return -_lex_eval_one(ls);
    case '!': return !_lex_eval_one(ls);
    case '~': return ~_lex_eval_one(ls);
    case '(': return (_lex_eval_top(ls));
    }
    if (isnum(*token)) return strtol(token, NULL, 0);
    if (!strcmp("defined", token)) {
        ls->noxid = ls->nodir = 1;
        size_t name_at = lext(ls);
        int const par = '(' == ls->tokens.ptr[name_at];
        if (par) name_at = lext(ls);
        int found = 0;
        for (size_t k = 0; k < ls->macros.len; ++k) if (!strcmp(ls->work.ptr+ls->macros.ptr[k].name, ls->tokens.ptr+name_at)) {
            found = 1;
            break;
        }
        if (par) lext(ls);
        ls->noxid = ls->nodir = 0;
        return found;
    }

    return 0;
}

static long _lex_eval_two(lex_state* const ls, long const lhs, size_t const op_at, int* const is_qu) {
    long rhs = _lex_eval_one(ls);
    size_t const nop_at = lext(ls);

#   define hash2(a, b) (((a)|(b)<<8)%35)
#   define hash1(op) (((op)[0]|(op)[1]<<8)%35)
    int const hop = hash1(ls->tokens.ptr+op_at);
    int then = 0;
    if (ls->tokens.ptr[nop_at] && strchr("!%&*+-/<=>^|", ls->tokens.ptr[nop_at])) {
        static char const prec[] = {
            [hash2('|','|')]=   0,
            [hash2('&','&')]=   1,
            [hash2('|','\0')]=  2,
            [hash2('^','\0')]=  3,
            [hash2('&','\0')]=  4,
            [hash2('=','=')]=   5,
            [hash2('!','=')]=   6,
            [hash2('<','\0')]=  7,
            [hash2('>','\0')]=  8,
            [hash2('<','=')]=   9,
            [hash2('>','=')]=  10,
            [hash2('<','<')]=  11,
            [hash2('>','>')]=  12,
            [hash2('+','\0')]= 13,
            [hash2('-','\0')]= 14,
            [hash2('*','\0')]= 15,
            [hash2('/','\0')]= 16,
            [hash2('%','\0')]= 17,
        };
        if (prec[hop] < prec[hash1(ls->tokens.ptr+nop_at)])
            rhs = _lex_eval_two(ls, rhs, nop_at, is_qu);
        else then = 1;
    } else if ('?' == ls->tokens.ptr[nop_at]) *is_qu = 1;
#   undef hash1

    long r = lhs;
    switch (hop) {
    case hash2('|','|'):  r = lhs || rhs; break;
    case hash2('&','&'):  r = lhs && rhs; break;
    case hash2('|','\0'): r = lhs |  rhs; break;
    case hash2('^','\0'): r = lhs ^  rhs; break;
    case hash2('&','\0'): r = lhs &  rhs; break;
    case hash2('=','='):  r = lhs == rhs; break;
    case hash2('!','='):  r = lhs != rhs; break;
    case hash2('<','\0'): r = lhs <  rhs; break;
    case hash2('>','\0'): r = lhs >  rhs; break;
    case hash2('<','='):  r = lhs <= rhs; break;
    case hash2('>','='):  r = lhs >= rhs; break;
    case hash2('<','<'):  r = lhs << rhs; break;
    case hash2('>','>'):  r = lhs >> rhs; break;
    case hash2('+','\0'): r = lhs +  rhs; break;
    case hash2('-','\0'): r = lhs -  rhs; break;
    case hash2('*','\0'): r = lhs *  rhs; break;
    case hash2('/','\0'): r = lhs /  rhs; break;
    case hash2('%','\0'): r = lhs %  rhs; break;
    }
#   undef hash2

    return then ? _lex_eval_two(ls, r, nop_at, is_qu) : r;
}

static long _lex_eval_top(lex_state* const ls) {
    long const lhs = _lex_eval_one(ls);
    size_t const op_at = lext(ls);

    int is_qu = '?' == ls->tokens.ptr[op_at];
    long const r = ls->tokens.ptr[op_at] && !strchr("?:)", ls->tokens.ptr[op_at])
        ? _lex_eval_two(ls, lhs, op_at, &is_qu)
        : lhs;

    if (is_qu) {
        long const consequence = _lex_eval_top(ls), alternative = _lex_eval_top(ls);
        return r ? consequence : alternative;
    }
    return r;
}

static long _lex_eval(lex_state* const ls) {
    // precondition: 0 == ls->ahead
    size_t const plen = ls->tokens.len;
    ls->nlend = 1;

    long const r = _lex_eval_top(ls);

    ls->nlend = 0;
    ls->tokens.len = plen;
    ls->ahead = 0;
    return r;
}

static void _lex_skipblock(lex_state* const ls, int const can_else) {
    size_t depth = 1;
    int done = 0;
    do {
        size_t const dir_at = ls->work.len;

        char c;
        while (strchr(blankchrs, c = _lex_getc(ls)));

        if ('#' == c) {
            do *push(&ls->work) = c = _lex_getc(ls);
            while (isidt(c));
            _lex_ungetc(ls, ls->work.ptr[ls->work.len-1]);
            ls->work.ptr[ls->work.len-1] = '\0';

#           define diris(__l) (!strcmp(__l, ls->work.ptr+dir_at))

            if (diris("ifdef") || diris("ifndef") || diris("if")) ++depth;
            else if (diris("endif") && !--depth) done = 1;
            else if (can_else && 1 == depth) {
                if (diris("else")) done = 1;
                else if (diris("elif") && (c = '\n', _lex_eval(ls))) done = 1;
            }

#           undef diris
        }

        if ('\n' != c) _lex_getdelim(ls, '\n');
        ls->work.len = dir_at;
    } while (!done && (!endd || (_lex_fclose(ls), curr->stream)));
}

static void _lex_definish(lex_state* const ls, char const maybe_nl, size_t const name_at, int const params) {
    // precondition: 0 == ls->ahead
    size_t const plen = ls->tokens.len;
    ls->nlend = ls->noxid = ls->nodir = 1;

    size_t const value_at = ls->work.len;
    size_t length = 0;
    if ('\n' != maybe_nl) for (size_t token_at; token_at = lext(ls), ls->tokens.ptr[token_at];) {
        size_t const token_len = strlen(ls->tokens.ptr+token_at);
        length+= token_len+1;
        strcpy(grow(&ls->work, ls->work.len, token_len+1), ls->tokens.ptr+token_at);
    }

    struct lex_macro* found = NULL;
    for (size_t k = 0; k < ls->macros.len; ++k) if (!strcmp(ls->work.ptr+ls->macros.ptr[k].name, ls->work.ptr+name_at)) {
        found = ls->macros.ptr+k;
        break;
    }
    *(found ? found : push(&ls->macros)) = (struct lex_macro){
        .name= name_at,
        .value= value_at,
        .length= length,
        .params= params,
    };

    ls->nlend = ls->noxid = ls->nodir = 0;
    ls->tokens.len = plen;
    ls->ahead = 0;
}

static void _lex_directive(lex_state* const ls) {
    char c;
#   define next() (c = _lex_getc(ls))
#   define accu() (*push(&ls->work) = next())

    size_t const dir_at = ls->work.len;

    next(); // #
    while (strchr(blankchrs, next()));
    if ('\n' == c) return;
    *push(&ls->work) = c;

    do accu();
    while (isidt(c));
    _lex_ungetc(ls, ls->work.ptr[ls->work.len-1]);
    ls->work.ptr[ls->work.len-1] = '\0';
    ls->work.len = dir_at;

    while (strchr(blankchrs, next()));

#   define diris(__l) (!strcmp(__l, ls->work.ptr+dir_at))
    if (0) ;

    else if (diris("error")) {
        size_t const message_at = ls->work.len;
        if ('\n' != c) {
            *push(&ls->work) = c;
            _lex_getdelim(ls, '\n');
        }
        *push(&ls->work) = '\0';
        ls->work.len = dir_at;
        _lex_ungetc(ls, '\n');
        on_lex_preprocerr(ls, (ls->work.ptr+message_at));

        _lex_fclose(ls);
    }

    else if (diris("include")) {
        size_t const path_at = ls->work.len;
        int expanded = 0;

        if (isidh(c)) {
            // precondition: 0 == ls->ahead
            size_t const plen = ls->tokens.len;
            ls->nlend = ls->nomrg = 1;

            // xxx: don't wanna support sys-style paths in expansion properly because too janky and you shouldn't anyways
            _lex_ungetc(ls, c);
            size_t const res_at = lext(ls);
            c = strcpy(ls->work.ptr+path_at, ls->tokens.ptr+res_at)[0];
            expanded = 1;

            ls->nlend = ls->nomrg = 0;
            ls->tokens.len = plen;
            ls->ahead = 0;
        }

        if ('"' == c) {
            if (!expanded) {
                while ('"' != accu() && !endd);
                ls->work.ptr[ls->work.len-1] = '\0';
                _lex_getdelim(ls, '\n');
            }
            ls->work.len = dir_at;

            char file[2048];

            char const* const dirname_end = strrchr(ls->work.ptr+curr->file, '/');
            if (dirname_end) {
                memcpy(file, ls->work.ptr+curr->file, dirname_end+1 - ls->work.ptr+curr->file);
                file[dirname_end+1 - ls->work.ptr+curr->file] = '\0';
            } else file[0] = '\0';

            size_t k = 0;
            do {
                if (file[0] && '/' != file[strlen(file)-1]) file[strlen(file)] = '/';
                strcat(file, ls->work.ptr+path_at);

                FILE* const stream = fopen(file, "r");
                if (stream) {
                    // needs to do it manually here because `_lex_ungetc` would carry the '\n' into the new file
                    ungetc('\n', curr->stream), --curr->line, ls->gotc = '\0';
                    size_t const file_at = ls->work.len;
                    strcpy(grow(&ls->work, ls->work.len, strlen(file)+1), file);
                    *push(&ls->sources) = (struct lex_source){.stream= stream, .file= file_at, .line= 1};
                    break;
                }

                if (k >= ls->paths.len) {
                    ls->work.len = dir_at;
                    _lex_ungetc(ls, '\n');
                    on_lex_missinclude(ls, (ls->work.ptr+path_at));
                    break;
                }
                strcpy(file, ls->paths.ptr[k++]);
            } while (1);
        }

        else if ('<' == c) {
            while ('>' != accu() && !endd);
            ls->work.ptr[ls->work.len-1] = '\0';
            _lex_getdelim(ls, '\n');
            ls->work.len = dir_at;
            _lex_ungetc(ls, '\n');
            on_lex_sysinclude(ls, (ls->work.ptr+path_at));
        }

        else _lex_ungetc(ls, '\n');
    }

    else if (diris("define")) {
        size_t const name_at = ls->work.len;
        *push(&ls->work) = c;
        do accu();
        while (isidt(c));
        ls->work.ptr[ls->work.len-1] = '\0';

        int params = -1;
        if ('(' == c) {
            ++params;
            while (strchr(blankchrs, next()));

            while (')' != c && !endd) {
                *push(&ls->work) = c;
                do accu();
                while (isidt(c) || '.' == c);
                ls->work.ptr[ls->work.len-1] = '\0';

                ++params;
                if (')' == c) break;
                while (strchr(blankchrs, next()));
                if (',' == c) while (strchr(blankchrs, next()));
            }
        }

        _lex_definish(ls, c, name_at, params);
        _lex_ungetc(ls, '\n');
    }

    else if (diris("undef")) {
        size_t const name_at = ls->work.len;

        *push(&ls->work) = c;
        do accu();
        while (!strchr(nlchrs blankchrs, c));
        ls->work.ptr[ls->work.len-1] = '\0';
        ls->work.len = dir_at;

        lex_undef(ls, ls->work.ptr+name_at);
        if ('\n' != c) {
            _lex_getdelim(ls, '\n');
            ls->work.len = dir_at;
        }
        _lex_ungetc(ls, '\n');
    }

    else if (diris("ifdef") || diris("ifndef") || diris("if")) {
        _lex_ungetc(ls, c);

        int cond = 0;
        if ('\0' == ls->work.ptr[dir_at+2]) cond = !!_lex_eval(ls);
        else {
            size_t const name_at = ls->work.len;
            int const n = 'n' == ls->work.ptr[dir_at+2];

            do accu();
            while (!strchr(nlchrs blankchrs, c));
            ls->work.ptr[ls->work.len-1] = '\0';
            ls->work.len = dir_at;

            for (size_t k = 0; k < ls->macros.len; ++k) if (!strcmp(ls->work.ptr+ls->macros.ptr[k].name, ls->work.ptr+name_at)) {
                cond = 1;
                break;
            }
            if (n) cond = !cond;
        }

        if (!cond) _lex_skipblock(ls, 1);
        else _lex_ungetc(ls, '\n');
    }

    else if (diris("else") || diris("elif")) {
        _lex_skipblock(ls, 0);
    }

    else if (diris("endif")) {
        if ('\n' != c) {
            _lex_getdelim(ls, '\n');
            ls->work.len = dir_at;
        }
        _lex_ungetc(ls, '\n');
    }

    else if (diris("line")) {
        // precondition: 0 == ls->ahead
        size_t const plen = ls->tokens.len;
        ls->nlend = ls->nomrg = 1;

        _lex_ungetc(ls, c);
        size_t const line_at = lext(ls), file_at = lext(ls);
        curr->line = atol(ls->tokens.ptr+line_at);
        if ('"' == ls->tokens.ptr[file_at]) {
            size_t const file_len = strlen(ls->tokens.ptr+file_at+1);
            ls->tokens.ptr[file_at+1+file_len-1] = '\0';
            curr->file = ls->work.len;
            strcpy(grow(&ls->work, ls->work.len, file_len+1), ls->tokens.ptr+file_at+1);
        } else ++curr->line;

        ls->nlend = ls->nomrg = 0;
        ls->tokens.len = plen;
        ls->ahead = 0;
        _lex_ungetc(ls, '\n');
    }

    else {
        ls->work.len+= strlen(ls->work.ptr+dir_at)+1;
        if ('\n' != c) {
            ls->work.ptr[ls->work.len-1] = ' ';
            *push(&ls->work) = c;
            _lex_getdelim(ls, '\n');
            *push(&ls->work) = '\0';
        } else ls->work.ptr[ls->work.len-1] = '\0';
        ls->work.len = dir_at;
        _lex_ungetc(ls, '\n');
        on_lex_unknowndir(ls, (ls->work.ptr+dir_at));
    }

#   undef diris

#   undef accu
#   undef next
}

static size_t _lex_expand_arg_asis(lex_state* const ls,
        struct lex_macro const* const macro, void const* const arry_args,
        char const* const name, size_t const name_len,
        size_t const range_start, size_t const range_end) {
    int const va_args = !strcmp("__VA_ARGS__", name);
    int found = -1;
    if (isidh(name[0])) {
        char const* const search_name = va_args ? "..." : name;
        char const* curr_name = ls->work.ptr+macro->name;
        for (int k = 0; curr_name+= strlen(curr_name)+1, k < macro->params; ++k) if (!strcmp(curr_name, search_name)) {
            found = k;
            break;
        }
    }

    if (!isidh(name[0]) || -1 == found) {
        memmove(ls->tokens.ptr+range_start, name, name_len);
        grow(&ls->tokens, range_end, name_len - (range_end-range_start));
        return name_len;
    }

    else {
        arry(size_t) const* const args = arry_args;

        size_t from_at = ls->work.len-1, repl_len = 1;
        if (found+1u == args->len || va_args) repl_len = ls->work.len - (from_at = args->ptr[found]);
        else if ((unsigned)found < args->len) repl_len = args->ptr[found+1]-2 - (from_at = args->ptr[found]);

        grow(&ls->tokens, range_end, repl_len - (range_end-range_start));
        memcpy(ls->tokens.ptr+range_start, ls->work.ptr+from_at, repl_len);
        return repl_len;
    }
}

static void _lex_expand_id_here(lex_state* const ls, size_t const token_at) {
    time_t tt;
    char replace[2048];
    size_t replace_len = 0;

#   define nameis(__l) (!strcmp(__l, ls->tokens.ptr+token_at))
    if (0) ;

    else if (nameis("__FILE__")) replace_len = lex_strquo(replace, sizeof replace-1, ls->work.ptr+curr->file, strlen(ls->work.ptr+curr->file));
    else if (nameis("__LINE__")) replace_len = snprintf(replace, sizeof replace-1, "%zu", curr->line);
    else if (nameis("__DATE__")) replace_len = strftime(replace, sizeof replace-1, "\"%b %e %Y\"", localtime((time(&tt), &tt)));
    else if (nameis("__TIME__")) replace_len = strftime(replace, sizeof replace-1, "\"%T\"", localtime((time(&tt), &tt)));

    else for (size_t k = 0; k < ls->macros.len; ++k) if (!ls->macros.ptr[k].marked && nameis(ls->work.ptr+ls->macros.ptr[k].name)) {
        struct lex_macro* const macro = ls->macros.ptr+k;
        arry(size_t) args = {0};

        if (-1 < macro->params) {
            ls->noxid = 1;
            size_t const par_at = lext(ls);
            ls->noxid = 0;
            if ('(' != ls->tokens.ptr[par_at]) {
                ls->ahead+= strlen(ls->tokens.ptr+par_at)+1;
                break;
            }
            // ls->tokens: name @ ( @ a @ , @ b @ ) @ ...
            //             (^r)   ^ [ doesn't exists -]

            ls->noxid = ls->nodir = 1;
            size_t comma_at;
            char c = 0;
            for (size_t p = 0; ')' != c && !endd; ++p) {
                size_t const since_at = ls->tokens.len - ls->ahead - 2; // ( @
                *push(&args) = ls->work.len+2;

                unsigned depth = 0;
                do {
                    comma_at = lext(ls), c = ls->tokens.ptr[comma_at];
                    if (!depth && ')' == c) break;
                    depth+= ('(' == c) - (')' == c);
                } while ((depth || ',' != c) && !endd);
                // ls->tokens: name @ ( @ a @ , @ b @ ) @ ...
                //                            ^

                size_t const length = comma_at-since_at;
                memcpy(grow(&ls->work, ls->work.len, length), ls->tokens.ptr+since_at, length);
                grow(&ls->tokens, comma_at, -length);
                comma_at-= length;
                // ls->tokens: name @ , @ b @ ) @ ...
                //                    ^
                // ls->work: ( @ a1 @ a2 @
                // ...
                // ls->work: ( @ a1 @ a2 @ , @ b1 @ b2 @
            }
            ls->noxid = ls->nodir = 0;
            // ls->tokens: name @ ) @ ...
            grow(&ls->tokens, comma_at+2, -2);
            // ls->tokens: name @ ...
        }

        memcpy(grow(&ls->tokens, token_at, macro->length+1), ls->work.ptr+macro->value, macro->length);
        ls->tokens.ptr[token_at+macro->length] = 0x1a;

        if (args.len) {
            for (size_t now_at = token_at, end_at = token_at+macro->length; now_at < end_at;) {
                char* const token = ls->tokens.ptr+now_at;
                size_t repl_len, range_len;

                if ('#' == token[0]) {
                    if ('#' == token[1]) {
                        // AAA @ ## @ bbb @  ->  AAABBB @
                        size_t const next_len = strlen(token+3);
                        repl_len = _lex_expand_arg_asis(ls,
                                macro, &args,
                                token+3, next_len+1,
                                now_at-1, now_at+3+next_len+1);
                        range_len = next_len+5;

                        if (!repl_len) *grow(&ls->tokens, now_at-1, 1) = '\0';
                        else --repl_len;
                    } else {
                        // # @ ccc @  ->  # CCC @ @  ->  " CCC " @
                        size_t const next_len = strlen(token+2);
                        repl_len = _lex_expand_arg_asis(ls,
                                macro, &args,
                                token+2, next_len+1,
                                now_at+1, now_at+2+next_len);
                        range_len = next_len+2;

                        if (!repl_len) *grow(&ls->tokens, now_at+1, 1) = '"';
                        ls->tokens.ptr[now_at] = ls->tokens.ptr[now_at+repl_len++] = '"';
                        for (size_t z = now_at+1; z < now_at+repl_len-1; ++z) {
                            char* const c = ls->tokens.ptr+z;
                            if ('\0' == *c) *c = ' ';
                            else if ('"' == *c || '\\' == *c) {
                                *grow(&ls->tokens, z++, 1) = '\\';
                                ++repl_len;
                            }
                        }
                    } // "#"
                } // "##?"

                else if (isidh(token[0])) {
                    range_len = strlen(token)+1;
                    int const past = !strcmp("##", token+range_len);
                    repl_len = _lex_expand_arg_asis(ls,
                            macro, &args,
                            token, range_len,
                            now_at, now_at+range_len);

                    if (!past) {
                        memcpy(grow(&ls->tokens, now_at+repl_len, 2), "\x18", 2);
                        ls->ahead = ls->tokens.len - now_at;
                        size_t indicator_at;
                        ls->nodir = 1;
                        do indicator_at = lext(ls);
                        while ('\x18' != ls->tokens.ptr[indicator_at]);
                        ls->nodir = 0;
                        grow(&ls->tokens, indicator_at+2, -2);
                        repl_len = indicator_at-now_at;
                    } // "id"
                } // "id" "##"?

                else repl_len = range_len = strlen(token)+1;

                now_at+= repl_len;
                end_at+= repl_len - range_len;
            }

            ls->work.len = args.ptr[0]-2;
            frry(&args);
        } // function-like

        else for (size_t now_at = token_at, end_at = token_at+macro->length; now_at < end_at;) {
            if ('#' == ls->tokens.ptr[now_at] && '#' == ls->tokens.ptr[now_at+1]) {
                // aaa @ ## @ bbb @  ->  aaabbb @
                grow(&ls->tokens, now_at+3, -4);
                --now_at;
                end_at-= 4;
            }
            now_at+= strlen(ls->tokens.ptr+now_at)+1;
        } // object-like

        ls->ahead = ls->tokens.len - token_at;
        macro->marked = 1;
        lext(ls);
        return;
    }
#   undef nameis

    if (replace_len) {
        size_t const token_len = strlen(ls->tokens.ptr+token_at);
        grow(&ls->tokens, token_at+token_len, replace_len-token_len);
        strcpy(ls->tokens.ptr+token_at, replace);
    }
}

void lex_define(lex_state* const ls, char const* const name, char const* const value) {
    size_t const name_at = ls->work.len, name_len = strcspn(name, "(");
    memcpy(grow(&ls->work, ls->work.len, name_len+1), name, name_len);
    ls->work.ptr[ls->work.len-1] = '\0';

    int params = -1;
    if ('(' == name[name_len]) {
        size_t k = name_len;
        ++params;
        while (strchr(blankchrs, name[++k]));

        while (')' != name[k] && name[k]) {
            do *push(&ls->work) = name[k++];
            while (isidt(name[k]) || '.' == name[k]);
            *push(&ls->work) = '\0';

            ++params;
            if (')' == name[k]) break;
            while (strchr(blankchrs, name[++k]));
            if (',' == name[k]) while (strchr(blankchrs, name[++k]));
        }
    }

    ls->cstream = value;
    _lex_definish(ls, *value, name_at, params);
    ls->cstream = NULL;
}

void lex_undef(lex_state* const ls, char const* const name) {
    for (size_t k = 0; k < ls->macros.len; ++k) if (!strcmp(name, ls->work.ptr+ls->macros.ptr[k].name)) {
        grow(&ls->macros, k+1, -1);
        break;
    }
}

void lex_incdir(lex_state* const ls, char const* const path) {
    *push(&ls->paths) = path;
}

void lex_entry(lex_state* const ls, FILE* const stream, char const* const file) {
    size_t const file_at = ls->work.len;
    strcpy(grow(&ls->work, ls->work.len, strlen(file)+1), file);
    *(!ls->sources.len ? push(&ls->sources) : curr) = (struct lex_source){
        .stream= stream,
        .file= file_at,
        .line= 1,
    };
    *push(&ls->tokens) = '\0';
    *push(&ls->tokens) = '\0';
}

void lex_rewind(lex_state* const ls, size_t const count) {
    for (size_t k = 0; k < count && ls->ahead+2 < ls->tokens.len; ++k) {
        ls->ahead+= 2;
        while (ls->tokens.ptr[ls->tokens.len - ls->ahead-1]) ++ls->ahead;
    }
}

void lex_free(lex_state* const ls) {
    frry(&ls->macros);
    frry(&ls->paths);
    each (&ls->sources) {
        if (ls->sources.ptr->stream && stdin != ls->sources.ptr->stream)
            fclose(ls->sources.ptr->stream);
        ls->sources.ptr->stream = NULL;
    }
    frry(&ls->sources);
    frry(&ls->work);
    frry(&ls->tokens);
}

long lex_eval(lex_state* const ls, char const* const expr) {
    ls->cstream = expr;
    long const r = _lex_eval(ls);
    ls->cstream = NULL;
    return r;
}

size_t lext(lex_state* const ls) {
#   define eof_token  (*push(&ls->tokens) = '\0', ls->tokens.len-1)
    if (!ls->ahead) {
        if (!ls->cstream && (!ls->sources.len || !curr->stream)) return eof_token;

        size_t const pline = ls->cstream ? 0 : curr->line;

        char c;
        char const* const blanks = ls->nlend ? blankchrs : nlchrs blankchrs;
        while (strchr(blanks, c = _lex_getc(ls)));
        if ('\n' == c) return eof_token;
        _lex_ungetc(ls, c);

        if (endd) {
            if (ls->cstream) return eof_token;
            _lex_fclose(ls);
            return lext(ls);
        }

        if (pline && '#' == c && !ls->nodir && ('\0' == ls->tokens.ptr[ls->tokens.len-2] || pline != curr->line)) {
            _lex_directive(ls);
            return lext(ls);
        }
    }
#   undef eof_token

    size_t const r = ls->ahead ? _lex_ahead(ls) : _lex_simple(ls);

    if (!ls->noxid && isidh(ls->tokens.ptr[r]))
        _lex_expand_id_here(ls, r);

    if (!ls->nomrg && '"' == ls->tokens.ptr[r]) {
        size_t const n = lext(ls);
        if ('"' == ls->tokens.ptr[n]) grow(&ls->tokens, n+1, -3);
        else ls->ahead+= strlen(ls->tokens.ptr+n)+1;
    }

    return r;
}

size_t lex_strquo(char* const quoted, size_t const size, char const* const unquoted, size_t const length) {
    if (size <3 +length) return 0;
    size_t d = 1, s = 0;
    quoted[0] = '"';

    while (d < size && s < length) {
        char c = unquoted[s++];

        if (' ' <= c && c <= '~' && !strchr("'\"?\\", c)) quoted[d++] = c;
        else {
            switch(c) {
            case '\'': c ='\''; break;
            case '\"': c = '"'; break;
            case '\?': c = '?'; break;
            case '\\': c ='\\'; break;
            case '\a': c = 'a'; break;
            case '\b': c = 'b'; break;
            case '\f': c = 'f'; break;
            case '\n': c = 'n'; break;
            case '\r': c = 'r'; break;
            case '\t': c = 't'; break;
            case '\v': c = 'v'; break;

            default:
                if (d+4 >= size) return 0;
                quoted[d++] = '\\';
                quoted[d++] = (c>>6 & 3) + '0';
                quoted[d++] = (c>>3 & 7) + '0';
                quoted[d++] = (c & 7) + '0';
                continue;
            }

            if (d+2 >= size) return 0;
            quoted[d++] = '\\';
            quoted[d++] = c;
        }
    }

    if (d+2 >= size) return 0;
    quoted[d++] = '"';
    quoted[d] = '\0';
    return d;
}

size_t lex_struqo(char* const unquoted, size_t const size, char const* const quoted) {
    char const c0 = quoted[0];
    size_t d = 0, s = 1;

    while (d < size && c0 != quoted[s]) {
        char c = quoted[s++];
        if (!c) return 0;

        if ('\\' != c) unquoted[d++] = c;
        else switch (c = quoted[s++]) {
        case'\'': unquoted[d++] = '\''; break;
        case '"': unquoted[d++] = '\"'; break;
        case '?': unquoted[d++] = '\?'; break;
        case'\\': unquoted[d++] = '\\'; break;
        case 'a': unquoted[d++] = '\a'; break;
        case 'b': unquoted[d++] = '\b'; break;
        case 'f': unquoted[d++] = '\f'; break;
        case 'n': unquoted[d++] = '\n'; break;
        case 'r': unquoted[d++] = '\r'; break;
        case 't': unquoted[d++] = '\t'; break;
        case 'v': unquoted[d++] = '\v'; break;

            long unsigned r;
            unsigned n;

        case 'x':
            r = 0;
            n = -1u;
            if (0)
        case 'u':
        case 'U':
                n = 4+(1-(c>>5&1))*4;

            static char const dgts[] = "0123456789abcdef";
            char const* v = strchr(dgts, quoted[s]|32);
            do r = (r<<4) + (v-dgts);
            while (--n && (v = strchr(dgts, quoted[++s]|32)));

            if ('x' == c) {
                unquoted[d++] = r;
                break;
            }

            if (r < 128) unquoted[d++] = r;
            else {
                if (d+2 >= size) return 0;
                char const x = r&63;
                r>>= 6;
                if (r < 32) unquoted[d++] = 192|r;
                else {
                    if (d+3 >= size) return 0;
                    char const y = r&63;
                    r>>= 6;
                    if (r < 16) unquoted[d++] = 224|r;
                    else {
                        if (d+4 >= size) return 0;
                        char const z = r&63;
                        unquoted[d++] = 240|r>>6;
                        unquoted[d++] = 128|z;
                    }
                    unquoted[d++] = 128|y;
                }
                unquoted[d++] = 128|x;
            }
            break;

        default:
            if ('0' <= c && c <= '7') {
                r = 0;
                do r = (r<<3) + (c-'0');
                while (c = quoted[s++], '0' <= c && c <= '7');
                --s;
                unquoted[d++] = r;
            } else unquoted[d++] = c;
        }
    }

    if (d+1 >= size) return 0;
    unquoted[d++] = '\0';
    return quoted[s+1] ? 0 : d;
}

size_t lex_getline(char* const res, size_t const size, char const* const file, size_t const line) {
    FILE* stream;
    if (!line || !(stream = fopen(file, "r"))) return *res = 0;

    for (size_t k = 0; k < line-1; k+= '\n' == getc(stream))
        if (feof(stream) || ferror(stream)) return fclose(stream), *res = 0;

    size_t r = 0;
    do {
        if ('\n' == (res[r] = getc(stream))) break;
        if (++r >= size || ferror(stream)) return fclose(stream), *res = 0;
    } while (!feof(stream));

    res[r] = '\0';
    return fclose(stream), r;
}

#undef nlchrs
#undef blankchrs

#undef isid_t
#undef isid_h
#undef isnum

#undef endd
#undef curr

#undef each
#undef last
#undef grow
#undef push
#undef frry

#undef arry

#endif // CINTRE_LEXER_H
