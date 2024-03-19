/// C lexer with inbuilt preprocessor; example:
/// ```c
/// lex_state ls = {0};              // cpp
/// ldef(&ls, "_XOPEN_SOURCE", "1"); //  -D_XOPEN_SOURCE=1
/// linc(&ls, "./lib/include");      //  -I./lib/include
/// lini(&ls, "./main.c");           //  ./main.c
///
/// while (!lend(&ls)) {
///     bufsl const token = lext(&ls);
///     // note: the last ("EOF") token has `token.len == 0`
///     printf("[%.*s:%zu] %.*s\n", (int)ls.file.len, ls.file.ptr, ls.line, (int)token.len, token.ptr);
/// }
///
/// ldel(&ls);
/// ```
///
/// comments can also be obtained by defining `on_lcom`,
/// unrecognised preprocessor directives (such as pragmas) with `on_lunr`
/// and "system includes" (angle bracket syntax) with `on_lsys`:
/// ```c
/// #define on_lcom(com) printf("%.*s\n", (unsigned)com.len, com.ptr);
/// #define on_lunr(dir) printf("%.*s\n", (unsigned)dir.len, dir.ptr);
/// #define on_lsys(pth) printf("%.*s\n", (unsigned)pth.len, pth.ptr);
/// ```

#include "common.h"

typedef struct lex_state {
    bufsl slice;
    bufsl file;
    size_t line;
    unsigned macro_depth;
    dyarr(struct _lex_state_source {
        bufsl file;
        buf text;
    }) sources;
    dyarr(bufsl) include_paths;
    dyarr(struct _lex_state_macro {
        bufsl name;
        bufsl repl;
        dyarr(struct { bufsl name; }) params;
    }) macros;
    dyarr(struct _lex_state_hold {
        bufsl slice;
        bufsl file;
        size_t line;
    }) include_stack;
    dyarr(char) ifdef_stack;
    dyarr(buf) workbufs;
} lex_state;

/// add to defined macros
void ldef(lex_state ref ls, char cref name, char cref value);
/// add to include paths
void linc(lex_state ref ls, char cref path);
/// set the entry file, lexer ready to go
void lini(lex_state ref ls, char cref entry);
/// clear and delete everything (do not hold on to bufsl tokens!)
void ldel(lex_state ref ls);
/// true if no more token (also last token is empty)
#define lend(__ls) (!(__ls)->slice.len && !(__ls)->include_stack.len)
/// compute a preprocessor expression
long lxpr(lex_state cref ls, bufsl ref xpr);
/// next token, move forward
bufsl lext(lex_state ref ls);

void ldef(lex_state ref ls, char cref name, char cref value) {
    struct _lex_state_macro* it = dyarr_push(&ls->macros);
    if (!it) exitf("OOM");
    *it = (struct _lex_state_macro){0};
    it->name.len = strlen(it->name.ptr = name);
    it->repl.len = strlen(it->repl.ptr = value);
}

void linc(lex_state ref ls, char cref path) {
    bufsl* it = dyarr_push(&ls->include_paths);
    if (!it) exitf("OOM");
    it->ptr = path;
    it->len = strlen(path);
}

void lini(lex_state ref ls, char cref entry) {
    struct _lex_state_source* src = dyarr_push(&ls->sources);
    if (!src) exitf("OOM");
    src->file = (bufsl){.ptr= entry, .len= strlen(entry)};
    src->text = read_all(entry);
    if (!src->text.len) exitf("Could not open entry file %s", entry);
    ls->slice.ptr = src->text.ptr;
    ls->slice.len = src->text.len;
    ls->file = src->file;
    ls->line = 1;
}

void ldel(lex_state ref ls) {
    for (size_t k = 0; k < ls->sources.len; k++) dyarr_clear(&ls->sources.ptr[k].text);
    dyarr_clear(&ls->sources);
    dyarr_clear(&ls->include_paths);
    for (size_t k = 0; k < ls->macros.len; k++) dyarr_clear(&ls->macros.ptr[k].params);
    dyarr_clear(&ls->macros);
    dyarr_clear(&ls->ifdef_stack);
    for (size_t k = 0; k < ls->workbufs.len; k++) dyarr_clear(&ls->workbufs.ptr[k]);
    dyarr_clear(&ls->workbufs);
}

static long _lex_atmxpr(lex_state cref ls, bufsl ref xpr) {
#   define nx() (++xpr->ptr, --xpr->len)
#   define at() (*xpr->ptr)
    long r = 0;
    if (!xpr->len) return 0;
    while (strchr(" \t\n\\", at()) && nx());
    if ('0' <= at() && at() <= '9') {
        unsigned shft = 0;
        char const* dgts = "0123456789";
        if ('0' == at() && nx()) switch (at()) {
        case 'x':
        case 'X': nx(); shft = 4; dgts = "0123456789abcdef"; break;
        default:        shft = 3; dgts = "01234567";         break;
        }
        if (!xpr->len) return 0;
        char const* v = strchr(dgts, at()|32);
        if (!v) return 0;
        do r = (!shft ? r*10 : r<<shft) + (v-dgts);
        while (nx() && (v = strchr(dgts, at()|32)));
        if (xpr->len && strchr("ulUL", at())) nx();
    } else if ('\'' == at()) {
        while (nx() && '\'' != at()) {
            if ('\\' == at()) switch (nx() ? at() : 0) {
            case '0': r = r<<8 | '\0'; break;
            case'\'': r = r<<8 | '\''; break;
            case '"': r = r<<8 | '\"'; break;
            case '?': r = r<<8 | '\?'; break;
            case'\\': r = r<<8 | '\\'; break;
            case 'a': r = r<<8 | '\a'; break;
            case 'b': r = r<<8 | '\b'; break;
            case 'f': r = r<<8 | '\f'; break;
            case 'n': r = r<<8 | '\n'; break;
            case 'r': r = r<<8 | '\r'; break;
            case 't': r = r<<8 | '\t'; break;
            case 'v': r = r<<8 | '\v'; break;
            default: return 0;
            } else r = r<<8 | at();
        }
        nx();
    } else if ('(' == at() && nx()) {
        r = lxpr(ls, xpr);
        if (xpr->len && ')' == at()) nx();
    } else if ('_' == at() || ('a' <= (at()|32) && (at()|32) <= 'z')) {
        bool defd = strlen("defined") < xpr->len && !memcmp("defined", xpr->ptr, strlen("defined"));
        bool pars;
        if (defd) {
            xpr->ptr+= strlen("defined");
            xpr->len-= strlen("defined");
            if (!xpr->len) return 0;
            while (strchr(" \t\n\\", at()) && nx());
            pars = xpr->len && '(' == at();
            if (pars && nx()) while (strchr(" \t\n\\", at()) && nx());
        }
        bufsl name = {.ptr= xpr->ptr};
        while (nx() && ('_' == at() || ('A' <= at() && at() <= 'Z') || ('a' <= at() && at() <= 'z') || ('0' <= at() && at() <= '9')));
        name.len = xpr->ptr - name.ptr;
        search_namespace(name, ls->macros) {
            bufsl w = ls->macros.ptr[k].repl;
            r = defd ? 1 : lxpr(ls, &w);
            break;
        }
        if (pars && xpr->len) {
            while (strchr(" \t\n\\", at()) && nx());
            if (xpr->len && ')' == at()) nx();
        }
    } else switch (at()) {
    case '-': nx(); r = -lxpr(ls, xpr); break;
    case '+': nx(); r = +lxpr(ls, xpr); break;
    case '~': nx(); r = ~lxpr(ls, xpr); break;
    case '!': nx(); r = !lxpr(ls, xpr); break;
    }
    return r;
}
enum _lex_operator {
    _lex_opnone,
    _lex_oplor,
    _lex_opland,
    _lex_opbor,
    _lex_opbxor,
    _lex_opband,
    _lex_opeq, _lex_opne,
    _lex_oplt, _lex_opgt, _lex_ople, _lex_opge,
    _lex_opbshl, _lex_opbshr,
    _lex_opadd, _lex_opsub,
    _lex_opmul, _lex_opdiv, _lex_oprem,
};
static inline long _lex_exexpr(long const lhs, enum _lex_operator const op, long const rhs) {
    switch (op) {
    case _lex_oplor:  return lhs || rhs;
    case _lex_opland: return lhs && rhs;
    case _lex_opbor:  return lhs |  rhs;
    case _lex_opbxor: return lhs ^  rhs;
    case _lex_opband: return lhs &  rhs;
    case _lex_opeq:   return lhs == rhs;
    case _lex_opne:   return lhs != rhs;
    case _lex_oplt:   return lhs <  rhs;
    case _lex_opgt:   return lhs >  rhs;
    case _lex_ople:   return lhs <= rhs;
    case _lex_opge:   return lhs >= rhs;
    case _lex_opbshl: return lhs << rhs;
    case _lex_opbshr: return lhs >> rhs;
    case _lex_opadd:  return lhs +  rhs;
    case _lex_opsub:  return lhs -  rhs;
    case _lex_opmul:  return lhs *  rhs;
    case _lex_opdiv:  return lhs /  rhs;
    case _lex_oprem:  return lhs %  rhs;
    default: return 0;
    }
}
static long _lex_oprxpr(lex_state cref ls, bufsl ref xpr, long lhs, enum _lex_operator lop) {
    long rhs;
    enum _lex_operator nop;
again:
    if (lop) {
        rhs = lxpr(ls, xpr);
        if (xpr->len) while (strchr(" \t\n\\", at()) && nx());
    }
    nop = _lex_opnone;
    char c;
    if (xpr->len) switch (c = at()) {
    case '+': nx(); nop = _lex_opadd;  break;
    case '-': nx(); nop = _lex_opsub;  break;
    case '*': nx(); nop = _lex_opmul;  break;
    case '/': nx(); nop = _lex_opdiv;  break;
    case '%': nx(); nop = _lex_oprem;  break;
    case '^': nx(); nop = _lex_opbxor; break;
    case '=': case '!':
        if (nx() && '=' == at()) nx();
        else return 0;
        nop = '=' == c ? _lex_opeq : _lex_opne;
    case ':':
        break;
    case '<': case '>':
        if (1 < xpr->len && '=' == xpr->ptr[1]) nx(), nop = '<' == c ? _lex_ople : _lex_opge;
        else
        // fall through
    case '&': case '|':
        if (1 < xpr->len && c == xpr->ptr[1]) nx(), nop = '&' == c ? _lex_opland : '|' == c ? _lex_oplor : '<' == c ? _lex_opbshl : _lex_opbshr;
        else nop = '&' == c ? _lex_opband : '|' == c ? _lex_opbor : '<' == c ? _lex_oplt : _lex_opgt;
        // fall through
    default: nx();
    }
    if (!lop && nop) { lop = nop; goto again; }
    if (lop < nop) {
        rhs = _lex_exexpr(lhs, nop, _lex_oprxpr(ls, xpr, rhs, nop));
        nop = lop;
    } else {
        lhs = _lex_exexpr(lhs, lop, rhs);
        if (nop) lhs = _lex_oprxpr(ls, xpr, lhs, nop);
    }
    return nop ? _lex_exexpr(lhs, nop, rhs) : lhs;
}
long lxpr(lex_state cref ls, bufsl ref xpr) {
    long first = _lex_atmxpr(ls, xpr);
    if (xpr->len) while (strchr(" \t\n\\", at()) && nx());
    if ('?' == at() && nx()) {
        long maybe = lxpr(ls, xpr);
        if (xpr->len && ':' == at()) nx();
        else return 0;
        return first ? maybe : lxpr(ls, xpr);
    }
    return xpr->len && !strchr(":)", at()) ? _lex_oprxpr(ls, xpr, first, _lex_opnone) : first;
#   undef at
#   undef nx
}

bufsl lext(lex_state ref ls) {
#   define nx() (ls->line+= --ls->slice.len && '\n' == *ls->slice.ptr && !ls->macro_depth, ++ls->slice.ptr)
#   define at() (*ls->slice.ptr)
#   define has(n) ((n) <= ls->slice.len)
#   define is(c) ((c) == at())
#   define isin(lo, hi) ((lo) <= at() && at() <= (hi))
#   define isid() (isin('A', 'Z') || isin('a', 'z') || is('_') || isin('0', '9'))
#   define skip(cx) while (has(1) && (cx)) nx()
#   define accu(wh) for ((wh).len = 0, (wh).ptr = ls->slice.ptr; !(wh).len; (wh).len = ls->slice.ptr-(wh).ptr)

    skip(strchr(" \t\n\r", at()));
    if (has(2) && is('\\')) return nx(), lext(ls);
    if (has(2) && is('/') && strchr("*/", (&at())[1])) {
#       ifdef on_lcom
        bufsl comtxt;
        accu(comtxt)
#       endif
        {
            nx();
            if ('*' == at()) while (nx(), has(2)) {
                if (is('*')) {
                    while (nx(), has(2) && is('\\') && '\n' == (&at())[1]) nx();
                    if (is('/')) { nx(); break; }
                }
            } else do if (has(1) && is('\\')) nx();
                while (nx(), has(1) && !is('\n'));
        }
#       ifdef on_lcom
        on_lcom(comtxt);
#       endif
        return lext(ls);
    }

    bool disab = ls->ifdef_stack.len && *dyarr_top(&ls->ifdef_stack);

    if (!ls->macro_depth && has(1) && is('#')) {
        nx();
        skip(strchr(" \t", at()));
        bufsl dir;
        accu(dir) skip(isid());
#       define diris(wo) (!memcmp(wo, dir.ptr, dir.len < strlen(wo) ? strlen(wo) : dir.len))

        if (!disab && diris("error")) {
            skip(strchr(" \t", at()));
            bufsl err;
            accu(err) {
                do if (has(1) && is('\\')) nx();
                while (nx(), has(1) && !is('\n'));
            }
            exitf("Error: %.*s", (unsigned)err.len, err.ptr);
        }

        else if (!disab && diris("include")) {
            skip(strchr(" \t", at()));
            char c = at();
            nx();
            bool s;
            bufsl path;
            accu(path) {
                if ((s = '<' == c)) skip(!is('>'));
                else if ('"' == c) skip(!is('"'));
            }
            nx();
            if (!s) {
                struct _lex_state_hold* hold = dyarr_push(&ls->include_stack);
                struct _lex_state_source* src = dyarr_push(&ls->sources); // xxx: should avoid re-reading
                if (src) src->text.ptr = NULL;
                if (!hold || !src) exitf("OOM");
                src->file = path;
                for (size_t k = 0; k < ls->include_paths.len; k++) {
                    char file[1024];
                    size_t n = 0;
                    bufsl const it = ls->include_paths.ptr[k];
                    memcpy(file+n, it.ptr, it.len); n+= it.len;
                    if ('/' != it.ptr[it.len-1]) file[n++] = '/';
                    memcpy(file+n, path.ptr, path.len); n+= path.len;
                    file[n] = '\0';
                    src->text = read_all(file);
                    if (src->text.len) break;
                }
                hold->slice = ls->slice;
                hold->file = ls->file;
                hold->line = ls->line;
                ls->slice.ptr = src->text.ptr;
                ls->slice.len = src->text.len;
                ls->file = src->file; // xxx: could be full and corrected path
                ls->line = 1;
                return lext(ls);
            }
#           ifdef on_lsys
            else on_lsys(path);
#           endif
        }

        else if (!disab && diris("line")) { // xxx: (<backslash><newline>)+ not handled in directive
            skip(strchr(" \t", at()));
            ls->line = 0;
            while (has(1) && isin('0', '9')) ls->line = ls->line*10 + at()-'0';
            skip(strchr(" \t", at()));
            if (has(1) && is('"')) {
                nx();
                accu(ls->file) skip(!is('"'));
                ls->file.len--;
            }
        }

        else if (!disab && diris("define")) {
            skip(strchr(" \t", at()));
            bufsl name;
            accu(name) skip(isid());
            struct _lex_state_macro* it = NULL;
            search_namespace(name, ls->macros) { it = ls->macros.ptr+k; break; }
            if (!it && !(it = dyarr_push(&ls->macros))) exitf("OOM");
            *it = (struct _lex_state_macro){0};
            it->name = name;
            if (is('(')) {
                nx();
                skip(strchr(" \t", at()));
                while (!is(')')) {
                    bufsl* arg = (bufsl*)dyarr_push(&it->params);
                    if (!arg) exitf("OOM");
                    accu(*arg) skip(isid() || is('.'));
                    skip(strchr(" \t", at()));
                    if (is(',')) {
                        nx();
                        skip(strchr(" \t", at()));
                    }
                }
                nx();
            }
            skip(strchr(" \t", at()));
            accu(it->repl) {
                if (!has(1) || is('\n')) break;
                do if (has(1) && is('\\')) nx();
                while (nx(), has(1) && !is('\n'));
            }
        }

        else if (!disab && diris("undef")) {
            skip(strchr(" \t", at()));
            bufsl name;
            accu(name) skip(isid());
            search_namespace(name, ls->macros) {
                dyarr_remove(&ls->macros, k, 1);
                break;
            }
        }

        else if (diris("ifdef") || diris("ifndef")) {
            skip(strchr(" \t", at()));
            bufsl name;
            accu(name) skip(isid());
            char* top = dyarr_push(&ls->ifdef_stack);
            if (!top) exitf("OOM");
            if (!(*top = disab*2)) {
                search_namespace(name, ls->macros) { *top = 0; break; }
                if ('n' == dir.ptr[2]) *top^= 1;
            }
        }

        else if (diris("if") || diris("elif")) {
            skip(strchr(" \t", at()));
            bufsl expr;
            accu(expr) {
                if (!has(1) || is('\n')) break;
                do if (has(1) && is('\\')) nx();
                while (nx(), has(1) && !is('\n'));
            }
            bool e = 'e' == dir.ptr[0];
            char* top = e ? dyarr_top(&ls->ifdef_stack) : dyarr_push(&ls->ifdef_stack);
            if (!top) exitf("OOM");
            *top = (e ? 1 != *top : disab) ? 2 : !lxpr(ls, &expr);
        }

        else if (diris("else")) {
            char* top = dyarr_top(&ls->ifdef_stack);
            if (top) *top^= 1;
        }

        else if (diris("endif")) {
            dyarr_pop(&ls->ifdef_stack);
        }

#       ifdef on_lunr
        else if (!disab) {
            bufsl dirlne;
            accu(dirlne) {
                if (!has(1) || is('\n')) break;
                do if (has(1) && is('\\')) nx();
                while (nx(), has(1) && !is('\n'));
            }
            dirlne.len+= dirlne.ptr-dir.ptr;
            dirlne.ptr = dir.ptr;
            on_lunr(dirlne);
        }
#       endif

#       undef diris
        if (has(1) && !is('\n')) do if (has(1) && is('\\')) nx(); while (nx(), has(1) && !is('\n'));
        return lext(ls);
    } // if is '#'

    if (disab) {
        skip(!is('\n'));
        if (has(1)) return lext(ls);
    }

    // xxx: (<backslash><newline>)+ in a token other than string literal not handled
    bufsl r;
    accu(r) {
        if (!has(1)) {
            if (!ls->include_stack.len) return r;
            struct _lex_state_hold* hold = dyarr_pop(&ls->include_stack);
            ls->slice = hold->slice;
            ls->file = hold->file;
            ls->line = hold->line;
            if (ls->macro_depth) ls->macro_depth--;
            return lext(ls);
        }

        if (isin('0', '9')) {
            char const* dgts = "0123456789";
            if (has(2) && is('0')) switch (nx(), at()) {
            case 'b': nx(); dgts = "01";               break;
            case 'o': nx(); dgts = "01234567";         break;
            case 'x': nx(); dgts = "0123456789abcdef"; break;
            }
            skip(strchr(dgts, at()|32));
        }

        else if (isid()) {
            bufsl name;
            accu(name) skip(isid());
            search_namespace(name, ls->macros) {
                struct _lex_state_macro* macro = ls->macros.ptr+k;
                size_t argc = 0;
                bufsl argv[128];
                bool isva = false;
                if ('(' == dyarr_top(&macro->name)[1]) { // xxx: 1-char read overrun
                    isva = macro->params.len && !memcmp("...", dyarr_top(&macro->params)->name.ptr, 3);
                    do {
                        nx();
                        skip(strchr(" \t\n", at()));
                        if (sizeof argv/sizeof*argv == argc) continue;
                        accu(argv[argc]) {
                            char const* pat = &at();
                            for (unsigned depth = 0; has(1); nx()) {
                                bool c = is(')');
                                if (!depth && (is(',') || c)) break;
                                depth+= is('(')-c;
                            }
                            if (&at() == pat) break;
                        }
                        while (argv[argc].len && strchr(" \t\n\\", argv[argc].ptr[argv[argc].len-1])) argv[argc].len--;
                        argc++;
                    } while (!is(')'));
                    nx();
                    while (argc < macro->params.len) {
                        argv[argc].ptr = argv[argc-1].ptr;
                        argv[argc++].len = 0;
                    }
                }
                struct _lex_state_hold* hold = dyarr_push(&ls->include_stack);
                buf* work = dyarr_push(&ls->workbufs);
                if (work) *work = (buf){0};
                if (macro->repl.len && (!hold || !work || !dyarr_replace(work, 0, 0, &macro->repl))) exitf("OOM");
                k = 0;
                while (k < work->len) {
#                   define nameis(li) (strlen(li) == name.len && !memcmp(li, name.ptr, strlen(li)))
#                   define cin(lo, hi) ((lo) <= c && c <= (hi))
                    char c = work->ptr[k];
                    switch (c) {
                    case '\'':
                    case '"':
                        k++;
                        do if (k < work->len && '\\' == work->ptr[k]) k++;
                        while (++k < work->len && c != work->ptr[k]);
                        k++;
                        break;
                    case '#':
                        if (work->len == ++k) break;
                        // xxx: <numsign> (<backslash><newline>)+ <numsign> not handled
                        if ('#' == work->ptr[k]) {
                            size_t st = k-1, ed = k;
                            while (st && strchr(" \t\n\\", work->ptr[st-1])) st--;
                            while (++ed < work->len && strchr(" \r\n\\", work->ptr[ed]));
                            dyarr_remove(work, k = st, ed-st);
                        } else {
                            size_t st = k-1;
                            while (k < work->len && strchr(" \r\n\\", work->ptr[k])) k++;
                            if (work->len == k) break;
                            bufsl name = {.ptr= work->ptr+k};
                            c = work->ptr[k];
                            while (k < work->len && ('_' == c || cin('A', 'Z') || cin('a', 'z') || cin('0', '9')))
                                c = work->ptr[++k];
                            name.len = work->ptr+k-name.ptr;
                            bufsl repl;
                            bool f = false;
                            if (nameis("__VA_ARGS__") && isva) {
                                repl.ptr = argv[macro->params.len-1].ptr;
                                repl.len = argv[argc-1].ptr - repl.ptr + argv[argc-1].len;
                                f = true;
                            } else search_namespace(name, macro->params) {
                                f = true;
                                repl = argv[k];
                                break;
                            }
                            if (!f) break;
                            size_t ln = (name.ptr+name.len)-(work->ptr+st);
                            work->ptr[st] = work->ptr[st+ln-1] = '"';
                            if (ln < 2) break;
                            if (!dyarr_replace(work, st+1, ln-2, &repl)) exitf("OOM");
                            for (size_t k = 0; k < repl.len; k++) {
                                if ('\\' == work->ptr[st+1+k]) k++;
                                else if ('\n' == work->ptr[st+1+k]) work->ptr[st+1+k] = ' ';
                            }
                        } // '#name'
                        break;
                    default:
                        if ('_' == c || cin('A', 'Z') || cin('a', 'z')) {
                            bufsl name = {.ptr= work->ptr+k++};
                            if (k < work->len) {
                                c = work->ptr[k];
                                while (k < work->len && ('_' == c || cin('A', 'Z') || cin('a', 'z') || cin('0', '9')))
                                    c = work->ptr[++k];
                            }
                            name.len = work->ptr+k-name.ptr;
                            bufsl repl = name;
                            char tmp[32];
                            time_t tt;
                            if (nameis("__VA_ARGS__") && isva) repl.len = argv[argc-1].ptr - (repl.ptr = argv[macro->params.len-1].ptr) + argv[argc-1].len;
                            else if (nameis("__FILE__")) repl = ls->file;
                            else if (nameis("__LINE__")) repl.len = snprintf((void*)(repl.ptr = tmp), sizeof tmp, "%zu", ls->line);
                            else if (nameis("__DATE__")) repl.len = strftime((void*)(repl.ptr = tmp), sizeof tmp, "\"%b %e %Y\"", localtime((time(&tt), &tt)));
                            else if (nameis("__TIME__")) repl.len = strftime((void*)(repl.ptr = tmp), sizeof tmp, "\"%T\"", localtime((time(&tt), &tt)));
                            else search_namespace(name, macro->params) { repl = argv[k]; break; }
                            if (name.ptr != repl.ptr && !dyarr_replace(work, name.ptr-work->ptr, name.len, &repl)) exitf("OOM");
                            k = k-name.len+repl.len;
                        } else k++;
                    }
#                   undef cin
#                   undef nameis
                } // for k in work
                hold->slice = ls->slice;
                hold->file = ls->file;
                hold->line = ls->line;
                ls->slice.ptr = work->ptr;
                ls->slice.len = work->len;
                ls->macro_depth++;
                return lext(ls);
            } // found name in macros
        }

        else if (is('\'') || is('"')) { // xxx: will not handle <backslash><backslash><newline> correctly
            char c = at();
            if (nx(), has(1)) {
                do if (has(1) && is('\\')) nx();
                while (nx(), has(1) && !is(c) && !is('\n'));
                if (has(1)) nx();
            }
        }

        else switch (has(2) ? at() : 0) {
        case '-':
            if ('>' == (&at())[1]) nx();
            else
            // fall through
        case '+': case '&': case '|': case '<': case '>':
            if (at() == (&at())[1]) {
                nx();
                if (has(2) && ('<' == at() || '>' == at()) && '=' == (&at())[1]) nx();
            } else
            // fall through
        case '*': case '/': case '%': case '^': case '=': case '!':
            if ('=' == (&at())[1]) nx();
            // fall through
        default: nx();
        }
    } // accu r

    return r;

#   undef accu
#   undef skip
#   undef isid
#   undef isin
#   undef is
#   undef has
#   undef at
#   undef nx
}
