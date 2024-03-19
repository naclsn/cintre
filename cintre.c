#include "common.h"
#include "lexer.h"

bufsl passthrough(bufsl it) {
    fprintf(stderr, "\x1b[36m%.*s \x1b[m", (unsigned)it.len, it.ptr);
    return it;
}
#define lext(_ls) passthrough(lext(_ls))
#define lextbang(_ls) (fprintf(stderr, "\x1b[31m!\x1b[m"), lext(_ls))

/// http://slebok.github.io/zoo/c/c99/iso-9899-1999/extracted/
/// https://en.cppreference.com/w/c/language/declarations

#define kws(a,b,c,...) ((a&31)<<10 | (b&31)<<5 | (c&31))
#define kw(s) kws(s[0],s[1],s[2],)
#define iskwx(tok, ...) !dyarr_cmp(&((bufsl){.ptr= (char[]){__VA_ARGS__}, .len= sizeof((char[]){__VA_ARGS__})}), &tok)
#define iskw(tok, askw, ...) (kws(__VA_ARGS__) == askw && iskwx(tok, __VA_ARGS__))

typedef struct declaration {
    enum decl_spec {
        SPEC_NONE= 0,
        SPEC_TYPEDEF= kws('t','y','p','e','d','e','f'),
        SPEC_EXTERN= kws('e','x','t','e','r','n'),
        SPEC_STATIC= kws('s','t','a','t','i','c'),
        SPEC_AUTO= kws('a','u','t','o'),
        SPEC_REGISTER= kws('r','e','g','i','s','t','e','r'),
    } spec;

    struct decl_type {
        enum type_qual {
            QUAL_END= 0,
            QUAL_CONST= kws('c','o','n','s','t'),
            QUAL_RESTRICT= kws('r','e','s','t','r','i','c','t'),
            QUAL_VOLATILE= kws('v','o','l','a','t','i','l','e'),
            QUAL_INLINE= kws('i','n','l','i','n','e'),
            QUAL_SIGNED= kws('s','i','g','n','e','d'),
            QUAL_UNSIGNED= kws('u','n','s','i','g','n','e','d'),
            QUAL_SHORT= kws('s','h','o','r','t'),
            QUAL_LONG= kws('l','o','n','g'),
            QUAL_COMPLEX= kws('c','o','m','p','l','e','x'),
            QUAL_IMAGINARY= kws('i','m','a','g','i','n','a','r','y'),
        } quals[8];

        enum type_kind {
            KIND_NOTAG= 0,
            KIND_STRUCT= kws('s','t','r','u','c','t'),
            KIND_UNION= kws('u','n','i','o','n'),
            KIND_ENUM= kws('e','n','u','m'),
            KIND_PTR= '*',
            KIND_FUN= ('('&31)<<5 | (')'&31),
            KIND_ARR= ('['&31)<<5 | (']'&31),
        } kind;

        union type_info {
            // if struct or union: fields, which are type+name (no decl spec)
            // if enum: the list of enumerators, which are name+(optional)value
            struct decl_type const* ptr;
            struct decl_type_fun {
                struct decl_type const* ret;
                size_t count;
                struct decl_type_param {
                    struct declaration const* decl;
                    struct decl_type_param* next;
                }* first;
            } fun;
            struct {
                struct decl_type const* item;
            } arr;
        } info;

        bufsl name;
    } type;

    bufsl name;
} declaration;

bufsl parse_declaration(lex_state ref ls,
        void ref usr, void on(void ref, declaration cref, bufsl ref tok),
        bufsl tok,
        declaration ref base);

bufsl _parse_declarator(lex_state ref ls,
        void ref usr, void on(void ref, declaration cref, bufsl ref tok),
        bufsl const tok1, bufsl const tok2,
        declaration cref base);

void print_declaration(FILE ref strm, declaration cref decl);

void print_decl_type(FILE ref strm, struct decl_type cref type) {
#   define unkw(k) ((char[4]){((k)>>10&31)|96, ((k)>>5&31)|96, ((k)&31)|96})
    switch (type->kind) {
    case KIND_NOTAG:
        break;

    case KIND_STRUCT:
    case KIND_UNION:
    case KIND_ENUM:
        fprintf(strm, "\x1b[33m%s\x1b[m ", unkw(type->kind));
        break;

    case KIND_PTR:
        fprintf(strm, "("); // yyy ofc, this wrong
        print_decl_type(strm, type->info.ptr);
        fprintf(strm, ")* ");
        break;

    case KIND_FUN:
        print_decl_type(strm, type->info.fun.ret);
        fprintf(strm, "!!!(");
        if (!type->info.fun.count) fprintf(strm, "void");
        for (struct decl_type_param* curr = type->info.fun.first; curr; curr = curr->next) {
            print_declaration(strm, curr->decl);
            if (curr->next) fprintf(strm, ", ");
        }
        fprintf(strm, ") ");
        break;

    case KIND_ARR:
        print_decl_type(strm, type->info.arr.item);
        fprintf(strm, "!!![] "); // TODO
        break;
    }

    for (unsigned k = 0; type->quals[k]; k++) fprintf(strm, "\x1b[34m%s\x1b[m ", unkw(type->quals[k]));

    if (type->name.len) fprintf(strm, "\x1b[33m%.*s\x1b[m ", (unsigned)type->name.len, type->name.ptr);
    //else fprintf(strm, "\x1b[33mint\x1b[m ");
}

void print_declaration(FILE ref strm, declaration cref decl) {
    if (decl->spec) fprintf(strm, "\x1b[32m%s\x1b[m ", unkw(decl->spec));
    print_decl_type(strm, &decl->type);
    fprintf(strm, "%.*s", (unsigned)decl->name.len, decl->name.ptr);
#   undef unkw
}

bufsl parse_expression(lex_state ref ls, bufsl const tok) {
    // TODO
    return lext(ls);
}

struct _parse_put_param_capt {
    lex_state* ls;
    declaration* func;
};

void _decl_put_param(struct _parse_put_param_capt ref capt, declaration cref decl, bufsl ref tok) {
    // FIXME: something breaks the 'float f', next to last, 3rd param (or is it just in printing it?)

    struct decl_type_fun* fun = &capt->func->type.info.fun;
    struct decl_type_param node = {.decl= decl};
    fun->count++;
    if (!fun->first) fun->first = &node;
    else for (struct decl_type_param* curr = fun->first ;3; curr = curr->next) if (!curr->next) {
        curr->next = &node;
        break;
    }

    if (tok->len && ',' == *tok->ptr)
        *tok = parse_declaration(capt->ls,
                capt, (void(*)())_decl_put_param,
                lext(capt->ls),
                &(declaration){0});
}

struct _parse_par_decl_capt {
    lex_state* ls;
    void* usr;
    void (*on)(void ref, declaration cref, bufsl ref);
    declaration const* outer;
};

void _parse_par_decl_unwind(struct _parse_par_decl_capt ref capt, declaration cref decl, bufsl ref tok) {
    // TODO: apply the transformation from `yyy->outer->type` to `decl` (or rather the `local` copy)

    declaration local = *decl;

    if (KIND_PTR == capt->outer->type.kind) {
        local.type = (struct decl_type){
            .quals= {
                [0]= capt->outer->type.quals[0],
                [1]= capt->outer->type.quals[1],
                [2]= capt->outer->type.quals[2],
                [3]= capt->outer->type.quals[3],
                [4]= capt->outer->type.quals[4],
                [5]= capt->outer->type.quals[5],
                [6]= capt->outer->type.quals[6],
                [7]= capt->outer->type.quals[7],
            },
            .kind= KIND_PTR,
            .info.ptr= &decl->type,
        };
    }

    // also somewhere in there
    local.name = capt->outer->name;

    capt->on(capt->usr, &local, tok);
}

void _parse_par_decl(struct _parse_par_decl_capt ref capt, declaration cref inner, bufsl ref tok) {
    *tok = lext(capt->ls);
    if (tok->len) {
        declaration const* outer = capt->outer;
        // reuses the same field, but in `_parse_par_decl_unwind` it will then
        // carry `inner`, ie the transformation to unwind
        capt->outer = inner;
        *tok = _parse_declarator(capt->ls,
                capt, (void(*)())_parse_par_decl_unwind,
                // fake token to get past the first switch
                (bufsl){.ptr= "@", .len= 1}, *tok,
                outer);
    }
    return;
}

bufsl _parse_declarator(lex_state ref ls, void ref usr, void on(void ref, declaration cref, bufsl ref tok), bufsl const tok1, bufsl const tok2, declaration cref base) {
    if (!tok1.len) return tok1;

    bufsl tok;
#   define is1(w) (tok.len && w == *tok.ptr)

    declaration decl = *base; // YYY: see if usl

    switch (*tok1.ptr) {
    case '(':
        tok = _parse_declarator(ls,
                &(struct _parse_par_decl_capt){.ls= ls, .usr= usr, .on= on, .outer= &decl},
                (void(*)())_parse_par_decl,
                tok2, lext(ls),
                &(declaration){.type= decl.type});
        return tok;

    case '*':
        tok = tok2;
        struct decl_type const hold = decl.type;
        decl.type = (struct decl_type){.kind= KIND_PTR, .info.ptr= &hold};
        while (3 < tok.len) {
            unsigned askw = kw(tok.ptr);
            switch (askw) {
            case QUAL_CONST: case QUAL_RESTRICT: case QUAL_VOLATILE:
                if (!iskwx(tok, 'c','o','n','s','t') && !iskwx(tok, 'r','e','s','t','r','i','c','t') && !iskwx(tok, 'v','o','l','a','t','i','l','e'))
                    break;
                for (unsigned k = 0; k < countof(decl.type.quals); k++) if (QUAL_END == decl.type.quals[k]) {
                    decl.type.quals[k] = askw;
                    break;
                }
                tok = lext(ls);
                continue;
            }
            break;
        }
        return _parse_declarator(ls, usr, on, tok, lext(ls), &decl);

    case '@': // YYY: see _parse_par_decl, will be worked out when making name optional (for function params)
        tok = tok2;
        break;

    default:
        // TODO: in function paramter declarations, can omit name (and must not
        //       continue on ',' but rather return)
        //if (isid()) { ... }
        //if (iskw(,, "register")
        decl.name = tok1;
        tok = tok2;
        if (is1(':')) tok = parse_expression(ls, lext(ls));
    } // switch with (<decl>), *<decl>

    if (is1('(')) {
        tok = lext(ls);
        struct decl_type const hold = decl.type;
        decl.type = (struct decl_type){.kind= KIND_FUN, .info.fun.ret= &hold};
        if (tok.len) {
            if (is1(')')) decl.type.info.fun.count = -1;
            else if (4 == tok.len && !memcmp("void", tok.ptr, 4)) lext(ls);
            else parse_declaration(ls, &(struct _parse_put_param_capt){.ls= ls, .func= &decl}, (void(*)())_decl_put_param, tok, &(declaration){0});
            tok = lext(ls);
        }
    } // if <decl>(<params>)

    else while (is1('[')) {
        tok = lext(ls);
        struct decl_type const hold = decl.type;
        decl.type = (struct decl_type){.kind= KIND_ARR, .info.arr.item= &hold};
        if (3 < tok.len) {
            unsigned askw = kw(tok.ptr);
            if (iskw(tok, askw, 's','t','a','t','i','c')) {
                // TODO: use askw
                tok = lext(ls);
            }
            while (3 < tok.len && (
                        iskw(tok, askw, 'c','o','n','s','t') ||
                        iskw(tok, askw, 'r','e','s','t','r','i','c','t') ||
                        iskw(tok, askw, 'v','o','l','a','t','i','l','e') )) {
                // TODO: add askw
                tok = lext(ls);
                askw = kw(tok.ptr);
            }
        }
        // TODO: add to type
        if (tok.len) switch (*tok.ptr) {
        case '*':
            lext(ls);
            // fall through
        case ']':
            tok = lext(ls);
            break;
        default:
            parse_expression(ls, tok);
            tok = lext(ls);
        }
    } // while <decl>[<arrinfo>]

    if (on) on(usr, &decl, &tok);

#   undef is1
    return tok;
} // _parse_declarator

bufsl parse_declaration(lex_state ref ls, void ref usr, void on(void ref, declaration cref, bufsl ref tok), bufsl tok, declaration ref base) {
#   define is(wo) (!dyarr_cmp((&(bufsl){.ptr= wo, .len= strlen(wo)}), &tok))
#   define is1(w) (tok.len && w == *tok.ptr)
#   define isid() (tok.len && ('_' == *tok.ptr || ('A' <= *tok.ptr && *tok.ptr <= 'Z') || ('a' <= *tok.ptr && *tok.ptr <= 'z') || ('0' <= *tok.ptr && *tok.ptr <= '9')))
#   define case_iskw(...) if (0) case kws(__VA_ARGS__): if (!iskwx(tok, __VA_ARGS__)) goto notkw;

    for (unsigned askw; tok.len; tok = lext(ls)) redo: switch (askw = tok.len <3 ? 0 : kw(tok.ptr)) {
    case 0: goto notkw;

    case_iskw('t','y','p','e','d','e','f') case_iskw('e','x','t','e','r','n') case_iskw('s','t','a','t','i','c') case_iskw('a','u','t','o') case_iskw('r','e','g','i','s','t','e','r')
        base->spec = askw;
        break;

    case_iskw('s','i','g','n','e','d') case_iskw('u','n','s','i','g','n','e','d') case_iskw('s','h','o','r','t') case_iskw('l','o','n','g')
        if (!base->type.name.len) base->type.name = (bufsl){.ptr= "int", .len= 3};
    case_iskw('c','o','n','s','t') case_iskw('r','e','s','t','r','i','c','t') case_iskw('v','o','l','a','t','i','l','e') case_iskw('i','n','l','i','n','e')
    case_iskw('c','o','m','p','l','e','x') case_iskw('i','m','a','g','i','n','a','r','y')
        for (unsigned k = 0; k < countof(base->type.quals); k++) if (QUAL_END == base->type.quals[k]) {
            base->type.quals[k] = askw;
            break;
        }
        break;

    case_iskw('s','t','r','u','c','t') case_iskw('u','n','i','o','n') case_iskw('e','n','u','m')
        base->type.kind = askw;
        tok = lext(ls);
        if (isid()) {
            base->type.name = tok;
            tok = lext(ls);
        }
        if (is1('{')) {
            // TODO: use
            if (KIND_ENUM == askw) {
                tok = lext(ls);
                do {
                    bufsl name = tok;
                    tok = lext(ls);
                    if (is1('=')) tok = parse_expression(ls, lext(ls));
                    if (is1(',')) tok = lext(ls);
                } while (isid());
            } else exitf("TODO: parse(ls, &base->type, _decl_put_field);");
        } else goto redo;
        break;

    default:
    notkw:
        ;
        bufsl tok1 = tok, tok2 = lext(ls);
        if (tok2.len && isid()) switch (*tok2.ptr) {
        case '(': // eg `int (a);`
            if (!base->type.name.len) {
                base->type.name = tok1;
                tok1 = tok2;
                tok2 = lext(ls);
            } // fall through
        case ',': case ';': case ')': case '[':
            break;
        default:
            base->type.name = tok1;
            tok = tok2;
            goto redo;
        }

        tok = _parse_declarator(ls, usr, on, tok1, tok2, base);
        return tok;
    } // while-switch tok

#   undef case_iskw
#   undef isid
#   undef is1
#   undef is
    return tok;
} // parse_declaration

#undef iskw
#undef iskwx
#undef kw
#undef kws

lex_state ls;

void cleanup(void) {
    ldel(&ls);
}

void show(void ref _, declaration cref decl, bufsl ref tok) {
    (void)_;
    print_declaration(stdout, decl);
    printf("; // %.*s\n", (unsigned)tok->len, tok->ptr);
}

int main(int argc, char** argv) {
    atexit(cleanup);
    char const* prog = (argc--, *argv++);
    if (!argc || !strcmp("-h", *argv) || !strcmp("--help", *argv)) exitf("Usage: %s <entry-file> [-D...,-I...]", prog);
    char const* file = (argc--, *argv++);

    linc(&ls, "./");

    while (argc) {
        char const* arg = (argc--, *argv++);

        if (!memcmp("-D", arg, 2)) {
            char* val = strchr(arg, '=');
            if (val) *(val++) = '\0';
            else val = "1";
            ldef(&ls, arg+2, val);
        }

        else if (!memcmp("-I", arg, 2)) {
            linc(&ls, arg+2);
        }

        else notif("unused or unimplemented argument: %s", arg);
    } // while args

    lini(&ls, file);

    bufsl tok = lext(&ls);
    declaration base = {0};
    while (tok.len) if ((tok = parse_declaration(&ls, NULL, show, tok, &base)).len)
        switch (*tok.ptr) {
        case '{':
            for (unsigned depth = 0; (tok = lext(&ls)).len; ) {
                bool c = '}' == *tok.ptr;
                if (!tok.len || (!depth && c)) break;
                depth+= ('{' == *tok.ptr)-c;
            }
            tok = lext(&ls);
            base = (declaration){0}; // reset
            continue;

        case '=':
            tok = parse_expression(&ls, lextbang(&ls));
            if (tok.len && ';' == *tok.ptr)

        case ';':
                base = (declaration){0}; // reset
            // fall through

        case ',':
            tok = lext(&ls);
            continue;

        default:
            exitf("other: %.*s", (unsigned)tok.len, tok.ptr);
        }

    return EXIT_SUCCESS;
} // main
