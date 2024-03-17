#include "common.h"
#include "lexer.h"

bufsl passthrough(bufsl it) {
    printf("\x1b[36m/* %.*s */\x1b[m", (unsigned)it.len, it.ptr);
    return it;
}
#define lext(_ls) passthrough(lext(ls))

/// http://slebok.github.io/zoo/c/c99/iso-9899-1999/extracted/
/// https://en.cppreference.com/w/c/language/declarations

#define kws(a,b,c,...) ((a&31)<<10 | (b&31)<<5 | (c&31))
#define kw(s) kws(s[0],s[1],s[2],)
#define iskwx(tok, ...) dyarr_cmp(&((bufsl){.ptr= (char[]){__VA_ARGS__}, .len= sizeof((char[]){__VA_ARGS__})}), &tok)
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
            TAG_NOTAG= 0,
            TAG_STRUCT= kws('s','t','r','u','c','t'),
            TAG_UNION= kws('u','n','i','o','n'),
            TAG_ENUM= kws('e','n','u','m'),
        } kind;

        // if struct or union: fields, which are type+name (no decl spec)
        // if enum: the list of enumerators, which are name+(optional)value

        bufsl name;
    } type;

    // (optional)value which can be bit more that an expression for function declaration

    bufsl name;
} declaration;

bufsl _parse_params(lex_state ref ls) {
    // note: "register" can be used, params may not have a name, a ',' is used
    //       to separate decls (see also in _parse_declarators)
    bufsl tok = lext(ls);
    if (dyarr_cmp(&((bufsl){.ptr= "void", .len= 4}), &tok)) tok = lext(ls);
    return tok;
}

bufsl _parse_expr(lex_state ref ls) {
    (void)lext(ls);
    return lext(ls);
}

bufsl _parse_body(lex_state ref ls) {
    bufsl tok;
    unsigned depth = 0;
    while ((tok = lext(ls)).len) {
        bool c = '}' == *tok.ptr;
        if (!tok.len || (!depth && c)) break;
        depth+= ('{' == *tok.ptr)-c;
    }
    return tok;
}

void _decl_put_field(void ref struct_or_union, declaration cref field_decl) {
    struct decl_type* it = struct_or_union;
    (void)it;
    (void)field_decl;
}

bufsl _parse_declarators(lex_state ref ls, void ref usr, void on(void ref, declaration cref), bufsl const tok1, bufsl const tok2, declaration cref base) {
    if (!tok1.len) return (bufsl){0};
#   define is1(w) (tok.len && w == *tok.ptr)
    bufsl tok;
    declaration decl = *base;

    switch (*tok1.ptr) {
    case '(':
        tok = lext(ls);
        if (is1(')')) decl.name = tok2; // eg `int (a);`
        else tok = _parse_declarators(ls, usr, on, tok2, tok, base);
        tok = lext(ls);
        break;

    case '*':
        tok = tok2;
        while (3 < tok.len) {
            unsigned askw = kw(tok.ptr);
            switch (askw) {
            case kws('c','o','n','s','t'): case kws('r','e','s','t','r','i','c','t'): case kws('v','o','l','a','t','i','l','e'):
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
        tok = _parse_declarators(ls, usr, on, tok, lext(ls), &decl);
        break;

    default:
        // TODO: in function paramter declarations, can omit name (and must not
        // continue on ',' but rather return)
        // note: I could still re-use this code, by carefully setting `base`
        //       and by making omitting the name working here, but function
        //       parameters can be declared with "register" storage class which
        //       is not technically part of the declarator (this very function)
        //if (isid()) { ... }
        //if (iskw(,, "register")
        decl.name = tok1;
        tok = tok2;
        if (is1(':')) tok = _parse_expr(ls);
    }

    if (is1('(')) {
        tok = _parse_params(ls);
        tok = lext(ls);
    }

    else while (is1('[')) {
        tok = lext(ls);
        if (3 < tok.len) {
            unsigned askw = kw(tok.ptr);
            if (iskw(tok, askw, 's','t','a','t','i','c')) {
                // TODO: use askw
                tok = lext(ls);
            }
            while (3 < tok.len && (
                        iskw(tok, askw, 'c','o','n','s','t') ||
                        iskw(tok, askw, 'r','e','s','t','r','i','c','t') ||
                        iskw(tok, askw, 'v','o','l','a','t','i','l','e')
                        )) {
                // TODO: add askw
                tok = lext(ls);
                askw = kw(tok.ptr);
            }
        }
        tok = _parse_expr(ls);
        tok = lext(ls);
    }

    if (!tok.len) return (bufsl){0};

    switch (*tok.ptr) {
    case '{':
        tok = _parse_body(ls);
        on(usr, &decl);
        // fall through

    case ')': case ';':
        return tok;

    case '=':
        tok = _parse_expr(ls);
        if (is1(';')) return tok;
        // fall through

    case ',':
        on(usr, &decl);
        tok = lext(ls);
        return _parse_declarators(ls, usr, on, tok, lext(ls), base);
    }

    return tok;
#   undef is1
}

bufsl parse(lex_state ref ls, void ref usr, void on(void ref, declaration cref)) {
    // <specs-and-quals>+ <declarator>,+ ;
    // <specs-and-quals> ::= <words>
    // <declarator> ::
    //      = <ident>
    //      | ( <declarator> )
    //      | * <quals>? <declarator>
    //      | <no-ptr> [ static? <quals>? <expr> ]
    //      | <no-ptr> [ <quals>? * ] (* will for now just fold the 2 into just <expr> *)
    //      | <no-ptr> ( <params> )
    declaration base = {0};

    bufsl tok;
    unsigned askw;
#   define is(wo) (!dyarr_cmp((&(bufsl){.ptr= wo, .len= strlen(wo)}), &tok))
#   define is1(w) (tok.len && w == *tok.ptr)
#   define isid() (tok.len && ('_' == *tok.ptr || ('A' <= *tok.ptr && *tok.ptr <= 'Z') || ('a' <= *tok.ptr && *tok.ptr <= 'z') || ('0' <= *tok.ptr && *tok.ptr <= '9')))
#   define case_iskw(...) if (0) case kws(__VA_ARGS__): if (!iskwx(tok, __VA_ARGS__)) goto notkw;

    while ((tok = lext(ls)).len && !is1('}')) switch (askw = tok.len <3 ? 0 : kw(tok.ptr)) {
    case 0: goto notkw;

    case_iskw('t','y','p','e','d','e','f') case_iskw('e','x','t','e','r','n') case_iskw('s','t','a','t','i','c') case_iskw('a','u','t','o') case_iskw('r','e','g','i','s','t','e','r')
        base.spec = askw;
        break;

    case_iskw('c','o','n','s','t') case_iskw('r','e','s','t','r','i','c','t') case_iskw('v','o','l','a','t','i','l','e') case_iskw('i','n','l','i','n','e')
    case_iskw('s','i','g','n','e','d') case_iskw('u','n','s','i','g','n','e','d') case_iskw('s','h','o','r','t') case_iskw('l','o','n','g')
    case_iskw('c','o','m','p','l','e','x') case_iskw('i','m','a','g','i','n','a','r','y')
        for (unsigned k = 0; k < countof(base.type.quals); k++) if (QUAL_END == base.type.quals[k]) {
            base.type.quals[k] = askw;
            break;
        }
        break;

    case_iskw('s','t','r','u','c','t') case_iskw('u','n','i','o','n') case_iskw('e','n','u','m')
        base.type.kind = askw;
        tok = lext(ls);
        if (isid()) {
            base.type.name = tok;
            tok = lext(ls);
        }
        if (is1('{')) {
            if (kws('u','n','i','o','n') == askw) {
                tok = lext(ls);
                do {
                    //name = tok;
                    tok = lext(ls);
                    if (is1('=')) tok = _parse_expr(ls);
                    if (is1(',')) tok = lext(ls);
                } while (isid());
            } else parse(ls, &base.type, _decl_put_field);
        }
        break;

    default:
    notkw:
        ;
        bufsl tok1 = tok, tok2 = lext(ls);
        if (tok2.len && isid()) switch (*tok2.ptr) {
        case '(': // eg `int (a);`
            if (!base.type.name.len) {
                base.type.name = tok1;
                tok1 = tok2;
                tok2 = lext(ls);
            } // fall through
        case ',': case ';': case '[':
            break;
        default:
            base.type.name = tok1;
            continue;
        }

        tok = _parse_declarators(ls, usr, on, tok1, tok2, &base);
        if (!is1(';')) return tok;
        base = (declaration){0};
    } // while-switch tok

#   undef case_iskw
#   undef isid
#   undef is1
#   undef is
    return tok;
}

#undef kw

lex_state ls;

void cleanup(void) {
    ldel(&ls);
}

void show_decl(void ref _, declaration cref decl) {
    (void)_;
#   define unkw(k) ((char[4]){((k)>>10&5)|96, ((k)>>5&5)|96, ((k)&5)|96})
    if (decl->spec) printf("%s ", unkw(decl->spec));
    for (unsigned k = 0; decl->type.quals[k]; k++)
        printf("%s ", unkw(decl->type.quals[k]));
    if (decl->type.kind) printf("%s ", unkw(decl->type.kind));
    printf("%.*s %.*s;\n", (unsigned)decl->type.name.len, decl->type.name.ptr, (unsigned)decl->name.len, decl->name.ptr);
#   undef unkw
}

int main(int argc, char** argv) {
    atexit(cleanup);
    char const* prog = (argc--, *argv++);
    if (!argc || !strcmp("-h", *argv) || !strcmp("--help", *argv)) exitf("Usage: %s <entry.h> [-D...,-I...]", prog);
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

    parse(&ls, NULL, show_decl);

    return EXIT_SUCCESS;
}
