/// C parser of sort
    // TODO: if (!tok.len) return tok;

#ifndef CINTRE_PARSER_H
#define CINTRE_PARSER_H

#include "common.h"
#include "lexer.h"

// struct declaration {{{
typedef struct declaration {
#define kws(a,b,c,...) ((a&31)<<10 | (b&31)<<5 | (c&31))

    enum decl_spec {
        SPEC_NONE= 0,
        SPEC_TYPEDEF= kws('t','y','p','e','d','e','f'),
        SPEC_EXTERN= kws('e','x','t','e','r','n'),
        SPEC_STATIC= kws('s','t','a','t','i','c'),
        SPEC_AUTO= kws('a','u','t','o'),
        SPEC_REGISTER= kws('r','e','g','i','s','t','e','r'),
    } spec;

    struct decl_type {
        // XXX: TODO: rename the type_.. to decl_type_..
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
            struct decl_type const* ptr;
            struct decl_type_comp {
                size_t count; // -1 if no body
                struct decl_type_field {
                    struct declaration const* decl;
                    struct decl_type_field* next;
                }* first;
            } comp; // struct or union
            struct decl_type_enu {
                size_t count;
                struct decl_type_enumer {
                    bufsl const name;
                    struct expression* expr;
                    struct decl_type_enumer* next;
                }* first;
            } enu; // enum
            struct decl_type_fun {
                struct decl_type const* ret;
                size_t count; // -1 when (), 0 when (void), n otherwise
                struct decl_type_param {
                    struct declaration const* decl;
                    struct decl_type_param* next;
                }* first;
            } fun;
            struct {
                struct decl_type const* item;
                struct expression* count; // NULL when [*] or [], n otherwise
                bool statik;
                enum type_qual quals[3];
            } arr;
        } info;

        bufsl name;
    } type;

    bufsl name;
} declaration;
// }}}

typedef struct parse_decl_state {
    lex_state* ls;
    void* usr;
    void (*on)(void ref usr, declaration cref decl, bufsl ref tok);
    bufsl tok;
    declaration base;
} parse_decl_state;

bufsl parse_declaration(parse_decl_state ref ps, bufsl tok);

// struct expression {{{
typedef struct expression {
    enum expr_kind {
        ATOM,

        BINOP_SUBSCR, BINOP_CALL,

        BINOP_TERNCOND,
        BINOP_TERNBRANCH,
        BINOP_COMMA,

        BINOP_ASGN,
        BINOP_ASGN_BOR, BINOP_ASGN_BXOR, BINOP_ASGN_BAND,
        BINOP_ASGN_BSHL, BINOP_ASGN_BSHR,
        BINOP_ASGN_SUB, BINOP_ASGN_ADD,
        BINOP_ASGN_REM, BINOP_ASGN_DIV, BINOP_ASGN_MUL,

        BINOP_LOR, BINOP_LAND,
        BINOP_BOR, BINOP_BXOR, BINOP_BAND,
        BINOP_EQ, BINOP_NE,
        BINOP_LT, BINOP_GT, BINOP_LE, BINOP_GE,
        BINOP_BSHL, BINOP_BSHR,
        BINOP_SUB, BINOP_ADD,
        BINOP_REM, BINOP_DIV, BINOP_MUL,

        //UNOP_SIZEOF, UNOP_ALIGNOF, //BINOP_OFFSETOF,
        UNOP_ADDR, UNOP_DEREF,
        //UNOP_CAST,
        UNOP_BNOT, UNOP_LNOT,
        UNOP_MINUS, UNOP_PLUS,
        UNOP_PRE_DEC, UNOP_PRE_INC,

        //UNOP_COMPLIT,
        UNOP_PMEMBER, UNOP_MEMBER,
        UNOP_POST_DEC, UNOP_POST_INC,
    } kind;

    union expr_info {
        bufsl atom;
        struct { struct expression* opr; } unary;
        struct { struct expression* lhs, * rhs; } binary;
        struct { struct expression* base, * args; } call;
        struct { struct expression* base, * off; } subscr;
        struct { struct expression* base; bufsl* name; } member;
    } info;

    // reserved for user
    void* usr;
} expression;
// }}}

typedef struct parse_expr_state {
    lex_state* ls;
    void* usr;
    void (*on)(void ref usr, expression ref expr, bufsl ref tok);
    bufsl tok;
} parse_expr_state;

bufsl parse_expression(parse_expr_state ref ps, bufsl const tok);

// ---

// parse declaration {{{
struct _parse_decl_capture;
typedef void _parse_decl_closure_t(parse_decl_state ref ps, struct _parse_decl_capture ref capt, declaration ref decl);
struct _parse_decl_capture {
    declaration* hold;
    struct _parse_decl_capture ref next;
    _parse_decl_closure_t ref then;
};
_parse_decl_closure_t _parse_decl_ator, _parse_decl_close, _parse_decl_post, _parse_decl_params, _parse_decl_enumer, _parse_decl_fields, _parse_decl_spec;

#define kw(s) kws(s[0],s[1],s[2],)
#define iskwx(tok, ...) !dyarr_cmp(&((bufsl){.ptr= (char[]){__VA_ARGS__}, .len= sizeof((char[]){__VA_ARGS__})}), &tok)
#define iskw(tok, askw, ...) (kws(__VA_ARGS__) == askw && iskwx(tok, __VA_ARGS__))

/// pre-ish declarator part with '('<decl>')' | '*'<decl>
void _parse_decl_ator(parse_decl_state ref ps, struct _parse_decl_capture ref capt, declaration ref decl) {
    switch (*ps->tok.ptr) {
    case '(':
        ps->tok = lext(ps->ls);
        declaration before = *decl;
        _parse_decl_ator(ps, &(struct _parse_decl_capture){
                .next= &(struct _parse_decl_capture){
                    .hold= &before,
                    .next= capt->next,
                    .then= capt->then,
                },
                .then= _parse_decl_close,
            }, decl);
        return;

    case '*':
        ps->tok = lext(ps->ls);
        declaration ptr = {
            .spec= decl->spec,
            .type= {.kind= KIND_PTR, .info.ptr= &decl->type},
            .name= decl->name,
        };
        for (unsigned askw; (3 < ps->tok.len && (askw = kw(ps->tok.ptr),
                    iskw(ps->tok, askw, 'c','o','n','s','t') ||
                    iskw(ps->tok, askw, 'r','e','s','t','r','i','c','t') ||
                    iskw(ps->tok, askw, 'v','o','l','a','t','i','l','e') ));
                ps->tok = lext(ps->ls)) {
            for (unsigned k = 0; k < countof(ptr.type.quals); k++) if (QUAL_END == ptr.type.quals[k]) {
                ptr.type.quals[k] = askw;
                break;
            }
        }
        _parse_decl_ator(ps, capt, &ptr);
        return;

    case '=': case ',': case ';': case ')':
        capt->then(ps, capt->next, decl);
        return;
    }

    decl->name = ps->tok;
    ps->tok = lext(ps->ls);
    _parse_decl_post(ps, capt, decl);
}

/// skip closing parenthesis and parse post
void _parse_decl_close(parse_decl_state ref ps, struct _parse_decl_capture ref capt, declaration ref decl) {
    //_expect(ps->tok, ")");
    ps->tok = lext(ps->ls);

    // TODO:
    if (ps->tok.len) {
        declaration ref before = capt->hold;

        void print_decl(declaration cref decl, unsigned const depth);
        printf("!! before is "); print_decl(before, 0); printf("\n");
        printf("!!   decl is "); print_decl(  decl, 0); printf("\n");

        if ('(' == *ps->tok.ptr) {
            // `.. (*a)(..)`
        }
        else if ('[' == *ps->tok.ptr) {
            // `.. (*a)[..]`
        }

        // setup the call to _post in a way that it fills in the inner to the ptr
    }

    _parse_decl_post(ps, capt, decl);
}

/// postfix declarator notations
void _parse_decl_post(parse_decl_state ref ps, struct _parse_decl_capture ref capt, declaration ref decl) {
    // '(' <params> ')'
    // '[' [static][const][idk] <expr>|*|<nothing> ']'

    switch (*ps->tok.ptr) {
    case '(':
        ps->tok = lext(ps->ls);
        declaration fun = {
            .spec= decl->spec,
            .type= {.kind= KIND_FUN, .info.fun.ret= &decl->type},
            .name= decl->name,
        };
        _parse_decl_params(ps, &(struct _parse_decl_capture){
                .hold= &fun,
                .next= capt->next,
                .then= capt->then,
            }, NULL);
        return;

    case '[':
        ps->tok = lext(ps->ls);
        declaration arr = {
            .spec= decl->spec,
            .type= {.kind= KIND_ARR, .info.arr.item= &decl->type},
            .name= decl->name,
        };
        if (3 < ps->tok.len && iskwx(ps->tok, 's','t','a','t','i','c')) {
            arr.type.info.arr.statik = true;
            ps->tok = lext(ps->ls);
        }
        for (unsigned askw; (3 < ps->tok.len && (askw = kw(ps->tok.ptr),
                    iskw(ps->tok, askw, 'c','o','n','s','t') ||
                    iskw(ps->tok, askw, 'r','e','s','t','r','i','c','t') ||
                    iskw(ps->tok, askw, 'v','o','l','a','t','i','l','e') ));
                ps->tok = lext(ps->ls)) {
            for (unsigned k = 0; k < countof(/*arr.type.quals*/arr.type.info.arr.quals); k++) if (QUAL_END == arr.type.quals[k]) {
                //arr.type.quals[k] = askw; // XXX: I'm confused tbh
                arr.type.info.arr.quals[k] = askw;
                break;
            }
        }

        if (ps->tok.len) switch (*ps->tok.ptr) {
        case '*':
            lext(ps->ls);
            //_expect(ps->tok, "]");
            // fall through
        case ']':
            ps->tok = lext(ps->ls);
            _parse_decl_post(ps, capt, &arr);
            return;
        }

        exitf("NIY: array size");
        parse_expression(&(parse_expr_state){
                .ls= ps->ls,
                // TODO:
                //.usr= [ps, arr= &arr, capt],
                //.on= (void ref usr, expression ref expr, bufsl ref tok) -> {
                //    _expect(tok, "]");
                //    usr->arr->type.info.arr.count = expr;
                //    ps->tok = lext(ps->ls);
                //    _parse_decl_post(ps, capt, &arr);
                //},
            }, ps->tok);
        return;
    }

    capt->then(ps, capt->next, decl);
}

/// parse the params of a function
void _parse_decl_params(parse_decl_state ref ps, struct _parse_decl_capture ref capt, declaration ref decl) {
    struct decl_type_param node = {.decl= decl}; // here so it's not deallocated before the recursion
    declaration ref fun_decl = capt->hold;

    bool last = ')' == *ps->tok.ptr;
    if (last || ',' == *ps->tok.ptr) {
        struct decl_type_fun* const fun = &fun_decl->type.info.fun;
        ps->tok = lext(ps->ls);

        if (!decl) {
            if (last) {
                fun->count = -1; // eg. `int a();`
                _parse_decl_post(ps, capt, fun_decl);
                return;
            } else exitf("NIY: syntax error in params");
        }

        if (!(last && !decl->name.len && 4 == decl->type.name.len && !memcmp("void", decl->type.name.ptr, 4))) {
            fun->count++;
            if (!fun->first) fun->first = &node;
            else for (struct decl_type_param* curr = fun->first ;3; curr = curr->next) if (!curr->next) {
                curr->next = &node;
                break;
            }
        }

        if (last) {
            _parse_decl_post(ps, capt, fun_decl);
            return;
        }
    }

    _parse_decl_spec(ps, &(struct _parse_decl_capture){
            .next= capt,
            .then= _parse_decl_params,
        }, &(declaration){0});
}

/// parse values of an enum
void _parse_decl_enumer(parse_decl_state ref ps, struct _parse_decl_capture ref capt, declaration ref _) {
    (void)_;
    struct decl_type_enumer node = {.name= ps->tok}; // here so it's not deallocated before the recursion
    declaration ref enu_decl = capt->hold;

    struct decl_type_enu* const enu = &enu_decl->type.info.enu;
    enu->count++;
    if (!enu->first) enu->first = &node;
    else for (struct decl_type_enumer* curr = enu->first ;3; curr = curr->next) if (!curr->next) {
        curr->next = &node;
        break;
    }

    if ('=' == *ps->tok.ptr) {
        ps->tok = lext(ps->ls);
        exitf("NIY: enumerator value");
        parse_expression(&(parse_expr_state){
                .ls= ps->ls,
                // TODO:
                //.usr= [ps, pexpr= &node.expr, capt],
                //.on= (void ref usr, expression ref expr, bufsl ref tok) -> {
                //    _expect(tok, "," or "}");
                //    *pexpr = expr;
                //    if ('}') _parse_decl_post;
                //    else _parse_decl_enumer;
                //}
            }, ps->tok);
        return;
    }

    if (',' == *ps->tok.ptr) ps->tok = lext(ps->ls);
    if ('}' == *ps->tok.ptr) {
        _parse_decl_post(ps, capt, enu_decl);
        return;
    }

    _parse_decl_enumer(ps, capt, NULL);
}

/// parse fields of a struct/union
void _parse_decl_fields(parse_decl_state ref ps, struct _parse_decl_capture ref capt, declaration ref decl) {
    struct decl_type_field node = {.decl= decl}; // here so it's not deallocated before the recursion
    declaration ref comp_decl = ((declaration**)capt->hold)[0]; // yyy: see at call location as for what is [0]
    declaration cref base = ((declaration**)capt->hold)[1];

    bool reset = ';' == *ps->tok.ptr;
    if (reset || ',' == *ps->tok.ptr) {
        struct decl_type_comp* const comp = &comp_decl->type.info.comp;
        ps->tok = lext(ps->ls);

        comp->count++;
        if (!comp->first) comp->first = &node;
        else for (struct decl_type_field* curr = comp->first ;3; curr = curr->next) if (!curr->next) {
            curr->next = &node;
            break;
        }

        if (':' == *ps->tok.ptr) {
            ps->tok = lext(ps->ls);
            exitf("NIY: bitfield width");
            parse_expression(&(parse_expr_state){
                    .ls= ps->ls,
                    // TODO:
                    //.usr= [ps, field= &decl, capt],
                    //.on= (void ref usr, expression ref expr, bufsl ref tok) -> {
                    //    _expect(tok, "," or ";");
                    //    usr->field... = expr;
                    //    ps->tok = lext(ps->ls);
                    //    // all the stuff below:
                    //    if ('}') _parse_decl_post;
                    //    else _parse_decl_spec;
                    //},
                }, ps->tok);
            return;
        }

        if ('}' == *ps->tok.ptr) {
            ps->tok = lext(ps->ls);
            _parse_decl_post(ps, capt, comp_decl);
            return;
        }
    }

    declaration niwbase = reset ? (declaration){0} : *base;
    _parse_decl_spec(ps, &(struct _parse_decl_capture){
            .next= capt,
            .then= _parse_decl_fields,
        }, &niwbase);
}

/// parse the specifier and initial qualifiers then one declarator
void _parse_decl_spec(parse_decl_state ref ps, struct _parse_decl_capture ref capt, declaration ref decl) {
#   define is1(w) (ps->tok.len && w == *ps->tok.ptr)
#   define isid() (ps->tok.len && ('_' == *ps->tok.ptr || ('A' <= *ps->tok.ptr && *ps->tok.ptr <= 'Z') || ('a' <= *ps->tok.ptr && *ps->tok.ptr <= 'z') || ('0' <= *ps->tok.ptr && *ps->tok.ptr <= '9')))
#   define case_iskw(...) if (0) case kws(__VA_ARGS__): if (!iskwx(ps->tok, __VA_ARGS__)) goto notkw;

    for (unsigned askw; ps->tok.len; ps->tok = lext(ps->ls)) redo: switch (askw = ps->tok.len <3 ? 0 : kw(ps->tok.ptr)) {
    case 0: goto notkw; // here as to remove warning for the `if` right after

    case_iskw('t','y','p','e','d','e','f') case_iskw('e','x','t','e','r','n') case_iskw('s','t','a','t','i','c') case_iskw('a','u','t','o') case_iskw('r','e','g','i','s','t','e','r')
        decl->spec = askw;
        continue;

    case_iskw('s','i','g','n','e','d') case_iskw('u','n','s','i','g','n','e','d') case_iskw('s','h','o','r','t') case_iskw('l','o','n','g')
        if (!decl->type.name.len) decl->type.name = (bufsl){.ptr= "int", .len= 3};
    case_iskw('c','o','n','s','t') case_iskw('r','e','s','t','r','i','c','t') case_iskw('v','o','l','a','t','i','l','e') case_iskw('i','n','l','i','n','e')
    case_iskw('c','o','m','p','l','e','x') case_iskw('i','m','a','g','i','n','a','r','y')
        for (unsigned k = 0; k < countof(decl->type.quals); k++) if (QUAL_END == decl->type.quals[k]) {
            decl->type.quals[k] = askw;
            break;
        }
        continue;

    case_iskw('s','t','r','u','c','t') case_iskw('u','n','i','o','n') case_iskw('e','n','u','m')
        decl->type.kind = askw;
        bool const e = KIND_ENUM == askw;
        ps->tok = lext(ps->ls);
        if (isid()) {
            decl->type.name = ps->tok;
            ps->tok = lext(ps->ls);
        }
        if (is1('{')) {
            ps->tok = lext(ps->ls);
            if (e) _parse_decl_enumer(ps, &(struct _parse_decl_capture){
                    .hold= decl,
                    .next= capt->next,
                    .then= capt->then,
                }, NULL);
            else _parse_decl_fields(ps, &(struct _parse_decl_capture){
                    .hold= (void*)(declaration*[2]){decl, &(declaration){0}}, // yyy: whatever, see _parse_decl_fields
                    .next= capt->next,
                    .then= capt->then,
                }, NULL);
            return;
        }
        if (!e) decl->type.info.comp.count = -1;
        goto redo;

    default:
    notkw:
        // at this point it can be (in a valid declaration):
        // - a name:
        //   - name of the type -> only if not already found
        //   - it's a declarator
        // - a '*' a '(' or -> it's a declarator
        // - (in the params of a function declaration):
        //   - a ',' or a ')' -> emit and return
        // - a '=' or a ',' or a ';' -> emit and return

        if (ps->tok.len) {
            if (isid()) {
                if (!decl->type.name.len || (
                            3 == decl->type.name.len && !memcmp("int", decl->type.name.ptr, 3)
                            && 6 == ps->tok.len && !memcmp("double", ps->tok.ptr, 6) )) {
                    decl->type.name = ps->tok;
                    continue;
                }
                declaration cpy = *decl;
                _parse_decl_ator(ps, capt, &cpy);
                return;
            }

            switch (*ps->tok.ptr) {
            case '=': case ',': case ';': case ')': // shortcut one stack frame
                capt->then(ps, capt->next, decl);
                return;
            case '*': case '(':;
                declaration cpy = *decl;
                _parse_decl_ator(ps, capt, &cpy);
                return;
            }
        }

        exitf("NIY: syntax error in spec");
    } // while-switch tok

#   undef case_iskw
#   undef isid
#   undef is1

    exitf("NIY: syntax error (I *think* we're not supposed to be here?)");
}

/// callback chain tail end which call user code
void _parse_decl_exit(parse_decl_state ref ps, struct _parse_decl_capture ref _, declaration ref decl) {
    (void)_;
    if (ps->on) ps->on(ps->usr, decl, &ps->tok);
}

bufsl parse_declaration(parse_decl_state ref ps, bufsl tok) {
    ps->tok = tok;
    _parse_decl_spec(ps, &(struct _parse_decl_capture){.then= _parse_decl_exit}, &ps->base);
    return ps->tok;
}

#undef iskw
#undef iskwx
#undef kw

#undef kws
// }}}

// parse expression {{{
struct _parse_expr_capture;
typedef void _parse_expr_closure_t(parse_expr_state ref ps, struct _parse_expr_capture ref capt, expression ref expr);
struct _parse_expr_capture {
    expression* hold;
    struct _parse_expr_capture ref next;
    _parse_expr_closure_t ref then; // its `hold` is in `next->hold`
};
_parse_expr_closure_t _parse_expr_one, _parse_expr_one_post, _parse_expr_one_lext, _parse_expr_one_lext_whole, _parse_expr_one_after, _parse_expr_two, _parse_expr_two_after, _parse_expr_entry, _parse_expr_continue, _parse_expr_exit;

enum expr_kind _parse_is_postfix(bufsl const tok) {
    if (2 == tok.len) switch (tok.ptr[0]<<8 | tok.ptr[1]) {
    case '-'<<8 | '-': return UNOP_POST_DEC;
    case '+'<<8 | '+': return UNOP_POST_INC;
    }
    return 0;
}
enum expr_kind _parse_is_prefix(bufsl const tok) {
    if (1 == tok.len) switch (tok.ptr[0]) {
    case '&': return UNOP_ADDR;
    case '*': return UNOP_DEREF;
    case '~': return UNOP_BNOT;
    case '!': return UNOP_LNOT;
    case '-': return UNOP_MINUS;
    case '+': return UNOP_PLUS;
    }
    if (2 == tok.len) switch (tok.ptr[0]<<8 | tok.ptr[1]) {
    case '-'<<8 | '-': return UNOP_PRE_DEC;
    case '+'<<8 | '+': return UNOP_PRE_INC;
    }
    return 0;
}
enum expr_kind _parse_is_infix(bufsl const tok) {
    if (1 == tok.len) switch (tok.ptr[0]) {
    case '?': return BINOP_TERNCOND;
    case ':': return BINOP_TERNBRANCH;
    case ',': return BINOP_COMMA;
    case '=': return BINOP_ASGN;
    case '|': return BINOP_BOR;
    case '^': return BINOP_BXOR;
    case '&': return BINOP_BAND;
    case '-': return BINOP_SUB;
    case '+': return BINOP_ADD;
    case '%': return BINOP_REM;
    case '/': return BINOP_DIV;
    case '*': return BINOP_MUL;
    case '<': return BINOP_LT;
    case '>': return BINOP_GT;
    }
    if (2 == tok.len) switch (tok.ptr[0]<<8 | tok.ptr[1]) {
    case '|'<<8 | '=': return BINOP_ASGN_BOR;
    case '^'<<8 | '=': return BINOP_ASGN_BXOR;
    case '&'<<8 | '=': return BINOP_ASGN_BAND;
    case '-'<<8 | '=': return BINOP_ASGN_SUB;
    case '+'<<8 | '=': return BINOP_ASGN_ADD;
    case '%'<<8 | '=': return BINOP_ASGN_REM;
    case '/'<<8 | '=': return BINOP_ASGN_DIV;
    case '*'<<8 | '=': return BINOP_ASGN_MUL;
    case '|'<<8 | '|': return BINOP_LOR;
    case '&'<<8 | '&': return BINOP_LAND;
    case '='<<8 | '=': return BINOP_EQ;
    case '!'<<8 | '=': return BINOP_NE;
    case '<'<<8 | '=': return BINOP_LE;
    case '>'<<8 | '=': return BINOP_GE;
    case '<'<<8 | '<': return BINOP_BSHL;
    case '>'<<8 | '>': return BINOP_BSHR;
    }
    if (3 == tok.len && '<' == tok.ptr[1] && '=' == tok.ptr[2]) switch (tok.ptr[0]) {
    case '<': return BINOP_ASGN_BSHL;
    case '>': return BINOP_ASGN_BSHR;
    }
    return 0;
}

/// parse one, including the prefix: [<prefix>] (<atom> | '('<expr>')') [<postfix>]
void _parse_expr_one(parse_expr_state ref ps, struct _parse_expr_capture ref capt, expression ref _) {
    (void)_;
    if (!ps->tok.len) return;

    enum expr_kind prefix = _parse_is_prefix(ps->tok);
    if (prefix) {
        ps->tok = lext(ps->ls);
        expression pre = {.kind= prefix};
        _parse_expr_one(ps, &(struct _parse_expr_capture){
                .next= &(struct _parse_expr_capture){
                    .hold= &pre,
                    .next= capt->next,
                    .then= capt->then,
                },
                .then= _parse_expr_one_post,
            }, NULL);
        return;
    }

    if ('(' == *ps->tok.ptr) {
        ps->tok = lext(ps->ls);
        _parse_expr_one(ps, &(struct _parse_expr_capture){
                .next= &(struct _parse_expr_capture){
                    .next= capt,
                    .then= _parse_expr_one_lext,
                },
                .then= _parse_expr_continue,
            }, NULL);
        return;
    }

    expression atom = {.kind= ATOM, .info.atom= ps->tok};
    ps->tok = lext(ps->ls);
    _parse_expr_one_after(ps, capt, &atom);
}

/// sets the operand (expr) of the prefix op in: <prefix> <expr> [<postfix>]
void _parse_expr_one_post(parse_expr_state ref ps, struct _parse_expr_capture ref capt, expression ref expr) {
    capt->hold->info.unary.opr = expr;
    _parse_expr_one_after(ps, capt, capt->hold);
}

/// skip a closing parenthesis in: '('<expr>')' [<postfix>]
void _parse_expr_one_lext(parse_expr_state ref ps, struct _parse_expr_capture ref capt, expression ref expr) {
    ps->tok = lext(ps->ls);
    _parse_expr_one_after(ps, capt, expr);
}

/// skip a closing thingy and set the thing within (arg): <expr> ('('<arg>')' | '['<arg>']') [<postfix>]
void _parse_expr_one_lext_whole(parse_expr_state ref ps, struct _parse_expr_capture ref capt, expression ref within) {
    ps->tok = lext(ps->ls);
    capt->hold->info.call.args = within;
    _parse_expr_one_after(ps, capt, capt->hold);
}

/// parse postfix part after expr: <expr> (<postfix> | '('<arg>')' | '['<arg>']' | ('.'|'->')<name>)
void _parse_expr_one_after(parse_expr_state ref ps, struct _parse_expr_capture ref capt, expression ref expr) {
    enum expr_kind postfix = _parse_is_postfix(ps->tok);
    if (postfix) {
        ps->tok = lext(ps->ls);
        expression post = {.kind= postfix, .info.unary.opr= expr};
        _parse_expr_one_after(ps, capt, &post);
        return;
    }

    bool call = false, pmem = false;
    if (ps->tok.len) switch (*ps->tok.ptr) {
    case '(':
        call = true;
        bufsl const nx = lext(ps->ls);
        if (!nx.len || ')' == *nx.ptr) {
            ps->tok = lext(ps->ls);
            expression access = {.kind= BINOP_CALL, .info.call.base= expr};
            _parse_expr_one_after(ps, capt, &access);
            return;
        }
        ps->tok = nx;
        if (0) // fall through
    case '[':
            ps->tok = lext(ps->ls);
        expression whole = {.kind= call ? BINOP_CALL : BINOP_SUBSCR, .info.call.base= expr};
        capt->hold = &whole;
        _parse_expr_one(ps, &(struct _parse_expr_capture){
                .next= &(struct _parse_expr_capture){
                    .next= capt,
                    .then= _parse_expr_one_lext_whole,
                },
                .then= _parse_expr_continue,
            }, NULL);
        return;

    case '-':
        if (2 != ps->tok.len || '>' != ps->tok.ptr[1]) break;
        pmem = true;
        // fall through
    case '.':
        ;
        bufsl name = lext(ps->ls);
        ps->tok = lext(ps->ls);
        expression access = {
            .kind= pmem ? UNOP_PMEMBER : UNOP_MEMBER,
            .info.member= {.base= expr, .name= &name},
        };
        _parse_expr_one_after(ps, capt, &access);
        return;
    }

    capt->then(ps, capt->next, expr);
}

/// parse two with lhs, lop and rhs known: <lhs> <lop> <rhs> [<nop>]
void _parse_expr_two(parse_expr_state ref ps, struct _parse_expr_capture ref capt, expression ref rhs) {
    enum expr_kind infix = _parse_is_infix(ps->tok);
    if (!infix) {
        capt->hold->info.binary.rhs = rhs;
        capt->then(ps, capt->next, capt->hold);
        return;
    }
    ps->tok = lext(ps->ls);

    enum expr_kind l = capt->hold->kind, n = infix;
    if (l < n || ( (BINOP_TERNCOND == l || BINOP_TERNBRANCH == l)
                && (BINOP_TERNCOND == n || BINOP_TERNBRANCH == n) )
              || ( (BINOP_ASGN <= l && l <= BINOP_ASGN_MUL)
                && (BINOP_ASGN <= n && n <= BINOP_ASGN_MUL) )) {
        expression in = {.kind= infix, .info.binary.lhs= rhs};
        _parse_expr_one(ps, &(struct _parse_expr_capture){
                .next= &(struct _parse_expr_capture){
                    .hold= &in,
                    .next= &(struct _parse_expr_capture){
                        .hold= capt->hold,
                        .next= capt->next,
                        .then= capt->then,
                    },
                    .then= _parse_expr_two_after,
                },
                .then= _parse_expr_two,
            }, NULL);
    } else {
        capt->hold->info.binary.rhs = rhs;
        expression in = {.kind= infix, .info.binary.lhs= capt->hold};
        _parse_expr_one(ps, &(struct _parse_expr_capture){
                .next= &(struct _parse_expr_capture){
                    .hold= &in,
                    .next= capt->next,
                    .then= capt->then,
                },
                .then= _parse_expr_two,
            }, NULL);
    }
}

/// handle the case in `_parse_expr_two` where nop comes before lop in precedence (it's executed when "bubbling" back toward `_parse_expr_exit`)
void _parse_expr_two_after(parse_expr_state ref ps, struct _parse_expr_capture ref capt, expression ref rhs) {
    capt->hold->info.binary.rhs = rhs;
    capt->then(ps, capt->next, capt->hold);
}

/// proper start the `_parse_expr_two` loop, sets up `_parse_expr_exit` at callback chain end
void _parse_expr_entry(parse_expr_state ref ps, struct _parse_expr_capture ref capt, expression ref lhs) {
    enum expr_kind infix = _parse_is_infix(ps->tok);
    if (infix) {
        ps->tok = lext(ps->ls);
        expression in = {.kind= infix, .info.binary.lhs= lhs};
        _parse_expr_one(ps, &(struct _parse_expr_capture){
                .next= &(struct _parse_expr_capture){
                    .hold= &in,
                    .then= _parse_expr_exit,
                },
                .then= _parse_expr_two,
            }, NULL);
        return;
    }

    _parse_expr_exit(ps, NULL, lhs);
}

/// similar to `_parse_expr_entry` for sub expr in: '('<expr>')' or arg in: <expr> ('('<arg>')' | '['<arg>'])
void _parse_expr_continue(parse_expr_state ref ps, struct _parse_expr_capture ref capt, expression ref lhs) {
    enum expr_kind infix = _parse_is_infix(ps->tok);
    if (infix) {
        ps->tok = lext(ps->ls);
        expression in = {.kind= infix, .info.binary.lhs= lhs};
        capt->hold = &in;
        _parse_expr_one(ps, &(struct _parse_expr_capture){
                .next= capt,
                .then= _parse_expr_two,
            }, NULL);
        return;
    }

    capt->then(ps, capt->next, lhs);
}

/// callback chain tail end which call user code
void _parse_expr_exit(parse_expr_state ref ps, struct _parse_expr_capture ref _, expression ref expr) {
    (void)_;
    if (ps->on) ps->on(ps->usr, expr, &ps->tok);
}

bufsl parse_expression(parse_expr_state ref ps, bufsl tok) {
    ps->tok = tok;
    _parse_expr_one(ps, &(struct _parse_expr_capture){.then= _parse_expr_entry}, NULL);
    return ps->tok;
}
// }}}

#endif // CINTRE_PARSER_H
