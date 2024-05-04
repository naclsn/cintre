/// C parser on top of the lexer; examples:
/// ```c
/// lex_state ls = ...;
/// my_state_t my_state = ...;
///
/// void accept_decl(my_state_t* me, declaration const* decl, bufsl* tok) { ... }
/// void accept_expr(my_state_t* me, expression* expr, bufsl* tok) { ... }
///
/// {
///     parse_decl_state decl_ps = {.ls= &ls, .usr= &my_state, .on= accept_decl};
///     bufsl after = parse_declaration(&decl_ps, lext(&ls));
///     // note: `int a, b;` has 2 declarations with the same "base", so it
///     // will be 2 calls to `parse_declaration` with the same state;
///     // however when the ';' is found the usual behavior is to reset:
///     if (';' == *after.ptr) decl_ps.base = (declaration){0};
/// }
///
/// {
///     parse_expr_state expr_ps = {.ls= &ls, .usr= &my_state, .on= accept_expr};
///     bufsl after = parse_expression(&expr_ps, lext(&ls));
/// }
/// ```
///
/// It makes use of the call stack, that is nothing is allocated on the heap
/// and the result needs to be handled in the "accept" callback (or copied over
/// to the heap at this point).

// TODO: once thoroughly tested, look for avoidable capture-copying (that could
// be replaced with mutating an existing capture) and extraneous calls
// (especially to _parse_expr_one_after)

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
        enum decl_type_qual {
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

        enum decl_type_kind {
            KIND_NOTAG= 0,
            KIND_STRUCT= kws('s','t','r','u','c','t'),
            KIND_UNION= kws('u','n','i','o','n'),
            KIND_ENUM= kws('e','n','u','m'),
            KIND_PTR= '*',
            KIND_FUN= ('('&31)<<5 | (')'&31),
            KIND_ARR= ('['&31)<<5 | (']'&31),
        } kind;

        union decl_type_info {
            struct declaration const* ptr;

            struct decl_type_comp {
                size_t count; // -1 if no body
                struct decl_type_field {
                    struct declaration const* decl;
                    struct expression* bitw; // NULL if not specified or irrelevant
                    struct decl_type_field* next;
                }* first;
            } comp; // struct or union

            struct decl_type_enu {
                size_t count;
                struct decl_type_enumer {
                    bufsl const name;
                    struct expression* expr; // NULL if not specified
                    struct decl_type_enumer* next;
                }* first;
            } enu; // enum

            struct decl_type_fun {
                struct declaration const* ret;
                size_t count; // -1 when (), 0 when (void), n otherwise
                struct decl_type_param {
                    struct declaration const* decl;
                    struct decl_type_param* next;
                }* first;
            } fun;

            struct deck_type_arr {
                struct declaration const* item;
                struct expression* count; // NULL when [*] or [], n otherwise
                bool statik;
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
        struct { struct expression* base; struct expr_call_arg { struct expression* expr; struct expr_call_arg* next; }* first; } call;
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
    // comma op not allowed in:
    // - declaration init (eg `int a = 42, b`)
    // - function args (eg `printf("d: %d", d)`)
    // - conditional alternative branch (eg `a ? b :3, d`)
    // - bitfield width (eg `struct { int a :3, b; }`)
    bool disallow_comma;
} parse_expr_state;

bufsl parse_expression(parse_expr_state ref ps, bufsl const tok);

// ---

#define _expect1(_tok)                                                \
    if (!(_tok)->len && (                                             \
        report_lex_locate(ps->ls, "Unexpected end of input"), true))  \
        return
#define _expect(_tok, ...)                                                                               \
    for (char const* const* _it = (char const*[]){__VA_ARGS__, NULL} ;3; _it++)                          \
        if (*_it) if (bufis(*(_tok), *_it)) break; else continue;                                        \
        else if (                                                                                        \
            report_lex_locate(ps->ls, "Expected " #__VA_ARGS__ ", got \"%.*s\"", bufmt(*(_tok))), true)  \
            return
#define _expectid(_tok)                                                                       \
    if (((*(_tok)->ptr|32) < 'a' || 'z' < (*(_tok)->ptr|32)) && (                             \
        report_lex_locate(ps->ls, "Expected identifier, got \"%.*s\"", bufmt(*_tok)), true))  \
        return

// parse declaration {{{
struct _parse_decl_capture;
typedef void _parse_decl_closure_t(parse_decl_state ref ps, struct _parse_decl_capture ref capt, declaration ref decl);
struct _parse_decl_capture {
    declaration* hold;
    struct _parse_decl_capture ref next;
    _parse_decl_closure_t ref then;
};
_parse_decl_closure_t _parse_decl_ator, _parse_decl_close, _parse_decl_fixup, _parse_decl_post, _parse_decl_params, _parse_decl_enumer, _parse_decl_fields, _parse_decl_spec;

#define _linked_it_type_comp struct decl_type_field
#define _linked_it_type_enu struct decl_type_enumer
#define _linked_it_type_fun struct decl_type_param
#define for_linked(__info, __ty) for (_linked_it_type_##__ty* curr = (__info).__ty.first; curr; curr = curr->next)

void _parse_on_array_size(void ref decl_ps_capt[3], expression ref expr, bufsl ref tok) {
    declaration ref arr = decl_ps_capt[0]; parse_decl_state ref ps = decl_ps_capt[1]; struct _parse_decl_capture ref capt = decl_ps_capt[2];
    _expect1(tok);
    _expect(tok, "]");
    arr->type.info.arr.count = expr;
    ps->tok = lext(ps->ls);
    _parse_decl_post(ps, capt, arr);
}
void _parse_on_enumer_value(void ref decl_ps_capt[3], expression ref expr, bufsl ref tok) {
    declaration ref enu = decl_ps_capt[0]; parse_decl_state ref ps = decl_ps_capt[1]; struct _parse_decl_capture ref capt = decl_ps_capt[2];
    _expect1(tok);
    for_linked (enu->type.info,enu) if (!curr->next) {
        curr->expr = expr;
        break;
    }
    if (',' == *tok->ptr) {
        ps->tok = lext(ps->ls);
        _expect1(&ps->tok);
    } else ps->tok = *tok;
    if ('}' == *ps->tok.ptr) {
        ps->tok = lext(ps->ls);
        _parse_decl_ator(ps, capt, enu);
    } else _parse_decl_enumer(ps, capt, NULL);
}
void _parse_on_bitfield_width(void ref decl_ps_capt[4], expression ref expr, bufsl ref tok) {
    declaration ref comp = decl_ps_capt[0], ref base = decl_ps_capt[1]; parse_decl_state ref ps = decl_ps_capt[2]; struct _parse_decl_capture ref capt = decl_ps_capt[3];
    _expect1(tok);
    _expect(tok, ",", ";");
    for_linked (comp->type.info,comp) if (!curr->next) {
        curr->bitw = expr;
        break;
    }
    bool const reset = ';' == *tok->ptr;
    ps->tok = lext(ps->ls);
    _expect1(&ps->tok);
    if (reset && '}' == *ps->tok.ptr) {
        ps->tok = lext(ps->ls);
        _parse_decl_ator(ps, capt, comp);
        return;
    }
    declaration niwbase = reset ? (declaration){0} : *base;
    _parse_decl_spec(ps, &(struct _parse_decl_capture){
            .next= capt,
            .then= _parse_decl_fields,
        }, &niwbase);
}

#define kw(s) kws(s[0],s[1],s[2],)
#define iskwx(tok, ...) !dyarr_cmp(&((bufsl){.ptr= (char[]){__VA_ARGS__}, .len= sizeof((char[]){__VA_ARGS__})}), &tok)
#define iskw(tok, askw, ...) (kws(__VA_ARGS__) == askw && iskwx(tok, __VA_ARGS__))

/// pre-ish declarator part with '('<decl>')' | '*'<decl>
void _parse_decl_ator(parse_decl_state ref ps, struct _parse_decl_capture ref capt, declaration ref decl) {
    _expect1(&ps->tok);
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
            }, &before);
        return;

    case '*':
        ps->tok = lext(ps->ls);
        declaration ptr = {
            .spec= decl->spec,
            .type= {.kind= KIND_PTR, .info.ptr= decl},
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

    case '=': case ',': case ';': case ')': case ':':
        capt->then(ps, capt->next, decl);
        return;
    }

    decl->name = ps->tok;
    ps->tok = lext(ps->ls);
    _parse_decl_post(ps, capt, decl);
}

/// skip closing parenthesis and parse post
void _parse_decl_close(parse_decl_state ref ps, struct _parse_decl_capture ref capt, declaration ref decl) {
    _expect1(&ps->tok);
    _expect(&ps->tok, ")");
    ps->tok = lext(ps->ls);

    declaration ref before = capt->hold;
    if (decl != before) {
        _parse_decl_post(ps, &(struct _parse_decl_capture){
                .next= &(struct _parse_decl_capture){
                    .hold= (void*)(declaration*[2]){before, decl}, // yyy: need both
                    .next= capt->next,
                    .then= capt->then,
                },
                .then= _parse_decl_fixup,
            }, before);
        return;
    }
    // eg. `int (a)` would get there, because `decl == before == ptr to the "int" typed base`

    _parse_decl_post(ps, capt, decl);
}

/// fixup after a parenthesised declarator like `(*a)`
void _parse_decl_fixup(parse_decl_state ref ps, struct _parse_decl_capture ref capt, declaration ref after) {
    declaration cref before = ((declaration**)capt->hold)[0]; // yyy: see at call location as for what is all that
    declaration* hold = ((declaration**)capt->hold)[1];

    // 'visit' hold; until find before; replace with after
    declaration** it = &hold;
    do switch ((*it)->type.kind) { // xxx: casts are to discard const qualifier
        case KIND_PTR: it = (declaration**)&(*it)->type.info.ptr;      break;
        case KIND_FUN: it = (declaration**)&(*it)->type.info.fun.ret;  break;
        case KIND_ARR: it = (declaration**)&(*it)->type.info.arr.item; break;
        default:;
    } while (before != *it);
    *it = after;

    capt->then(ps, capt->next, hold);
}

/// postfix declarator notations
void _parse_decl_post(parse_decl_state ref ps, struct _parse_decl_capture ref capt, declaration ref decl) {
    // '(' <params> ')'
    // '[' [static][const][idk] <expr>|*|<nothing> ']'

    if (ps->tok.len) switch (*ps->tok.ptr) {
    case '(':
        ps->tok = lext(ps->ls);
        declaration fun = {
            .spec= decl->spec,
            .type= {.kind= KIND_FUN, .info.fun.ret= decl},
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
            .type= {.kind= KIND_ARR, .info.arr.item= decl},
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
            for (unsigned k = 0; k < countof(arr.type.quals); k++) if (QUAL_END == arr.type.quals[k]) {
                arr.type.quals[k] = askw;
                break;
            }
        }

        if (ps->tok.len) switch (*ps->tok.ptr) {
        case '*':
            ps->tok = lext(ps->ls);
            _expect1(&ps->tok);
            _expect(&ps->tok, "]");
            // fall through
        case ']':
            ps->tok = lext(ps->ls);
            _parse_decl_post(ps, capt, &arr);
            return;
        }

        parse_expression(&(parse_expr_state){
                .ls= ps->ls,
                .usr= (void*[3]){&arr, ps, capt},
                .on= (void(*)())_parse_on_array_size,
            }, ps->tok);
        return;
    }

    capt->then(ps, capt->next, decl);
}

/// parse the params of a function
void _parse_decl_params(parse_decl_state ref ps, struct _parse_decl_capture ref capt, declaration ref decl) {
    struct decl_type_param node = {.decl= decl}; // here so it's not deallocated before the recursion
    declaration ref fun = capt->hold;

    _expect1(&ps->tok);
    bool last = ')' == *ps->tok.ptr;
    if (last || ',' == *ps->tok.ptr) {
        union decl_type_info ref info = &fun->type.info;
        ps->tok = lext(ps->ls);

        if (!decl) {
            if (last) {
                info->fun.count = -1; // eg. `int a();`
                _parse_decl_post(ps, capt, fun);
            } else report_lex_locate(ps->ls, "Expected parameter declaration, got \"%.*s\"", bufmt(ps->tok));
            return;
        }

        if (!(last && !decl->name.len && 4 == decl->type.name.len && !memcmp("void", decl->type.name.ptr, 4))) {
            info->fun.count++;
            if (!info->fun.first) info->fun.first = &node;
            else for_linked (*info,fun) if (!curr->next) {
                curr->next = &node;
                break;
            }
        }

        if (last) {
            _parse_decl_post(ps, capt, fun);
            return;
        }

        _expect1(&ps->tok);
    }
    if (')' == *ps->tok.ptr) {
        report_lex_locate(ps->ls, "Expected parameter declaration, got \"%.*s\"", bufmt(ps->tok));
        return;
    }

    _parse_decl_spec(ps, &(struct _parse_decl_capture){
            .next= capt,
            .then= _parse_decl_params,
        }, &(declaration){0});
}

/// parse values of an enum
void _parse_decl_enumer(parse_decl_state ref ps, struct _parse_decl_capture ref capt, declaration ref _) {
    (void)_;
    _expect1(&ps->tok);
    _expectid(&ps->tok);

    struct decl_type_enumer node = {.name= ps->tok}; // here so it's not deallocated before the recursion
    declaration ref enu = capt->hold;
    union decl_type_info ref info = &enu->type.info;

    ps->tok = lext(ps->ls);
    _expect1(&ps->tok);

    info->enu.count++;
    if (!info->enu.first) info->enu.first = &node;
    else for_linked (*info,enu) if (!curr->next) {
        curr->next = &node;
        break;
    }

    if ('=' == *ps->tok.ptr) {
        ps->tok = lext(ps->ls);
        parse_expression(&(parse_expr_state){
                .ls= ps->ls,
                .usr= (void*[3]){enu, ps, capt},
                .on= (void(*)())_parse_on_enumer_value,
                .disallow_comma= true,
            }, ps->tok);
        return;
    }

    if (',' == *ps->tok.ptr) {
        ps->tok = lext(ps->ls);
        _expect1(&ps->tok);
    }
    if ('}' == *ps->tok.ptr) {
        ps->tok = lext(ps->ls);
        _parse_decl_ator(ps, capt, enu);
    } else _parse_decl_enumer(ps, capt, NULL);
}

/// parse fields of a struct/union
void _parse_decl_fields(parse_decl_state ref ps, struct _parse_decl_capture ref capt, declaration ref decl) {
    struct decl_type_field node = {.decl= decl}; // here so it's not deallocated before the recursion
    declaration ref comp = ((declaration**)capt->hold)[0]; // yyy: see at call location as for what is all that
    declaration* ref base = ((declaration**)capt->hold)+1;

    _expect1(&ps->tok);
    bool bitw = ':' == *ps->tok.ptr;
    bool reset = ';' == *ps->tok.ptr;
    if (bitw || reset || ',' == *ps->tok.ptr) {
        union decl_type_info ref info = &comp->type.info;
        ps->tok = lext(ps->ls);
        _expect1(&ps->tok);

        if (!decl) {
            report_lex_locate(ps->ls, "Expected field declaration, got \"%.*s\"", bufmt(ps->tok));
            return;
        }

        info->comp.count++;
        if (!info->comp.first) info->comp.first = &node;
        else for_linked (*info,comp) if (!curr->next) {
            curr->next = &node;
            break;
        }

        if (bitw) {
            parse_expression(&(parse_expr_state){
                    .ls= ps->ls,
                    .usr= (void*[4]){comp, *base, ps, capt},
                    .on= (void(*)())_parse_on_bitfield_width,
                    .disallow_comma= true,
                }, ps->tok);
            return;
        }

        if ('}' == *ps->tok.ptr) {
            if (!reset) {
                _expect(&ps->tok, ";");
                return;
            }
            ps->tok = lext(ps->ls);
            _parse_decl_ator(ps, capt, comp);
            return;
        }
    }

    if ('}' == *ps->tok.ptr) {
        if (decl) {
            _expect(&ps->tok, ";");
            return;
        }
        ps->tok = lext(ps->ls);
        _parse_decl_ator(ps, capt, comp);
        return;
    }

    declaration niwbase = reset ? (declaration){0} : **base;
    //*base = &niwbase; // yyy: once thoroughly tested, this may be an avoidable capture-copying
    _parse_decl_spec(ps, &(struct _parse_decl_capture){
            //.next= capt,
            .next= &(struct _parse_decl_capture){
                .hold= (void*)(declaration*[2]){comp, &niwbase}, // yyy: whatever, see _parse_decl_fields
                .next= capt->next,
                .then= capt->then,
            },
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
        if (!ps->tok.len) break;
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
        // - (in the fields of a struct):
        //   - a ':' -> emit and return
        // - a '=' or a ',' or a ';' -> emit and return

        if (isid()) {
            if (!decl->type.name.len ||
                    (bufis(decl->type.name, "int") &&
                     ( bufis(ps->tok, "int")    ||
                       bufis(ps->tok, "char")   || // signed/unsigned char
                       bufis(ps->tok, "double") )) // long double
               ) {
                decl->type.name = ps->tok;
                continue;
            }
            declaration cpy = *decl;
            _parse_decl_ator(ps, capt, &cpy);
            return;
        }

        switch (*ps->tok.ptr) {
        case '=': case ',': case ';': case ')': case ':': // shortcut one stack frame
            capt->then(ps, capt->next, decl);
            return;
        case '*': case '(':;
            declaration cpy = *decl;
            _parse_decl_ator(ps, capt, &cpy);
            return;
        }

        _parse_decl_post(ps, capt, decl);
        return;
    } // for-switch tok

#   undef case_iskw
#   undef isid
#   undef is1

    capt->then(ps, capt->next, decl);
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

#undef for_linked
#undef _linked_it_type_fun
#undef _linked_it_type_enu
#undef _linked_it_type_comp
// }}}

// parse expression {{{
struct _parse_expr_capture;
typedef void _parse_expr_closure_t(parse_expr_state ref ps, struct _parse_expr_capture ref capt, expression ref expr);
struct _parse_expr_capture {
    expression* hold;
    struct _parse_expr_capture ref next;
    _parse_expr_closure_t ref then; // its `hold` is in `next->hold`
};
_parse_expr_closure_t _parse_expr_one, _parse_expr_one_post, _parse_expr_one_lext_parenth, _parse_expr_one_lext_oneafter, _parse_expr_fun_args, _parse_expr_one_after, _parse_expr_tern_cond, _parse_expr_tern_branch, _parse_expr_two, _parse_expr_two_after, _parse_expr_entry, _parse_expr_continue, _parse_expr_exit;

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
enum expr_kind _parse_is_infix(bufsl const tok, bool const disallow_comma) {
    if (1 == tok.len) switch (tok.ptr[0]) {
    case '?': return BINOP_TERNCOND;
    //case ':': return BINOP_TERNBRANCH;
    case ',': return disallow_comma ? 0 : BINOP_COMMA;
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
    if (3 == tok.len && tok.ptr[0] == tok.ptr[1] && '=' == tok.ptr[2]) switch (tok.ptr[0]) {
    case '<': return BINOP_ASGN_BSHL;
    case '>': return BINOP_ASGN_BSHR;
    }
    return 0;
}

/// parse one, including the prefix: [<prefix>] (<atom> | '('<expr>')') [<postfix>]
void _parse_expr_one(parse_expr_state ref ps, struct _parse_expr_capture ref capt, expression ref _) {
    (void)_;
    _expect1(&ps->tok);

    enum expr_kind const prefix = _parse_is_prefix(ps->tok);
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
        // yyy: any non null if disallow comma was set
        capt->hold = ps->disallow_comma ? (void*)"" : NULL; // (yyy: avoids capture copy?)
        ps->disallow_comma = false;
        ps->tok = lext(ps->ls);
        _parse_expr_one(ps, &(struct _parse_expr_capture){
                .next= &(struct _parse_expr_capture){
                    .next= capt,
                    .then= _parse_expr_one_lext_parenth,
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
    //capt->then(ps, capt->next, capt->hold); // yyy: extraneous call?
    _parse_expr_one_after(ps, capt, capt->hold);
}

/// skip a closing parenthesis in: '('<expr>')' [<postfix>]
void _parse_expr_one_lext_parenth(parse_expr_state ref ps, struct _parse_expr_capture ref capt, expression ref expr) {
    _expect1(&ps->tok);
    _expect(&ps->tok, ")");
    // yyy: any non null if disallow comma was set
    ps->disallow_comma = !!capt->hold;
    ps->tok = lext(ps->ls);
    //capt->then(ps, capt->next, capt->hold); // yyy: extraneous call?
    _parse_expr_one_after(ps, capt, expr);
}

/// skip a closing bracket and set the offset ("within"): <expr> '['<off>']' [<postfix>]
void _parse_expr_one_lext_oneafter(parse_expr_state ref ps, struct _parse_expr_capture ref capt, expression ref within) {
    _expect1(&ps->tok);
    _expect(&ps->tok, "]");
    capt->hold->info.subscr.off = within;
    // yyy: any non null if disallow comma was set
    ps->disallow_comma = !!capt->hold->usr;
    capt->hold->usr = NULL;
    ps->tok = lext(ps->ls);
    //capt->then(ps, capt->next, capt->hold); // yyy: extraneous call?
    _parse_expr_one_after(ps, capt, capt->hold);
}

/// parse the arguments of a function call: <expr> '('<args>','..')' [<postfix>]
void _parse_expr_fun_args(parse_expr_state ref ps, struct _parse_expr_capture ref capt, expression ref expr) {
    _expect1(&ps->tok);
    _expect(&ps->tok, ",", ")");

    expression ref callbase = capt->hold;
    struct expr_call_arg* it = callbase->info.call.first;
    while (it->next) it = it->next;
    it->expr = expr;

    if (')' == *ps->tok.ptr) {
        // yyy: any non null if disallow comma was set
        ps->disallow_comma = !!callbase->usr;
        callbase->usr = NULL;
        ps->tok = lext(ps->ls);
        //capt->then(ps, capt->next, callbase); // yyy: extraneous call?
        _parse_expr_one_after(ps, capt, callbase);
        return;
    }

    it->next = &(struct expr_call_arg){0};
    ps->tok = lext(ps->ls);
    _parse_expr_one(ps, &(struct _parse_expr_capture){
            .next= &(struct _parse_expr_capture){
                .next= capt,
                .then= _parse_expr_fun_args,
            },
            .then= _parse_expr_continue,
        }, NULL);
}

/// parse postfix part after expr: <expr> (<postfix> | '('<arg>')' | '['<off>']' | ('.'|'->')<name>)
void _parse_expr_one_after(parse_expr_state ref ps, struct _parse_expr_capture ref capt, expression ref expr) {
    enum expr_kind const postfix = _parse_is_postfix(ps->tok);
    if (postfix) {
        ps->tok = lext(ps->ls);
        expression post = {.kind= postfix, .info.unary.opr= expr};
        _parse_expr_one_after(ps, capt, &post);
        return;
    }

    bool pmem = false;
    if (ps->tok.len) switch (*ps->tok.ptr) {
    case '(':
        ps->tok = lext(ps->ls);
        if (ps->tok.len && ')' == *ps->tok.ptr) {
            ps->tok = lext(ps->ls);
            expression access = {.kind= BINOP_CALL, .info.call.base= expr};
            _parse_expr_one_after(ps, capt, &access);
            return;
        }
        expression callbase = {
            .kind= BINOP_CALL,
            .info.call= {
                .base= expr,
                .first= &(struct expr_call_arg){0},
            },
            // yyy: any non null if disallow comma was set
            .usr= ps->disallow_comma ? (void*)"" : NULL,
        };
        ps->disallow_comma = true;
        capt->hold = &callbase; // (yyy: avoids capture copy?)
        _parse_expr_one(ps, &(struct _parse_expr_capture){
                .next= &(struct _parse_expr_capture){
                    .next= capt,
                    .then= _parse_expr_fun_args,
                },
                .then= _parse_expr_continue,
            }, NULL);
        return;

    case '[':
        ps->tok = lext(ps->ls);
        expression whole = {
            .kind= BINOP_SUBSCR,
            .info.subscr.base= expr,
            // yyy: any non null if disallow comma was set
            .usr= ps->disallow_comma ? (void*)"" : NULL,
        };
        ps->disallow_comma = false;
        capt->hold = &whole; // (yyy: avoids capture copy?)
        _parse_expr_one(ps, &(struct _parse_expr_capture){
                .next= &(struct _parse_expr_capture){
                    .next= capt,
                    .then= _parse_expr_one_lext_oneafter,
                },
                .then= _parse_expr_continue,
            }, NULL);
        return;

    case '-':
        if (2 != ps->tok.len || '>' != ps->tok.ptr[1]) break;
        pmem = true;
        if (0) // fall through
    case '.':
            if (1 != ps->tok.len) break;
        bufsl name = lext(ps->ls);
        _expect1(&name);
        _expectid(&name);
        expression access = {
            .kind= pmem ? UNOP_PMEMBER : UNOP_MEMBER,
            .info.member= {.base= expr, .name= &name},
        };
        ps->tok = lext(ps->ls);
        _parse_expr_one_after(ps, capt, &access);
        return;
    }

    capt->then(ps, capt->next, expr);
}

/// lands there after the first branch of the ternary, so on the ':', no comma op in the third operand
void _parse_expr_tern_cond(parse_expr_state ref ps, struct _parse_expr_capture ref capt, expression ref consequence) {
    _expect1(&ps->tok);
    _expect(&ps->tok, ":");
    expression ref condition_root = capt->hold;
    expression branches = {.kind= BINOP_TERNBRANCH, .info.binary.lhs= consequence};
    condition_root->info.binary.rhs = &branches;

    ps->disallow_comma = true;
    ps->tok = lext(ps->ls);
    // <cond_root.lhs> '?' <conseq> ':' <altern>
    //                                  ^
    _parse_expr_one(ps, &(struct _parse_expr_capture){
            .next= &(struct _parse_expr_capture){
                .next= capt,
                .then= _parse_expr_tern_branch,
            },
            .then= _parse_expr_continue,
        }, NULL);
}

/// after the third operand; finishs the whole ternary and restores the disallow comma state
void _parse_expr_tern_branch(parse_expr_state ref ps, struct _parse_expr_capture ref capt, expression ref alternative) {
    expression ref condition_root = capt->hold;
    condition_root->info.binary.rhs->info.binary.rhs = alternative;
    // yyy: any non null if disallow comma was set
    ps->disallow_comma = !!condition_root->usr;
    condition_root->usr = NULL;

    if (!ps->disallow_comma && 1 == ps->tok.len && ',' == *ps->tok.ptr) {
        ps->tok = lext(ps->ls);
        expression in = {.kind= BINOP_COMMA, .info.binary.lhs= condition_root};
        _parse_expr_one(ps, &(struct _parse_expr_capture){
                .next= &(struct _parse_expr_capture){
                    .next= &(struct _parse_expr_capture){
                        .hold= &in,
                        .next= capt->next,
                        .then= capt->then,
                    },
                    .then= _parse_expr_two_after,
                },
                .then= _parse_expr_continue,
            }, NULL);
        return;
    }

    capt->then(ps, capt->next, condition_root);
}

/// parse two with lhs, lop and rhs known: <lhs> <lop> <rhs> [<nop>]
void _parse_expr_two(parse_expr_state ref ps, struct _parse_expr_capture ref capt, expression ref rhs) {
    enum expr_kind const infix = _parse_is_infix(ps->tok, ps->disallow_comma);
    if (!infix) {
        capt->hold->info.binary.rhs = rhs;
        capt->then(ps, capt->next, capt->hold);
        return;
    }
    ps->tok = lext(ps->ls);

    if (BINOP_TERNCOND == infix) {
        capt->hold->info.binary.rhs = rhs;
        expression in = {.kind= infix, .info.binary.lhs= capt->hold};
        // yyy: any non null if disallow comma was set
        in.usr = ps->disallow_comma ? (void*)"" : NULL;
        ps->disallow_comma = false;
        // <in> '?' <..> ':' <..>
        //          ^
        _parse_expr_one(ps, &(struct _parse_expr_capture){
                .next= &(struct _parse_expr_capture){
                    .next= &(struct _parse_expr_capture){
                        .hold= &in,
                        .next= capt->next,
                        .then= capt->then,
                    },
                    .then= _parse_expr_tern_cond,
                },
                .then= _parse_expr_continue,
            }, NULL);
        return;
    }

    if (BINOP_COMMA == infix) {
        capt->hold->info.binary.rhs = rhs;
        expression in = {.kind= infix, .info.binary.lhs= capt->hold};
        _parse_expr_one(ps, &(struct _parse_expr_capture){
                .next= &(struct _parse_expr_capture){
                    .next= &(struct _parse_expr_capture){
                        .hold= &in,
                        .next= capt->next,
                        .then= capt->then,
                    },
                    .then= _parse_expr_two_after
                },
                .then= _parse_expr_continue,
            }, NULL);
        return;
    }

    enum expr_kind const l = capt->hold->kind, n = infix;
    if (l < n || ( (BINOP_ASGN <= l && l <= BINOP_ASGN_MUL)
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
void _parse_expr_entry(parse_expr_state ref ps, struct _parse_expr_capture ref _, expression ref lhs) {
    (void)_;
    enum expr_kind const infix = _parse_is_infix(ps->tok, ps->disallow_comma);
    if (infix) {
        ps->tok = lext(ps->ls);
        expression in = {.kind= infix, .info.binary.lhs= lhs};

        if (BINOP_TERNCOND == infix) {
            // yyy: any non null if disallow comma was set
            in.usr = ps->disallow_comma ? (void*)"" : NULL;
            ps->disallow_comma = false;
            // <lhs> '?' <..> ':' <..>
            //           ^
            _parse_expr_one(ps, &(struct _parse_expr_capture){
                    .next= &(struct _parse_expr_capture){
                        .next= &(struct _parse_expr_capture){
                            .hold= &in,
                            .then= _parse_expr_exit,
                        },
                        .then= _parse_expr_tern_cond,
                    },
                    .then= _parse_expr_continue,
                }, NULL);
            return;
        }

        if (BINOP_COMMA == infix) {
            _parse_expr_one(ps, &(struct _parse_expr_capture){
                    .next= &(struct _parse_expr_capture){
                        .next= &(struct _parse_expr_capture){
                            .hold= &in,
                            .then= _parse_expr_exit,
                        },
                        .then= _parse_expr_two_after,
                    },
                    .then= _parse_expr_continue,
                }, NULL);
            return;
        }

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

/// similar to `_parse_expr_entry` for sub expr in: '('<expr>')' and '?'<expr>':' and ','<expr> or arg in: <expr> ('('<arg>')' | '['<arg>'])
void _parse_expr_continue(parse_expr_state ref ps, struct _parse_expr_capture ref capt, expression ref lhs) {
    enum expr_kind const infix = _parse_is_infix(ps->tok, ps->disallow_comma);
    if (infix) {
        ps->tok = lext(ps->ls);
        expression in = {.kind= infix, .info.binary.lhs= lhs};

        if (BINOP_TERNCOND == infix) {
            // yyy: any non null if disallow comma was set
            in.usr = ps->disallow_comma ? (void*)"" : NULL;
            ps->disallow_comma = false;
            // <lhs> '?' <..> ':' <..>
            //           ^
            _parse_expr_one(ps, &(struct _parse_expr_capture){
                    .next= &(struct _parse_expr_capture){
                        .next= &(struct _parse_expr_capture){
                            .hold= &in,
                            .next= capt->next,
                            .then= capt->then,
                        },
                        .then= _parse_expr_tern_cond,
                    },
                    .then= _parse_expr_continue,
                }, NULL);
            return;
        }

        if (BINOP_COMMA == infix) {
            _parse_expr_one(ps, &(struct _parse_expr_capture){
                    .next= &(struct _parse_expr_capture){
                        .next= &(struct _parse_expr_capture){
                            .hold= &in,
                            .next= capt->next,
                            .then= capt->then,
                        },
                        .then= _parse_expr_two_after,
                    },
                    .then= _parse_expr_continue,
                }, NULL);
            return;
        }

        capt->hold = &in; // yyy: modifies capture
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

#undef _expect
#undef _expect1

#endif // CINTRE_PARSER_H
