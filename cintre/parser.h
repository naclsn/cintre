/// C parser on top of the lexer; examples:
/// ```c
/// ct_lex_state ls = ...;
/// my_state_t my_state = ...;
///
/// void ct_accept_decl(my_state_t* me, ct_declaration const* decl, ct_bufsl* tok) { ... }
/// void ct_accept_expr(my_state_t* me, ct_expression* expr, ct_bufsl* tok) { ... }
///
/// {
///     ct_parse_decl_state decl_ps = {.ls= &ls, .usr= &my_state, .on= ct_accept_decl};
///     ct_bufsl after = ct_parse_declaration(&decl_ps, ct_lext(&ls));
///     // note: `int a, b;` has 2 declarations with the same "base", so it
///     // will be 2 calls to `ct_parse_declaration` with the same state;
///     // however when the ';' is found the usual behavior is to reset:
///     if (';' == *after.ptr) decl_ps.base = (ct_declaration){0};
/// }
///
/// {
///     ct_parse_expr_state expr_ps = {.ls= &ls, .usr= &my_state, .on= ct_accept_expr};
///     ct_bufsl after = ct_parse_expression(&expr_ps, ct_lext(&ls));
/// }
/// ```
///
/// It makes use of the call stack, meaning nothing is allocated on the heap
/// and the result needs to be handled in the "accept" callback (or copied over
/// to the heap at this point).
///
/// Parsing (especially of declarations and expressions) does not need to know
/// the kind of an identifier (in `size_t len`, the kind of `len` is "object"
/// while the kind of `size_t` is "type"); however, beacause of the syntax of
/// the cast operator, the C syntax is ambiguous:
/// ```c
/// void size_t(int); // (say)
/// (size_t)(1+2*3);
///
/// int size_t; // (say)
/// (size_t)-1;
/// ```
/// This is the only cases I could come up with, and it will be "sanely" handled
/// as a cast operation, which means `(puts)("hi :3")` is also a cast to a type
/// named `puts`.


// TODO: once thoroughly tested, look for avoidable capture-copying (that could
// be replaced with mutating an existing capture) and extraneous calls
// (especially to _ct_parse_expr_one_after)

#ifndef CINTRE_PARSER_H
#define CINTRE_PARSER_H

#include "common.h"
#include "lexer.h"

// struct ct_declaration {{{
typedef struct ct_declaration {
#define kws(a,b,c,...) ((a&31)<<10 | (b&31)<<5 | (c&31))

    enum ct_decl_spec {
        CT_SPEC_NONE= 0,
        CT_SPEC_TYPEDEF= kws('t','y','p','e','d','e','f'),
        CT_SPEC_EXTERN= kws('e','x','t','e','r','n'),
        CT_SPEC_STATIC= kws('s','t','a','t','i','c'),
        CT_SPEC_AUTO= kws('a','u','t','o'),
        CT_SPEC_REGISTER= kws('r','e','g','i','s','t','e','r'),
    } spec;

    bool is_inline;

    struct ct_decl_type {
        enum ct_decl_type_qual {
            CT_QUAL_END= 0,
            CT_QUAL_CONST= kws('c','o','n','s','t'),
            CT_QUAL_RESTRICT= kws('r','e','s','t','r','i','c','t'),
            CT_QUAL_VOLATILE= kws('v','o','l','a','t','i','l','e'),
            CT_QUAL_SIGNED= kws('s','i','g','n','e','d'),
            CT_QUAL_UNSIGNED= kws('u','n','s','i','g','n','e','d'),
            CT_QUAL_SHORT= kws('s','h','o','r','t'),
            CT_QUAL_LONG= kws('l','o','n','g'),
            CT_QUAL_COMPLEX= kws('c','o','m','p','l','e','x'),
            CT_QUAL_IMAGINARY= kws('i','m','a','g','i','n','a','r','y'),
        } quals[8];

        enum ct_decl_type_kind {
            CT_KIND_NOTAG= 0,
            CT_KIND_STRUCT= kws('s','t','r','u','c','t'),
            CT_KIND_UNION= kws('u','n','i','o','n'),
            CT_KIND_ENUM= kws('e','n','u','m'),
            CT_KIND_PTR= '*',
            CT_KIND_FUN= ('('&31)<<5 | (')'&31),
            CT_KIND_ARR= ('['&31)<<5 | (']'&31),
        } kind;

        union ct_decl_type_info {
            struct ct_declaration const* ptr;

            struct ct_decl_type_comp {
                size_t count; // -1 if no body
                struct ct_decl_type_field {
                    struct ct_declaration const* decl;
                    struct ct_expression* bitw; // NULL if not specified or irrelevant
                    struct ct_decl_type_field* next;
                }* first;
            } comp; // struct or union

            struct ct_decl_type_enu {
                size_t count; // 0 if no enumerator (because `enum e {}` is invalid anyways)
                struct ct_decl_type_enumer {
                    ct_bufsl const name;
                    struct ct_expression* expr; // NULL if not specified
                    struct ct_decl_type_enumer* next;
                }* first;
            } enu; // enum

            struct ct_decl_type_fun {
                struct ct_declaration const* ret;
                size_t count; // -1 when (), 0 when (void), n otherwise
                struct ct_decl_type_param {
                    struct ct_declaration const* decl; // last one NULL if variadic
                    struct ct_decl_type_param* next;
                }* first;
            } fun;

            struct ct_deck_type_arr {
                struct ct_declaration const* item;
                struct ct_expression* count; // NULL when [*] or [], n otherwise
                bool is_static;
            } arr;
        } info;

        ct_bufsl name;
    } type;

    ct_bufsl name;
} ct_declaration;
// }}}

typedef struct ct_parse_decl_state {
    ct_lex_state* ls;
    void* usr;
    void (*on)(void ref usr, ct_declaration cref decl, ct_bufsl ref tok);
    ct_bufsl tok;
    ct_declaration base;
} ct_parse_decl_state;

ct_bufsl ct_parse_declaration(ct_parse_decl_state ref ps, ct_bufsl tok);

// struct ct_expression {{{
typedef struct ct_expression {
    enum ct_expr_kind {
        CT_ATOM,

        CT_BINOP_SUBSCR, CT_BINOP_CALL,

        CT_BINOP_TERNCOND,
        CT_BINOP_TERNBRANCH,
        CT_BINOP_COMMA,

        CT_BINOP_ASGN,
        CT_BINOP_ASGN_BOR, CT_BINOP_ASGN_BXOR, CT_BINOP_ASGN_BAND,
        CT_BINOP_ASGN_BSHL, CT_BINOP_ASGN_BSHR,
        CT_BINOP_ASGN_SUB, CT_BINOP_ASGN_ADD,
        CT_BINOP_ASGN_REM, CT_BINOP_ASGN_DIV, CT_BINOP_ASGN_MUL,

        CT_BINOP_LOR, CT_BINOP_LAND,
        CT_BINOP_BOR, CT_BINOP_BXOR, CT_BINOP_BAND,
        CT_BINOP_EQ, CT_BINOP_NE,
        CT_BINOP_LT, CT_BINOP_GT, CT_BINOP_LE, CT_BINOP_GE,
        CT_BINOP_BSHL, CT_BINOP_BSHR,
        CT_BINOP_SUB, CT_BINOP_ADD,
        CT_BINOP_REM, CT_BINOP_DIV, CT_BINOP_MUL,

        //CT_UNOP_SIZEOF, CT_UNOP_ALIGNOF, //CT_BINOP_OFFSETOF,
        CT_UNOP_ADDR, CT_UNOP_DEREF,
        CT_UNOP_CAST,
        CT_UNOP_BNOT, CT_UNOP_LNOT,
        CT_UNOP_MINUS, CT_UNOP_PLUS,
        CT_UNOP_PRE_DEC, CT_UNOP_PRE_INC,

        //CT_UNOP_COMPLIT,
        CT_UNOP_PMEMBER, CT_UNOP_MEMBER,
        CT_UNOP_POST_DEC, CT_UNOP_POST_INC,
    } kind;

    union ct_expr_info {
        ct_bufsl atom;
        struct { struct ct_expression* opr; } unary;
        struct { struct ct_expression* lhs, * rhs; } binary;
        struct { struct ct_expression* base; struct ct_expr_call_arg { struct ct_expression* expr; struct ct_expr_call_arg* next; }* first; } call;
        struct { struct ct_expression* base, * off; } subscr;
        struct { struct ct_expression* opr; struct ct_decl_type const* type; } cast;
        struct { struct ct_expression* base; ct_bufsl* name; } member;
    } info;

    // reserved for user
    void* usr;
} ct_expression;
// }}}

typedef struct ct_parse_expr_state {
    ct_lex_state* ls;
    void* usr;
    void (*on)(void ref usr, ct_expression ref expr, ct_bufsl ref tok);
    ct_bufsl tok;
    // comma op not allowed in:
    // - declaration init (eg `int a = 42, b`)
    // - function args (eg `printf("d: %d", d)`)
    // - conditional alternative branch (eg `a ? b :3, d`)
    // - bitfield width (eg `struct { int a :3, b; }`)
    bool disallow_comma;
} ct_parse_expr_state;

ct_bufsl ct_parse_expression(ct_parse_expr_state ref ps, ct_bufsl const tok);

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
    if (!isidstart(*(_tok)->ptr) && (                                                         \
        report_lex_locate(ps->ls, "Expected identifier, got \"%.*s\"", bufmt(*_tok)), true))  \
        return

// parse ct_declaration {{{
struct _ct_parse_decl_capture;
typedef void _ct_parse_decl_closure_t(ct_parse_decl_state ref ps, struct _ct_parse_decl_capture ref capt, ct_declaration ref decl);
struct _ct_parse_decl_capture {
    ct_declaration* hold;
    struct _ct_parse_decl_capture ref next;
    _ct_parse_decl_closure_t ref then;
};
_ct_parse_decl_closure_t _ct_parse_decl_ator, _ct_parse_decl_close, _ct_parse_decl_fixup, _ct_parse_decl_post, _ct_parse_decl_params, _ct_parse_decl_enumer, _ct_parse_decl_fields, _ct_parse_decl_spec;

#define _linked_it_type_comp struct ct_decl_type_field
#define _linked_it_type_enu struct ct_decl_type_enumer
#define _linked_it_type_fun struct ct_decl_type_param
#define for_linked(__info, __ty) for (_linked_it_type_##__ty* curr = (__info).__ty.first; curr; curr = curr->next)

void _ct_parse_on_array_size(void ref decl_ps_capt[3], ct_expression ref expr, ct_bufsl ref tok)
{
    ct_declaration ref arr = decl_ps_capt[0]; ct_parse_decl_state ref ps = decl_ps_capt[1]; struct _ct_parse_decl_capture ref capt = decl_ps_capt[2];
    _expect1(tok);
    _expect(tok, "]");
    arr->type.info.arr.count = expr;
    ps->tok = ct_lext(ps->ls);
    _ct_parse_decl_post(ps, capt, arr);
}
void _ct_parse_on_enumer_value(void ref decl_ps_capt[3], ct_expression ref expr, ct_bufsl ref tok)
{
    ct_declaration ref enu = decl_ps_capt[0]; ct_parse_decl_state ref ps = decl_ps_capt[1]; struct _ct_parse_decl_capture ref capt = decl_ps_capt[2];
    _expect1(tok);
    for_linked (enu->type.info,enu) if (!curr->next) {
        curr->expr = expr;
        break;
    }
    if (',' == *tok->ptr) {
        ps->tok = ct_lext(ps->ls);
        _expect1(&ps->tok);
    } else ps->tok = *tok;
    if ('}' == *ps->tok.ptr) {
        ps->tok = ct_lext(ps->ls);
        _ct_parse_decl_ator(ps, capt, enu);
    } else _ct_parse_decl_enumer(ps, capt, NULL);
}
void _ct_parse_on_bitfield_width(void ref decl_ps_capt[4], ct_expression ref expr, ct_bufsl ref tok)
{
    ct_declaration ref comp = decl_ps_capt[0], ref base = decl_ps_capt[1]; ct_parse_decl_state ref ps = decl_ps_capt[2]; struct _ct_parse_decl_capture ref capt = decl_ps_capt[3];
    _expect1(tok);
    _expect(tok, ",", ";");
    for_linked (comp->type.info,comp) if (!curr->next) {
        curr->bitw = expr;
        break;
    }
    bool const reset = ';' == *tok->ptr;
    ps->tok = ct_lext(ps->ls);
    _expect1(&ps->tok);
    if (reset && '}' == *ps->tok.ptr) {
        ps->tok = ct_lext(ps->ls);
        _ct_parse_decl_ator(ps, capt, comp);
        return;
    }
    ct_declaration niwbase = reset ? (ct_declaration){0} : *base;
    _ct_parse_decl_spec(ps, &(struct _ct_parse_decl_capture){
            .next= capt,
            .then= _ct_parse_decl_fields,
        }, &niwbase);
}

#define kw(s) kws(s[0],s[1],s[2],)
#define iskwx(tok, ...) !dyarr_cmp(&((ct_bufsl){.ptr= (char[]){__VA_ARGS__}, .len= sizeof((char[]){__VA_ARGS__})}), &tok)
#define iskw(tok, askw, ...) (kws(__VA_ARGS__) == askw && iskwx(tok, __VA_ARGS__))

/// pre-ish declarator part with '('<decl>')' | '*'<decl>
void _ct_parse_decl_ator(ct_parse_decl_state ref ps, struct _ct_parse_decl_capture ref capt, ct_declaration ref decl)
{
    _expect1(&ps->tok);
    switch (*ps->tok.ptr) {
    case '(':
        ps->tok = ct_lext(ps->ls);
        ct_declaration before = *decl;
        _ct_parse_decl_ator(ps, &(struct _ct_parse_decl_capture){
                .next= &(struct _ct_parse_decl_capture){
                    .hold= &before,
                    .next= capt->next,
                    .then= capt->then,
                },
                .then= _ct_parse_decl_close,
            }, &before);
        return;

    case '*':
        ps->tok = ct_lext(ps->ls);
        ct_declaration ptr = {
            .spec= decl->spec,
            .is_inline= decl->is_inline, // yyy: y not
            .type= {.kind= CT_KIND_PTR, .info.ptr= decl},
            .name= decl->name,
        };
        decl->name.len = 0;
        for (unsigned askw; (3 < ps->tok.len && (askw = kw(ps->tok.ptr),
                    iskw(ps->tok, askw, 'c','o','n','s','t') ||
                    iskw(ps->tok, askw, 'r','e','s','t','r','i','c','t') ||
                    iskw(ps->tok, askw, 'v','o','l','a','t','i','l','e') ));
                ps->tok = ct_lext(ps->ls)) {
            for (unsigned k = 0; k < countof(ptr.type.quals); k++) if (CT_QUAL_END == ptr.type.quals[k]) {
                ptr.type.quals[k] = askw;
                break;
            }
        }
        _ct_parse_decl_ator(ps, capt, &ptr);
        return;

    case '=': case ',': case ';': case ')': case ':':
        capt->then(ps, capt->next, decl);
        return;
    }

    if (ps->tok.len && isidstart(*ps->tok.ptr)) {
        decl->name = ps->tok;
        ps->tok = ct_lext(ps->ls);
    }
    _ct_parse_decl_post(ps, capt, decl);
}

/// skip closing parenthesis and parse post
void _ct_parse_decl_close(ct_parse_decl_state ref ps, struct _ct_parse_decl_capture ref capt, ct_declaration ref decl)
{
    _expect1(&ps->tok);
    _expect(&ps->tok, ")");
    ps->tok = ct_lext(ps->ls);

    ct_declaration ref before = capt->hold;
    if (decl != before) {
        _ct_parse_decl_post(ps, &(struct _ct_parse_decl_capture){
                .next= &(struct _ct_parse_decl_capture){
                    .hold= (void*)(ct_declaration*[2]){before, decl}, // yyy: need both
                    .next= capt->next,
                    .then= capt->then,
                },
                .then= _ct_parse_decl_fixup,
            }, before);
        return;
    }
    // eg. `int (a)` would get there, because `decl == before == ptr to the "int" typed base`

    _ct_parse_decl_post(ps, capt, decl);
}

/// fixup after a parenthesised declarator like `(*a)`
void _ct_parse_decl_fixup(ct_parse_decl_state ref ps, struct _ct_parse_decl_capture ref capt, ct_declaration ref after)
{
    ct_declaration cref before = ((ct_declaration**)capt->hold)[0]; // yyy: see at call location as for what is all that
    ct_declaration* hold = ((ct_declaration**)capt->hold)[1];

    // 'visit' hold; until find before; replace with after
    ct_declaration** it = &hold;
    do switch ((*it)->type.kind) { // xxx: casts are to discard const qualifier
        case CT_KIND_PTR: it = (ct_declaration**)&(*it)->type.info.ptr;      break;
        case CT_KIND_FUN: it = (ct_declaration**)&(*it)->type.info.fun.ret;  break;
        case CT_KIND_ARR: it = (ct_declaration**)&(*it)->type.info.arr.item; break;
        default:;
    } while (before != *it);
    *it = after;

    capt->then(ps, capt->next, hold);
}

/// postfix declarator notations
void _ct_parse_decl_post(ct_parse_decl_state ref ps, struct _ct_parse_decl_capture ref capt, ct_declaration ref decl)
{
    // '(' <params> ')'
    // '[' [static][const][idk] <expr>|*|<nothing> ']'

    if (ps->tok.len) switch (*ps->tok.ptr) {
    case '(':
        ps->tok = ct_lext(ps->ls);
        ct_declaration fun = {
            .spec= decl->spec,
            .is_inline= decl->is_inline,
            .type= {.kind= CT_KIND_FUN, .info.fun.ret= decl},
            .name= decl->name,
        };
        decl->name.len = 0;

        if (bufis(ps->tok, "...")) {
            ps->tok = ct_lext(ps->ls);
            _expect1(&ps->tok);
            _expect(&ps->tok, ")");
            ps->tok = ct_lext(ps->ls);

            fun.type.info.fun.count = 1;
            fun.type.info.fun.first = &(struct ct_decl_type_param){0};

            _ct_parse_decl_post(ps, capt, &fun);
            return;
        }

        _ct_parse_decl_params(ps, &(struct _ct_parse_decl_capture){
                .hold= &fun,
                .next= capt->next,
                .then= capt->then,
            }, NULL);
        return;

    case '[':
        ps->tok = ct_lext(ps->ls);
        ct_declaration arr = {
            .spec= decl->spec,
            .is_inline= decl->is_inline, // yyy: y not
            .type= {.kind= CT_KIND_ARR, .info.arr.item= decl},
            .name= decl->name,
        };
        decl->name.len = 0;
        if (3 < ps->tok.len && iskwx(ps->tok, 's','t','a','t','i','c')) {
            arr.type.info.arr.is_static = true;
            ps->tok = ct_lext(ps->ls);
        }
        for (unsigned askw; (3 < ps->tok.len && (askw = kw(ps->tok.ptr),
                    iskw(ps->tok, askw, 'c','o','n','s','t') ||
                    iskw(ps->tok, askw, 'r','e','s','t','r','i','c','t') ||
                    iskw(ps->tok, askw, 'v','o','l','a','t','i','l','e') ));
                ps->tok = ct_lext(ps->ls)) {
            for (unsigned k = 0; k < countof(arr.type.quals); k++) if (CT_QUAL_END == arr.type.quals[k]) {
                arr.type.quals[k] = askw;
                break;
            }
        }

        if (ps->tok.len) switch (*ps->tok.ptr) {
        case '*':
            ps->tok = ct_lext(ps->ls);
            _expect1(&ps->tok);
            _expect(&ps->tok, "]");
            // fall through
        case ']':
            ps->tok = ct_lext(ps->ls);
            _ct_parse_decl_post(ps, capt, &arr);
            return;
        }

        ct_parse_expression(&(ct_parse_expr_state){
                .ls= ps->ls,
                .usr= (void*[3]){&arr, ps, capt},
                .on= (void(*)())_ct_parse_on_array_size,
            }, ps->tok);
        return;
    }

    capt->then(ps, capt->next, decl);
}

/// parse the params of a function
void _ct_parse_decl_params(ct_parse_decl_state ref ps, struct _ct_parse_decl_capture ref capt, ct_declaration ref decl)
{
    struct ct_decl_type_param node = {.decl= decl}; // here so it's not deallocated before the recursion
    ct_declaration ref fun = capt->hold;

    _expect1(&ps->tok);
    bool const last = ')' == *ps->tok.ptr;
    if (last || ',' == *ps->tok.ptr) {
        union ct_decl_type_info ref info = &fun->type.info;
        ps->tok = ct_lext(ps->ls);

        if (bufis(ps->tok, "...")) {
            ps->tok = ct_lext(ps->ls);
            _expect1(&ps->tok);
            _expect(&ps->tok, ")");
            ps->tok = ct_lext(ps->ls);

            info->fun.count++;
            if (!info->fun.first) info->fun.first = &node;
            else for_linked (*info,fun) if (!curr->next) {
                curr->next = &node;
                break;
            }

            info->fun.count++;
            node.next = &(struct ct_decl_type_param){0};

            _ct_parse_decl_post(ps, capt, fun);
            return;
        }

        if (!decl) {
            if (last) {
                info->fun.count = -1; // eg. `int a();`
                _ct_parse_decl_post(ps, capt, fun);
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
            _ct_parse_decl_post(ps, capt, fun);
            return;
        }

        _expect1(&ps->tok);
    }
    if (')' == *ps->tok.ptr) {
        report_lex_locate(ps->ls, "Expected parameter declaration, got \"%.*s\"", bufmt(ps->tok));
        return;
    }

    _ct_parse_decl_spec(ps, &(struct _ct_parse_decl_capture){
            .next= capt,
            .then= _ct_parse_decl_params,
        }, &(ct_declaration){0});
}

/// parse values of an enum
void _ct_parse_decl_enumer(ct_parse_decl_state ref ps, struct _ct_parse_decl_capture ref capt, ct_declaration ref _)
{
    (void)_;
    _expect1(&ps->tok);
    _expectid(&ps->tok);

    struct ct_decl_type_enumer node = {.name= ps->tok}; // here so it's not deallocated before the recursion
    ct_declaration ref enu = capt->hold;
    union ct_decl_type_info ref info = &enu->type.info;

    ps->tok = ct_lext(ps->ls);
    _expect1(&ps->tok);

    info->enu.count++;
    if (!info->enu.first) info->enu.first = &node;
    else for_linked (*info,enu) if (!curr->next) {
        curr->next = &node;
        break;
    }

    if ('=' == *ps->tok.ptr) {
        ps->tok = ct_lext(ps->ls);
        ct_parse_expression(&(ct_parse_expr_state){
                .ls= ps->ls,
                .usr= (void*[3]){enu, ps, capt},
                .on= (void(*)())_ct_parse_on_enumer_value,
                .disallow_comma= true,
            }, ps->tok);
        return;
    }

    if (',' == *ps->tok.ptr) {
        ps->tok = ct_lext(ps->ls);
        _expect1(&ps->tok);
    }
    if ('}' == *ps->tok.ptr) {
        ps->tok = ct_lext(ps->ls);
        _ct_parse_decl_ator(ps, capt, enu);
    } else _ct_parse_decl_enumer(ps, capt, NULL);
}

/// parse fields of a struct/union
void _ct_parse_decl_fields(ct_parse_decl_state ref ps, struct _ct_parse_decl_capture ref capt, ct_declaration ref decl)
{
    struct ct_decl_type_field node = {.decl= decl}; // here so it's not deallocated before the recursion
    ct_declaration ref comp = ((ct_declaration**)capt->hold)[0]; // yyy: see at call location as for what is all that
    ct_declaration* ref base = ((ct_declaration**)capt->hold)+1;

    _expect1(&ps->tok);
    bool const bitw = ':' == *ps->tok.ptr;
    bool const reset = ';' == *ps->tok.ptr;
    if (bitw || reset || ',' == *ps->tok.ptr) {
        union ct_decl_type_info ref info = &comp->type.info;
        ps->tok = ct_lext(ps->ls);
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
            ct_parse_expression(&(ct_parse_expr_state){
                    .ls= ps->ls,
                    .usr= (void*[4]){comp, *base, ps, capt},
                    .on= (void(*)())_ct_parse_on_bitfield_width,
                    .disallow_comma= true,
                }, ps->tok);
            return;
        }

        if ('}' == *ps->tok.ptr) {
            if (!reset) {
                _expect(&ps->tok, ";");
                return;
            }
            ps->tok = ct_lext(ps->ls);
            _ct_parse_decl_ator(ps, capt, comp);
            return;
        }
    }

    if ('}' == *ps->tok.ptr) {
        if (decl) {
            _expect(&ps->tok, ";");
            return;
        }
        ps->tok = ct_lext(ps->ls);
        _ct_parse_decl_ator(ps, capt, comp);
        return;
    }

    ct_declaration niwbase = reset ? (ct_declaration){0} : **base;
    //*base = &niwbase; // yyy: once thoroughly tested, this may be an avoidable capture-copying
    _ct_parse_decl_spec(ps, &(struct _ct_parse_decl_capture){
            //.next= capt,
            .next= &(struct _ct_parse_decl_capture){
                .hold= (void*)(ct_declaration*[2]){comp, &niwbase}, // yyy: whatever, see _ct_parse_decl_fields
                .next= capt->next,
                .then= capt->then,
            },
            .then= _ct_parse_decl_fields,
        }, &niwbase);
}

/// parse the specifier and initial qualifiers then one declarator
void _ct_parse_decl_spec(ct_parse_decl_state ref ps, struct _ct_parse_decl_capture ref capt, ct_declaration ref decl)
{
#   define case_iskw(...) if (0) case kws(__VA_ARGS__): if (!iskwx(ps->tok, __VA_ARGS__)) goto notkw;

    for (unsigned askw; ps->tok.len; ps->tok = ct_lext(ps->ls)) redo: switch (askw = ps->tok.len <3 ? 0 : kw(ps->tok.ptr)) {
    case 0: goto notkw; // here as to remove warning for the `if` right after

    case_iskw('t','y','p','e','d','e','f') case_iskw('e','x','t','e','r','n') case_iskw('s','t','a','t','i','c') case_iskw('a','u','t','o') case_iskw('r','e','g','i','s','t','e','r')
        decl->spec = askw;
        continue;

    case_iskw('s','i','g','n','e','d') case_iskw('u','n','s','i','g','n','e','d') case_iskw('s','h','o','r','t') case_iskw('l','o','n','g')
        if (!decl->type.name.len) decl->type.name = (ct_bufsl){.ptr= "int", .len= 3};
    case_iskw('c','o','n','s','t') case_iskw('r','e','s','t','r','i','c','t') case_iskw('v','o','l','a','t','i','l','e')
    case_iskw('c','o','m','p','l','e','x') case_iskw('i','m','a','g','i','n','a','r','y')
        for (unsigned k = 0; k < countof(decl->type.quals); k++) if (CT_QUAL_END == decl->type.quals[k]) {
            decl->type.quals[k] = askw;
            break;
        }
        continue;
    case_iskw('i','n','l','i','n','e')
        decl->is_inline = true;
        continue;

    case_iskw('s','t','r','u','c','t') case_iskw('u','n','i','o','n') case_iskw('e','n','u','m')
        decl->type.kind = askw;
        bool const e = CT_KIND_ENUM == askw;
        ps->tok = ct_lext(ps->ls);
        if (ps->tok.len && isidstart(*ps->tok.ptr)) {
            decl->type.name = ps->tok;
            ps->tok = ct_lext(ps->ls);
        }
        if (ps->tok.len && '{' == *ps->tok.ptr) {
            ps->tok = ct_lext(ps->ls);
            if (e) _ct_parse_decl_enumer(ps, &(struct _ct_parse_decl_capture){
                    .hold= decl,
                    .next= capt->next,
                    .then= capt->then,
                }, NULL);
            else _ct_parse_decl_fields(ps, &(struct _ct_parse_decl_capture){
                    .hold= (void*)(ct_declaration*[2]){decl, &(ct_declaration){0}}, // yyy: whatever, see _ct_parse_decl_fields
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

        if (isidstart(*ps->tok.ptr)) {
            if (!decl->type.name.len ||
                    (bufis(decl->type.name, "int") &&
                     ( bufis(ps->tok, "int")    ||
                       bufis(ps->tok, "char")   || // signed/unsigned char
                       bufis(ps->tok, "double") )) // long double
               ) {
                decl->type.name = ps->tok;
                continue;
            }
            ct_declaration cpy = *decl;
            _ct_parse_decl_ator(ps, capt, &cpy);
            return;
        }

        switch (*ps->tok.ptr) {
        case '=': case ',': case ';': case ')': case ':': // shortcut one stack frame
            capt->then(ps, capt->next, decl);
            return;
        case '*': case '(':;
            ct_declaration cpy = *decl;
            _ct_parse_decl_ator(ps, capt, &cpy);
            return;
        case '[':
            _ct_parse_decl_post(ps, capt, decl);
            return;
        }

        report_lex_locate(ps->ls, "Expected declarator, got \"%.*s\"", bufmt(ps->tok));
        return;
    } // for-switch tok

#   undef case_iskw

    capt->then(ps, capt->next, decl);
}

/// callback chain tail end which call user code
void _ct_parse_decl_exit(ct_parse_decl_state ref ps, struct _ct_parse_decl_capture ref _, ct_declaration ref decl)
{
    (void)_;
    if (ps->on) ps->on(ps->usr, decl, &ps->tok);
}

ct_bufsl ct_parse_declaration(ct_parse_decl_state ref ps, ct_bufsl tok)
{
    ps->tok = tok;
    _ct_parse_decl_spec(ps, &(struct _ct_parse_decl_capture){.then= _ct_parse_decl_exit}, &ps->base);
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

// parse ct_expression {{{
struct _ct_parse_expr_capture;
typedef void _ct_parse_expr_closure_t(ct_parse_expr_state ref ps, struct _ct_parse_expr_capture ref capt, ct_expression ref expr);
struct _ct_parse_expr_capture {
    ct_expression* hold;
    struct _ct_parse_expr_capture ref next;
    _ct_parse_expr_closure_t ref then; // its `hold` is in `next->hold`
};
_ct_parse_expr_closure_t _ct_parse_expr_one, _ct_parse_expr_one_post, _ct_parse_expr_finish_cast, _ct_parse_expr_one_lext_parenth, _ct_parse_expr_one_lext_oneafter, _ct_parse_expr_fun_args, _ct_parse_expr_one_after, _ct_parse_expr_tern_cond, _ct_parse_expr_tern_branch, _ct_parse_expr_two, _ct_parse_expr_two_after, _ct_parse_expr_entry, _ct_parse_expr_continue, _ct_parse_expr_exit;

void _ct_parse_on_cast_type(void ref capt_ps[2], ct_declaration cref decl, ct_bufsl ref tok)
{
    struct _ct_parse_expr_capture ref capt = capt_ps[0]; ct_parse_expr_state ref ps = capt_ps[1];
    _expect1(tok);
    _expect(tok, ")");

    ps->tok = ct_lext(ps->ls);
    _expect1(&ps->tok);

    if ('{' == *ps->tok.ptr) {
        notif("NIY: compound literal");
        //ct_expression comp = {.kind= CT_UNOP_COMPLIT, .info.comp.type= &decl->type};
        *tok = ps->tok;
        return;
    }

    ct_expression cast = {.kind= CT_UNOP_CAST, .info.cast.type= &decl->type};
    capt->hold = &cast;
    _ct_parse_expr_one(ps, &(struct _ct_parse_expr_capture){
            .next= capt,
            .then= _ct_parse_expr_finish_cast,
        }, NULL);
}

enum ct_expr_kind _ct_parse_is_postfix(ct_bufsl const tok)
{
    if (2 == tok.len) switch (tok.ptr[0]<<8 | tok.ptr[1]) {
    case '-'<<8 | '-': return CT_UNOP_POST_DEC;
    case '+'<<8 | '+': return CT_UNOP_POST_INC;
    }
    return 0;
}
enum ct_expr_kind _ct_parse_is_prefix(ct_bufsl const tok)
{
    if (1 == tok.len) switch (tok.ptr[0]) {
    case '&': return CT_UNOP_ADDR;
    case '*': return CT_UNOP_DEREF;
    case '~': return CT_UNOP_BNOT;
    case '!': return CT_UNOP_LNOT;
    case '-': return CT_UNOP_MINUS;
    case '+': return CT_UNOP_PLUS;
    }
    if (2 == tok.len) switch (tok.ptr[0]<<8 | tok.ptr[1]) {
    case '-'<<8 | '-': return CT_UNOP_PRE_DEC;
    case '+'<<8 | '+': return CT_UNOP_PRE_INC;
    }
    return 0;
}
enum ct_expr_kind _ct_parse_is_infix(ct_bufsl const tok, bool const disallow_comma)
{
    if (1 == tok.len) switch (tok.ptr[0]) {
    case '?': return CT_BINOP_TERNCOND;
    //case ':': return CT_BINOP_TERNBRANCH;
    case ',': return disallow_comma ? 0 : CT_BINOP_COMMA;
    case '=': return CT_BINOP_ASGN;
    case '|': return CT_BINOP_BOR;
    case '^': return CT_BINOP_BXOR;
    case '&': return CT_BINOP_BAND;
    case '-': return CT_BINOP_SUB;
    case '+': return CT_BINOP_ADD;
    case '%': return CT_BINOP_REM;
    case '/': return CT_BINOP_DIV;
    case '*': return CT_BINOP_MUL;
    case '<': return CT_BINOP_LT;
    case '>': return CT_BINOP_GT;
    }
    if (2 == tok.len) switch (tok.ptr[0]<<8 | tok.ptr[1]) {
    case '|'<<8 | '=': return CT_BINOP_ASGN_BOR;
    case '^'<<8 | '=': return CT_BINOP_ASGN_BXOR;
    case '&'<<8 | '=': return CT_BINOP_ASGN_BAND;
    case '-'<<8 | '=': return CT_BINOP_ASGN_SUB;
    case '+'<<8 | '=': return CT_BINOP_ASGN_ADD;
    case '%'<<8 | '=': return CT_BINOP_ASGN_REM;
    case '/'<<8 | '=': return CT_BINOP_ASGN_DIV;
    case '*'<<8 | '=': return CT_BINOP_ASGN_MUL;
    case '|'<<8 | '|': return CT_BINOP_LOR;
    case '&'<<8 | '&': return CT_BINOP_LAND;
    case '='<<8 | '=': return CT_BINOP_EQ;
    case '!'<<8 | '=': return CT_BINOP_NE;
    case '<'<<8 | '=': return CT_BINOP_LE;
    case '>'<<8 | '=': return CT_BINOP_GE;
    case '<'<<8 | '<': return CT_BINOP_BSHL;
    case '>'<<8 | '>': return CT_BINOP_BSHR;
    }
    if (3 == tok.len && tok.ptr[0] == tok.ptr[1] && '=' == tok.ptr[2]) switch (tok.ptr[0]) {
    case '<': return CT_BINOP_ASGN_BSHL;
    case '>': return CT_BINOP_ASGN_BSHR;
    }
    return 0;
}

/// parse one, including the prefix: [<prefix>] (<atom> | '('<expr>')') [<postfix>]
void _ct_parse_expr_one(ct_parse_expr_state ref ps, struct _ct_parse_expr_capture ref capt, ct_expression ref is_in_par)
{
    _expect1(&ps->tok);

    enum ct_expr_kind const prefix = _ct_parse_is_prefix(ps->tok);
    if (prefix) {
        ps->tok = ct_lext(ps->ls);
        ct_expression pre = {.kind= prefix};
        _ct_parse_expr_one(ps, &(struct _ct_parse_expr_capture){
                .next= &(struct _ct_parse_expr_capture){
                    .hold= &pre,
                    .next= capt->next,
                    .then= capt->then,
                },
                .then= _ct_parse_expr_one_post,
            }, NULL);
        return;
    }

#   define firstcharid(_tok) (('a' <= (*(_tok).ptr|32) && (*(_tok).ptr|32) <= 'z') || '_' == *(_tok).ptr)
#   define firstcharlit(_tok) (('0' <= *(_tok).ptr && *(_tok).ptr <= '9') || '.' == *(_tok).ptr || '"' == *(_tok).ptr || '\'' == *(_tok.ptr))

    if ('(' == *ps->tok.ptr) {
        ps->tok = ct_lext(ps->ls);
        _expect1(&ps->tok);

        if (firstcharid(ps->tok) && (
                bufis(ps->tok, "char")     ||
                bufis(ps->tok, "short")    ||
                bufis(ps->tok, "int")      ||
                bufis(ps->tok, "long")     ||
                bufis(ps->tok, "signed")   ||
                bufis(ps->tok, "unsigned") ||
                bufis(ps->tok, "float")    ||
                bufis(ps->tok, "double")   ||
                bufis(ps->tok, "void")     ||
                bufis(ps->tok, "struct")   ||
                bufis(ps->tok, "union")    ||
                bufis(ps->tok, "enum")     ||
                bufis(ps->tok, "typedef")  ||
                bufis(ps->tok, "const")    )) {
            ct_parse_declaration(&(ct_parse_decl_state){
                    .ls= ps->ls,
                    .usr= (void*[2]){capt, ps},
                    .on= (void(*)())_ct_parse_on_cast_type,
                }, ps->tok);
            return;
        }

        // yyy: any non null if disallow comma was set
        capt->hold = ps->disallow_comma ? (void*)"" : NULL; // (yyy: avoids capture copy?)
        ps->disallow_comma = false;

        _ct_parse_expr_one(ps, &(struct _ct_parse_expr_capture){
                .next= &(struct _ct_parse_expr_capture){
                    .next= capt,
                    .then= _ct_parse_expr_one_lext_parenth,
                },
                .then= _ct_parse_expr_continue,
            }, (void*)"");
        return;
    }

    // xxx: condition is pretty weak.. this syntax is only valid after an '='
    // moved back into ct_declaration to handle because it's not quite an ct_expression,
    // but the parsing will still be the same so idk how it'll be done
    //if ('{' == *ps->tok.ptr) {
    //    // TODO/XXX
    //    //notif("NIY: declaration compound literal (skipping for now)");
    //    for (unsigned depth = 0; (ps->tok = ct_lext(ps->ls)).len; ) {
    //        bool c = '}' == *ps->tok.ptr;
    //        if (!ps->tok.len || (!depth && c)) break;
    //        depth+= ('{' == *ps->tok.ptr)-c;
    //    }
    //    ps->tok = ct_lext(ps->ls);
    //    return;
    //}

    // TODO: join adjacent string literals (would like to say this should be done in the lexer tho-)
    ct_expression atom = {.kind= CT_ATOM, .info.atom= ps->tok};
    ps->tok = ct_lext(ps->ls);

    // yyy: any non null if comming from right above (ie is in a parenthesised thing)
    if (is_in_par && ps->tok.len && firstcharid(atom.info.atom)) {
        // could still be a cast here if one of:
        // - tok is an id (2 idends in a row)
        // - tok is ')' and either:
        //   - next is a '{'
        //   - next is a _expr_one (lit, ident, unop, '(')
        // - tok is '*' and either:
        //   - next is a ')'
        //   - next is a '*'
        //   - next is a '['
        //   - next is a qual (in `const`, `restrict`, `volatile`)
        // - tok is '[' and _looking ahead untile matching ']'_ and then ')' and then next is a '{'

        if (firstcharid(ps->tok)) {
            // (size_t const
            //             ^
            // XXX: lexer_recycle
            ps->ls->slice.ptr-= ps->tok.len, ps->ls->slice.len+= ps->tok.len;
            ct_parse_declaration(&(ct_parse_decl_state){
                    .ls= ps->ls,
                    .usr= (void*[2]){capt->next->next, ps},
                    .on= (void(*)())_ct_parse_on_cast_type,
                }, atom.info.atom);
            return;
        }

        ct_bufsl const ahead = ct_lext(ps->ls);
        // XXX: lexer_recycle
        ps->ls->slice.ptr-= ahead.len, ps->ls->slice.len+= ahead.len;
        if (ahead.len) switch (*ps->tok.ptr) {
        case ')':
            // (size_t) ...
            //          ^
            if (strchr("{(~!-+", *ahead.ptr) || firstcharlit(ahead) || firstcharid(ahead)) {
                _ct_parse_on_cast_type(
                        (void*[2]){capt->next->next, ps},
                        &(ct_declaration){.type.name= atom.info.atom},
                        &ps->tok
                    );
                return;
            }
            break;

        case '*':
            // (size_t* ...
            //          ^
            if (strchr(")*[", *ahead.ptr) || (firstcharid(ahead) && (
                    bufis(ahead, "const")    ||
                    bufis(ahead, "restrict") ||
                    bufis(ahead, "volatile") )) ) {
                ct_parse_declaration(&(ct_parse_decl_state){
                        .ls= ps->ls,
                        .usr= (void*[2]){capt->next->next, ps},
                        .on= (void(*)())_ct_parse_on_cast_type,
                        .base.type.name= atom.info.atom,
                    }, ps->tok);
                return;
            }
            break;

        case '[':
            // (size_t[ ...
            //          ^
            notif("NIY: (maybe) compound literal for array (for now parsed as expression)");
            //return;
        }
    }

#   undef firstcharlit
#   undef firstcharid

    _ct_parse_expr_one_after(ps, capt, &atom);
}

/// exactly same as _ct_parse_expr_one_post but for '('<type>')' <expr>
void _ct_parse_expr_finish_cast(ct_parse_expr_state ref ps, struct _ct_parse_expr_capture ref capt, ct_expression ref expr)
{
    capt->hold->info.cast.opr = expr;
    capt->then(ps, capt->next, capt->hold);
}

/// sets the operand (expr) of the prefix op in: <prefix> <expr> [<postfix>]
void _ct_parse_expr_one_post(ct_parse_expr_state ref ps, struct _ct_parse_expr_capture ref capt, ct_expression ref expr)
{
    capt->hold->info.unary.opr = expr;
    capt->then(ps, capt->next, capt->hold);
}

/// skip a closing parenthesis in: '('<expr>')' [<postfix>]
void _ct_parse_expr_one_lext_parenth(ct_parse_expr_state ref ps, struct _ct_parse_expr_capture ref capt, ct_expression ref expr)
{
    _expect1(&ps->tok);
    _expect(&ps->tok, ")");
    // yyy: any non null if disallow comma was set
    ps->disallow_comma = !!capt->hold;
    ps->tok = ct_lext(ps->ls);
    //capt->then(ps, capt->next, capt->hold); // yyy: extraneous call?
    _ct_parse_expr_one_after(ps, capt, expr);
}

/// skip a closing bracket and set the offset ("within"): <expr> '['<off>']' [<postfix>]
void _ct_parse_expr_one_lext_oneafter(ct_parse_expr_state ref ps, struct _ct_parse_expr_capture ref capt, ct_expression ref within)
{
    _expect1(&ps->tok);
    _expect(&ps->tok, "]");
    capt->hold->info.subscr.off = within;
    // yyy: any non null if disallow comma was set
    ps->disallow_comma = !!capt->hold->usr;
    capt->hold->usr = NULL;
    ps->tok = ct_lext(ps->ls);
    //capt->then(ps, capt->next, capt->hold); // yyy: extraneous call?
    _ct_parse_expr_one_after(ps, capt, capt->hold);
}

/// parse the arguments of a function call: <expr> '('<args>','..')' [<postfix>]
void _ct_parse_expr_fun_args(ct_parse_expr_state ref ps, struct _ct_parse_expr_capture ref capt, ct_expression ref expr)
{
    _expect1(&ps->tok);
    _expect(&ps->tok, ",", ")");

    ct_expression ref callbase = capt->hold;
    struct ct_expr_call_arg* it = callbase->info.call.first;
    while (it->next) it = it->next;
    it->expr = expr;

    if (')' == *ps->tok.ptr) {
        // yyy: any non null if disallow comma was set
        ps->disallow_comma = !!callbase->usr;
        callbase->usr = NULL;
        ps->tok = ct_lext(ps->ls);
        //capt->then(ps, capt->next, callbase); // yyy: extraneous call?
        _ct_parse_expr_one_after(ps, capt, callbase);
        return;
    }

    it->next = &(struct ct_expr_call_arg){0};
    ps->tok = ct_lext(ps->ls);
    _ct_parse_expr_one(ps, &(struct _ct_parse_expr_capture){
            .next= &(struct _ct_parse_expr_capture){
                .next= capt,
                .then= _ct_parse_expr_fun_args,
            },
            .then= _ct_parse_expr_continue,
        }, NULL);
}

/// parse postfix part after expr: <expr> (<postfix> | '('<arg>')' | '['<off>']' | ('.'|'->')<name>)
void _ct_parse_expr_one_after(ct_parse_expr_state ref ps, struct _ct_parse_expr_capture ref capt, ct_expression ref expr)
{
    enum ct_expr_kind const postfix = _ct_parse_is_postfix(ps->tok);
    if (postfix) {
        ps->tok = ct_lext(ps->ls);
        ct_expression post = {.kind= postfix, .info.unary.opr= expr};
        _ct_parse_expr_one_after(ps, capt, &post);
        return;
    }

    bool pmem = false;
    if (ps->tok.len) switch (*ps->tok.ptr) {
    case '(':
        ps->tok = ct_lext(ps->ls);
        if (ps->tok.len && ')' == *ps->tok.ptr) {
            ps->tok = ct_lext(ps->ls);
            ct_expression access = {.kind= CT_BINOP_CALL, .info.call.base= expr};
            _ct_parse_expr_one_after(ps, capt, &access);
            return;
        }
        ct_expression callbase = {
            .kind= CT_BINOP_CALL,
            .info.call= {
                .base= expr,
                .first= &(struct ct_expr_call_arg){0},
            },
            // yyy: any non null if disallow comma was set
            .usr= ps->disallow_comma ? (void*)"" : NULL,
        };
        ps->disallow_comma = true;
        capt->hold = &callbase; // (yyy: avoids capture copy?)
        _ct_parse_expr_one(ps, &(struct _ct_parse_expr_capture){
                .next= &(struct _ct_parse_expr_capture){
                    .next= capt,
                    .then= _ct_parse_expr_fun_args,
                },
                .then= _ct_parse_expr_continue,
            }, NULL);
        return;

    case '[':
        ps->tok = ct_lext(ps->ls);
        ct_expression whole = {
            .kind= CT_BINOP_SUBSCR,
            .info.subscr.base= expr,
            // yyy: any non null if disallow comma was set
            .usr= ps->disallow_comma ? (void*)"" : NULL,
        };
        ps->disallow_comma = false;
        capt->hold = &whole; // (yyy: avoids capture copy?)
        _ct_parse_expr_one(ps, &(struct _ct_parse_expr_capture){
                .next= &(struct _ct_parse_expr_capture){
                    .next= capt,
                    .then= _ct_parse_expr_one_lext_oneafter,
                },
                .then= _ct_parse_expr_continue,
            }, NULL);
        return;

    case '-':
        if (2 != ps->tok.len || '>' != ps->tok.ptr[1]) break;
        pmem = true;
        if (0) // fall through
    case '.':
            if (1 != ps->tok.len) break;
        ct_bufsl name = ct_lext(ps->ls);
        _expect1(&name);
        _expectid(&name);
        ct_expression access = {
            .kind= pmem ? CT_UNOP_PMEMBER : CT_UNOP_MEMBER,
            .info.member= {.base= expr, .name= &name},
        };
        ps->tok = ct_lext(ps->ls);
        _ct_parse_expr_one_after(ps, capt, &access);
        return;
    }

    capt->then(ps, capt->next, expr);
}

/// lands there after the first branch of the ternary, so on the ':', no comma op in the third operand
void _ct_parse_expr_tern_cond(ct_parse_expr_state ref ps, struct _ct_parse_expr_capture ref capt, ct_expression ref consequence)
{
    _expect1(&ps->tok);
    _expect(&ps->tok, ":");
    ct_expression ref condition_root = capt->hold;
    ct_expression branches = {.kind= CT_BINOP_TERNBRANCH, .info.binary.lhs= consequence};
    condition_root->info.binary.rhs = &branches;

    ps->disallow_comma = true;
    ps->tok = ct_lext(ps->ls);
    // <cond_root.lhs> '?' <conseq> ':' <altern>
    //                                  ^
    _ct_parse_expr_one(ps, &(struct _ct_parse_expr_capture){
            .next= &(struct _ct_parse_expr_capture){
                .next= capt,
                .then= _ct_parse_expr_tern_branch,
            },
            .then= _ct_parse_expr_continue,
        }, NULL);
}

/// after the third operand; finishs the whole ternary and restores the disallow comma state
void _ct_parse_expr_tern_branch(ct_parse_expr_state ref ps, struct _ct_parse_expr_capture ref capt, ct_expression ref alternative)
{
    ct_expression ref condition_root = capt->hold;
    condition_root->info.binary.rhs->info.binary.rhs = alternative;
    // yyy: any non null if disallow comma was set
    ps->disallow_comma = !!condition_root->usr;
    condition_root->usr = NULL;

    if (!ps->disallow_comma && 1 == ps->tok.len && ',' == *ps->tok.ptr) {
        ps->tok = ct_lext(ps->ls);
        ct_expression in = {.kind= CT_BINOP_COMMA, .info.binary.lhs= condition_root};
        _ct_parse_expr_one(ps, &(struct _ct_parse_expr_capture){
                .next= &(struct _ct_parse_expr_capture){
                    .next= &(struct _ct_parse_expr_capture){
                        .hold= &in,
                        .next= capt->next,
                        .then= capt->then,
                    },
                    .then= _ct_parse_expr_two_after,
                },
                .then= _ct_parse_expr_continue,
            }, NULL);
        return;
    }

    capt->then(ps, capt->next, condition_root);
}

/// parse two with lhs, lop and rhs known: <lhs> <lop> <rhs> [<nop>]
void _ct_parse_expr_two(ct_parse_expr_state ref ps, struct _ct_parse_expr_capture ref capt, ct_expression ref rhs)
{
    enum ct_expr_kind const infix = _ct_parse_is_infix(ps->tok, ps->disallow_comma);
    if (!infix) {
        capt->hold->info.binary.rhs = rhs;
        capt->then(ps, capt->next, capt->hold);
        return;
    }
    ps->tok = ct_lext(ps->ls);

    if (CT_BINOP_TERNCOND == infix) {
        capt->hold->info.binary.rhs = rhs;
        ct_expression in = {.kind= infix, .info.binary.lhs= capt->hold};
        // yyy: any non null if disallow comma was set
        in.usr = ps->disallow_comma ? (void*)"" : NULL;
        ps->disallow_comma = false;
        // <in> '?' <..> ':' <..>
        //          ^
        _ct_parse_expr_one(ps, &(struct _ct_parse_expr_capture){
                .next= &(struct _ct_parse_expr_capture){
                    .next= &(struct _ct_parse_expr_capture){
                        .hold= &in,
                        .next= capt->next,
                        .then= capt->then,
                    },
                    .then= _ct_parse_expr_tern_cond,
                },
                .then= _ct_parse_expr_continue,
            }, NULL);
        return;
    }

    if (CT_BINOP_COMMA == infix) {
        capt->hold->info.binary.rhs = rhs;
        ct_expression in = {.kind= infix, .info.binary.lhs= capt->hold};
        _ct_parse_expr_one(ps, &(struct _ct_parse_expr_capture){
                .next= &(struct _ct_parse_expr_capture){
                    .next= &(struct _ct_parse_expr_capture){
                        .hold= &in,
                        .next= capt->next,
                        .then= capt->then,
                    },
                    .then= _ct_parse_expr_two_after
                },
                .then= _ct_parse_expr_continue,
            }, NULL);
        return;
    }

    enum ct_expr_kind const l = capt->hold->kind, n = infix;
    if (l < n || ( (CT_BINOP_ASGN <= l && l <= CT_BINOP_ASGN_MUL)
                && (CT_BINOP_ASGN <= n && n <= CT_BINOP_ASGN_MUL) )) {
        ct_expression in = {.kind= infix, .info.binary.lhs= rhs};
        _ct_parse_expr_one(ps, &(struct _ct_parse_expr_capture){
                .next= &(struct _ct_parse_expr_capture){
                    .hold= &in,
                    .next= &(struct _ct_parse_expr_capture){
                        .hold= capt->hold,
                        .next= capt->next,
                        .then= capt->then,
                    },
                    .then= _ct_parse_expr_two_after,
                },
                .then= _ct_parse_expr_two,
            }, NULL);
    } else {
        capt->hold->info.binary.rhs = rhs;
        ct_expression in = {.kind= infix, .info.binary.lhs= capt->hold};
        _ct_parse_expr_one(ps, &(struct _ct_parse_expr_capture){
                .next= &(struct _ct_parse_expr_capture){
                    .hold= &in,
                    .next= capt->next,
                    .then= capt->then,
                },
                .then= _ct_parse_expr_two,
            }, NULL);
    }
}

/// handle the case in `_ct_parse_expr_two` where nop comes before lop in precedence (it's executed when "bubbling" back toward `_ct_parse_expr_exit`)
void _ct_parse_expr_two_after(ct_parse_expr_state ref ps, struct _ct_parse_expr_capture ref capt, ct_expression ref rhs)
{
    capt->hold->info.binary.rhs = rhs;
    capt->then(ps, capt->next, capt->hold);
}

/// proper start the `_ct_parse_expr_two` loop, sets up `_ct_parse_expr_exit` at callback chain end
void _ct_parse_expr_entry(ct_parse_expr_state ref ps, struct _ct_parse_expr_capture ref _, ct_expression ref lhs)
{
    (void)_;
    enum ct_expr_kind const infix = _ct_parse_is_infix(ps->tok, ps->disallow_comma);
    if (infix) {
        ps->tok = ct_lext(ps->ls);
        ct_expression in = {.kind= infix, .info.binary.lhs= lhs};

        if (CT_BINOP_TERNCOND == infix) {
            // yyy: any non null if disallow comma was set
            in.usr = ps->disallow_comma ? (void*)"" : NULL;
            ps->disallow_comma = false;
            // <lhs> '?' <..> ':' <..>
            //           ^
            _ct_parse_expr_one(ps, &(struct _ct_parse_expr_capture){
                    .next= &(struct _ct_parse_expr_capture){
                        .next= &(struct _ct_parse_expr_capture){
                            .hold= &in,
                            .then= _ct_parse_expr_exit,
                        },
                        .then= _ct_parse_expr_tern_cond,
                    },
                    .then= _ct_parse_expr_continue,
                }, NULL);
            return;
        }

        if (CT_BINOP_COMMA == infix) {
            _ct_parse_expr_one(ps, &(struct _ct_parse_expr_capture){
                    .next= &(struct _ct_parse_expr_capture){
                        .next= &(struct _ct_parse_expr_capture){
                            .hold= &in,
                            .then= _ct_parse_expr_exit,
                        },
                        .then= _ct_parse_expr_two_after,
                    },
                    .then= _ct_parse_expr_continue,
                }, NULL);
            return;
        }

        _ct_parse_expr_one(ps, &(struct _ct_parse_expr_capture){
                .next= &(struct _ct_parse_expr_capture){
                    .hold= &in,
                    .then= _ct_parse_expr_exit,
                },
                .then= _ct_parse_expr_two,
            }, NULL);
        return;
    }

    _ct_parse_expr_exit(ps, NULL, lhs);
}

/// similar to `_ct_parse_expr_entry` for sub expr in: '('<expr>')' and '?'<expr>':' and ','<expr> or arg in: <expr> ('('<arg>')' | '['<arg>'])
void _ct_parse_expr_continue(ct_parse_expr_state ref ps, struct _ct_parse_expr_capture ref capt, ct_expression ref lhs)
{
    enum ct_expr_kind const infix = _ct_parse_is_infix(ps->tok, ps->disallow_comma);
    if (infix) {
        ps->tok = ct_lext(ps->ls);
        ct_expression in = {.kind= infix, .info.binary.lhs= lhs};

        if (CT_BINOP_TERNCOND == infix) {
            // yyy: any non null if disallow comma was set
            in.usr = ps->disallow_comma ? (void*)"" : NULL;
            ps->disallow_comma = false;
            // <lhs> '?' <..> ':' <..>
            //           ^
            _ct_parse_expr_one(ps, &(struct _ct_parse_expr_capture){
                    .next= &(struct _ct_parse_expr_capture){
                        .next= &(struct _ct_parse_expr_capture){
                            .hold= &in,
                            .next= capt->next,
                            .then= capt->then,
                        },
                        .then= _ct_parse_expr_tern_cond,
                    },
                    .then= _ct_parse_expr_continue,
                }, NULL);
            return;
        }

        if (CT_BINOP_COMMA == infix) {
            _ct_parse_expr_one(ps, &(struct _ct_parse_expr_capture){
                    .next= &(struct _ct_parse_expr_capture){
                        .next= &(struct _ct_parse_expr_capture){
                            .hold= &in,
                            .next= capt->next,
                            .then= capt->then,
                        },
                        .then= _ct_parse_expr_two_after,
                    },
                    .then= _ct_parse_expr_continue,
                }, NULL);
            return;
        }

        capt->hold = &in; // yyy: modifies capture
        _ct_parse_expr_one(ps, &(struct _ct_parse_expr_capture){
                .next= capt,
                .then= _ct_parse_expr_two,
            }, NULL);
        return;
    }

    capt->then(ps, capt->next, lhs);
}

/// callback chain tail end which call user code
void _ct_parse_expr_exit(ct_parse_expr_state ref ps, struct _ct_parse_expr_capture ref _, ct_expression ref expr)
{
    (void)_;
    if (ps->on) ps->on(ps->usr, expr, &ps->tok);
}

ct_bufsl ct_parse_expression(ct_parse_expr_state ref ps, ct_bufsl tok)
{
    ps->tok = tok;
    _ct_parse_expr_one(ps, &(struct _ct_parse_expr_capture){.then= _ct_parse_expr_entry}, NULL);
    return ps->tok;
}
// }}}

#undef _expect
#undef _expect1

#endif // CINTRE_PARSER_H
