/// C parser on top of the lexer; examples:
/// ```c
/// lex_state ls = ...;
/// my_state_t my_state = ...;
///
/// void accept_decl(my_state_t* me, declaration const* decl, tokt* tok) {
///     // note: `int a, b;` has 2 declarations with the same "base"
///     if (',' == *tokn(after)) *tok = parse_declaration_continue(&decl_ps, lext(&ls), decl_ps.base);
///     else if (';' == *tokn(after)) *tok = lext(&ls), decl_ps.base = NULL;
/// }
/// void accept_expr(my_state_t* me, expression* expr, tokt* tok) {
///     ...
/// }
///
/// {
///     parse_decl_state decl_ps = {.ls= &ls, .usr= &my_state, .on= accept_decl};
///     tokt after = parse_declaration(&decl_ps, lext(&ls));
/// }
///
/// {
///     parse_expr_state expr_ps = {.ls= &ls, .usr= &my_state, .on= accept_expr};
///     tokt after = parse_expression(&expr_ps, lext(&ls));
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
// (especially to _parse_expr_one_after)

#ifndef CINTRE_PARSER_H
#define CINTRE_PARSER_H

#include "common.h"
#include "lexer.h"

// struct declaration {{{
typedef struct declaration {
#define kws(a,b,c,...) (((a)&31)<<10 | ((b)&31)<<5 | ((c)&31))

    enum decl_spec {
        DECL_SPEC_NONE= 0,
        DECL_SPEC_TYPEDEF= kws('t','y','p','e','d','e','f'),
        DECL_SPEC_EXTERN= kws('e','x','t','e','r','n'),
        DECL_SPEC_STATIC= kws('s','t','a','t','i','c'),
        DECL_SPEC_AUTO= kws('a','u','t','o'),
        DECL_SPEC_REGISTER= kws('r','e','g','i','s','t','e','r'),
    } spec;

    bool is_inline;

    struct decl_type {
        enum decl_type_qual {
            DECL_QUAL_END= 0,
            DECL_QUAL_CONST= kws('c','o','n','s','t'),
            DECL_QUAL_RESTRICT= kws('r','e','s','t','r','i','c','t'),
            DECL_QUAL_VOLATILE= kws('v','o','l','a','t','i','l','e'),
            DECL_QUAL_SIGNED= kws('s','i','g','n','e','d'),
            DECL_QUAL_UNSIGNED= kws('u','n','s','i','g','n','e','d'),
            DECL_QUAL_SHORT= kws('s','h','o','r','t'),
            DECL_QUAL_LONG= kws('l','o','n','g'),
            DECL_QUAL_COMPLEX= kws('c','o','m','p','l','e','x'),
            DECL_QUAL_IMAGINARY= kws('i','m','a','g','i','n','a','r','y'),
        } quals[8];

        enum decl_type_kind {
            DECL_KIND_NOTAG= 0,
            DECL_KIND_STRUCT= kws('s','t','r','u','c','t'),
            DECL_KIND_UNION= kws('u','n','i','o','n'),
            DECL_KIND_ENUM= kws('e','n','u','m'),
            DECL_KIND_PTR= '*',
            DECL_KIND_FUN= ('('&31)<<5 | (')'&31),
            DECL_KIND_ARR= ('['&31)<<5 | (']'&31),
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
                size_t count; // 0 if no enumerator (because `enum e {}` is invalid anyways)
                struct decl_type_enumer {
                    tokt const name;
                    struct expression* expr; // NULL if not specified
                    struct decl_type_enumer* next;
                }* first;
            } enu; // enum

            struct decl_type_fun {
                struct declaration const* ret;
                size_t count; // -1 when (), 0 when (void), n otherwise
                struct decl_type_param {
                    struct declaration const* decl; // last one NULL if variadic
                    struct decl_type_param* next;
                }* first;
            } fun;

            struct decl_type_arr {
                struct declaration const* item;
                struct expression* count; // NULL when [*] or [], n otherwise
                bool is_static;
            } arr;
        } info;

        tokt name;
    } type;

    tokt name;
} declaration;
// }}}

typedef struct parse_decl_state {
    lex_state* ls;
    void* usr;
    void (*on)(void ref usr, declaration cref decl, tokt ref tok);
    tokt tok;
    declaration* base;
} parse_decl_state;

tokt parse_declaration(parse_decl_state ref ps, tokt tok);

// struct expression {{{
typedef struct expression {
    enum expr_kind {
        EXPR_ATOM,
        EXPR_COMPLIT,

        EXPR_BINOP_SUBSCR, EXPR_BINOP_CALL,

        EXPR_BINOP_TERNCOND,
        EXPR_BINOP_TERNBRANCH,
        EXPR_BINOP_COMMA,

        EXPR_BINOP_ASGN,
        EXPR_BINOP_ASGN_BOR, EXPR_BINOP_ASGN_BXOR, EXPR_BINOP_ASGN_BAND,
        EXPR_BINOP_ASGN_BSHL, EXPR_BINOP_ASGN_BSHR,
        EXPR_BINOP_ASGN_SUB, EXPR_BINOP_ASGN_ADD,
        EXPR_BINOP_ASGN_REM, EXPR_BINOP_ASGN_DIV, EXPR_BINOP_ASGN_MUL,

        EXPR_BINOP_LOR, EXPR_BINOP_LAND,
        EXPR_BINOP_BOR, EXPR_BINOP_BXOR, EXPR_BINOP_BAND,
        EXPR_BINOP_EQ, EXPR_BINOP_NE,
        EXPR_BINOP_LT, EXPR_BINOP_GT, EXPR_BINOP_LE, EXPR_BINOP_GE,
        EXPR_BINOP_BSHL, EXPR_BINOP_BSHR,
        EXPR_BINOP_SUB, EXPR_BINOP_ADD,
        EXPR_BINOP_REM, EXPR_BINOP_DIV, EXPR_BINOP_MUL,

        //EXPR_UNOP_SIZEOF, EXPR_UNOP_ALIGNOF, //EXPR_BINOP_OFFSETOF,
        EXPR_UNOP_ADDR, EXPR_UNOP_DEREF,
        EXPR_UNOP_CAST,
        EXPR_UNOP_BNOT, EXPR_UNOP_LNOT,
        EXPR_UNOP_MINUS, EXPR_UNOP_PLUS,
        EXPR_UNOP_PRE_DEC, EXPR_UNOP_PRE_INC,

        EXPR_UNOP_PMEMBER, EXPR_UNOP_MEMBER,
        EXPR_UNOP_POST_DEC, EXPR_UNOP_POST_INC,
    } kind;

    union expr_info {
        tokt atom;

        struct {
            struct decl_type const* type; // NULL if not provided at this point of the syntax
            struct expr_comp_entry {
                struct expr_comp_desig {
                    bool is_field, is_subscript;
                    union {
                        tokt field;
                        struct expression* subscript;
                    } info;
                    struct expr_comp_desig* next;
                }* desig; // can be NULL if none (eg. {0})
                struct expression* value;
                struct expr_comp_entry* next;
            }* first;
        } comp;

        struct { struct expression* opr; } unary;
        struct { struct expression* lhs, * rhs; } binary;

        struct {
            struct expression* base;
            struct expr_call_arg {
                struct expression* expr;
                struct expr_call_arg* next;
            }* first;
        } call;

        struct { struct expression* base,* off; } subscr;
        struct {
            struct expression* opr;
            struct decl_type const* type;
        } cast;

        struct {
            struct expression* base;
            tokt name;
        } member;
    } info;

    // reserved for user
    void* usr;
} expression;
// }}}

typedef struct parse_expr_state {
    lex_state* ls;
    void* usr;
    void (*on)(void ref usr, expression ref expr, tokt ref tok);
    tokt tok;
    // comma op not allowed in:
    // - declaration init (eg `int a = 42, b`)
    // - function args (eg `printf("d: %d", d)`)
    // - conditional alternative branch (eg `a ? b :3, d`)
    // - bitfield width (eg `struct { int a :3, b; }`)
    bool disallow_comma;
    // type-less compound literal allowed in:
    // - complete declaration (eg `some a = {0}`)
    // - compound literal value (eg `..{..= {0}}`)
    bool allow_topcomplit;
} parse_expr_state;

tokt parse_expression(parse_expr_state ref ps, tokt const tok);

// ---

#define pstokn(__at) (ps->ls->tokens.ptr+(__at))

#define _expect1(_tok)                                                \
    if (!*pstokn(*(_tok)) && (                                        \
        report_lex_locate(ps->ls, "Unexpected end of input"), true))  \
        return
#define _expect(_tok, ...)                                                                                  \
    for (char const* const* _it = (char const*[]){__VA_ARGS__, NULL} ;3; _it++)                             \
        if (*_it) if (!strcmp(*_it, pstokn(*(_tok)))) break; else continue;                                 \
        else if (                                                                                           \
            report_lex_locate(ps->ls, "Expected " #__VA_ARGS__ ", got %s", quoted(pstokn(*(_tok)))), true)  \
            return
#define _expectid(_tok)                                                                            \
    if (!isidstart(*pstokn(*(_tok))) && (                                                          \
        report_lex_locate(ps->ls, "Expected identifier, got %s", quoted(pstokn(*(_tok)))), true))  \
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

void _parse_on_array_size(void ref decl_ps_capt[3], expression ref expr, tokt ref tok)
{
    declaration ref arr = decl_ps_capt[0]; parse_decl_state ref ps = decl_ps_capt[1]; struct _parse_decl_capture ref capt = decl_ps_capt[2];
    _expect1(tok);
    _expect(tok, "]");
    arr->type.info.arr.count = expr;
    ps->tok = lext(ps->ls);
    _parse_decl_post(ps, capt, arr);
}
void _parse_on_enumer_value(void ref decl_ps_capt[3], expression ref expr, tokt ref tok)
{
    declaration ref enu = decl_ps_capt[0]; parse_decl_state ref ps = decl_ps_capt[1]; struct _parse_decl_capture ref capt = decl_ps_capt[2];
    _expect1(tok);
    for_linked (enu->type.info,enu) if (!curr->next) {
        curr->expr = expr;
        break;
    }
    if (',' == *pstokn(*tok)) {
        ps->tok = lext(ps->ls);
        _expect1(&ps->tok);
    } else ps->tok = *tok;
    if ('}' == *pstokn(ps->tok)) {
        ps->tok = lext(ps->ls);
        _parse_decl_ator(ps, capt, enu);
    } else _parse_decl_enumer(ps, capt, NULL);
}
void _parse_on_bitfield_width(void ref decl_ps_capt[4], expression ref expr, tokt ref tok)
{
    declaration ref comp = decl_ps_capt[0], ref base = decl_ps_capt[1]; parse_decl_state ref ps = decl_ps_capt[2]; struct _parse_decl_capture ref capt = decl_ps_capt[3];
    _expect1(tok);
    _expect(tok, ",", ";");
    for_linked (comp->type.info,comp) if (!curr->next) {
        curr->bitw = expr;
        break;
    }
    bool const reset = ';' == *pstokn(*tok);
    ps->tok = lext(ps->ls);
    _expect1(&ps->tok);
    if (reset && '}' == *pstokn(ps->tok)) {
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

#define kw(s) ((s)[0] && (s)[1] ? kws((s)[0],(s)[1],(s)[2],) : 0)
#define iskwx(tok, ...) (!strcmp((char[]){__VA_ARGS__, '\0'}, tok))
#define iskw(tok, askw, ...) (kws(__VA_ARGS__) == askw && iskwx(tok, __VA_ARGS__))

/// pre-ish declarator part with '('<decl>')' | '*'<decl>
void _parse_decl_ator(parse_decl_state ref ps, struct _parse_decl_capture ref capt, declaration ref decl)
{
    _expect1(&ps->tok);
    switch (*pstokn(ps->tok)) {
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
            .is_inline= decl->is_inline, // yyy: y not
            .type= {.kind= DECL_KIND_PTR, .info.ptr= decl},
            .name= decl->name,
        };
        decl->name = 0;
        for (unsigned askw; (askw = kw(pstokn(ps->tok)),
                    iskw(pstokn(ps->tok), askw, 'c','o','n','s','t') ||
                    iskw(pstokn(ps->tok), askw, 'r','e','s','t','r','i','c','t') ||
                    iskw(pstokn(ps->tok), askw, 'v','o','l','a','t','i','l','e') );
                ps->tok = lext(ps->ls)) {
            for (unsigned k = 0; k < countof(ptr.type.quals); k++) if (DECL_QUAL_END == ptr.type.quals[k]) {
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

    if (isidstart(*pstokn(ps->tok))) {
        decl->name = ps->tok;
        ps->tok = lext(ps->ls);
    }
    _parse_decl_post(ps, capt, decl);
}

/// skip closing parenthesis and parse post
void _parse_decl_close(parse_decl_state ref ps, struct _parse_decl_capture ref capt, declaration ref decl)
{
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
void _parse_decl_fixup(parse_decl_state ref ps, struct _parse_decl_capture ref capt, declaration ref after)
{
    declaration cref before = ((declaration**)capt->hold)[0]; // yyy: see at call location as for what is all that
    declaration* hold = ((declaration**)capt->hold)[1];

    // 'visit' hold; until find before; replace with after
    declaration** it = &hold;
    do switch ((*it)->type.kind) { // xxx: casts are to discard const qualifier
        case DECL_KIND_PTR: it = (declaration**)&(*it)->type.info.ptr;      break;
        case DECL_KIND_FUN: it = (declaration**)&(*it)->type.info.fun.ret;  break;
        case DECL_KIND_ARR: it = (declaration**)&(*it)->type.info.arr.item; break;
            // unreachable cases
        case DECL_KIND_NOTAG: case DECL_KIND_STRUCT: case DECL_KIND_UNION: case DECL_KIND_ENUM:;
        }
    while (before != *it);
    *it = after;

    capt->then(ps, capt->next, hold);
}

/// postfix declarator notations
void _parse_decl_post(parse_decl_state ref ps, struct _parse_decl_capture ref capt, declaration ref decl)
{
    // '(' <params> ')'
    // '[' [static][const][idk] <expr>|*|<nothing> ']'

    switch (*pstokn(ps->tok)) {
    case '(':
        ps->tok = lext(ps->ls);
        declaration fun = {
            .spec= decl->spec,
            .is_inline= decl->is_inline,
            .type= {.kind= DECL_KIND_FUN, .info.fun.ret= decl},
            .name= decl->name,
        };
        decl->name = 0;

        if (!strcmp("...", pstokn(ps->tok))) {
            ps->tok = lext(ps->ls);
            _expect1(&ps->tok);
            _expect(&ps->tok, ")");
            ps->tok = lext(ps->ls);

            fun.type.info.fun.count = 1;
            fun.type.info.fun.first = &(struct decl_type_param){0};

            _parse_decl_post(ps, capt, &fun);
            return;
        }

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
            .is_inline= decl->is_inline, // yyy: y not
            .type= {.kind= DECL_KIND_ARR, .info.arr.item= decl},
            .name= decl->name,
        };
        decl->name = 0;
        if (iskwx(pstokn(ps->tok), 's','t','a','t','i','c')) {
            arr.type.info.arr.is_static = true;
            ps->tok = lext(ps->ls);
        }
        for (unsigned askw; (askw = kw(pstokn(ps->tok)),
                    iskw(pstokn(ps->tok), askw, 'c','o','n','s','t') ||
                    iskw(pstokn(ps->tok), askw, 'r','e','s','t','r','i','c','t') ||
                    iskw(pstokn(ps->tok), askw, 'v','o','l','a','t','i','l','e') );
                ps->tok = lext(ps->ls)) {
            for (unsigned k = 0; k < countof(arr.type.quals); k++) if (DECL_QUAL_END == arr.type.quals[k]) {
                arr.type.quals[k] = askw;
                break;
            }
        }

        switch (*pstokn(ps->tok)) {
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
void _parse_decl_params(parse_decl_state ref ps, struct _parse_decl_capture ref capt, declaration ref decl)
{
    struct decl_type_param node = {.decl= decl}; // here so it's not deallocated before the recursion
    declaration ref fun = capt->hold;

    _expect1(&ps->tok);
    bool const last = ')' == *pstokn(ps->tok);
    if (last || ',' == *pstokn(ps->tok)) {
        union decl_type_info ref info = &fun->type.info;
        ps->tok = lext(ps->ls);

        if (!strcmp("...", pstokn(ps->tok))) {
            ps->tok = lext(ps->ls);
            _expect1(&ps->tok);
            _expect(&ps->tok, ")");
            ps->tok = lext(ps->ls);

            info->fun.count++;
            if (!info->fun.first) info->fun.first = &node;
            else for_linked (*info,fun) if (!curr->next) {
                curr->next = &node;
                break;
            }

            info->fun.count++;
            node.next = &(struct decl_type_param){0};

            _parse_decl_post(ps, capt, fun);
            return;
        }

        if (!decl) {
            if (last) {
                info->fun.count = -1; // eg. `int a();`
                _parse_decl_post(ps, capt, fun);
            } else report_lex_locate(ps->ls, "Expected parameter declaration, got %s", quoted(pstokn(ps->tok)));
            return;
        }

        if (!(last && !*pstokn(decl->name) && !strcmp("void", pstokn(decl->type.name)))) {
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
    if (')' == *pstokn(ps->tok)) {
        report_lex_locate(ps->ls, "Expected parameter declaration, got %s", quoted(pstokn(ps->tok)));
        return;
    }

    _parse_decl_spec(ps, &(struct _parse_decl_capture){
            .next= capt,
            .then= _parse_decl_params,
        }, &(declaration){0});
}

/// parse values of an enum
void _parse_decl_enumer(parse_decl_state ref ps, struct _parse_decl_capture ref capt, declaration ref _)
{
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

    if ('=' == *pstokn(ps->tok)) {
        ps->tok = lext(ps->ls);
        parse_expression(&(parse_expr_state){
                .ls= ps->ls,
                .usr= (void*[3]){enu, ps, capt},
                .on= (void(*)())_parse_on_enumer_value,
                .disallow_comma= true,
            }, ps->tok);
        return;
    }

    if (',' == *pstokn(ps->tok)) {
        ps->tok = lext(ps->ls);
        _expect1(&ps->tok);
    }
    if ('}' == *pstokn(ps->tok)) {
        ps->tok = lext(ps->ls);
        _parse_decl_ator(ps, capt, enu);
    } else _parse_decl_enumer(ps, capt, NULL);
}

/// parse fields of a struct/union
void _parse_decl_fields(parse_decl_state ref ps, struct _parse_decl_capture ref capt, declaration ref decl)
{
    struct decl_type_field node = {.decl= decl}; // here so it's not deallocated before the recursion
    declaration ref comp = ((declaration**)capt->hold)[0]; // yyy: see at call location as for what is all that
    declaration* ref base = ((declaration**)capt->hold)+1;

    _expect1(&ps->tok);
    bool const bitw = ':' == *pstokn(ps->tok);
    bool const reset = ';' == *pstokn(ps->tok);
    if (bitw || reset || ',' == *pstokn(ps->tok)) {
        union decl_type_info ref info = &comp->type.info;
        ps->tok = lext(ps->ls);
        _expect1(&ps->tok);

        if (!decl) {
            report_lex_locate(ps->ls, "Expected field declaration, got %s", quoted(pstokn(ps->tok)));
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

        if ('}' == *pstokn(ps->tok)) {
            if (!reset) {
                _expect(&ps->tok, ";");
                return;
            }
            ps->tok = lext(ps->ls);
            _parse_decl_ator(ps, capt, comp);
            return;
        }
    }

    if ('}' == *pstokn(ps->tok)) {
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
void _parse_decl_spec(parse_decl_state ref ps, struct _parse_decl_capture ref capt, declaration ref decl)
{
#   define case_iskw(...) if (0) case kws(__VA_ARGS__): if (!iskwx(pstokn(ps->tok), __VA_ARGS__)) goto notkw;

    for (unsigned askw; *pstokn(ps->tok); ps->tok = lext(ps->ls)) redo: switch (askw = kw(pstokn(ps->tok))) {
    case 0: goto notkw; // here as to remove warning for the `if` right after

    case_iskw('t','y','p','e','d','e','f') case_iskw('e','x','t','e','r','n') case_iskw('s','t','a','t','i','c') case_iskw('a','u','t','o') case_iskw('r','e','g','i','s','t','e','r')
        decl->spec = askw;
        continue;

    case_iskw('s','i','g','n','e','d') case_iskw('u','n','s','i','g','n','e','d') case_iskw('s','h','o','r','t') case_iskw('l','o','n','g')
        // inserts the "int" into the token stream, it will cause a broken
        // token history in some cases (eg. `unsigned int` -> `unsigned int
        // int`) that's the easiest and most sane solution I have
        if (!*pstokn(decl->type.name)) lex_inject(ps->ls, "int");
    case_iskw('c','o','n','s','t') case_iskw('r','e','s','t','r','i','c','t') case_iskw('v','o','l','a','t','i','l','e')
    case_iskw('c','o','m','p','l','e','x') case_iskw('i','m','a','g','i','n','a','r','y')
        for (unsigned k = 0; k < countof(decl->type.quals); k++) if (DECL_QUAL_END == decl->type.quals[k]) {
            decl->type.quals[k] = askw;
            break;
        }
        continue;
    case_iskw('i','n','l','i','n','e')
        decl->is_inline = true;
        continue;

    case_iskw('s','t','r','u','c','t') case_iskw('u','n','i','o','n') case_iskw('e','n','u','m')
        decl->type.kind = askw;
        bool const e = DECL_KIND_ENUM == askw;
        ps->tok = lext(ps->ls);
        if (isidstart(*pstokn(ps->tok))) {
            decl->type.name = ps->tok;
            ps->tok = lext(ps->ls);
        }
        if ('{' == *pstokn(ps->tok)) {
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
        if (!*pstokn(ps->tok)) break;
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

        if (isidstart(*pstokn(ps->tok))) {
            if (!*pstokn(decl->type.name) ||
                    (!strcmp("int", pstokn(decl->type.name)) &&
                     ( !strcmp("int",    pstokn(ps->tok)) ||
                       !strcmp("char",   pstokn(ps->tok)) || // signed/unsigned char
                       !strcmp("double", pstokn(ps->tok)) )) // long double
               ) {
                decl->type.name = ps->tok;
                continue;
            }
            declaration cpy = *decl;
            _parse_decl_ator(ps, capt, &cpy);
            return;
        }

        switch (*pstokn(ps->tok)) {
        case '=': case ',': case ';': case ')': case ':': // shortcut one stack frame
            capt->then(ps, capt->next, decl);
            return;
        case '*': case '(':;
            declaration cpy = *decl;
            _parse_decl_ator(ps, capt, &cpy);
            return;
        case '[':
            _parse_decl_post(ps, capt, decl);
            return;
        }

        report_lex_locate(ps->ls, "Expected declarator, got %s", quoted(pstokn(ps->tok)));
        return;
    } // for-switch tok

#   undef case_iskw

    capt->then(ps, capt->next, decl);
}

/// callback chain tail end which call user code
void _parse_decl_exit(parse_decl_state ref ps, struct _parse_decl_capture ref _, declaration ref decl)
{
    (void)_;
    if (ps->on) ps->on(ps->usr, decl, &ps->tok);
}

tokt parse_declaration(parse_decl_state ref ps, tokt tok)
{
    ps->tok = tok;
    declaration base = {0};
    if (!ps->base) ps->base = &base;
    _parse_decl_spec(ps, &(struct _parse_decl_capture){.then= _parse_decl_exit}, ps->base);
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
_parse_expr_closure_t _parse_expr_one, _parse_expr_one_post, _parse_expr_finish_cast, _parse_expr_one_lext_parenth, _parse_expr_one_lext_oneafter, _parse_expr_fun_args, _parse_expr_comp, _parse_expr_comp_desig, _parse_expr_one_after, _parse_expr_tern_cond, _parse_expr_tern_branch, _parse_expr_two, _parse_expr_two_after, _parse_expr_entry, _parse_expr_continue, _parse_expr_exit;

void _parse_on_cast_type(void ref capt_ps[2], declaration cref decl, tokt ref tok)
{
    struct _parse_expr_capture ref capt = capt_ps[0]; parse_expr_state ref ps = capt_ps[1];
    _expect1(tok);
    _expect(tok, ")");

    ps->tok = lext(ps->ls);
    _expect1(&ps->tok);

    if ('{' == *pstokn(ps->tok)) {
        *tok = ps->tok;
        expression complit = {.kind= EXPR_COMPLIT, .info.comp.type= &decl->type};
        // yyy: capt->hold = &complit?
        _parse_expr_comp(ps, &(struct _parse_expr_capture){
                .hold= &complit,
                .next= capt->next,
                .then= capt->then,
            }, NULL);
        return;
    }

    expression cast = {.kind= EXPR_UNOP_CAST, .info.cast.type= &decl->type};
    capt->hold = &cast;
    _parse_expr_one(ps, &(struct _parse_expr_capture){
            .next= capt,
            .then= _parse_expr_finish_cast,
        }, NULL);
}

enum expr_kind _parse_is_postfix(char cref tok)
{
    if (!tok[0]) return 0;
    switch (tok[0] <<8| tok[1]) {
    case '-' <<8| '-': return EXPR_UNOP_POST_DEC;
    case '+' <<8| '+': return EXPR_UNOP_POST_INC;
    }
    return 0;
}
enum expr_kind _parse_is_prefix(char cref tok)
{
    if (!tok[0]) return 0;
    switch (tok[0] <<8| tok[1]) {
    case '&' <<8| '\0': return EXPR_UNOP_ADDR;
    case '*' <<8| '\0': return EXPR_UNOP_DEREF;
    case '~' <<8| '\0': return EXPR_UNOP_BNOT;
    case '!' <<8| '\0': return EXPR_UNOP_LNOT;
    case '-' <<8| '\0': return EXPR_UNOP_MINUS;
    case '+' <<8| '\0': return EXPR_UNOP_PLUS;
    case '-' <<8| '-': return EXPR_UNOP_PRE_DEC;
    case '+' <<8| '+': return EXPR_UNOP_PRE_INC;
    }
    return 0;
}
enum expr_kind _parse_is_infix(char cref tok, bool const disallow_comma)
{
    if (!tok[0]) return 0;
    switch (tok[0] <<8| tok[1]) {
    case '?' <<8| '\0': return EXPR_BINOP_TERNCOND;
    //case ' <<8| '\0':': return EXPR_BINOP_TERNBRANCH;
    case ',' <<8| '\0': return disallow_comma ? 0 : EXPR_BINOP_COMMA;
    case '=' <<8| '\0': return EXPR_BINOP_ASGN;
    case '|' <<8| '\0': return EXPR_BINOP_BOR;
    case '^' <<8| '\0': return EXPR_BINOP_BXOR;
    case '&' <<8| '\0': return EXPR_BINOP_BAND;
    case '-' <<8| '\0': return EXPR_BINOP_SUB;
    case '+' <<8| '\0': return EXPR_BINOP_ADD;
    case '%' <<8| '\0': return EXPR_BINOP_REM;
    case '/' <<8| '\0': return EXPR_BINOP_DIV;
    case '*' <<8| '\0': return EXPR_BINOP_MUL;
    case '<' <<8| '\0': return EXPR_BINOP_LT;
    case '>' <<8| '\0': return EXPR_BINOP_GT;
    case '|' <<8| '=': return EXPR_BINOP_ASGN_BOR;
    case '^' <<8| '=': return EXPR_BINOP_ASGN_BXOR;
    case '&' <<8| '=': return EXPR_BINOP_ASGN_BAND;
    case '-' <<8| '=': return EXPR_BINOP_ASGN_SUB;
    case '+' <<8| '=': return EXPR_BINOP_ASGN_ADD;
    case '%' <<8| '=': return EXPR_BINOP_ASGN_REM;
    case '/' <<8| '=': return EXPR_BINOP_ASGN_DIV;
    case '*' <<8| '=': return EXPR_BINOP_ASGN_MUL;
    case '|' <<8| '|': return EXPR_BINOP_LOR;
    case '&' <<8| '&': return EXPR_BINOP_LAND;
    case '=' <<8| '=': return EXPR_BINOP_EQ;
    case '!' <<8| '=': return EXPR_BINOP_NE;
    case '<' <<8| '=': return EXPR_BINOP_LE;
    case '>' <<8| '=': return EXPR_BINOP_GE;
    case '<' <<8| '<': return '=' == tok[2] ? EXPR_BINOP_ASGN_BSHL : EXPR_BINOP_BSHL;
    case '>' <<8| '>': return '=' == tok[2] ? EXPR_BINOP_ASGN_BSHR : EXPR_BINOP_BSHR;
    }
    return 0;
}

/// parse one, including the prefix: [<prefix>] (<atom> | '('<expr>')') [<postfix>]
void _parse_expr_one(parse_expr_state ref ps, struct _parse_expr_capture ref capt, expression ref is_in_par)
{
    _expect1(&ps->tok);

    if (ps->allow_topcomplit) {
        if ('{' == *pstokn(ps->tok)) {
            expression complit = {.kind= EXPR_COMPLIT};
            // yyy: capt->hold = &complit?
            _parse_expr_comp(ps, &(struct _parse_expr_capture){
                    .hold= &complit,
                    .next= capt->next,
                    .then= capt->then,
                }, NULL);
            return;
        } else ps->allow_topcomplit = false;
    }

    enum expr_kind const prefix = _parse_is_prefix(pstokn(ps->tok));
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

#   define firstcharid(_tok) isidstart(*pstokn((_tok)))
#   define firstcharlit(_tok) (('0' <= *pstokn((_tok)) && *pstokn((_tok)) <= '9') || '.' == *pstokn((_tok)) || '"' == *pstokn((_tok)) || '\'' == *pstokn((_tok)))

    if ('(' == *pstokn(ps->tok)) {
        ps->tok = lext(ps->ls);
        _expect1(&ps->tok);

        if (firstcharid(ps->tok) && (
                !strcmp("char",     pstokn(ps->tok)) ||
                !strcmp("short",    pstokn(ps->tok)) ||
                !strcmp("int",      pstokn(ps->tok)) ||
                !strcmp("long",     pstokn(ps->tok)) ||
                !strcmp("signed",   pstokn(ps->tok)) ||
                !strcmp("unsigned", pstokn(ps->tok)) ||
                !strcmp("float",    pstokn(ps->tok)) ||
                !strcmp("double",   pstokn(ps->tok)) ||
                !strcmp("void",     pstokn(ps->tok)) ||
                !strcmp("struct",   pstokn(ps->tok)) ||
                !strcmp("union",    pstokn(ps->tok)) ||
                !strcmp("enum",     pstokn(ps->tok)) ||
                !strcmp("typedef",  pstokn(ps->tok)) ||
                !strcmp("const",    pstokn(ps->tok)) )) {
            parse_declaration(&(parse_decl_state){
                    .ls= ps->ls,
                    .usr= (void*[2]){capt, ps},
                    .on= (void(*)())_parse_on_cast_type,
                }, ps->tok);
            return;
        }

        // yyy: any non null if disallow comma was set
        capt->hold = ps->disallow_comma ? (void*)"" : NULL; // (yyy: avoids capture copy?)
        ps->disallow_comma = false;

        _parse_expr_one(ps, &(struct _parse_expr_capture){
                .next= &(struct _parse_expr_capture){
                    .next= capt,
                    .then= _parse_expr_one_lext_parenth,
                },
                .then= _parse_expr_continue,
            }, (void*)"");
        return;
    }

    expression atom = {.kind= EXPR_ATOM, .info.atom= ps->tok};
    ps->tok = lext(ps->ls);

    // yyy: any non null if comming from right above (ie is in a parenthesised thing)
    if (is_in_par && firstcharid(atom.info.atom)) {
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
            lex_rewind(ps->ls, 1);
            parse_declaration(&(parse_decl_state){
                    .ls= ps->ls,
                    .usr= (void*[2]){capt->next->next, ps},
                    .on= (void(*)())_parse_on_cast_type,
                }, atom.info.atom);
            return;
        }

        tokt const ahead = lext(ps->ls);
        lex_rewind(ps->ls, 1);
        switch (*pstokn(ps->tok)) {
        case ')':
            // (size_t) ...
            //          ^
            if (strchr("{(~!-+", *pstokn(ahead)) || firstcharlit(ahead) || firstcharid(ahead)) {
                _parse_on_cast_type(
                        (void*[2]){capt->next->next, ps},
                        &(declaration){.type.name= atom.info.atom},
                        &ps->tok
                    );
                return;
            }
            break;

        case '*':
            // (size_t* ...
            //          ^
            if (strchr(")*[", *pstokn(ahead)) || (firstcharid(ahead) && (
                    !strcmp("const",    pstokn(ahead)) ||
                    !strcmp("restrict", pstokn(ahead)) ||
                    !strcmp("volatile", pstokn(ahead)) )) ) {
                parse_declaration(&(parse_decl_state){
                        .ls= ps->ls,
                        .usr= (void*[2]){capt->next->next, ps},
                        .on= (void(*)())_parse_on_cast_type,
                        .base= &(declaration){.type.name= atom.info.atom},
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

    _parse_expr_one_after(ps, capt, &atom);
}

/// exactly same as _parse_expr_one_post but for '('<type>')' <expr>
void _parse_expr_finish_cast(parse_expr_state ref ps, struct _parse_expr_capture ref capt, expression ref expr)
{
    capt->hold->info.cast.opr = expr;
    capt->then(ps, capt->next, capt->hold);
}

/// sets the operand (expr) of the prefix op in: <prefix> <expr> [<postfix>]
void _parse_expr_one_post(parse_expr_state ref ps, struct _parse_expr_capture ref capt, expression ref expr)
{
    capt->hold->info.unary.opr = expr;
    capt->then(ps, capt->next, capt->hold);
}

/// skip a closing parenthesis in: '('<expr>')' [<postfix>]
void _parse_expr_one_lext_parenth(parse_expr_state ref ps, struct _parse_expr_capture ref capt, expression ref expr)
{
    _expect1(&ps->tok);
    _expect(&ps->tok, ")");
    // yyy: any non null if disallow comma was set
    ps->disallow_comma = !!capt->hold;
    ps->tok = lext(ps->ls);
    //capt->then(ps, capt->next, capt->hold); // yyy: extraneous call?
    _parse_expr_one_after(ps, capt, expr);
}

/// skip a closing bracket and set the offset ("within"): <expr> '['<off>']' [<postfix>]
void _parse_expr_one_lext_oneafter(parse_expr_state ref ps, struct _parse_expr_capture ref capt, expression ref within)
{
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
void _parse_expr_fun_args(parse_expr_state ref ps, struct _parse_expr_capture ref capt, expression ref expr)
{
    _expect1(&ps->tok);
    _expect(&ps->tok, ",", ")");

    expression ref callbase = capt->hold;
    struct expr_call_arg* it = callbase->info.call.first;
    while (it->next) it = it->next;
    it->expr = expr;

    if (')' == *pstokn(ps->tok)) {
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

/// parse a compound literal starting at the '{'
void _parse_expr_comp(parse_expr_state ref ps, struct _parse_expr_capture ref capt, expression ref expr)
{
    _expect1(&ps->tok);
    struct expr_comp_entry niw = {0};
    expression ref complit = capt->hold;

    if (!expr) {
        _expect(&ps->tok, "{");

        ps->tok = lext(ps->ls);
        if ('}' == *pstokn(ps->tok)) {
            // = {} (C23 but whever)
            ps->tok = lext(ps->ls);
            capt->then(ps, capt->next, complit);
            return;
        }
        complit->info.comp.first = &niw;

        complit->usr = ps->disallow_comma ? (void*)"" : NULL;
        ps->disallow_comma = true;
        ps->allow_topcomplit = true;
    }

    else {
        _expect(&ps->tok, ",", "}");

        for (struct expr_comp_entry* curr = complit->info.comp.first; curr; curr = curr->next) if (!curr->next) {
            curr->value = expr;
            curr->next = &niw;
            break;
        }

        if (',' == *pstokn(ps->tok)) ps->tok = lext(ps->ls);
        if ('}' == *pstokn(ps->tok)) {
            ps->tok = lext(ps->ls);

            // yyy: any non null if disallow comma was set
            ps->disallow_comma = !!complit->usr;
            complit->usr = NULL;
            ps->allow_topcomplit = false;
            capt->then(ps, capt->next, complit);
            return;
        }
    }

    _parse_expr_comp_desig(ps, capt, NULL);
}

/// for the chain of designator: {'.' <name> | '[' <expr> ']'}
void _parse_expr_comp_desig(parse_expr_state ref ps, struct _parse_expr_capture ref capt, expression ref subscr_expr)
{
    struct expr_comp_entry* entry;
    for (entry = capt->hold->info.comp.first; entry; entry = entry->next) if (!entry->next) break;

    if (subscr_expr) {
        for (struct expr_comp_desig* desig = entry->desig; desig; desig = desig->next) if (!desig->next) {
            desig->info.subscript = subscr_expr;
            break;
        }

        _expect1(&ps->tok);
        _expect(&ps->tok, "]");
        ps->disallow_comma = true;
        ps->allow_topcomplit = true;
        ps->tok = lext(ps->ls);
    }

    struct expr_comp_desig niw = {
        .is_field= '.' == pstokn(ps->tok)[0] && '\0' == pstokn(ps->tok)[1],
        .is_subscript= '[' == *pstokn(ps->tok),
    };

    if (!niw.is_field && !niw.is_subscript) {
        _expect1(&ps->tok);
        if (entry->desig) { _expect(&ps->tok, "="); }

        _parse_expr_one(ps, &(struct _parse_expr_capture){
                .next= &(struct _parse_expr_capture){
                    .next= capt,
                    .then= _parse_expr_comp,
                },
                .then= _parse_expr_continue,
            }, NULL);
        return;
    }

    if (!entry->desig) entry->desig = &niw;
    else for (struct expr_comp_desig* desig = entry->desig; desig; desig = desig->next) if (!desig->next) {
        desig->next = &niw;
        break;
    }

    if (niw.is_field) {
        niw.info.field = lext(ps->ls);
        ps->tok = lext(ps->ls);
        _parse_expr_comp_desig(ps, capt, NULL);
        return;
    }

    if (niw.is_subscript) {
        ps->tok = lext(ps->ls);
        ps->disallow_comma = false;
        ps->allow_topcomplit = false;
        _parse_expr_one(ps, &(struct _parse_expr_capture){
                .next= &(struct _parse_expr_capture){
                    .next= capt,
                    .then= _parse_expr_comp_desig,
                },
                .then= _parse_expr_continue,
            }, NULL);
        return;
    }
}

/// parse postfix part after expr: <expr> (<postfix> | '('<arg>')' | '['<off>']' | ('.'|'->')<name>)
void _parse_expr_one_after(parse_expr_state ref ps, struct _parse_expr_capture ref capt, expression ref expr)
{
    enum expr_kind const postfix = _parse_is_postfix(pstokn(ps->tok));
    if (postfix) {
        ps->tok = lext(ps->ls);
        expression post = {.kind= postfix, .info.unary.opr= expr};
        _parse_expr_one_after(ps, capt, &post);
        return;
    }

    bool pmem = false;
    switch (*pstokn(ps->tok)) {
    case '(':
        ps->tok = lext(ps->ls);
        if (')' == *pstokn(ps->tok)) {
            ps->tok = lext(ps->ls);
            expression access = {.kind= EXPR_BINOP_CALL, .info.call.base= expr};
            _parse_expr_one_after(ps, capt, &access);
            return;
        }
        expression callbase = {
            .kind= EXPR_BINOP_CALL,
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
            .kind= EXPR_BINOP_SUBSCR,
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
        if ('>' != pstokn(ps->tok)[1]) break;
        pmem = true;
        if (0) // fall through
    case '.':
            if (pstokn(ps->tok)[1]) break;
        tokt name = lext(ps->ls);
        _expect1(&name);
        _expectid(&name);
        expression access = {
            .kind= pmem ? EXPR_UNOP_PMEMBER : EXPR_UNOP_MEMBER,
            .info.member= {.base= expr, .name= name},
        };
        ps->tok = lext(ps->ls);
        _parse_expr_one_after(ps, capt, &access);
        return;
    }

    capt->then(ps, capt->next, expr);
}

/// lands there after the first branch of the ternary, so on the ':', no comma op in the third operand
void _parse_expr_tern_cond(parse_expr_state ref ps, struct _parse_expr_capture ref capt, expression ref consequence)
{
    _expect1(&ps->tok);
    _expect(&ps->tok, ":");
    expression ref condition_root = capt->hold;
    expression branches = {.kind= EXPR_BINOP_TERNBRANCH, .info.binary.lhs= consequence};
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
void _parse_expr_tern_branch(parse_expr_state ref ps, struct _parse_expr_capture ref capt, expression ref alternative)
{
    expression ref condition_root = capt->hold;
    condition_root->info.binary.rhs->info.binary.rhs = alternative;
    // yyy: any non null if disallow comma was set
    ps->disallow_comma = !!condition_root->usr;
    condition_root->usr = NULL;

    if (!ps->disallow_comma && ',' == *pstokn(ps->tok)) {
        ps->tok = lext(ps->ls);
        expression in = {.kind= EXPR_BINOP_COMMA, .info.binary.lhs= condition_root};
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
void _parse_expr_two(parse_expr_state ref ps, struct _parse_expr_capture ref capt, expression ref rhs)
{
    enum expr_kind const infix = _parse_is_infix(pstokn(ps->tok), ps->disallow_comma);
    if (!infix) {
        capt->hold->info.binary.rhs = rhs;
        capt->then(ps, capt->next, capt->hold);
        return;
    }
    ps->tok = lext(ps->ls);

    if (EXPR_BINOP_TERNCOND == infix) {
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

    if (EXPR_BINOP_COMMA == infix) {
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
    if (l < n || ( (EXPR_BINOP_ASGN <= l && l <= EXPR_BINOP_ASGN_MUL)
                && (EXPR_BINOP_ASGN <= n && n <= EXPR_BINOP_ASGN_MUL) )) {
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
void _parse_expr_two_after(parse_expr_state ref ps, struct _parse_expr_capture ref capt, expression ref rhs)
{
    capt->hold->info.binary.rhs = rhs;
    capt->then(ps, capt->next, capt->hold);
}

/// proper start the `_parse_expr_two` loop, sets up `_parse_expr_exit` at callback chain end
void _parse_expr_entry(parse_expr_state ref ps, struct _parse_expr_capture ref _, expression ref lhs)
{
    (void)_;
    enum expr_kind const infix = _parse_is_infix(pstokn(ps->tok), ps->disallow_comma);
    if (infix) {
        ps->tok = lext(ps->ls);
        expression in = {.kind= infix, .info.binary.lhs= lhs};

        if (EXPR_BINOP_TERNCOND == infix) {
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

        if (EXPR_BINOP_COMMA == infix) {
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
void _parse_expr_continue(parse_expr_state ref ps, struct _parse_expr_capture ref capt, expression ref lhs)
{
    enum expr_kind const infix = _parse_is_infix(pstokn(ps->tok), ps->disallow_comma);
    if (infix) {
        ps->tok = lext(ps->ls);
        expression in = {.kind= infix, .info.binary.lhs= lhs};

        if (EXPR_BINOP_TERNCOND == infix) {
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

        if (EXPR_BINOP_COMMA == infix) {
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
void _parse_expr_exit(parse_expr_state ref ps, struct _parse_expr_capture ref _, expression ref expr)
{
    (void)_;
    if (ps->on) ps->on(ps->usr, expr, &ps->tok);
}

tokt parse_expression(parse_expr_state ref ps, tokt tok)
{
    ps->tok = tok;
    _parse_expr_one(ps, &(struct _parse_expr_capture){.then= _parse_expr_entry}, NULL);
    return ps->tok;
}
// }}}

#undef _expectid
#undef _expect
#undef _expect1

#undef pstokn

#endif // CINTRE_PARSER_H
