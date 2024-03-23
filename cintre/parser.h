/// C parser of sort

#ifndef CINTRE_PARSER_H
#define CINTRE_PARSER_H

#include "common.h"
#include "lexer.h"

#define kws(a,b,c,...) ((a&31)<<10 | (b&31)<<5 | (c&31))

// struct declaration {{{
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
            struct decl_type const* ptr;
            struct decl_type_obj {
                size_t count;
                struct decl_type_field {
                    struct declaration const* decl;
                    struct decl_type_field* next;
                }* first;
            } obj; // obj is struct or union
            struct decl_type_enu {
                size_t count;
                struct decl_type_enumer {
                    bufsl const name;
                    struct decl_type_enumer* next;
                }* first;
            } enu; // TODO: see in parse_declaration
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
                size_t count; // -1 when [] or [*], 0 when (void), n otherwise
            } arr;
        } info;

        bufsl name;
    } type;

    bufsl name;
} declaration;
typedef void on_decl(void ref usr, declaration cref decl, bufsl ref tok);
typedef struct parse_decl_state {
    lex_state* ls;
    void* usr;
    on_decl* on;
    declaration base;
} parse_decl_state;
// }}}
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
} expression;
typedef void on_expr(void ref usr, expression ref expr, bufsl ref tok);
typedef struct parse_expr_state {
    lex_state* ls;
    void* usr;
    on_expr* on;
    bufsl tok;
} parse_expr_state;
// }}}
bufsl parse_expression(parse_expr_state ref ps, bufsl const tok);

// ---

// parse declaration {{{
#define kw(s) kws(s[0],s[1],s[2],)
#define iskwx(tok, ...) !dyarr_cmp(&((bufsl){.ptr= (char[]){__VA_ARGS__}, .len= sizeof((char[]){__VA_ARGS__})}), &tok)
#define iskw(tok, askw, ...) (kws(__VA_ARGS__) == askw && iskwx(tok, __VA_ARGS__))

bufsl _parse_declarator(lex_state ref ls, void ref usr, on_decl on, bufsl const tok1, bufsl const tok2, declaration cref base);

// [struct|union] <name>? { <decls> } {{{
struct _parse_put_field_capt {
    lex_state* ls;
    void* usr;
    on_decl* on;
    declaration* objc;
};

void _decl_put_field(struct _parse_put_field_capt ref capt, declaration cref decl, bufsl ref tok) {
    struct decl_type_obj* obj = &capt->objc->type.info.obj;
    struct decl_type_field node = {.decl= decl};
    obj->count++;
    if (!obj->first) obj->first = &node;
    else for (struct decl_type_field* curr = obj->first ;3; curr = curr->next) if (!curr->next) {
        curr->next = &node;
        break;
    }

    bufsl nx = lext(capt->ls);
    if (tok->len && nx.len && '}' != *nx.ptr)
        *tok = parse_declaration(&(parse_decl_state){
                .ls= capt->ls,
                .usr= capt, .on= (void(*)())_decl_put_field,
                .base= ';' == *tok->ptr ? (declaration){0} : *decl},
                nx);
    else {
        *tok = lext(capt->ls);
        if (tok->len && ';' == *tok->ptr) {
            if (capt->on) capt->on(capt->usr, capt->objc, tok);
        } else *tok = _parse_declarator(capt->ls, capt->usr, capt->on, *tok, lext(capt->ls), capt->objc);
    }
}
// }}}

// <decl>(<params>) {{{
struct _parse_put_param_capt {
    lex_state* ls;
    void* usr;
    on_decl* on;
    declaration* func;
};

void _decl_put_param(struct _parse_put_param_capt ref capt, declaration cref decl, bufsl ref tok) {
    struct decl_type_fun* fun = &capt->func->type.info.fun;
    struct decl_type_param node = {.decl= decl};
    fun->count++;
    if (!fun->first) fun->first = &node;
    else for (struct decl_type_param* curr = fun->first ;3; curr = curr->next) if (!curr->next) {
        curr->next = &node;
        break;
    }

    if (tok->len && ',' == *tok->ptr)
        *tok = parse_declaration(&(parse_decl_state){
                .ls= capt->ls,
                .usr= capt, .on= (void(*)())_decl_put_param},
                lext(capt->ls));
    else {
        if (1 == fun->count && !fun->first->decl->type.name.len && 4 == fun->first->decl->name.len && !memcmp("void", fun->first->decl->name.ptr, 4)) {
            fun->count = 0;
            fun->first = NULL;
        }
        *tok = lext(capt->ls);
        if (capt->on) capt->on(capt->usr, capt->func, tok);
    }
}
// }}}

// (<decl>) {{{
struct _parse_par_decl_capt {
    lex_state* ls;
    void* usr;
    on_decl* on;
    declaration const* outer;
};

void _parse_par_decl_unwind(struct _parse_par_decl_capt ref capt, declaration cref decl, bufsl ref tok) {
    // TODO: apply the transformation from `capt->outer->type` to `decl` (or rather the `local` copy)

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

    if (capt->on) capt->on(capt->usr, &local, tok);
}

void _parse_par_decl(struct _parse_par_decl_capt ref capt, declaration cref inner, bufsl ref tok) {
    *tok = lext(capt->ls);
    declaration const* outer = capt->outer;
    // reuses the same field, but in `_parse_par_decl_unwind` it will then
    // carry `inner`, ie the transformation to unwind
    capt->outer = inner;
    *tok = _parse_declarator(capt->ls,
            capt, (void(*)())_parse_par_decl_unwind,
            // fake token to get past the first switch
            (bufsl){.ptr= ")", .len= 1}, *tok,
            outer);
}
// }}}

bufsl _parse_declarator(lex_state ref ls, void ref usr, on_decl on, bufsl const tok1, bufsl const tok2, declaration cref base) {
    if (!tok1.len) return tok1;

    bufsl tok;
#   define is1(w) (tok.len && w == *tok.ptr)

    declaration decl = *base;

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

    default:
        decl.name = tok1;
        // fall through
    case ')':
        tok = tok2;
        if (is1(':')) tok = parse_expression(&(parse_expr_state){.ls= ls}, lext(ls));
    } // switch with (<decl>), *<decl>

    if (is1('(')) {
        tok = lext(ls);
        struct decl_type const hold = decl.type;
        decl.type = (struct decl_type){.kind= KIND_FUN, .info.fun.ret= &hold};
        if (tok.len) {
            if (!is1(')')) return parse_declaration(&(parse_decl_state){
                    .ls= ls,
                    .usr= &(struct _parse_put_param_capt){.ls= ls, .usr= usr, .on= on, .func= &decl},
                    .on= (void(*)())_decl_put_param},
                    tok);
            decl.type.info.fun.count = -1;
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
            parse_expression(&(parse_expr_state){.ls= ls}, tok);
            tok = lext(ls);
        }
    } // while <decl>[<arrinfo>]

    if (on) on(usr, &decl, &tok);

#   undef is1
    return tok;
} // _parse_declarator

bufsl parse_declaration(parse_decl_state ref ps, bufsl tok) {
#   define is(wo) (!dyarr_cmp((&(bufsl){.ptr= wo, .len= strlen(wo)}), &tok))
#   define is1(w) (tok.len && w == *tok.ptr)
#   define isid() (tok.len && ('_' == *tok.ptr || ('A' <= *tok.ptr && *tok.ptr <= 'Z') || ('a' <= *tok.ptr && *tok.ptr <= 'z') || ('0' <= *tok.ptr && *tok.ptr <= '9')))
#   define case_iskw(...) if (0) case kws(__VA_ARGS__): if (!iskwx(tok, __VA_ARGS__)) goto notkw;

    for (unsigned askw; tok.len; tok = lext(ps->ls)) redo: switch (askw = tok.len <3 ? 0 : kw(tok.ptr)) {
    case 0: goto notkw;

    case_iskw('t','y','p','e','d','e','f') case_iskw('e','x','t','e','r','n') case_iskw('s','t','a','t','i','c') case_iskw('a','u','t','o') case_iskw('r','e','g','i','s','t','e','r')
        ps->base.spec = askw;
        break;

    case_iskw('s','i','g','n','e','d') case_iskw('u','n','s','i','g','n','e','d') case_iskw('s','h','o','r','t') case_iskw('l','o','n','g')
        if (!ps->base.type.name.len) ps->base.type.name = (bufsl){.ptr= "int", .len= 3};
    case_iskw('c','o','n','s','t') case_iskw('r','e','s','t','r','i','c','t') case_iskw('v','o','l','a','t','i','l','e') case_iskw('i','n','l','i','n','e')
    case_iskw('c','o','m','p','l','e','x') case_iskw('i','m','a','g','i','n','a','r','y')
        for (unsigned k = 0; k < countof(ps->base.type.quals); k++) if (QUAL_END == ps->base.type.quals[k]) {
            ps->base.type.quals[k] = askw;
            break;
        }
        break;

    case_iskw('s','t','r','u','c','t') case_iskw('u','n','i','o','n') case_iskw('e','n','u','m')
        ps->base.type.kind = askw;
        tok = lext(ps->ls);
        if (isid()) {
            ps->base.type.name = tok;
            tok = lext(ps->ls);
        }
        if (is1('{')) {
            tok = lext(ps->ls);
            if (KIND_ENUM == askw) {
                // TODO: store the names and values
                do {
                    bufsl name = tok;
                    tok = lext(ps->ls);
                    if (is1('=')) tok = parse_expression(&(parse_expr_state){.ls= ps->ls}, lext(ps->ls));
                    if (is1(',')) tok = lext(ps->ls);
                } while (isid());
            } else if (!is1('}')) return parse_declaration(&(parse_decl_state){
                    .ls= ps->ls,
                    .usr= &(struct _parse_put_field_capt){.ls= ps->ls, .usr= ps->usr, .on= ps->on, .objc= &ps->base},
                    .on= (void(*)())_decl_put_field},
                    lext(ps->ls));
            tok = lext(ps->ls);
            if (is1(';')) {
                if (ps->on) ps->on(ps->usr, &ps->base, &tok);
                return tok;
            } else goto redo;
        } else {
            if (KIND_STRUCT == askw || KIND_UNION == askw)
                ps->base.type.info.obj.count = -1;
            goto redo;
        }
        break;

    default:
    notkw:
        ;
        bufsl tok1 = tok, tok2 = lext(ps->ls);
        if (tok2.len && isid()) switch (*tok2.ptr) {
        case '(': // eg `int (a);`
            if (!ps->base.type.name.len) {
                ps->base.type.name = tok1;
                tok1 = tok2;
                tok2 = lext(ps->ls);
            } // fall through
        case ',': case ';': case ')': case '[':
            break;
        default:
            ps->base.type.name = tok1;
            tok = tok2;
            goto redo;
        }

        tok = _parse_declarator(ps->ls, ps->usr, ps->on, tok1, tok2, &ps->base);
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
// }}}

// parse expression {{{
struct _capture;
typedef void _closure_t(parse_expr_state ref ps, struct _capture ref capt, expression ref expr);
struct _capture {
    expression* hold;
    struct _capture ref next;
    _closure_t ref then; // its `hold` is in `next->hold`
};
_closure_t _parse_one, _parse_one_post, _parse_one_lext, _parse_one_lext_whole, _parse_one_after, _parse_two, _parse_two_after, _parse_entry, _parse_continue, _parse_exit;

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
void _parse_one(parse_expr_state ref ps, struct _capture ref capt, expression ref _) {
    (void)_;
    if (!ps->tok.len) return;

    enum expr_kind prefix = _parse_is_prefix(ps->tok);
    if (prefix) {
        ps->tok = lext(ps->ls);
        expression pre = {.kind= prefix};
        _parse_one(ps, &(struct _capture){
                .next= &(struct _capture){
                    .hold= &pre,
                    .next= capt->next,
                    .then= capt->then,
                },
                .then= _parse_one_post,
            }, NULL);
        return;
    }

    if ('(' == *ps->tok.ptr) {
        ps->tok = lext(ps->ls);
        _parse_one(ps, &(struct _capture){
                .next= &(struct _capture){
                    .next= capt,
                    .then= _parse_one_lext,
                },
                .then= _parse_continue,
            }, NULL);
        return;
    }

    expression atom = {.kind= ATOM, .info.atom= ps->tok};
    ps->tok = lext(ps->ls);
    _parse_one_after(ps, capt, &atom);
}

/// sets the operand (expr) of the prefix op in: <prefix> <expr> [<postfix>]
void _parse_one_post(parse_expr_state ref ps, struct _capture ref capt, expression ref expr) {
    capt->hold->info.unary.opr = expr;
    _parse_one_after(ps, capt, capt->hold);
}

/// skip a closing parenthesis in: '('<expr>')' [<postfix>]
void _parse_one_lext(parse_expr_state ref ps, struct _capture ref capt, expression ref expr) {
    ps->tok = lext(ps->ls);
    _parse_one_after(ps, capt, expr);
}

/// skip a closing thingy and set the thing within (arg): <expr> ('('<arg>')' | '['<arg>']') [<postfix>]
void _parse_one_lext_whole(parse_expr_state ref ps, struct _capture ref capt, expression ref within) {
    ps->tok = lext(ps->ls);
    capt->hold->info.call.args = within;
    _parse_one_after(ps, capt, capt->hold);
}

/// parse postfix part after expr: <expr> (<postfix> | '('<arg>')' | '['<arg>']' | ('.'|'->')<name>)
void _parse_one_after(parse_expr_state ref ps, struct _capture ref capt, expression ref expr) {
    enum expr_kind postfix = _parse_is_postfix(ps->tok);
    if (postfix) {
        ps->tok = lext(ps->ls);
        expression post = {.kind= postfix, .info.unary.opr= expr};
        _parse_one_after(ps, capt, &post);
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
            _parse_one_after(ps, capt, &access);
            return;
        }
        ps->tok = nx;
        if (0) // fall through
    case '[':
            ps->tok = lext(ps->ls);
        expression whole = {.kind= call ? BINOP_CALL : BINOP_SUBSCR, .info.call.base= expr};
        capt->hold = &whole;
        _parse_one(ps, &(struct _capture){
                .next= &(struct _capture){
                    .next= capt,
                    .then= _parse_one_lext_whole,
                },
                .then= _parse_continue,
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
        _parse_one_after(ps, capt, &access);
        return;
    }

    capt->then(ps, capt->next, expr);
}

/// parse two with lhs, lop and rhs known: <lhs> <lop> <rhs> [<nop>]
void _parse_two(parse_expr_state ref ps, struct _capture ref capt, expression ref rhs) {
    enum expr_kind infix = _parse_is_infix(ps->tok);
    if (!infix) {
        capt->hold->info.binary.rhs = rhs;
        capt->then(ps, capt->next, capt->hold);
        return;
    }
    ps->tok = lext(ps->ls);

    enum expr_kind l = capt->hold->kind, n = infix;
    if (l < n || ( (BINOP_TERNCOND == l || BINOP_TERNBRANCH == l)
                && (BINOP_TERNCOND == n || BINOP_TERNBRANCH == n) )) {
        expression in = {.kind= infix, .info.binary.lhs= rhs};
        _parse_one(ps, &(struct _capture){
                .next= &(struct _capture){
                    .hold= &in,
                    .next= &(struct _capture){
                        .hold= capt->hold,
                        .next= capt->next,
                        .then= capt->then,
                    },
                    .then= _parse_two_after,
                },
                .then= _parse_two,
            }, NULL);
    } else {
        capt->hold->info.binary.rhs = rhs;
        expression in = {.kind= infix, .info.binary.lhs= capt->hold};
        _parse_one(ps, &(struct _capture){
                .next= &(struct _capture){
                    .hold= &in,
                    .next= capt->next,
                    .then= capt->then,
                },
                .then= _parse_two,
            }, NULL);
    }
}

/// handle the case in `_parse_two` where nop comes before lop in precedence (it's executed when "bubbling" back toward `_parse_exit`)
void _parse_two_after(parse_expr_state ref ps, struct _capture ref capt, expression ref rhs) {
    capt->hold->info.binary.rhs = rhs;
    capt->then(ps, capt->next, capt->hold);
}

/// proper start the `_parse_two` loop, sets up `_parse_exit` at callback chain end
void _parse_entry(parse_expr_state ref ps, struct _capture ref capt, expression ref lhs) {
    enum expr_kind infix = _parse_is_infix(ps->tok);
    if (infix) {
        ps->tok = lext(ps->ls);
        expression in = {.kind= infix, .info.binary.lhs= lhs};
        _parse_one(ps, &(struct _capture){
                .next= &(struct _capture){
                    .hold= &in,
                    .then= _parse_exit,
                },
                .then= _parse_two,
            }, NULL);
        return;
    }

    _parse_exit(ps, NULL, lhs);
}

/// similar to `_parse_entry` for sub expr in: '('<expr>')' or arg in: <expr> ('('<arg>')' | '['<arg>'])
void _parse_continue(parse_expr_state ref ps, struct _capture ref capt, expression ref lhs) {
    enum expr_kind infix = _parse_is_infix(ps->tok);
    if (infix) {
        ps->tok = lext(ps->ls);
        expression in = {.kind= infix, .info.binary.lhs= lhs};
        capt->hold = &in;
        _parse_one(ps, &(struct _capture){
                .next= capt,
                .then= _parse_two,
            }, NULL);
        return;
    }

    capt->then(ps, capt->next, lhs);
}

/// callback chain tail end which call user code
void _parse_exit(parse_expr_state ref ps, struct _capture ref _, expression ref expr) {
    (void)_;
    if (ps->on) ps->on(ps->usr, expr, &ps->tok);
}

bufsl parse_expression(parse_expr_state ref ps, bufsl tok) {
    ps->tok = tok;
    _parse_one(ps, &(struct _capture){.then= _parse_entry}, NULL);
    return ps->tok;
}
// }}}

#endif // CINTRE_PARSER_H
