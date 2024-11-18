/// Semantic checker on top of the parser; it is tightly linked with the
/// compiler, checking may itself emit bytecode by calling the compiler for
/// certain expressions (string literals, compound literals); example:
/// ```c
/// compile_state cs = {0};
///
/// struct adpt_type cref ty = check_expression(&cs, expr);
/// if (ty) print_type(stdout, ty);
/// ```

#ifndef CINTRE_CHECKER_H
#define CINTRE_CHECKER_H

#include "common.h"
#include "parser.h"
#include "adapter.h"

typedef struct bytecode {
    unsigned char *ptr;
    size_t len, cap;
} bytecode;

typedef struct compile_state {
    lex_state const* ls;

    bytecode res;
    size_t vsp;

    void* usr;
    struct adpt_item const* (*lookup)(void* usr, char cref name);

    // used to deduplicat string literals
    //dyarr(buf) checker_alloc_interned;
} compile_state;

// needed for cases:
// - ptr decay (arr type to ptr)
// - address of (ptr around type)
// - cast (type to itself)
// all can be freed right after the corresponding call to `compile_expression`
typedef struct comp_checked {
    struct adpt_type const type;
    union comp_chk_info {
        size_t str_variable; // if str, the location on stack ('variable') where the array is allocated
        // TODO(maybe): union {} num_value; // if num, the value
        // TODO(maybe): struct adpt_item ident_lookup; // if ident, the result of `lookup`
    } info;
} const* (*comp_alloc_checked_ator)(void* usr, struct comp_checked const niw);

/// does modify the expression by at least adding a typing info in the usr fields
struct adpt_type const* check_expression(compile_state ref cs, expression ref expr, comp_alloc_checked_ator const alloc_checked);

#define cstokn(__at) (cs->ls->tokens.ptr+(__at))

// check utils {{{
bool _are_types_compatible(struct adpt_type cref dst, struct adpt_type cref src)
{
    // yeah no idea either that'll do for now until it actually shows too much
#   define _are_types_same(_d, _s) (  \
        (_d)->tyty == (_s)->tyty &&   \
        (_d)->size == (_s)->size &&   \
        (_d)->align == (_s)->align )
    if (_are_types_same(dst, src)) return true;

    bool const dst_isnum = ADPT_TYPE_CHAR <= dst->tyty && dst->tyty <= ADPT_TYPE_DOUBLE;
    bool const src_isnum = ADPT_TYPE_CHAR <= src->tyty && src->tyty <= ADPT_TYPE_DOUBLE;
    if (dst_isnum && src_isnum) return true;

    if (ADPT_TYPE_STRUCT == dst->tyty && ADPT_TYPE_STRUCT == src->tyty) {
        if (dst->size != src->size || dst->align != src->align ||
                dst->info.comp.count != src->info.comp.count)
            return false;

        for (size_t k = 0; k < dst->info.comp.count; k++)
            if (!_are_types_same(
                        dst->info.comp.fields[k].type,
                        src->info.comp.fields[k].type
                        )) return false;

        return true;
    }

    if (ADPT_TYPE_FUN == dst->tyty || ADPT_TYPE_FUN == src->tyty) {
        struct adpt_type const* dst_tail = dst;
        while (ADPT_TYPE_PTR == dst_tail->tyty) dst_tail = dst->info.ptr;
        struct adpt_type const* src_tail = src;
        while (ADPT_TYPE_PTR == src_tail->tyty) src_tail = src->info.ptr;

        if (ADPT_TYPE_FUN != dst->tyty || ADPT_TYPE_FUN != src->tyty ||
                dst->info.fun.count != src->info.fun.count ||
                !_are_types_same(dst->info.fun.ret, src->info.fun.ret))
            return false;

        for (size_t k = 0; k < dst->info.fun.count; k++)
            if (!_are_types_same(
                        dst->info.fun.params[k].type,
                        src->info.fun.params[k].type
                        )) return false;

        return true;
    }

    if (ADPT_TYPE_ARR == dst->tyty) return false;

    if (ADPT_TYPE_PTR == dst->tyty) switch (src->tyty) {
        struct adpt_type const* src_under;
    case ADPT_TYPE_PTR: src_under = src->info.ptr; if (0)
    case ADPT_TYPE_ARR: src_under = src->info.arr.item;
        return _are_types_same(dst->info.ptr, src_under) ||
                ADPT_TYPE_VOID == dst->info.ptr->tyty ||
                ADPT_TYPE_VOID == src_under->tyty;
    case ADPT_TYPE_VOID: case ADPT_TYPE_CHAR: case ADPT_TYPE_UCHAR: case ADPT_TYPE_SCHAR: case ADPT_TYPE_SHORT: case ADPT_TYPE_INT: case ADPT_TYPE_LONG: case ADPT_TYPE_USHORT: case ADPT_TYPE_UINT: case ADPT_TYPE_ULONG: case ADPT_TYPE_FLOAT: case ADPT_TYPE_DOUBLE: case ADPT_TYPE_STRUCT: case ADPT_TYPE_UNION: case ADPT_TYPE_FUN: case ADPT_TYPE_NAMED:
        return false;
    }

    return false;
#   undef _are_types_same
}

bool _is_expr_lvalue(compile_state cref cs, expression cref expr)
{
    switch (expr->kind) {
    case EXPR_ATOM:
        if (!isidstart(*cstokn(expr->info.atom))) return false;
        struct adpt_item const* found = cs->lookup(cs->usr, cstokn(expr->info.atom));
        if (!found) return notif("Unknown name: %s", quoted(cstokn(expr->info.atom))), false; // XXX: double error message
        return ADPT_ITEM_OBJECT == found->kind || ADPT_ITEM_VARIABLE == found->kind;

    case EXPR_BINOP_SUBSCR:
    case EXPR_UNOP_DEREF:
    case EXPR_UNOP_PMEMBER:
    case EXPR_UNOP_MEMBER:
        return true;

    default: // (43 cases ><'')
        return false;
    }
}

struct adpt_type const* _cast_type(compile_state ref cs, struct decl_type cref ty, comp_alloc_checked_ator const alloc_checked)
{
    switch (ty->kind) {
    case DECL_KIND_NOTAG:;
        bool _signed = false, _unsigned = false, _short = false, _long = false;
        for (size_t k = 0; DECL_QUAL_END != ty->quals[k]; k++) switch (ty->quals[k]) {
            case DECL_QUAL_SIGNED:    _signed = true;          break;
            case DECL_QUAL_UNSIGNED:  _unsigned = true;        break;
            case DECL_QUAL_SHORT:     _short = true;           break;
            case DECL_QUAL_LONG:      _long = true;            break;
            case DECL_QUAL_COMPLEX:   notif("NIY: complex");   return NULL;
            case DECL_QUAL_IMAGINARY: notif("NIY: imaginary"); return NULL;
                // noop cases
            case DECL_QUAL_END: case DECL_QUAL_CONST: case DECL_QUAL_RESTRICT: case DECL_QUAL_VOLATILE:;
            }

#       define nameis(s)  (!strcmp(s, cstokn(ty->name)))
        if (nameis("char"))
            return _signed ? &adptb_schar_type
                 : _unsigned ? &adptb_uchar_type
                 : &adptb_char_type;
        if (nameis("int"))
            return _short ? (_unsigned ? &adptb_ushort_type : &adptb_short_type)
                 : _long ? (_unsigned ? &adptb_ulong_type : &adptb_long_type)
                 : _unsigned ? &adptb_uint_type : &adptb_int_type;
        if (nameis("float"))  return &adptb_float_type;
        if (nameis("double")) return &adptb_double_type;
        if (nameis("void"))   return &adptb_void_type;
#       undef nameis

        struct adpt_item cref it = cs->lookup(cs->usr, cstokn(ty->name));
        if (!it) return notif("Unknown type name: %s", quoted(cstokn(ty->name))), NULL; // XXX: double error message
        if (ADPT_ITEM_TYPEDEF == it->kind) {
            struct adpt_type cref ty = _truetype(it->type);
            if (ADPT_TYPE_VOID <= ty->tyty && ty->tyty <= ADPT_TYPE_DOUBLE)
                return it->type;
        }
        return NULL;

    case DECL_KIND_ENUM: return &adptb_int_type;

    case DECL_KIND_PTR:;
        struct adpt_type cref ptr = _cast_type(cs, &ty->info.ptr->type, alloc_checked);
        if (!ptr) return NULL;

        return (void*)alloc_checked(cs->usr, (struct comp_checked){.type= {
                .size= sizeof(void*),
                .align= sizeof(void*),
                .tyty= ADPT_TYPE_PTR,
                .info.ptr= ptr,
            }});

    case DECL_KIND_STRUCT:
    case DECL_KIND_UNION:
    case DECL_KIND_FUN:
    case DECL_KIND_ARR:
        return NULL;
    }
    // unreachable
    return NULL;
}
// }}}

struct slot;
void compile_expression(compile_state ref cs, expression cref expr, struct slot ref slot);

struct adpt_type const* check_expression(compile_state ref cs, expression ref expr, comp_alloc_checked_ator const alloc_checked)
{
#   define fail(...)  return notif(__VA_ARGS__), NULL
#   define failforward(id, from)  for (id = check_expression(cs, from, alloc_checked); !id; ) return NULL; //fail("here") // yyy: could do 'in blabla expr'
#   define isint(__ty)  (ADPT_TYPE_CHAR <= (__ty)->tyty && (__ty)->tyty <= ADPT_TYPE_ULONG)
#   define issgn(__ty)  (ADPT_TYPE_SCHAR <= (__ty)->tyty && (__ty)->tyty <= ADPT_TYPE_LONG)
#   define isflt(__ty)  (ADPT_TYPE_FLOAT <= (__ty)->tyty && (__ty)->tyty <= ADPT_TYPE_DOUBLE)
#   define isnum(__ty)  (isint(__ty) || isflt(__ty))
#   define isfun(__ty)  (ADPT_TYPE_FUN == (__ty)->tyty)
#   define isptr(__ty)  (ADPT_TYPE_PTR == (__ty)->tyty)
#   define isarr(__ty)  (ADPT_TYPE_ARR == (__ty)->tyty)
#   define isindir(__ty)  (isptr(__ty) || isarr(__ty))
#   define atindir(__ty)  (isptr(__ty) ? (__ty)->info.ptr : (__ty)->info.arr.item)

#   define fail_got_type(__ty, ...)  return (  \
        fprintf(stderr, __VA_ARGS__),          \
        fprintf(stderr, ", got: "),            \
        print_type(stderr, (__ty), false),     \
        fprintf(stderr, "\n"),                 \
        NULL)
#   define fail_got_2types(__1, __2, ...)  return (  \
        fprintf(stderr, __VA_ARGS__),                \
        fprintf(stderr, ", got: "),                  \
        print_type(stderr, (__1), false),            \
        fprintf(stderr, " and "),                    \
        print_type(stderr, (__2), false),            \
        fprintf(stderr, "\n"),                       \
        NULL)

    struct adpt_type const *opr, *lhs, *rhs, *base, *off;
    struct adpt_type const *topr, *tlhs, *trhs, *tbase, *toff;

    switch (expr->kind) {
    case EXPR_ATOM:;
        char cref atom = cstokn(expr->info.atom);
        if ('"' == *atom) {
            /* version with interning; removed for now until deemed necessary and done better
            buf data = {
                .ptr= mallox(data.cap = strlen(atom)),
                .len= lex_struqo(data.ptr, data.cap, atom),
            };

            size_t found_at, found_off, str_variable;
            for (found_at = 0; found_at < cs->checker_alloc_interned.len; found_at++) {
                buf cref it = cs->checker_alloc_interned.ptr+found_at;
                char cref first = memchr(it->ptr, data.ptr[0], it->len);
                found_off = first-it->ptr;
                if (first && data.len <= it->len-found_off && !memcmp(first, data.ptr, data.len))
                    break;
            }
            if (found_at < cs->checker_alloc_interned.len) {
                size_t const cs_vsp_back_then = cs->checker_alloc_interned.ptr[found_at].cap;
                str_variable = cs_vsp_back_then+found_off;
                free(data.ptr);
            }

            else {
                // xxx: not pretty; cut down version of _alloc_slot with just the
                // _emit_instr_w_opr for the push then call to _emit_data based on
                // the assumption that compiler.h is included anyways

                cs->vsp-= data.len;

                unsigned count = 1;
                for (size_t it = data.len; it; count++) it>>= 7;

                unsigned char* op = dyarr_insert(&cs->res, cs->res.len, count);
                *op = 0x0f;
                size_t it = data.len;
                do {
                    unsigned char l = it&127;
                    *++op = !!(it>>= 7)<<7 | l;
                } while (it);

                void _emit_data(compile_state ref cs, size_t dst, size_t width, unsigned char const* data);
                _emit_data(cs, 0, data.len, (unsigned char*)data.ptr);

                str_variable = cs->vsp;

                buf ref intern = dyarr_push(&cs->checker_alloc_interned);
                *intern = data;
                // yyy: store the location of the array here, it is retrived
                // when matching a new string literal at some other call. 2:
                // - buffers in `chk_interned` should not be changed
                // - `cs->vsp` should not be 0 so it won't mess with frry
                intern->cap = cs->vsp;
            }*/

            // xxx: may not be enough because of \u and \U it can be longer than the atom
            char* const ptr = mallox(strlen(atom));
            size_t const len = lex_struqo(ptr, strlen(atom), atom);

            // xxx: not pretty; cut down version of _alloc_slot with just the
            // _emit_instr_w_opr for the push then call to _emit_data based on
            // the assumption that compiler.h is included anyways
            cs->vsp-= len;

            unsigned count = 1;
            for (size_t it = len; it; count++) it>>= 7;

            unsigned char* op = dyarr_insert(&cs->res, cs->res.len, count);
            *op = 0x0f;
            size_t it = len;
            do {
                unsigned char l = it&127;
                *++op = !!(it>>= 7)<<7 | l;
            } while (it);

            void _emit_data(compile_state ref cs, size_t dst, size_t width, unsigned char const* data);
            _emit_data(cs, 0, len, (unsigned char*)ptr);

            return expr->usr = (void*)alloc_checked(cs->usr, (struct comp_checked){
                    .type= {
                        .size= sizeof(char*),
                        .align= sizeof(char*),
                        .tyty= ADPT_TYPE_ARR,
                        .info.arr= {
                            .item= &adptb_char_type,
                            .count= len,
                        },
                    },
                    .info.str_variable= cs->vsp,
                });
        }

        // xxx: not supposed to be of type `char` but `int`
        if ('\'' == *atom) return expr->usr = (void*)&adptb_char_type;
        if (('0' <= *atom && *atom <= '9') || '.' == *atom) {
            bool isfp = false; // is fp if has '.' or e/E/p/P
            size_t const len = strlen(atom);
            char const* const isfpif = 2 < len && 'x' == atom[1] ? ".Pp" : ".Ee";
            for (size_t k = 0; atom[k]; k++) if (strchr(isfpif, atom[k])) {
                isfp = true;
                break;
            }
            // f, u, l, ll, ul, lu, llu, ull
            bool const isfloat = !(2 < len && 'x' == atom[1]) && 'f' == (atom[len-1]|32); // not if hex
            bool isunsigned, islong;
            if ((islong = 'l' == (atom[len-1]|32)) || (isunsigned = 'u' == (atom[len-1]|32))) {
                if (islong) isunsigned = 'u' == (atom[len-2-('l' == (atom[len-2]|32))]|32);
                else if (isunsigned) islong = 'l' == (atom[len-2]|32);
            }
            return expr->usr = (void*)( isfloat ? &adptb_float_type
                                      : isfp ? &adptb_double_type
                                      : islong ? (isunsigned ? &adptb_ulong_type : &adptb_long_type)
                                      : isunsigned ? &adptb_uint_type : &adptb_int_type
                                      );
        }

        struct adpt_item cref found = cs->lookup(cs->usr, cstokn(expr->info.atom));
        if (!found) fail("Unknown name: %s", quoted(cstokn(expr->info.atom)));
        if (ADPT_ITEM_TYPEDEF == found->kind) fail("Unexpected type name %s", quoted(cstokn(expr->info.atom)));
        return expr->usr = (void*)found->type;

    case EXPR_COMPLIT:
        // here
        notif("NIY: (checker) compound literal");
        return NULL;

    case EXPR_BINOP_SUBSCR:
        failforward(base, expr->info.subscr.base);
        failforward(off, expr->info.subscr.off);
        tbase = _truetype(base);
        toff = _truetype(off);
        if (!isindir(tbase)) fail_got_type(base, "Base of subscript expression is not of a pointer type");
        if (!isint(toff)) fail_got_type(off, "Offset of subscript expression is not of an integral type");
        return expr->usr = (void*)atindir(tbase);

    case EXPR_BINOP_CALL:
        failforward(base, expr->info.call.base);
        tbase = _truetype(base);
        if (!isfun(tbase)) fail_got_type(base, "Base of call expression is not of a function type");
        size_t k, count = tbase->info.fun.count;
        if (15 < count) fail("NIY: function call with more than 15 arguments");
        struct expr_call_arg const* cons = expr->info.call.first;
        for (k = 0; k < count && cons; k++, cons = cons->next) {
            struct adpt_type cref param = tbase->info.fun.params[k].type;
            struct adpt_type const* arg;
            failforward(arg, cons->expr);
            struct adpt_type cref tparam = _truetype(param);
            struct adpt_type cref targ = _truetype(arg);
            if (!_are_types_compatible(tparam, targ)) fail_got_2types(arg, param, "Argument and parameter %zu are not compatible", k+1);
        }
        if (cons) {
            while (cons) k++, cons = cons->next;
            fail("Too many arguments: %zu provided, expected %zu", k, count);
        }
        if (k < count) fail("Not enough arguments: %zu provided, expected %zu", k+!!cons, tbase->info.fun.count);
        return expr->usr = (void*)tbase->info.fun.ret;

    case EXPR_BINOP_TERNBRANCH:
        fail("Broken tree with dangling ternary branches");
    case EXPR_BINOP_TERNCOND:
        if (EXPR_BINOP_TERNBRANCH != expr->info.binary.rhs->kind) fail("Broken tree with dangling ternary condition");
        failforward(opr, expr->info.binary.lhs); // condition
        topr = _truetype(opr);
        if (!isint(topr)) fail_got_type(opr, "Condition is not of an integral type");
        failforward(lhs, expr->info.binary.rhs->info.binary.lhs); // consequence
        failforward(rhs, expr->info.binary.rhs->info.binary.rhs); // alternative
        tlhs = _truetype(lhs);
        trhs = _truetype(rhs);
        if (isnum(tlhs) && isnum(trhs)) {
            bool const lf = isflt(tlhs), rf = isflt(trhs);
            // (yyy: approximation of implicit conversions' "common real type")
            return expr->usr = (void*)( lf == rf ? (
                                            tlhs->size == trhs->size ? (issgn(tlhs) ? rhs : lhs) :
                                            tlhs->size < trhs->size ? rhs : lhs )
                                      : lf ? lhs : rhs
                                      );
        }
        // xxx: this is quite incorrect but whatever for now
        if (_are_types_compatible(tlhs, trhs)) return expr->usr = (void*)lhs;
        fail_got_2types(lhs, rhs, "Branche values are not compatible");

    case EXPR_BINOP_COMMA:
        failforward(lhs, expr->info.binary.lhs);
        failforward(rhs, expr->info.binary.rhs);
        return expr->usr = (void*)rhs;

    case EXPR_BINOP_ASGN:
    case EXPR_BINOP_ASGN_BOR:
    case EXPR_BINOP_ASGN_BXOR:
    case EXPR_BINOP_ASGN_BAND:
    case EXPR_BINOP_ASGN_BSHL:
    case EXPR_BINOP_ASGN_BSHR:
    case EXPR_BINOP_ASGN_SUB:
    case EXPR_BINOP_ASGN_ADD:
    case EXPR_BINOP_ASGN_REM:
    case EXPR_BINOP_ASGN_DIV:
    case EXPR_BINOP_ASGN_MUL:
        if (!_is_expr_lvalue(cs, expr->info.binary.lhs)) fail("Expression is not assignable");
        failforward(lhs, expr->info.binary.lhs);
        failforward(rhs, expr->info.binary.rhs);
        tlhs = _truetype(lhs);
        trhs = _truetype(rhs);
        switch (expr->kind) {
        case EXPR_BINOP_ASGN_ADD:
        case EXPR_BINOP_ASGN_SUB:
            if (isptr(tlhs) && isint(trhs)) break;
            // fall through
        case EXPR_BINOP_ASGN_DIV:
        case EXPR_BINOP_ASGN_MUL:
            if (!isnum(tlhs) || !isnum(trhs)) fail_got_2types(lhs, rhs, "Both operands are not of an arithmetic type");
            break;
        case EXPR_BINOP_ASGN_BOR:
        case EXPR_BINOP_ASGN_BXOR:
        case EXPR_BINOP_ASGN_BAND:
        case EXPR_BINOP_ASGN_BSHL:
        case EXPR_BINOP_ASGN_BSHR:
        case EXPR_BINOP_ASGN_REM:
            if (!isint(tlhs) || !isint(trhs)) fail_got_2types(lhs, rhs, "Both operands are not of an integral type");
            break;
        default: // (38 cases ><'')
            if (!_are_types_compatible(tlhs, trhs)) fail_got_2types(lhs, rhs, "Operands are not compatible");
        }
        return expr->usr = (void*)lhs;

    case EXPR_BINOP_LOR:
    case EXPR_BINOP_LAND:
    case EXPR_BINOP_EQ:
    case EXPR_BINOP_NE:
    case EXPR_BINOP_LT:
    case EXPR_BINOP_GT:
    case EXPR_BINOP_LE:
    case EXPR_BINOP_GE:
        failforward(lhs, expr->info.binary.lhs);
        failforward(rhs, expr->info.binary.rhs);
        tlhs = _truetype(lhs);
        trhs = _truetype(rhs);
        if ((isnum(tlhs) && isnum(trhs)) ||
                ( (isint(tlhs) || isindir(tlhs)) &&
                  (isint(trhs) || isindir(trhs)) ))
            return expr->usr = (void*)&adptb_int_type;
        fail_got_2types(lhs, rhs, "Values are not comparable");

    case EXPR_BINOP_BOR:
    case EXPR_BINOP_BXOR:
    case EXPR_BINOP_BAND:
    case EXPR_BINOP_BSHL:
    case EXPR_BINOP_BSHR:
    case EXPR_BINOP_REM:
        failforward(lhs, expr->info.binary.lhs);
        failforward(rhs, expr->info.binary.rhs);
        tlhs = _truetype(lhs);
        trhs = _truetype(rhs);
        // (yyy: approximation of implicit conversions' "common real type")
        if (isint(tlhs) && isint(trhs)) return expr->usr = (void*)(
                tlhs->size == trhs->size ? (issgn(tlhs) ? rhs : lhs) :
                tlhs->size < trhs->size ? rhs : lhs );
        fail_got_2types(lhs, rhs, "Both operands are not of an integral type");

    case EXPR_BINOP_SUB:
    case EXPR_BINOP_ADD:
    case EXPR_BINOP_DIV:
    case EXPR_BINOP_MUL:
        failforward(lhs, expr->info.binary.lhs);
        failforward(rhs, expr->info.binary.rhs);
        tlhs = _truetype(lhs);
        trhs = _truetype(rhs);
        if (EXPR_BINOP_SUB == expr->kind || EXPR_BINOP_ADD == expr->kind) {
            if (isint(tlhs)) {
                if (isptr(trhs)) return expr->usr = (void*)rhs;
                if (isarr(trhs)) return expr->usr = (void*)alloc_checked(cs->usr, (struct comp_checked){.type= {
                            .size= sizeof(void*),
                            .align= sizeof(void*),
                            .tyty= ADPT_TYPE_PTR,
                            .info.ptr= trhs->info.arr.item,
                        }});
            }
            if (isint(trhs)) {
                if (isptr(tlhs)) return expr->usr = (void*)tlhs;
                if (isarr(tlhs)) return expr->usr = (void*)alloc_checked(cs->usr, (struct comp_checked){.type= {
                            .size= sizeof(void*),
                            .align= sizeof(void*),
                            .tyty= ADPT_TYPE_PTR,
                            .info.ptr= tlhs->info.arr.item,
                        }});
            }
        } // ptr arith
        if (isnum(tlhs) && isnum(trhs)) {
            bool const lf = isflt(tlhs), rf = isflt(trhs);
            // (yyy: approximation of implicit conversions' "common real type")
            return expr->usr = (void*)( lf == rf ? (
                                            tlhs->size == trhs->size ? (issgn(tlhs) ? rhs : lhs) :
                                            tlhs->size < trhs->size ? rhs : lhs )
                                      : lf ? lhs : rhs
                                      );
        }
        fail_got_2types(lhs, rhs, "Both operands are not of an arithmetic type");

    case EXPR_UNOP_ADDR:
        if (_is_expr_lvalue(cs, expr->info.unary.opr)) {
            failforward(opr, expr->info.unary.opr);
            return expr->usr = (void*)alloc_checked(cs->usr, (struct comp_checked){.type= {
                    .size= sizeof(void*),
                    .align= sizeof(void*),
                    .tyty= ADPT_TYPE_PTR,
                    .info.ptr= opr,
                }});
        }
        fail("Cannot take the address of expression");
    case EXPR_UNOP_DEREF:
        failforward(opr, expr->info.unary.opr);
        topr = _truetype(opr);
        if (isindir(topr)) return expr->usr = (void*)atindir(topr);
        fail_got_type(opr, "Operand is not of a pointer type");

    case EXPR_UNOP_CAST:
        failforward(opr, expr->info.cast.opr);
        struct adpt_type cref tyto = _cast_type(cs, expr->info.cast.type, alloc_checked);
        if (!tyto) fail("Type invalid in cast expression");
        topr = _truetype(opr);
        struct adpt_type cref ttyto = _truetype(tyto);
        // allowed:
        // - void
        // - int <-> ptr
        // - int <-> flt
        // - ptr[obj] <-> ptr[obj]
        // - ptr[fun] <-> ptr[fun]
        if (&adptb_void_type == ttyto ||
                (isnum(ttyto) && isnum(topr)) ||
                (isint(ttyto) && isindir(topr)) || (isptr(ttyto) && isint(topr)) ||
                (isptr(ttyto) && isindir(topr) && isfun(ttyto->info.ptr) == isfun(atindir(topr)))
           ) return expr->usr = (void*)tyto;
        fail_got_2types(opr, tyto, "Operand cannot be casted to this type");

    case EXPR_UNOP_PMEMBER:
    case EXPR_UNOP_MEMBER:
        failforward(opr, expr->info.member.base);
        topr = _truetype(opr);
        if (EXPR_UNOP_MEMBER != expr->kind && !isindir(topr)) fail_got_type(topr, "Operand is not of a pointer type");
        struct adpt_type cref comp = EXPR_UNOP_MEMBER == expr->kind ? topr : atindir(topr);
        if (ADPT_TYPE_STRUCT != comp->tyty && ADPT_TYPE_UNION != comp->tyty) fail("Base of member expression is not a of a structure or union type");
        for (size_t k = 0; k < comp->info.comp.count; k++)
            if (!strcmp(comp->info.comp.fields[k].name, cstokn(expr->info.member.name)))
                return expr->usr = (void*)comp->info.comp.fields[k].type;
        fail("Field %s not found in operand %s type", quoted(cstokn(expr->info.member.name)), ADPT_TYPE_STRUCT == comp->tyty ? "structure" : "union");

    case EXPR_UNOP_BNOT:
    case EXPR_UNOP_LNOT:
        failforward(opr, expr->info.unary.opr);
        topr = _truetype(opr);
        if (isint(topr)) return expr->usr = (void*)(EXPR_UNOP_LNOT == expr->kind ? &adptb_int_type : topr);
        fail_got_type(opr, "Operand is not of an integral type");

    case EXPR_UNOP_MINUS:
    case EXPR_UNOP_PLUS:
        failforward(opr, expr->info.unary.opr);
        topr = _truetype(opr);
        if (isnum(topr)) return expr->usr = (void*)opr;
        fail_got_type(opr, "Operand is not of an arithmetic type");

    case EXPR_UNOP_PRE_DEC:
    case EXPR_UNOP_PRE_INC:
    case EXPR_UNOP_POST_DEC:
    case EXPR_UNOP_POST_INC:
        if (!_is_expr_lvalue(cs, expr->info.unary.opr)) fail("Expression is not assignable");
        failforward(opr, expr->info.unary.opr);
        topr = _truetype(opr);
        if (isnum(topr) || isptr(topr)) return expr->usr = (void*)opr;
        fail_got_type(opr, "Operand is not of an arithmetic type");
    }

    fail("Broken tree with unknown expression kind %d", expr->kind);

#   undef fail_got_2types
#   undef fail_got_type

#   undef atindir
#   undef isindir
#   undef isarr
#   undef isptr
#   undef isfun
#   undef isnum
#   undef isflt
#   undef isint
#   undef failforward
#   undef fail
} // check_expression

#undef cstokn

#endif // CINTRE_CHECKER_H
