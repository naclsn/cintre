/// Semantic checker on top of the parser; it is tightly linked with the
/// compiler, checking may itself emit bytecode by calling the compiler for
/// certain expressions (string literals, compound literals); example:
/// ```c
/// ct_compile_state cs = {0};
///
/// struct ct_adpt_type cref ty = ct_check_expression(&cs, expr);
/// if (ty) ct_print_type(stdout, ty);
/// ```

#ifndef CINTRE_CHECKER_H
#define CINTRE_CHECKER_H

#include "common.h"
#include "parser.h"
#include "adapter.h"

typedef struct ct_bytecode {
    unsigned char *ptr;
    size_t len, cap;
} ct_bytecode;

typedef struct ct_compile_state {
    ct_bytecode res;
    size_t vsp;

    void* usr;
    struct ct_adpt_item const* (*lookup)(void* usr, ct_bufsl const name);

    // needed for cases:
    // - ptr decay (arr type to ptr)
    // - address of (ptr around type)
    // - cast (type to itself)
    ct_dyarr(struct ct_adpt_type) chk_work;
    // used to deduplicat string literals
    ct_dyarr(ct_buf) chk_interned;
} ct_compile_state;

/// does modify the expression by at least adding a typing info in the usr fields
struct ct_adpt_type const* ct_check_expression(ct_compile_state ref cs, ct_expression ref expr);

// check utils {{{
bool _ct_are_types_compatible(struct ct_adpt_type cref dst, struct ct_adpt_type cref src)
{
    // yeah no idea either that'll do for now until it actually shows too much
#   define _are_types_same(_d, _s) (  \
        (_d)->tyty == (_s)->tyty &&   \
        (_d)->size == (_s)->size &&   \
        (_d)->align == (_s)->align )
    if (_are_types_same(dst, src)) return true;

    bool const dst_isnum = CT_TYPE_CHAR <= dst->tyty && dst->tyty <= CT_TYPE_DOUBLE;
    bool const src_isnum = CT_TYPE_CHAR <= src->tyty && src->tyty <= CT_TYPE_DOUBLE;
    if (dst_isnum && src_isnum) return true;

    if (CT_TYPE_STRUCT == dst->tyty && CT_TYPE_STRUCT == src->tyty) {
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

    if (CT_TYPE_FUN == dst->tyty || CT_TYPE_FUN == src->tyty) {
        struct ct_adpt_type const* dst_tail = dst;
        while (CT_TYPE_PTR == dst_tail->tyty) dst_tail = dst->info.ptr;
        struct ct_adpt_type const* src_tail = src;
        while (CT_TYPE_PTR == src_tail->tyty) src_tail = src->info.ptr;

        if (CT_TYPE_FUN != dst->tyty || CT_TYPE_FUN != src->tyty ||
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

    if (CT_TYPE_ARR == dst->tyty) return false;

    if (CT_TYPE_PTR == dst->tyty) switch (src->tyty) {
        struct ct_adpt_type const* src_under;
    case CT_TYPE_PTR: src_under = src->info.ptr; if (0)
    case CT_TYPE_ARR: src_under = src->info.arr.item;
        return _are_types_same(dst->info.ptr, src_under) ||
                CT_TYPE_VOID == dst->info.ptr->tyty ||
                CT_TYPE_VOID == src_under->tyty;
    default:;
    }

    return false;
#   undef _are_types_same
}

bool _ct_is_expr_lvalue(ct_expression cref expr)
{
    // yeah no idea either that'll do for now until it actually shows too much
    switch (expr->kind) {
    case CT_ATOM:;
        return isidstart(*expr->info.atom.ptr);
    case CT_BINOP_SUBSCR:
    case CT_UNOP_DEREF:
    case CT_UNOP_PMEMBER:
    case CT_UNOP_MEMBER:
        return true;
    default:
        return false;
    }
}

struct ct_adpt_type const* _ct_cast_type(ct_compile_state ref cs, struct ct_decl_type cref ty)
{
    switch (ty->kind) {
    case CT_KIND_NOTAG:;
        bool _signed = false, _unsigned = false, _short = false, _long = false;
        for (size_t k = 0; CT_QUAL_END != ty->quals[k]; k++) switch (ty->quals[k]) {
            case CT_QUAL_SIGNED:    _signed = true;          break;
            case CT_QUAL_UNSIGNED:  _unsigned = true;        break;
            case CT_QUAL_SHORT:     _short = true;           break;
            case CT_QUAL_LONG:      _long = true;            break;
            case CT_QUAL_COMPLEX:   notif("NIY: complex");   return NULL;
            case CT_QUAL_IMAGINARY: notif("NIY: imaginary"); return NULL;
            default:;
        }

#       define nameis(s)  (strlen(s) == ty->name.len && !memcmp(s, ty->name.ptr, strlen(s)))
        if (nameis("char"))
            return _signed ? &ct_adptb_schar_type
                 : _unsigned ? &ct_adptb_uchar_type
                 : &ct_adptb_char_type;
        if (nameis("int") || !ty->name.len)
            return _short ? (_unsigned ? &ct_adptb_ushort_type : &ct_adptb_short_type)
                 : _long ? (_unsigned ? &ct_adptb_ulong_type : &ct_adptb_long_type)
                 : _unsigned ? &ct_adptb_uint_type : &ct_adptb_int_type;
        if (nameis("float"))  return &ct_adptb_float_type;
        if (nameis("double")) return &ct_adptb_double_type;
        if (nameis("void"))   return &ct_adptb_void_type;
#       undef nameis

        struct ct_adpt_item cref it = cs->lookup(cs->usr, ty->name);
        if (it && CT_ITEM_TYPEDEF == it->kind) {
            struct ct_adpt_type cref ty = _ct_truetype(it->type);
            if (CT_TYPE_VOID <= ty->tyty && ty->tyty <= CT_TYPE_DOUBLE)
                return it->type;
        }
        return NULL;

    case CT_KIND_ENUM: return &ct_adptb_int_type;

    case CT_KIND_PTR:;
        struct ct_adpt_type cref ptr = _ct_cast_type(cs, &ty->info.ptr->type);
        if (!ptr) return NULL;

        return memcpy(dyarr_push(&cs->chk_work), &(struct ct_adpt_type){
                .size= sizeof(void*),
                .align= sizeof(void*),
                .tyty= CT_TYPE_PTR,
                .info.ptr= ptr,
            }, sizeof(struct ct_adpt_type));

    default: return NULL;
    }
}
// }}}

struct ct_slot;
void ct_compile_expression(ct_compile_state ref cs, ct_expression cref expr, struct ct_slot ref ct_slot);

struct ct_adpt_type const* ct_check_expression(ct_compile_state ref cs, ct_expression ref expr)
{
#   define fail(...)  return notif(__VA_ARGS__), NULL
#   define failforward(id, from)  for (id = ct_check_expression(cs, from); !id; ) fail("here")
#   define isint(__ty)  (CT_TYPE_CHAR <= (__ty)->tyty && (__ty)->tyty <= CT_TYPE_ULONG)
#   define issgn(__ty)  (CT_TYPE_SCHAR <= (__ty)->tyty && (__ty)->tyty <= CT_TYPE_LONG)
#   define isflt(__ty)  (CT_TYPE_FLOAT <= (__ty)->tyty && (__ty)->tyty <= CT_TYPE_DOUBLE)
#   define isnum(__ty)  (isint(__ty) || isflt(__ty))
#   define isfun(__ty)  (CT_TYPE_FUN == (__ty)->tyty)
#   define isptr(__ty)  (CT_TYPE_PTR == (__ty)->tyty)
#   define isarr(__ty)  (CT_TYPE_ARR == (__ty)->tyty)
#   define isindir(__ty)  (isptr(__ty) || isarr(__ty))
#   define atindir(__ty)  (isptr(__ty) ? (__ty)->info.ptr : (__ty)->info.arr.item)

    struct ct_adpt_type const *opr, *lhs, *rhs, *base, *off;
    struct ct_adpt_type const *topr, *tlhs, *trhs;

    switch (expr->kind) {
    case CT_ATOM:;
        char const c = *expr->info.atom.ptr;
        if ('"' == c) {
            size_t const len = expr->info.atom.len;
            char cref ptr = expr->info.atom.ptr;
            ct_buf data = {.ptr= ct_mallox(data.cap = len)};
            for (size_t k = 1; k < len-1;)
                data.ptr[data.len++] = '\\' == ptr[k] ? ct_unescape(ptr, len, &k) : ptr[k++];
            data.ptr[data.len++] = '\0';

            size_t found_at, found_off;
            for (found_at = 0; found_at < cs->chk_interned.len; found_at++) {
                ct_buf cref it = cs->chk_interned.ptr+found_at;
                char cref first = memchr(it->ptr, data.ptr[0], it->len);
                found_off = first-it->ptr;
                if (first && data.len <= it->len-found_off && !memcmp(first, data.ptr, data.len))
                    break;
            }
            if (found_at < cs->chk_interned.len) {
                size_t const cs_vsp_back_then = cs->chk_interned.ptr[found_at].cap;
                // (yyy: same discussion as in the new string literal case bellow)
                expr->info.atom.len = cs_vsp_back_then+found_off;
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

                void _ct_emit_data(ct_compile_state ref cs, size_t dst, size_t width, unsigned char const* data);
                _ct_emit_data(cs, 0, data.len, (unsigned char*)data.ptr);

                // yyy: store the location of the array here, it is retrieved
                // by the compiler. 2 things:
                // - atom.ptr is not changed, so ptr[0] is still '"' which the
                //   compiler needs to know what it is dealing with
                // - atom.len is no longer needed because the data has already
                //   been essentially emitted, len "points" to this 'variable'
                expr->info.atom.len = cs->vsp;

                ct_buf ref intern = dyarr_push(&cs->chk_interned);
                *intern = data;
                // yyy: store the location of the array here, it is retrived
                // when matching a new string literal at some other call. 2:
                // - buffers in `chk_interned` should not be changed
                // - `cs->vsp` should not be 0 so it won't mess with frry
                intern->cap = cs->vsp;
            }

            return expr->usr = memcpy(dyarr_push(&cs->chk_work), &(struct ct_adpt_type){
                    .size= sizeof(char*),
                    .align= sizeof(char*),
                    .tyty= CT_TYPE_ARR,
                    .info.arr= {
                        .item= &ct_adptb_char_type,
                        .count= data.len,
                    },
                }, sizeof(struct ct_adpt_type));
        }

        // xxx: not supposed to be of type `char` but `int`
        if ('\'' == c) return expr->usr = (void*)&ct_adptb_char_type;
        if (('0' <= c && c <= '9') || '.' == c) {
            bool isfp = false; // is fp if has '.' or e/E/p/P
            size_t const len = expr->info.atom.len;
            char cref ptr = expr->info.atom.ptr;
            char const* const isfpif = 2 < len && 'x' == ptr[1] ? ".Pp" : ".Ee";
            for (size_t k = 0; k < len; k++) if (strchr(isfpif, ptr[k])) {
                isfp = true;
                break;
            }
            // f, u, l, ll, ul, lu, llu, ull
            bool const isfloat = !(2 < len && 'x' == ptr[1]) && 'f' == (ptr[len-1]|32); // not if hex
            bool isunsigned, islong;
            if ((islong = 'l' == (ptr[len-1]|32)) || (isunsigned = 'u' == (ptr[len-1]|32))) {
                if (islong) isunsigned = 'u' == (ptr[len-2-('l' == (ptr[len-2]|32))]|32);
                else if (isunsigned) islong = 'l' == (ptr[len-2]|32);
            }
            return expr->usr = (void*)( isfloat ? &ct_adptb_float_type
                                      : isfp ? &ct_adptb_double_type
                                      : islong ? (isunsigned ? &ct_adptb_ulong_type : &ct_adptb_long_type)
                                      : isunsigned ? &ct_adptb_uint_type : &ct_adptb_int_type
                                      );
        }

        struct ct_adpt_item cref found = cs->lookup(cs->usr, expr->info.atom);
        if (!found) fail("Unknown name: '%.*s'", bufmt(expr->info.atom));
        if (CT_ITEM_TYPEDEF == found->kind) fail("Unexpected type name '%.*s'", bufmt(expr->info.atom));
        return expr->usr = (void*)found->type;

    case CT_BINOP_SUBSCR:
        failforward(base, expr->info.subscr.base);
        failforward(off, expr->info.subscr.off);
        base = _ct_truetype(base);
        off = _ct_truetype(off);
        if (!isindir(base)) fail("Base of subscript expression is not of a pointer type");
        if (!isint(off)) fail("Offset of subscript expression is not of an integral type");
        return expr->usr = (void*)atindir(base);

    case CT_BINOP_CALL:
        failforward(base, expr->info.call.base);
        base = _ct_truetype(base);
        if (!isfun(base)) fail("Base of call expression is not of a function type");
        size_t k, count = base->info.fun.count;
        if (15 < count) fail("NIY: function call with more than 15 arguments");
        struct ct_expr_call_arg const* cons = expr->info.call.first;
        for (k = 0; k < count && cons; k++, cons = cons->next) {
            struct ct_adpt_type cref param = base->info.fun.params[k].type;
            struct ct_adpt_type const* arg;
            failforward(arg, cons->expr);
            if (!_ct_are_types_compatible(param, arg)) fail("Argument %zu's type cannot be assigned to corresponding parameter", k+1);
        }
        if (cons) {
            while (cons) k++, cons = cons->next;
            fail("Too many arguments: %zu provided, expected %zu", k, count);
        }
        if (k < count) fail("Not enough arguments: %zu provided, expected %zu", k+!!cons, base->info.fun.count);
        return expr->usr = (void*)base->info.fun.ret;

    case CT_BINOP_TERNBRANCH:
        fail("Broken tree with dangling ternary branches");
    case CT_BINOP_TERNCOND:
        if (CT_BINOP_TERNBRANCH != expr->info.binary.rhs->kind) fail("Broken tree with dangling ternary condition");
        failforward(opr, expr->info.binary.lhs); // condition
        opr = _ct_truetype(opr);
        if (!isint(opr)) fail("Condition is not of an integral type");
        failforward(lhs, expr->info.binary.rhs->info.binary.lhs); // consequence
        failforward(rhs, expr->info.binary.rhs->info.binary.rhs); // alternative
        tlhs = _ct_truetype(lhs);
        trhs = _ct_truetype(rhs);
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
        if (_ct_are_types_compatible(tlhs, trhs)) return expr->usr = (void*)lhs;
        fail("Branches are not of compatible types");

    case CT_BINOP_COMMA:
        failforward(lhs, expr->info.binary.lhs);
        failforward(rhs, expr->info.binary.rhs);
        return expr->usr = (void*)rhs;

    case CT_BINOP_ASGN:
    case CT_BINOP_ASGN_BOR:
    case CT_BINOP_ASGN_BXOR:
    case CT_BINOP_ASGN_BAND:
    case CT_BINOP_ASGN_BSHL:
    case CT_BINOP_ASGN_BSHR:
    case CT_BINOP_ASGN_SUB:
    case CT_BINOP_ASGN_ADD:
    case CT_BINOP_ASGN_REM:
    case CT_BINOP_ASGN_DIV:
    case CT_BINOP_ASGN_MUL:
        if (!_ct_is_expr_lvalue(expr->info.binary.lhs)) fail("Expression is not assignable");
        failforward(lhs, expr->info.binary.lhs);
        failforward(rhs, expr->info.binary.rhs);
        tlhs = _ct_truetype(lhs);
        trhs = _ct_truetype(rhs);
        switch (expr->kind) {
        case CT_BINOP_ASGN_ADD:
        case CT_BINOP_ASGN_SUB:
            if (isptr(lhs) && isint(rhs)) break;
            // fall through
        case CT_BINOP_ASGN_DIV:
        case CT_BINOP_ASGN_MUL:
            if (!isnum(rhs) || !isnum(lhs)) fail("Both operands are not of an arithmetic type");
            break;
        case CT_BINOP_ASGN_BOR:
        case CT_BINOP_ASGN_BXOR:
        case CT_BINOP_ASGN_BAND:
        case CT_BINOP_ASGN_BSHL:
        case CT_BINOP_ASGN_BSHR:
        case CT_BINOP_ASGN_REM:
            if (!isint(rhs) || !isint(lhs)) fail("Both operands are not of an integral type");
            break;
        default:
            if (!_ct_are_types_compatible(lhs, rhs)) fail("Value type cannot be assigned to destination");
        }
        return expr->usr = (void*)lhs;

    case CT_BINOP_LOR:
    case CT_BINOP_LAND:
    case CT_BINOP_EQ:
    case CT_BINOP_NE:
    case CT_BINOP_LT:
    case CT_BINOP_GT:
    case CT_BINOP_LE:
    case CT_BINOP_GE:
        failforward(lhs, expr->info.binary.lhs);
        failforward(rhs, expr->info.binary.rhs);
        lhs = _ct_truetype(lhs);
        rhs = _ct_truetype(rhs);
        if ((isnum(lhs) && isnum(rhs)) ||
                ( (isint(lhs) || isindir(lhs)) &&
                  (isint(rhs) || isindir(rhs)) ))
            return expr->usr = (void*)&ct_adptb_int_type;
        fail("Values are not comparable");

    case CT_BINOP_BOR:
    case CT_BINOP_BXOR:
    case CT_BINOP_BAND:
    case CT_BINOP_BSHL:
    case CT_BINOP_BSHR:
    case CT_BINOP_REM:
        failforward(lhs, expr->info.binary.lhs);
        failforward(rhs, expr->info.binary.rhs);
        tlhs = _ct_truetype(lhs);
        trhs = _ct_truetype(rhs);
        // (yyy: approximation of implicit conversions' "common real type")
        if (isint(tlhs) && isint(trhs)) return expr->usr = (void*)(
                tlhs->size == trhs->size ? (issgn(tlhs) ? rhs : lhs) :
                tlhs->size < trhs->size ? rhs : lhs );
        fail("Both operands are not of an integral type");

    case CT_BINOP_SUB:
    case CT_BINOP_ADD:
    case CT_BINOP_DIV:
    case CT_BINOP_MUL:
        failforward(lhs, expr->info.binary.lhs);
        failforward(rhs, expr->info.binary.rhs);
        tlhs = _ct_truetype(lhs);
        trhs = _ct_truetype(rhs);
        if (CT_BINOP_SUB == expr->kind || CT_BINOP_ADD == expr->kind) {
            if (isint(tlhs)) {
                if (isptr(trhs)) return expr->usr = (void*)rhs;
                if (isarr(trhs)) return expr->usr = memcpy(dyarr_push(&cs->chk_work), &(struct ct_adpt_type){
                            .size= sizeof(void*),
                            .align= sizeof(void*),
                            .tyty= CT_TYPE_PTR,
                            .info.ptr= trhs->info.arr.item,
                        }, sizeof(struct ct_adpt_type));
            }
            if (isint(trhs)) {
                if (isptr(tlhs)) return expr->usr = (void*)tlhs;
                if (isarr(tlhs)) return expr->usr = memcpy(dyarr_push(&cs->chk_work), &(struct ct_adpt_type){
                            .size= sizeof(void*),
                            .align= sizeof(void*),
                            .tyty= CT_TYPE_PTR,
                            .info.ptr= tlhs->info.arr.item,
                        }, sizeof(struct ct_adpt_type));
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
        fail("Both operands are not of an arithmetic type");

    case CT_UNOP_ADDR:
        if (_ct_is_expr_lvalue(expr->info.unary.opr)) {
            failforward(opr, expr->info.unary.opr);
            return expr->usr = memcpy(dyarr_push(&cs->chk_work), &(struct ct_adpt_type){
                    .size= sizeof(void*),
                    .align= sizeof(void*),
                    .tyty= CT_TYPE_PTR,
                    .info.ptr= opr,
                }, sizeof(struct ct_adpt_type));
        }
        fail("Cannot take the address of expression");
    case CT_UNOP_DEREF:
        failforward(opr, expr->info.unary.opr);
        opr = _ct_truetype(opr);
        if (isindir(opr)) return expr->usr = (void*)atindir(opr);
        fail("Operand is not of a pointer type");

    case CT_UNOP_CAST:
        failforward(opr, expr->info.cast.opr);
        struct ct_adpt_type cref tyto = _ct_cast_type(cs, expr->info.cast.type);
        if (!tyto) fail("Type invalid in cast expression");
        opr = _ct_truetype(opr);
        struct ct_adpt_type cref ttyto = _ct_truetype(tyto);
        // allowed:
        // - void
        // - int <-> ptr
        // - int <-> flt
        // - ptr[obj] <-> ptr[obj]
        // - ptr[fun] <-> ptr[fun]
        if (&ct_adptb_void_type == ttyto ||
                (isnum(ttyto) && isnum(opr)) ||
                (isint(ttyto) && isindir(opr)) || (isptr(ttyto) && isint(opr)) ||
                (isptr(ttyto) && isindir(opr) && isfun(ttyto->info.ptr) == isfun(atindir(opr)))
           ) return expr->usr = (void*)tyto;
        fail("Operand cannot be casted to this type");

    case CT_UNOP_PMEMBER:
    case CT_UNOP_MEMBER:
        failforward(opr, expr->info.member.base);
        opr = _ct_truetype(opr);
        if (CT_UNOP_MEMBER != expr->kind && !isindir(opr)) fail("Operand is not of a pointer type");
        struct ct_adpt_type cref comp = CT_UNOP_MEMBER == expr->kind ? opr : isptr(opr) ? opr->info.ptr : opr->info.arr.item;
        if (CT_TYPE_STRUCT != comp->tyty && CT_TYPE_UNION != comp->tyty) fail("Base of member expression is not a of a structure or union type");
        for (size_t k = 0; k < comp->info.comp.count; k++)
            if (bufis(*expr->info.member.name, comp->info.comp.fields[k].name))
                return expr->usr = (void*)comp->info.comp.fields[k].type;
        fail("Field '%.*s' not found in operand %s type", bufmt(*expr->info.member.name), CT_TYPE_STRUCT == comp->tyty ? "structure" : "union");

    case CT_UNOP_BNOT:
    case CT_UNOP_LNOT:
        failforward(opr, expr->info.unary.opr);
        topr = _ct_truetype(opr);
        if (isint(topr)) return expr->usr = (void*)(CT_UNOP_LNOT == expr->kind ? &ct_adptb_int_type : topr);
        fail("Operand is not of an integral type");

    case CT_UNOP_MINUS:
    case CT_UNOP_PLUS:
        failforward(opr, expr->info.unary.opr);
        topr = _ct_truetype(opr);
        if (isnum(topr)) return expr->usr = (void*)opr;
        fail("Operand is not of an arithmetic type");

    case CT_UNOP_PRE_DEC:
    case CT_UNOP_PRE_INC:
    case CT_UNOP_POST_DEC:
    case CT_UNOP_POST_INC:
        if (!_ct_is_expr_lvalue(expr->info.unary.opr)) fail("Expression is not assignable");
        failforward(opr, expr->info.unary.opr);
        topr = _ct_truetype(opr);
        if (isnum(topr) || isptr(topr)) return expr->usr = (void*)opr;
        fail("Operand is not of an arithmetic type");
    }

    fail("Broken tree with unknown expression kind %d", expr->kind);

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
} // ct_check_expression

#endif // CINTRE_CHECKER_H
