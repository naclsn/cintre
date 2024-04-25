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

typedef dyarr(unsigned char) bytecode;

typedef struct compile_state {
    bytecode res;
    size_t vsp;

    void* usr;
    struct adpt_item const* (*lookup)(void* usr, bufsl const name);
} compile_state;

/// does modify the expression by at least adding a typing info in the usr fields
struct adpt_type const* check_expression(compile_state cref cs, expression ref expr);

// check utils {{{
bool _are_types_compatible(struct adpt_type cref dst, struct adpt_type cref src) {
    // yeah no idea either that'll do for now until it actually shows too much
#   define _are_types_same(_d, _s) (  \
        (_d)->tyty == (_s)->tyty &&   \
        (_d)->size == (_s)->size &&   \
        (_d)->align == (_s)->align )
    if (_are_types_same(dst, src)) return true;

    bool const dst_isnum = TYPE_CHAR <= dst->tyty && dst->tyty <= TYPE_DOUBLE;
    bool const src_isnum = TYPE_CHAR <= src->tyty && src->tyty <= TYPE_DOUBLE;
    if (dst_isnum && src_isnum) return true;

    if (TYPE_STRUCT == dst->tyty && TYPE_STRUCT == src->tyty) {
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

    if (TYPE_FUN == dst->tyty || TYPE_FUN == src->tyty) {
        struct adpt_type const* dst_tail = dst;
        while (TYPE_PTR == dst_tail->tyty) dst_tail = dst->info.ptr;
        struct adpt_type const* src_tail = src;
        while (TYPE_PTR == src_tail->tyty) src_tail = src->info.ptr;

        if (TYPE_FUN != dst->tyty || TYPE_FUN != src->tyty ||
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

    if (TYPE_ARR == dst->tyty) return false;

    if (TYPE_PTR == dst->tyty) switch (src->tyty) {
        struct adpt_type const* src_under;
    case TYPE_PTR: src_under = src->info.ptr; if (0)
    case TYPE_ARR: src_under = src->info.arr.item;
        return _are_types_same(dst->info.ptr, src_under) ||
                TYPE_VOID == dst->info.ptr->tyty ||
                TYPE_VOID == src_under->tyty;
    default:;
    }

    return false;
#   undef _are_types_same
}

bool _is_expr_lvalue(expression cref expr) {
    // yeah no idea either that'll do for now until it actually shows too much
    switch (expr->kind) {
    case ATOM:;
        char const c = *expr->info.atom.ptr|32;
        return 'a' <= c && c <= 'z';
    case BINOP_SUBSCR:
    case UNOP_DEREF:
    case UNOP_PMEMBER:
    case UNOP_MEMBER:
        return true;
    default:
        return false;
    }
}
// }}}

struct slot;
void compile_expression(compile_state ref cs, expression cref expr, struct slot ref slot);

struct adpt_type const* check_expression(compile_state cref cs, expression ref expr) {
#   define fail(...)  return notif(__VA_ARGS__), NULL
#   define failforward(id, from)  for (id = check_expression(cs, from); !id; ) fail("here")
#   define isint(__ty)  (TYPE_CHAR <= (__ty)->tyty && (__ty)->tyty <= TYPE_ULONG)
#   define isflt(__ty)  (TYPE_FLOAT <= (__ty)->tyty && (__ty)->tyty <= TYPE_DOUBLE)
#   define isnum(__ty)  (isint(__ty) || isflt(__ty))
#   define isfun(__ty)  (TYPE_FUN == (__ty)->tyty)
#   define isptr(__ty)  (TYPE_PTR == (__ty)->tyty)
#   define isarr(__ty)  (TYPE_ARR == (__ty)->tyty)
#   define isindir(__ty)  (isptr(__ty) || isarr(__ty))
#   define atindir(__ty)  (isptr(__ty) ? (__ty)->info.ptr : (__ty)->info.arr.item)

    struct adpt_type const *opr, *lhs, *rhs, *base, *off;

    switch (expr->kind) {
    case ATOM:;
        char const c = *expr->info.atom.ptr;
        if ('"' == c) {
            static struct adpt_type const string = {
                .size= sizeof(char*),
                .align= sizeof(char*),
                .tyty= TYPE_PTR,
                .info.ptr= &adptb_char_type,
            };
            return expr->usr = (void*)&string;
        }
        if ('\'' == c) return expr->usr = (void*)&adptb_char_type;
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
            return expr->usr = (void*)( isfloat ? &adptb_float_type
                                      : isfp ? &adptb_double_type
                                      : islong ? (isunsigned ? &adptb_ulong_type : &adptb_long_type)
                                      : isunsigned ? &adptb_uint_type : &adptb_int_type
                                      );
        }
        struct adpt_item cref found = cs->lookup(cs->usr, expr->info.atom);
        if (!found) fail("Unknown name: '%.*s'", bufmt(expr->info.atom));
        if (ITEM_TYPEDEF == found->kind) fail("Unexpected type name '%.*s'", bufmt(expr->info.atom));
        return expr->usr = (void*)found->type;

    case BINOP_SUBSCR:
        failforward(base, expr->info.subscr.base);
        failforward(off, expr->info.subscr.off);
        if (!isindir(base)) fail("Base of subscript expression is not of a pointer type");
        if (!isint(off)) fail("Offset of subscript expression is not of an integral type");
        return expr->usr = (void*)atindir(base);

    case BINOP_CALL:
        failforward(base, expr->info.call.base);
        if (!isfun(base)) fail("Base of call expression is not of a function type");
        expression* cons = expr->info.call.args;
        size_t k, count = base->info.fun.count;
        if (15 < count) fail("NIY: function call with more than 15 arguments");
        for (k = 0; k < count && cons; k++) {
            struct adpt_type const* param = base->info.fun.params[count-1-k].type;
            struct adpt_type const* arg;
            if (k+1 == count) {
                if (BINOP_COMMA == cons->kind) {
                    while (k++, BINOP_COMMA == cons->kind) cons = cons->info.binary.lhs;
                    fail("Too many arguments: %zu provided, expected %zu", k, count);
                }
                failforward(arg, cons);
            } else {
                if (BINOP_COMMA != cons->kind) break;
                failforward(arg, cons->info.binary.rhs);
                cons = cons->info.binary.lhs;
            }
            if (!_are_types_compatible(param, arg)) fail("Argument %zu's type cannot be assigned to corresponding parameter", k);
        }
        if (k < count) fail("Not enough arguments: %zu provided, expected %zu", k+!!cons, base->info.fun.count);
        return expr->usr = (void*)base->info.fun.ret;


    case BINOP_TERNCOND:
    case BINOP_TERNBRANCH:
        fail("NIY: ternary");

    case BINOP_COMMA:
        failforward(lhs, expr->info.binary.lhs);
        failforward(rhs, expr->info.binary.rhs);
        return expr->usr = (void*)rhs;

    case BINOP_ASGN:
    case BINOP_ASGN_BOR:
    case BINOP_ASGN_BXOR:
    case BINOP_ASGN_BAND:
    case BINOP_ASGN_BSHL:
    case BINOP_ASGN_BSHR:
    case BINOP_ASGN_SUB:
    case BINOP_ASGN_ADD:
    case BINOP_ASGN_REM:
    case BINOP_ASGN_DIV:
    case BINOP_ASGN_MUL:
        failforward(lhs, expr->info.binary.lhs);
        failforward(rhs, expr->info.binary.rhs);
        // TODO: compatible with op (same as respective normal binop)
        if (!_are_types_compatible(lhs, rhs)) fail("Value's type cannot be assigned to destination");
        if (!_is_expr_lvalue(expr->info.binary.lhs)) fail("Expression is not assignable");
        return expr->usr = (void*)lhs;

    case BINOP_LOR:
    case BINOP_LAND:
    case BINOP_EQ:
    case BINOP_NE:
    case BINOP_LT:
    case BINOP_GT:
    case BINOP_LE:
    case BINOP_GE:
        failforward(lhs, expr->info.binary.lhs);
        failforward(rhs, expr->info.binary.rhs);
        if ((isnum(lhs) || isindir(lhs)) && (isnum(rhs) || isindir(rhs))) // xxx: means can compare ptr and float for no reason..
            return expr->usr = (void*)&adptb_int_type; // yyy
        fail("Values are not comparable");

    case BINOP_BOR:
    case BINOP_BXOR:
    case BINOP_BAND:
    case BINOP_BSHL:
    case BINOP_BSHR:
    case BINOP_REM:
        failforward(lhs, expr->info.binary.lhs);
        failforward(rhs, expr->info.binary.rhs);
        if (isint(lhs) && isint(rhs)) return expr->usr = (void*)lhs; // yyy
        fail("Both operands are not of an integral type");

    case BINOP_SUB:
    case BINOP_ADD:
        failforward(lhs, expr->info.binary.lhs);
        failforward(rhs, expr->info.binary.rhs);
        if (isnum(lhs) && isindir(rhs)) return expr->usr = (void*)rhs; // XXX: forwards the array type :/
        if (isnum(rhs) && isindir(lhs)) return expr->usr = (void*)lhs;
        if (0) {
            // fall through
    case BINOP_DIV:
    case BINOP_MUL:
            failforward(lhs, expr->info.binary.lhs);
            failforward(rhs, expr->info.binary.rhs);
        }
        if (isnum(lhs) && isnum(rhs)) {
            bool const lf = isflt(lhs), rf = isflt(rhs);
            // (yyy: approximation of implicit conversions' "common real type")
            return expr->usr = (void*)( lf == rf ? (lhs->size < rhs->size ? rhs : lhs)
                                      : lf ? lhs : rhs
                                      );
        }
        fail("Both operands are not of an arithmetic type");

    case UNOP_ADDR: {
            fail("NIY: address of");
            static struct adpt_type const voidp = {
                .size= sizeof(void*),
                .align= sizeof(void*),
                .tyty= TYPE_PTR,
                .info.ptr= &adptb_void_type,
            };
            return expr->usr = (void*)&voidp; // XXX: yeah, nahh
        }
        fail("Cannot take the address of expression");
    case UNOP_DEREF:
        failforward(opr, expr->info.unary.opr);
        if (isindir(opr)) return expr->usr = (void*)atindir(opr);
        fail("Operand is not of a pointer type");

    case UNOP_PMEMBER:
        failforward(opr, expr->info.unary.opr);
        if (!isindir(opr)) fail("Operand is not of a pointer type");
        if (0)
    case UNOP_MEMBER:
            failforward(opr, expr->info.unary.opr);
        fail("NIY: member access");

    case UNOP_BNOT:
    case UNOP_LNOT:
        failforward(opr, expr->info.unary.opr);
        if (isint(opr)) return expr->usr = (void*)opr;
        fail("Operand is not of an integral type");

    case UNOP_MINUS:
    case UNOP_PLUS:
        failforward(opr, expr->info.unary.opr);
        if (isnum(opr)) return expr->usr = (void*)opr;
        fail("Operand is not of an arithmetic type");

    case UNOP_PRE_DEC:
    case UNOP_PRE_INC:
    case UNOP_POST_DEC:
    case UNOP_POST_INC:
        failforward(opr, expr->info.unary.opr);
        if (!_is_expr_lvalue(expr->info.unary.opr)) fail("Expression is not assignable");
        if (isnum(opr) || isindir(opr)) return expr->usr = (void*)opr; // XXX: forwards the array type :/
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
} // check_expression

#endif // CINTRE_CHECKER_H
