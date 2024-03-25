/// compile to some bytecode
///
/// jmp, brz
/// push "stallocini"(size_x_count, log2align) 0 +size*count codes -- 1
/// pop (size_x_count)
/// copy (size_x_count) 2 -- 0
/// call ... offsets between arguments
///
/// [f]cvt(log2fromw, log2tow)
///
/// [f]<unop>(log2width) 1 -- 1
/// [f]<binop>(log2width) 2 -- 1
/// unops(6 +1 or 2 later): bnot lnot minus plus dec inc
/// binops(16): eq ne lt gt le ge bor bxor band bshl bshr sub add rem div mul
///
/// hi    lo
/// ........
/// 00000000 -> nop
/// 00000001 -> debug
/// 0000[][] -> push(log2width_of_sxc:2, align:2) (and sxc it at least 01 ie 2 bytes)
/// 0001[][] -> pushi(log2width_of_sxc:2, align:2)
/// 001f[][] -> cvt(f:1, from:2, to:2) -> 4*2 free op codes: same type conversions -> (pop(f=0)/copy(f=1))(log2width_of_sxc:2)
/// 01f[-][] -> unops(f:1, which:3, width:2) -> some free op codes (invalid float ops)
/// 011c[]00 -> (jmp(c=1)/brz(c=0))(log2width_of_sxc:2)
/// 011...01 -> 8 free op codes
/// 1f[--][] -> binop(f:1, which:4, width:2) -> some free op codes (invalid float ops)
/// 11[--]00 -> call(n:4) (limit of 16 arguments)
/// 11....01 -> 16 free op codes -> reserved maybe for >16 arguments
///
/// fcvt8toN will probably be the isanysetN (ie `!!some`)
/// (and fcvt16toN could be ffsN)
///
/// log2width - width is in bytes, so:
///   uchar(u8): 0
///   ushort(u16): 1
///   uint(u32): 2
///   ulong(u64): 3
///
///   float(f32): 2
///   double(f64): 3

#ifndef CINTRE_COMPILER_H
#define CINTRE_COMPILER_H

#include "common.h"
#include "parser.h"
#include "adapter.h"

typedef dyarr(unsigned char) bytecode;

enum _bc_op_un {
    BC_UNOP_BNOT,
    BC_UNOP_LNOT,
    BC_UNOP_MINUS,
    BC_UNOP_BANYS,
    BC_UNOP_DEC,
    BC_UNOP_INC,
};
enum _bc_op_bin {
    BC_BINOP_EQ,
    BC_BINOP_NE,
    BC_BINOP_LT,
    BC_BINOP_GT,
    BC_BINOP_LE,
    BC_BINOP_GE,
    BC_BINOP_BOR,
    BC_BINOP_BXOR,
    BC_BINOP_BAND,
    BC_BINOP_BSHL,
    BC_BINOP_BSHR,
    BC_BINOP_SUB,
    BC_BINOP_ADD,
    BC_BINOP_REM,
    BC_BINOP_DIV,
    BC_BINOP_MUL,
};

struct adpt_type const* compile_expression(bytecode ref res, expression ref expr, struct adpt_item const* lookup(bufsl const name));

struct adpt_type const* compile_expression(bytecode ref res, expression ref expr, struct adpt_item const* lookup(bufsl const name)) {
#   define fail(...)  return notif(__VA_ARGS__), NULL
#   define failforward(id, from)  for (id = compile_expression(res, from, lookup); !id; ) fail("here")
#   define isint(__ty)  (ADPT_KIND_CHAR <= (__ty)->kind && (__ty)->kind <= ADPT_KIND_ENUM)
#   define isflt(__ty)  (ADPT_KIND_FLOAT <= (__ty)->kind && (__ty)->kind <= ADPT_KIND_LONGDOUBLE)
#   define isnum(__ty)  (isint(__ty) || isflt(__ty))
#   define isptr(__ty)  (ADPT_KIND_PTR == (__ty)->kind)
#   define isfun(__ty)  (ADPT_KIND_FUN == (__ty)->kind)

    typedef unsigned char co;
    struct adpt_type const *opr, *lhs, *rhs, *base, *off;

    switch (expr->kind) {
    case ATOM:;
        char c = *expr->info.atom.ptr;
        if ('"' == c) {
            static struct adpt_type string = {
                .size= sizeof(void*),
                .align= sizeof(void*),
                .kind= ADPT_KIND_PTR,
                .info.to= &adptb_char_type,
            };
            return &string;
        }
        if ('\'' == c) return &adptb_char_type;
        if ('0' <= c && c <= '9') {
            co* w = dyarr_insert(res, res->len, 1+1+4);
            if (!w) fail("OOM");
            w[0] = 16/*push*/ | 0/*log2 1*/<<2 | 2/*log2 alignof(int)*/;
            w[1] = 4; /* this '1' above ^ and the 4 below v */
            int v = atoi(expr->info.atom.ptr);
            memcpy(w+2, (char*)&v, 4);
            return &adptb_int_type; // yyy
        }
        struct adpt_item const* found = lookup(expr->info.atom);
        if (!found) fail("Unknown name: '%.*s'", bufmt(expr->info.atom));
        return found->type;

    case BINOP_SUBSCR:
        failforward(base, expr->info.subscr.base);
        failforward(off, expr->info.subscr.off);
        if (!isptr(base)) fail("Base of subscript expression is not of a pointer type");
        if (!isint(off)) fail("Offset of subscript expression is not of an integral type");
        return base->info.to;

    case BINOP_CALL:
        failforward(base, expr->info.call.base);
        if (!isfun(base)) fail("Base of call expression is not of a function type");
        expression* cons = expr->info.call.args;
        size_t k;
        for (k = 0; k < base->info.fun.count && cons; k++) {
            struct adpt_type const* arg;
            if (k+1 == base->info.fun.count) {
                failforward(arg, cons);
                cons = NULL;
            } else {
                if (BINOP_COMMA != cons->kind) fail("Not enough arguments: %zu provided, expected %zu", k+1, base->info.fun.count);
                failforward(arg, cons->info.binary.rhs);
                cons = cons->info.binary.lhs;
            }
            // TODO: check assignable-to: arg to base->info.fun.params[base->info.fun.count-1-k].type
        }
        if (cons) fail("Too many arguments: %zu provided, expected %zu", k, base->info.fun.count);
        return base->info.fun.ret;

    case BINOP_TERNCOND:
    case BINOP_TERNBRANCH:
        fail("NIY: ternary");

    case BINOP_COMMA:
        failforward(lhs, expr->info.binary.lhs);
        failforward(rhs, expr->info.binary.rhs);
        return rhs;

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
        // TODO: check assignable-to: rhs to lhs
        fail("NIY: assignment");

    case BINOP_LOR:
    case BINOP_LAND:
        return &adptb_int_type; // yyy

    case BINOP_EQ:
    case BINOP_NE:
    case BINOP_LT:
    case BINOP_GT:
    case BINOP_LE:
    case BINOP_GE:
        failforward(lhs, expr->info.binary.lhs);
        failforward(rhs, expr->info.binary.rhs);
        if ((isnum(lhs) && isnum(rhs)) || (isptr(lhs) && isptr(rhs))) return &adptb_int_type; // yyy
        fail("Values are not comparable");

    case BINOP_BOR:
    case BINOP_BXOR:
    case BINOP_BAND:
    case BINOP_BSHL:
    case BINOP_BSHR:
        failforward(lhs, expr->info.binary.lhs);
        failforward(rhs, expr->info.binary.rhs);
        if (isint(lhs) && isint(rhs)) return lhs; // yyy
        fail("Both operands are not of an integral type");

    case BINOP_SUB:
    case BINOP_ADD:
        failforward(lhs, expr->info.binary.lhs);
        failforward(rhs, expr->info.binary.rhs);
        bool lnum = isnum(lhs), rnum = isnum(rhs);
        if (lnum && rnum) {
            *dyarr_push(res) = 128/*binop*/ | 0/*int*/<<6 | (BINOP_ADD==expr->kind?BC_BINOP_ADD:BC_BINOP_SUB)<<2 | 2/*log2 sizeof(int)*/;
            return lhs; // yyy
        }
        if (lnum && isptr(rhs)) return rhs;
        if (rnum && isptr(lhs)) return lhs;
        fail("Both operands are not of an arithmetic type");

    case BINOP_REM:
    case BINOP_DIV:
    case BINOP_MUL:
        failforward(lhs, expr->info.binary.lhs);
        failforward(rhs, expr->info.binary.rhs);
        if (isnum(lhs) && isnum(rhs)) {
            *dyarr_push(res) = 128/*binop*/ | 0/*int*/<<6 | (BINOP_MUL==expr->kind?BC_BINOP_MUL:BINOP_DIV==expr->kind?BC_BINOP_DIV:BC_BINOP_REM)<<2 | 2/*log2 sizeof(int)*/;
            return lhs; // yyy
        }
        fail("Both operands are not of an arithmetic type");

    case UNOP_ADDR:
        fail("NIY: address of");
    case UNOP_DEREF:
        failforward(opr, expr->info.unary.opr);
        if (isptr(opr)) return opr->info.to;
        fail("Operand is not of a pointer type");

    case UNOP_PMEMBER:
        failforward(opr, expr->info.unary.opr);
        if (!isptr(opr)) fail("Operand is not of a pointer type");
        if (0)
    case UNOP_MEMBER:
            failforward(opr, expr->info.unary.opr);
        fail("NIY: member access");

    case UNOP_BNOT:
    case UNOP_LNOT:
        failforward(opr, expr->info.unary.opr);
        if (isint(opr)) return opr; // yyy (lnot)
        fail("Operand is not of an integral type");

    case UNOP_MINUS:
    case UNOP_PLUS:
        failforward(opr, expr->info.unary.opr);
        if (isnum(opr)) {
            if (UNOP_MINUS==expr->kind) *dyarr_push(res) = 64/*unop*/ | 0/*int*/<<5 | BC_UNOP_MINUS<<2 | 2/*log2 sizeof(int)*/;
            return opr;
        }
        fail("Operand is not of an arithmetic type");

    case UNOP_PRE_DEC:
    case UNOP_PRE_INC:
    case UNOP_POST_DEC:
    case UNOP_POST_INC:
        failforward(opr, expr->info.unary.opr);
        if (isnum(opr) || isptr(opr)) return opr;
        fail("Operand is not of an arithmetic type");
    }

    return NULL;

#   undef isfun
#   undef isptr
#   undef isnum
#   undef isflt
#   undef isint
#   undef failforward
#   undef fail
}

#endif // CINTRE_COMPILER_H
