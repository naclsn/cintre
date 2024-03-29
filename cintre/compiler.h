/// compile to some bytecode

#ifndef CINTRE_COMPILER_H
#define CINTRE_COMPILER_H

#include "common.h"
#include "parser.h"
#include "adapter.h"

typedef dyarr(unsigned char) bytecode;

struct slot {
  struct adpt_type const* /*const*/ ty;
  size_t /*const*/ loc;
  struct _use_info {
      bool used;
      union {
          char c;
          signed char sc;
          signed short ss;
          signed int si;
          signed long sl;
          unsigned char uc;
          unsigned short us;
          unsigned int ui;
          unsigned long ul;
          float f;
          double d;
      } value;
  } info;
};

typedef struct compile_state {
    bytecode res;
    size_t vsp;
    struct adpt_item const* (*lookup)(bufsl const name);
    void (*typehole)(struct adpt_type cref expect);
} compile_state;

struct adpt_type const* check_expression(compile_state ref cs, expression ref expr);
void compile_expression(compile_state ref cs, expression ref expr, struct slot ref slot);

bool compile_expression_tmp_wrap(compile_state ref cs, expression ref expr) {
    //cs->vsp = 0;
    struct slot slot = {.ty= check_expression(cs, expr)};
    if (!slot.ty) return false;

    // yyy: say (because slot.ty is int)
    *dyarr_push(&cs->res) = 0x0f;
    *dyarr_push(&cs->res) = slot.ty->size;
    cs->vsp-= slot.ty->size;
    slot.loc = cs->vsp;

    compile_expression(cs, expr, &slot);
    if (slot.info.used) return true;

    // yyy: say (wrong in many places)
    unsigned char* r = dyarr_insert(&cs->res, cs->res.len, 1+1+1+4);
    if (!r) exitf("OOM");
    r[0] = 0x1d;
    r[1] = slot.loc-cs->vsp;
    r[2] = slot.ty->size;
    memcpy(r+3, (char*)&slot.info.value.ul, slot.ty->size); // (yyy: endianness shortcut)
    slot.info.used = true;
    return true;
}

struct adpt_type const* check_expression(compile_state ref cs, expression ref expr) {
#   define fail(...)  return notif(__VA_ARGS__), NULL
#   define failforward(id, from)  for (id = check_expression(cs, from); !id; ) fail("here")
#   define isint(__ty)  (ADPT_KIND_CHAR <= (__ty)->kind && (__ty)->kind <= ADPT_KIND_ENUM)
#   define isflt(__ty)  (ADPT_KIND_FLOAT <= (__ty)->kind && (__ty)->kind <= ADPT_KIND_DOUBLE)
#   define isnum(__ty)  (isint(__ty) || isflt(__ty))
#   define isptr(__ty)  (ADPT_KIND_PTR == (__ty)->kind)
#   define isfun(__ty)  (ADPT_KIND_FUN == (__ty)->kind)

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
        if ('0' <= c && c <= '9') return &adptb_int_type; // TODO: double and suffixes
        struct adpt_item const* found = cs->lookup(expr->info.atom);
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
        size_t k, count = base->info.fun.count;
        for (k = 0; k < count && cons; k++) {
            struct adpt_type const* arg = base->info.fun.params[count-1-k].type;
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
            // TODO: assignable
        }
        if (k < count) fail("Not enough arguments: %zu provided, expected %zu", k+!!cons, base->info.fun.count);
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
        // TODO: assignable
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
        if (lnum && rnum) return lhs; // yyy
        if (lnum && isptr(rhs)) return rhs;
        if (rnum && isptr(lhs)) return lhs;
        fail("Both operands are not of an arithmetic type");

    case BINOP_REM:
    case BINOP_DIV:
    case BINOP_MUL:
        failforward(lhs, expr->info.binary.lhs);
        failforward(rhs, expr->info.binary.rhs);
        if (isnum(lhs) && isnum(rhs)) return lhs; // yyy
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
        if (isint(opr)) return opr;
        fail("Operand is not of an integral type");

    case UNOP_MINUS:
    case UNOP_PLUS:
        failforward(opr, expr->info.unary.opr);
        if (isnum(opr)) return opr;
        fail("Operand is not of an arithmetic type");

    case UNOP_PRE_DEC:
    case UNOP_PRE_INC:
    case UNOP_POST_DEC:
    case UNOP_POST_INC:
        failforward(opr, expr->info.unary.opr);
        if (isnum(opr) || isptr(opr)) return opr;
        fail("Operand is not of an arithmetic type");
    }

    fail("Broken tree with unknown expression kind %d", expr->kind);

#   undef isfun
#   undef isptr
#   undef isnum
#   undef isflt
#   undef isint
#   undef failforward
#   undef fail
} // typeof

// compile utils {{{
enum _arith_op {
    _ops_add=   0x30, _ops_bor=    0x34,
    _ops_sub=   0x40, _ops_bxor=   0x44,
    _ops_mul=   0x50, _ops_bshl=   0x54,
    _ops_div=   0x60, _ops_bshr=   0x64,
    _ops_rem=   0x70, _ops_band=   0x74,
    _ops_addi=  0x80, _ops_bori=   0x84,
    _ops_subi=  0x90, _ops_bxori=  0x94,
    _ops_muli=  0xa0, _ops_bshli=  0xa4,
    _ops_divi=  0xb0, _ops_bshri=  0xb4,
    _ops_remi=  0xc0, _ops_bandi=  0xc4,
    _ops_rsubi= 0xd0,
    _ops_rdivi= 0xe0, _ops_rbshli= 0xe4,
    _ops_rremi= 0xf0, _ops_rbshri= 0xf4,
};

enum _arith_w {
    _oprw_8,
    _oprw_16,
    _oprw_32,
    _oprw_64,
    _oprw_f= 0xf,
    _oprw_d= 0xd
};

void _emit_arith(bytecode ref res, enum _arith_op fop, enum _arith_w w, size_t d, size_t a, size_t b) {
    unsigned c = 1;
    if (d) for (size_t k = d; k; c++) k>>= 7; else c++;
    if (a) for (size_t k = a; k; c++) k>>= 7; else c++;
    if (b) for (size_t k = b; k; c++) k>>= 7; else c++;
    unsigned char* op = dyarr_insert(res, res->len, c);
    if (!op) exitf("OOM");
    *op = fop + w;
    do { unsigned char k = d&127; *++op = !!(d>>= 7)<<7 | k; } while (d);
    do { unsigned char k = a&127; *++op = !!(a>>= 7)<<7 | k; } while (a);
    do { unsigned char k = b&127; *++op = !!(b>>= 7)<<7 | k; } while (b);
}

enum _arith_w _slot_arith_w(struct slot cref slot) {
    switch (slot->ty->kind) {
    case ADPT_KIND_CHAR:   return _oprw_8;
    case ADPT_KIND_UCHAR:  return _oprw_8;
    case ADPT_KIND_SCHAR:  return _oprw_8;
    case ADPT_KIND_SHORT:  return _oprw_16;
    case ADPT_KIND_INT:    return _oprw_32;
    case ADPT_KIND_LONG:   return _oprw_64;
    case ADPT_KIND_USHORT: return _oprw_16;
    case ADPT_KIND_UINT:   return _oprw_32;
    case ADPT_KIND_ULONG:  return _oprw_64;
    case ADPT_KIND_ENUM:   return _oprw_32;
    case ADPT_KIND_FLOAT:  return _oprw_f;
    case ADPT_KIND_DOUBLE: return _oprw_d;
    default: return 0;
    }
}

size_t _slot_v_bytes(struct slot cref slot) {
    return slot->info.value.ul; // XXX
    //switch (slot->ty->kind) {
    //case ADPT_KIND_CHAR:   return *(size_t*)&slot->info.value.c;
    //case ADPT_KIND_UCHAR:  return *(size_t*)&slot->info.value.uc;
    //case ADPT_KIND_SCHAR:  return *(size_t*)&slot->info.value.sc;
    //case ADPT_KIND_SHORT:  return *(size_t*)&slot->info.value.ss;
    //case ADPT_KIND_INT:    return *(size_t*)&slot->info.value.si;
    //case ADPT_KIND_LONG:   return *(size_t*)&slot->info.value.sl;
    //case ADPT_KIND_USHORT: return *(size_t*)&slot->info.value.us;
    //case ADPT_KIND_UINT:   return *(size_t*)&slot->info.value.ui;
    //case ADPT_KIND_ULONG:  return *(size_t*)&slot->info.value.ul;
    //case ADPT_KIND_ENUM:   return *(size_t*)&slot->info.value.si;
    //case ADPT_KIND_FLOAT:  return *(size_t*)&slot->info.value.f;
    //case ADPT_KIND_DOUBLE: return *(size_t*)&slot->info.value.d;
    //default: return 0;
    //}
}

#define _cfold_arith_unary(dst, op, opr)  \
    do switch ((dst)->ty->kind) {  \
    case ADPT_KIND_CHAR:   (dst)->info.value.c  = op (opr)->info.value.c;  break;  \
    case ADPT_KIND_UCHAR:  (dst)->info.value.uc = op (opr)->info.value.uc; break;  \
    case ADPT_KIND_SCHAR:  (dst)->info.value.sc = op (opr)->info.value.sc; break;  \
    case ADPT_KIND_SHORT:  (dst)->info.value.ss = op (opr)->info.value.ss; break;  \
    case ADPT_KIND_INT:    (dst)->info.value.si = op (opr)->info.value.si; break;  \
    case ADPT_KIND_LONG:   (dst)->info.value.sl = op (opr)->info.value.sl; break;  \
    case ADPT_KIND_USHORT: (dst)->info.value.us = op (opr)->info.value.us; break;  \
    case ADPT_KIND_UINT:   (dst)->info.value.ui = op (opr)->info.value.ui; break;  \
    case ADPT_KIND_ULONG:  (dst)->info.value.ul = op (opr)->info.value.ul; break;  \
    case ADPT_KIND_ENUM:   (dst)->info.value.si = op (opr)->info.value.si; break;  \
    case ADPT_KIND_FLOAT:  (dst)->info.value.f  = op (opr)->info.value.f;  break;  \
    case ADPT_KIND_DOUBLE: (dst)->info.value.d  = op (opr)->info.value.d;  break;  \
    case ADPT_KIND_PTR: return;  \
    default: return;  \
    } while (0)

#define _cfold_arith_binary(dst, op, lhs, rhs)  \
    do switch ((dst)->ty->kind) {  \
    case ADPT_KIND_CHAR:   (dst)->info.value.c  = (lhs)->info.value.c  op (rhs)->info.value.c;  break;  \
    case ADPT_KIND_UCHAR:  (dst)->info.value.uc = (lhs)->info.value.uc op (rhs)->info.value.uc; break;  \
    case ADPT_KIND_SCHAR:  (dst)->info.value.sc = (lhs)->info.value.sc op (rhs)->info.value.sc; break;  \
    case ADPT_KIND_SHORT:  (dst)->info.value.ss = (lhs)->info.value.ss op (rhs)->info.value.ss; break;  \
    case ADPT_KIND_INT:    (dst)->info.value.si = (lhs)->info.value.si op (rhs)->info.value.si; break;  \
    case ADPT_KIND_LONG:   (dst)->info.value.sl = (lhs)->info.value.sl op (rhs)->info.value.sl; break;  \
    case ADPT_KIND_USHORT: (dst)->info.value.us = (lhs)->info.value.us op (rhs)->info.value.us; break;  \
    case ADPT_KIND_UINT:   (dst)->info.value.ui = (lhs)->info.value.ui op (rhs)->info.value.ui; break;  \
    case ADPT_KIND_ULONG:  (dst)->info.value.ul = (lhs)->info.value.ul op (rhs)->info.value.ul; break;  \
    case ADPT_KIND_ENUM:   (dst)->info.value.si = (lhs)->info.value.si op (rhs)->info.value.si; break;  \
    case ADPT_KIND_FLOAT:  (dst)->info.value.f  = (lhs)->info.value.f  op (rhs)->info.value.f;  break;  \
    case ADPT_KIND_DOUBLE: (dst)->info.value.d  = (lhs)->info.value.d  op (rhs)->info.value.d;  break;  \
    case ADPT_KIND_PTR: return;  \
    default: return;  \
    } while (0)

unsigned _l2(size_t n) {
    for (unsigned k = 0; k < 8*sizeof n; k++) {
        if (n&1) return k;
        n>>= 1;
    }
    return -1;
}
// }}}

void compile_expression(compile_state ref cs, expression ref expr, struct slot ref slot) {
    size_t pvsp = cs->vsp;
#   define at(__slt)  ((__slt)->loc - cs->vsp)

    switch (expr->kind) {
    case ATOM:;
        char c = *expr->info.atom.ptr;
        if ('"' == c) {
            slot->info.value.ul = -1;
            //exitf("should have been replaced with its slot during checking");
            return;
        }

        if ('\'' == c) {
            char v = expr->info.atom.ptr[1];
            if ('\\' == v) switch (expr->info.atom.ptr[2]) {
            case '0': v = '\0'; break;
            case'\'': v = '\''; break;
            case '"': v = '\"'; break;
            case '?': v = '\?'; break;
            case'\\': v = '\\'; break;
            case 'a': v = '\a'; break;
            case 'b': v = '\b'; break;
            case 'f': v = '\f'; break;
            case 'n': v = '\n'; break;
            case 'r': v = '\r'; break;
            case 't': v = '\t'; break;
            case 'v': v = '\v'; break;
            }
            slot->info.value.c = v;
            // TODO: conversion to slot->ty
            return;
        }

        if ('0' <= c && c <= '9') {
            slot->info.value.si = atoi(expr->info.atom.ptr);
            // XXX: double and suffixes
            // TODO: conversion to slot->ty
            return;
        }

        //// xxx: remove
        //if (1 == expr->info.atom.len && '_' == c) {
        //    cs->typehole(slot->ty);
        //    return;
        //}

        //struct adpt_item const* found = cs->lookup(expr->info.atom);
        //void* v = found->as.object;
        ////while (isptr(base)) base = base->info.to; // XXX: automatic fun to ptr something something..?
        //slot->ty = found->type;
        return;

    case BINOP_SUBSCR:
        return;

    case BINOP_CALL:
        return;

    case BINOP_TERNCOND:
    case BINOP_TERNBRANCH:
        return;

    case BINOP_COMMA:
        return;

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
        return;

    case BINOP_LOR:
    case BINOP_LAND:
        return;

    case BINOP_EQ:
    case BINOP_NE:
    case BINOP_LT:
    case BINOP_GT:
    case BINOP_LE:
    case BINOP_GE:
        return;

    case BINOP_BOR:
    case BINOP_BXOR:
    case BINOP_BAND:
    case BINOP_BSHL:
    case BINOP_BSHR:
        return;

    case BINOP_SUB:
    case BINOP_ADD:;
        // (TODO: when one is pointer)
        struct slot lhs = *slot, rhs = *slot;
        compile_expression(cs, expr->info.binary.lhs, &lhs);
        if (lhs.info.used) {
            cs->vsp = (cs->vsp-4) & (~(size_t)0<<_l2(4));
            *dyarr_push(&cs->res) = 0x0f;
            *dyarr_push(&cs->res) = pvsp-cs->vsp;
            rhs.loc = cs->vsp;
        }
        compile_expression(cs, expr->info.binary.rhs, &rhs);
        switch (lhs.info.used << 1 | rhs.info.used) {
        case 3: // both used
            _emit_arith(&cs->res,
                    BINOP_SUB == expr->kind ? _ops_sub : _ops_add, _slot_arith_w(slot),
                    at(slot), at(&lhs), at(&rhs));
            slot->info.used = true;
            *dyarr_push(&cs->res) = 0x0d;
            *dyarr_push(&cs->res) = pvsp-cs->vsp;
            cs->vsp = pvsp;
            return;
        case 2: // only lhs used
            cs->res.len-= 2; // yyy: undo push
            cs->vsp = pvsp;
            _emit_arith(&cs->res,
                    BINOP_SUB == expr->kind ? _ops_rsubi : _ops_addi, _slot_arith_w(slot),
                    at(slot), _slot_v_bytes(&rhs), at(&lhs));
            slot->info.used = true;
            return;
        case 1: // only rhs used
            _emit_arith(&cs->res,
                    BINOP_SUB == expr->kind ? _ops_subi : _ops_addi, _slot_arith_w(slot),
                    at(slot), _slot_v_bytes(&lhs), at(&rhs));
            slot->info.used = true;
            return;
        case 0:
            if (BINOP_SUB == expr->kind) _cfold_arith_binary(slot, -, &lhs, &rhs);
            else                         _cfold_arith_binary(slot, +, &lhs, &rhs);
        }
        return;

    case BINOP_REM:
    case BINOP_DIV:
    case BINOP_MUL:
        return;

    case UNOP_ADDR:
    case UNOP_DEREF:
        return;

    case UNOP_PMEMBER:
    case UNOP_MEMBER:
        return;

    case UNOP_BNOT:
    case UNOP_LNOT:
        return;

    case UNOP_MINUS:
    case UNOP_PLUS:
        compile_expression(cs, expr->info.unary.opr, slot);
        if (UNOP_PLUS == expr->kind) return;
        if (!slot->info.used) _cfold_arith_unary(slot, -, slot);
        else _emit_arith(&cs->res, _ops_subi, _slot_arith_w(slot), slot->loc, 0, slot->loc);
        return;

    case UNOP_PRE_DEC:
    case UNOP_PRE_INC:
    case UNOP_POST_DEC:
    case UNOP_POST_INC:
        return;
    }

#   undef at
}

#endif // CINTRE_COMPILER_H
