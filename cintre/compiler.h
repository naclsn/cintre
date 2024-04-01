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
  size_t /*const*/ end;

  enum {
      _slot_value,
      _slot_used,
      _slot_variable,
  } usage;
  union {
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
      struct slot const* variable; // XXX: say
  } as;
};

typedef struct compile_state {
    bytecode res;
    size_t vsp;
    struct adpt_item const* (*lookup)(bufsl const name);
    void (*typehole)(struct adpt_type cref expect);
} compile_state;

/// does modify the expression by at least adding a typing info (ADPT_KIND) in the usr fields
struct adpt_type const* check_expression(compile_state ref cs, expression ref expr);
/// the expression should have been sieved through `check_expression` first
void compile_expression(compile_state ref cs, expression ref expr, struct slot ref slot);

bool compile_expression_tmp_wrap(compile_state ref cs, expression ref expr) {
    //cs->vsp = 0;
    struct slot slot = {.ty= check_expression(cs, expr)};
    if (!slot.ty) return false;

    *dyarr_push(&cs->res) = 0x0f;
    *dyarr_push(&cs->res) = slot.ty->size;
    cs->vsp-= slot.ty->size;
    slot.loc = cs->vsp;

    compile_expression(cs, expr, &slot);
    if (_slot_used == slot.usage) return true;

    // yyy: say (wrong in many ways)
    unsigned char* r = dyarr_insert(&cs->res, cs->res.len, 1+1+1+4);
    if (!r) exitf("OOM");
    r[0] = 0x1d;
    r[1] = slot.loc-cs->vsp;
    r[2] = slot.ty->size;
    memcpy(r+3, (char*)&slot.as.value.ul, slot.ty->size); // (yyy: endianness shortcut)
    slot.usage = _slot_used;
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
            static struct adpt_type const string = {
                .size= sizeof(char*),
                .align= sizeof(char*),
                .kind= ADPT_KIND_PTR,
                .info.to= &adptb_char_type,
            };
            return expr->usr = (void*)&string;
        }
        if ('\'' == c) return expr->usr = (void*)&adptb_char_type;
        if ('0' <= c && c <= '9') return expr->usr = (void*)&adptb_int_type; // TODO: double and suffixes
        struct adpt_item const* found = cs->lookup(expr->info.atom);
        if (!found) fail("Unknown name: '%.*s'", bufmt(expr->info.atom));
        return expr->usr = (void*)found->type;

    case BINOP_SUBSCR:
        failforward(base, expr->info.subscr.base);
        failforward(off, expr->info.subscr.off);
        if (!isptr(base)) fail("Base of subscript expression is not of a pointer type");
        if (!isint(off)) fail("Offset of subscript expression is not of an integral type");
        return expr->usr = (void*)base->info.to;

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
        // TODO: assignable
        fail("NIY: assignment");

    case BINOP_LOR:
    case BINOP_LAND:
        return expr->usr = (void*)&adptb_int_type; // yyy

    case BINOP_EQ:
    case BINOP_NE:
    case BINOP_LT:
    case BINOP_GT:
    case BINOP_LE:
    case BINOP_GE:
        failforward(lhs, expr->info.binary.lhs);
        failforward(rhs, expr->info.binary.rhs);
        if ((isnum(lhs) && isnum(rhs)) || (isptr(lhs) && isptr(rhs))) return expr->usr = (void*)&adptb_int_type; // yyy
        fail("Values are not comparable");

    case BINOP_BOR:
    case BINOP_BXOR:
    case BINOP_BAND:
    case BINOP_BSHL:
    case BINOP_BSHR:
        failforward(lhs, expr->info.binary.lhs);
        failforward(rhs, expr->info.binary.rhs);
        if (isint(lhs) && isint(rhs)) return expr->usr = (void*)lhs; // yyy
        fail("Both operands are not of an integral type");

    case BINOP_SUB:
    case BINOP_ADD:
        failforward(lhs, expr->info.binary.lhs);
        failforward(rhs, expr->info.binary.rhs);
        bool lnum = isnum(lhs), rnum = isnum(rhs);
        if (lnum && rnum) return expr->usr = (void*)lhs; // yyy
        if (lnum && isptr(rhs)) return expr->usr = (void*)rhs;
        if (rnum && isptr(lhs)) return expr->usr = (void*)lhs;
        fail("Both operands are not of an arithmetic type");

    case BINOP_REM:
    case BINOP_DIV:
    case BINOP_MUL:
        failforward(lhs, expr->info.binary.lhs);
        failforward(rhs, expr->info.binary.rhs);
        if (isnum(lhs) && isnum(rhs)) return expr->usr = (void*)lhs; // yyy
        fail("Both operands are not of an arithmetic type");

    case UNOP_ADDR:
        fail("NIY: address of");
    case UNOP_DEREF:
        failforward(opr, expr->info.unary.opr);
        if (isptr(opr)) return expr->usr = (void*)opr->info.to;
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
        // TODO: assignable
        failforward(opr, expr->info.unary.opr);
        if (isnum(opr) || isptr(opr)) return expr->usr = (void*)opr;
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
unsigned _l2(size_t n) {
    for (unsigned k = 0; k < 8*sizeof n; k++) {
        if (n&1) return k;
        n>>= 1;
    }
    return -1;
}

#define _emit_instr_w_opr(__code, ...) do {                          \
    size_t const arr[] = {__VA_ARGS__};                              \
                                                                     \
    unsigned count = 1;                                              \
    for (size_t k = 0; k < countof(arr); k++) {                      \
        if (arr[k]) for (size_t it = arr[k]; it; count++) it>>= 7;   \
        else count++;                                                \
    }                                                                \
                                                                     \
    unsigned char* op = dyarr_insert(&cs->res, cs->res.len, count);  \
    if (!op) exitf("OOM");                                           \
    *op = __code;                                                    \
                                                                     \
    for (size_t k = 0; k < countof(arr); k++) {                      \
        size_t it = arr[k];                                          \
        do {                                                         \
            unsigned char l = it&127;                                \
            *++op = !!(it>>= 7)<<7 | l;                              \
        } while (it);                                                \
    }                                                                \
} while (0)

void _alloc_slot(compile_state ref cs, struct slot ref slot) {
    slot->end = cs->vsp;
    cs->vsp = (cs->vsp - slot->ty->size) & (~(size_t)0<<_l2(slot->ty->size));
    _emit_instr_w_opr(0x0f, slot->end - cs->vsp);
    slot->loc = cs->vsp;
}

void _rewind_slot(compile_state ref cs, struct slot ref slot) {
    _emit_instr_w_opr(0x0d, slot->end - cs->vsp);
    cs->vsp = slot->end;
}

void _emit_data(compile_state ref cs, size_t dst, size_t width, unsigned char const* data) {
    _emit_instr_w_opr(0x1d, dst, width);
    unsigned char* dt = dyarr_insert(&cs->res, cs->res.len, width);
    if (!dt) exitf("OOM");
    memcpy(dt, data, width);
}

void _emit_move(compile_state ref cs, size_t dst, size_t width, size_t src) {
    _emit_instr_w_opr(0x1f, dst, width, src);
}

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

void _emit_arith(compile_state ref cs, enum _arith_op aop, enum _arith_w w, size_t dst, size_t a, size_t b) {
    _emit_instr_w_opr(aop+w, dst, a, b);
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
    return slot->as.value.ul; // XXX
    //switch (slot->ty->kind) {
    //case ADPT_KIND_CHAR:   return *(size_t*)&slot->value.c;
    //case ADPT_KIND_UCHAR:  return *(size_t*)&slot->value.uc;
    //case ADPT_KIND_SCHAR:  return *(size_t*)&slot->value.sc;
    //case ADPT_KIND_SHORT:  return *(size_t*)&slot->value.ss;
    //case ADPT_KIND_INT:    return *(size_t*)&slot->value.si;
    //case ADPT_KIND_LONG:   return *(size_t*)&slot->value.sl;
    //case ADPT_KIND_USHORT: return *(size_t*)&slot->value.us;
    //case ADPT_KIND_UINT:   return *(size_t*)&slot->value.ui;
    //case ADPT_KIND_ULONG:  return *(size_t*)&slot->value.ul;
    //case ADPT_KIND_ENUM:   return *(size_t*)&slot->value.si;
    //case ADPT_KIND_FLOAT:  return *(size_t*)&slot->value.f;
    //case ADPT_KIND_DOUBLE: return *(size_t*)&slot->value.d;
    //default: return 0;
    //}
}

//#   define at(__slt)  ((_slot_variable == (__slt)->usage ? (__slt)->as.variable : (__slt))->loc - cs->vsp)
#   define at(__slt)  ((__slt)->loc - cs->vsp)
void _emit_extend(compile_state ref cs, struct slot cref big_dst, struct slot cref small_src) {
    // XXX: sign/zero
    *dyarr_push(&cs->res) = _l2(small_src->ty->size)<<4 | _l2(big_dst->ty->size);
    *dyarr_push(&cs->res) = at(big_dst); // yyy: dst
    *dyarr_push(&cs->res) = at(small_src); // yyy: src
}

#define _cfold_arith_ints(dst, lhs, op, rhs)  \
    do switch ((dst)->ty->kind) {  \
    case ADPT_KIND_CHAR:   (dst)->as.value.c  = (lhs)->as.value.c  op (rhs)->as.value.c;  break;  \
    case ADPT_KIND_UCHAR:  (dst)->as.value.uc = (lhs)->as.value.uc op (rhs)->as.value.uc; break;  \
    case ADPT_KIND_SCHAR:  (dst)->as.value.sc = (lhs)->as.value.sc op (rhs)->as.value.sc; break;  \
    case ADPT_KIND_SHORT:  (dst)->as.value.ss = (lhs)->as.value.ss op (rhs)->as.value.ss; break;  \
    case ADPT_KIND_INT:    (dst)->as.value.si = (lhs)->as.value.si op (rhs)->as.value.si; break;  \
    case ADPT_KIND_LONG:   (dst)->as.value.sl = (lhs)->as.value.sl op (rhs)->as.value.sl; break;  \
    case ADPT_KIND_USHORT: (dst)->as.value.us = (lhs)->as.value.us op (rhs)->as.value.us; break;  \
    case ADPT_KIND_UINT:   (dst)->as.value.ui = (lhs)->as.value.ui op (rhs)->as.value.ui; break;  \
    case ADPT_KIND_ULONG:  (dst)->as.value.ul = (lhs)->as.value.ul op (rhs)->as.value.ul; break;  \
    case ADPT_KIND_ENUM:   (dst)->as.value.si = (lhs)->as.value.si op (rhs)->as.value.si; break;  \
    default:;  \
    } while (0)

#define _cfold_arith(dst, lhs, op, rhs)  \
    do switch ((dst)->ty->kind) {  \
    case ADPT_KIND_CHAR:   (dst)->as.value.c  = (lhs)->as.value.c  op (rhs)->as.value.c;  break;  \
    case ADPT_KIND_UCHAR:  (dst)->as.value.uc = (lhs)->as.value.uc op (rhs)->as.value.uc; break;  \
    case ADPT_KIND_SCHAR:  (dst)->as.value.sc = (lhs)->as.value.sc op (rhs)->as.value.sc; break;  \
    case ADPT_KIND_SHORT:  (dst)->as.value.ss = (lhs)->as.value.ss op (rhs)->as.value.ss; break;  \
    case ADPT_KIND_INT:    (dst)->as.value.si = (lhs)->as.value.si op (rhs)->as.value.si; break;  \
    case ADPT_KIND_LONG:   (dst)->as.value.sl = (lhs)->as.value.sl op (rhs)->as.value.sl; break;  \
    case ADPT_KIND_USHORT: (dst)->as.value.us = (lhs)->as.value.us op (rhs)->as.value.us; break;  \
    case ADPT_KIND_UINT:   (dst)->as.value.ui = (lhs)->as.value.ui op (rhs)->as.value.ui; break;  \
    case ADPT_KIND_ULONG:  (dst)->as.value.ul = (lhs)->as.value.ul op (rhs)->as.value.ul; break;  \
    case ADPT_KIND_ENUM:   (dst)->as.value.si = (lhs)->as.value.si op (rhs)->as.value.si; break;  \
    case ADPT_KIND_FLOAT:  (dst)->as.value.f  = (lhs)->as.value.f  op (rhs)->as.value.f;  break;  \
    case ADPT_KIND_DOUBLE: (dst)->as.value.d  = (lhs)->as.value.d  op (rhs)->as.value.d;  break;  \
    case ADPT_KIND_PTR: exitf("NIY: pointer arithmetics");  \
    default:;  \
    } while (0)
// }}}

void compile_expression(compile_state ref cs, expression ref expr, struct slot ref slot) {
    size_t plen = cs->res.len;
    size_t pvsp = cs->vsp;

    switch (expr->kind) {
    case ATOM:;
        char c = *expr->info.atom.ptr;
        if ('"' == c) {
            slot->as.variable = (void*)-1;
            slot->usage = _slot_variable;
            exitf("NYI: string literal (should have been replaced with its slot during checking)");
            // XXX: nah- it's gonna need to be a pointer proper, be it on the stack or not
            //      SO, will eventually need an instruction to write sp to a slot
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
#           ifdef NO_CFOLD
            _emit_data(cs, at(slot), sizeof v, (unsigned char*)&v);
            slot->usage = _slot_used;
#           else
            slot->as.value.c = v;
            slot->usage = _slot_value;
#           endif
            return;
        }

        if ('0' <= c && c <= '9') {
            int v = atoi(expr->info.atom.ptr);
#           ifdef NO_CFOLD
            _emit_data(cs, at(slot), sizeof v, (unsigned char*)&v); // (yyy: endianness shortcut)
            slot->usage = _slot_used;
#           else
            slot->as.value.si = v;
            // XXX: double and suffixes
            slot->usage = _slot_value;
#           endif
            return;
        }

        //struct adpt_item const* found = cs->lookup(expr->info.atom);
        //void* v = found->as.object;
        ////while (isptr(base)) base = base->info.to; // XXX: automatic fun to ptr something something..?
        //slot->ty = found->type;
        return;

    case BINOP_SUBSCR:
        return;

    case BINOP_CALL:;
        /*
        struct slot fun = {.ty= &(struct adpt_type){.kind= ADPT_KIND_FUN},};
        _alloc_slot(cs, &fun); //.end = cs->vsp; cs->vsp = blabla; .loc = cs->vsp;
        compile_expression(cs, expr->info.call.base, &fun);

        if (_slot_value == fun.info.usage)
            _emit_value(cs, &fun);
        else if (_slot_variable == fun.info.usage) {
            _rewind_slot(cs, &fun); //cs->vsp = (&fun)->end;
            fun = *fun.info.as.variable;
        }

        _emit_call_base(cs, at(slot), at(&fun));

        expression* cons = expr->info.call.args;
        size_t k, count = fun.ty->info.fun.count;
        for (k = 0; k < count; k++) {
            struct slot arg = {.ty= fun.ty->info.fun.params[count-1-k].type};
            _alloc_slot(cs, &arg);
            if (k+1 == count) {
                compile_expression(cs, cons, &arg);
            } else {
                compile_expression(cs, cons->info.binary.rhs, &arg);
                cons = cons->info.binary.lhs;
            }

            if (_slot_value == arg.info.usage)
                _emit_value(cs, &arg);
            else if (_slot_variable == arg.info.usage) {
                _rewind_slot(cs, &arg);
                arg = *arg.info.as.variable;
            }

            _emit_call_arg(cs, at(&arg));
        }

        _rewind_slot(cs, &fun); //cs->vsp = pvsp;
        */
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
    case BINOP_ADD:
    case BINOP_REM:
    case BINOP_DIV:
    case BINOP_MUL: {
            // TODO: so there is a lot of room for improvement and less moving the data around
            //       but this was to get off the ground with a straightforward version
            struct adpt_type const* lhs_ty = expr->info.binary.lhs->usr;
            struct adpt_type const* rhs_ty = expr->info.binary.rhs->usr;

            // figure out in which type is the calculation performed in
            // TODO: no floating points for now
            // (yyy: gross approximation of implicit conversions' "common real type" with ints only)
            struct adpt_type const* const ty = lhs_ty->size < rhs_ty->size ? rhs_ty : lhs_ty;
            struct slot res = {.ty= ty};
            _alloc_slot(cs, &res); // res in the comon type

            struct slot lr[2] = {0};
            for (size_t k = 0; k < 2; k++) {
                struct adpt_type const* const xhs_ty = !k ? lhs_ty : rhs_ty;
                // push and do xhs
                struct slot xhs = {.ty= xhs_ty};
                _alloc_slot(cs, &xhs);
                compile_expression(cs, !k ? expr->info.binary.lhs : expr->info.binary.rhs, &xhs);
                // (todo: if _slot_value, if _slot_variable)
                lr[k].ty = ty;
                if (xhs_ty->size < ty->size) {
                    // then (if needed) push and cvt xhs
                    _alloc_slot(cs, lr+k);
                    _emit_extend(cs, lr+k, &xhs);
                } else lr[k] = xhs;
            }

            // do the operation
            switch (expr->kind) {
            case BINOP_SUB: _emit_arith(cs, _ops_sub, _slot_arith_w(&res), at(&res), at(lr+0), at(lr+1)); break;
            case BINOP_ADD: _emit_arith(cs, _ops_add, _slot_arith_w(&res), at(&res), at(lr+0), at(lr+1)); break;
            case BINOP_REM: _emit_arith(cs, _ops_rem, _slot_arith_w(&res), at(&res), at(lr+0), at(lr+1)); break;
            case BINOP_DIV: _emit_arith(cs, _ops_div, _slot_arith_w(&res), at(&res), at(lr+0), at(lr+1)); break;
            case BINOP_MUL: _emit_arith(cs, _ops_mul, _slot_arith_w(&res), at(&res), at(lr+0), at(lr+1)); break;
            default:;
            }

            // cvt copy into slot
            if (ty->size < slot->ty->size)
                _emit_extend(cs, slot, &res);
            else
                _emit_move(cs, at(slot), slot->ty->size, at(&res));
            slot->usage = _slot_used;

            _rewind_slot(cs, &res);
        }
        return;

    case UNOP_ADDR:
    case UNOP_DEREF:
        return;

    case UNOP_PMEMBER:
    case UNOP_MEMBER:
        return;

    case UNOP_BNOT:
    case UNOP_LNOT:
    case UNOP_MINUS:
    case UNOP_PLUS:
        // remark: the destination slot's type should already match the operand's
        // ie we don't worry about slot width nor conversions here
        compile_expression(cs, expr->info.unary.opr, slot);

        switch (slot->usage) {
        case _slot_value:
            switch (expr->kind) {
            case UNOP_BNOT: _cfold_arith_ints(slot, &(struct slot){0}, |~, slot); break;
            case UNOP_LNOT: _cfold_arith(slot, &(struct slot){0}, ||!, slot); break;
            case UNOP_MINUS: _cfold_arith(slot, &(struct slot){0}, -, slot); break;
            case UNOP_PLUS: break;
            default:;
            }
            break;

        case _slot_used:
            if (UNOP_MINUS == expr->kind) _emit_arith(cs, _ops_subi, _slot_arith_w(slot), at(slot), 0, at(slot));
            break;

        case _slot_variable:
            if (UNOP_MINUS == expr->kind) _emit_arith(cs, _ops_subi, _slot_arith_w(slot), at(slot), 0, at(slot->as.variable));
            else if (UNOP_PLUS == expr->kind) _emit_move(cs, at(slot), slot->ty->size, at(slot->as.variable));
            slot->usage = _slot_used;
        }
        return;

    case UNOP_PRE_DEC:
    case UNOP_PRE_INC:
    case UNOP_POST_DEC:
    case UNOP_POST_INC:
        return;
    }

#   undef at
} // compile_expression

#endif // CINTRE_COMPILER_H
