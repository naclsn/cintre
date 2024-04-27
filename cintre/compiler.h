/// Bytecode compiler on top of the parser; example:
/// (interface is being worked on..)
/// ```c
/// compile_state cs = {0};
///
/// struct slot slot = {.ty= check_expression(&cs, expr)};
/// if (slot.ty) {
///     _alloc_slot(&cs, &slot);
///
///     compile_expression(&cs, expr, &slot);
///     switch (slot.usage) { ... }
///
///     cs->res; // this is the bytecode
/// }
/// ```
///
/// A "slot" represents an expression's result "at runtime"; it consists of:
///
/// - a type (the type of the expression) mainly needed for its size and
///   alignment (the real slot may be larger than the type size when a slot is
///   being recycled for 'optimisation')
///
/// - a location on the runtime stack (end and codeat should be internal only)
///
/// - a "usage" which should be:
///   * _slot_value: the slot is not used, and the value was computed at
///     compile-time (found in `as.value.<ty>`)
///   * _slot_used: the slot is used, that is a sequence of instruction was
///     emitted that writes to the slot's stack location
///   * _slot_variable: the slot is not used, the value will be found (at
///     runtime) at the location `as.variable`

#ifndef CINTRE_COMPILER_H
#define CINTRE_COMPILER_H

#include "common.h"
#include "parser.h"
#include "adapter.h"
#include "checker.h"

// yyy: the `compile_state` is actually defined in "checker.h"

struct slot {
  struct adpt_type const* /*const*/ ty;

  size_t /*const*/ loc; // location of the slot on the stack, of alignment `ty->align` and size at least `ty->size`
  size_t /*const*/ end; // end of the slot, so real size would be `end - loc`
  size_t /*const*/ codeat; // len of code at time of stack allocation, ie where the `push <sz>` instruction is

  enum {
      //_slot_broken,
      _slot_value= 1,
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
          unsigned char bytes[8]; // (yyy: sizeof @This() -- assumes 64b)
      } value;
      size_t variable;
  } as;
};

/// the expression should have been sieved through `check_expression` first
void compile_expression(compile_state ref cs, expression cref expr, struct slot ref slot);

// ---

// compile utils {{{
#define at(__slt)  ((__slt)->loc - cs->vsp)
#define atv(__slt) ((__slt)->as.variable - cs->vsp)

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
    slot->codeat = cs->res.len;
    slot->end = cs->vsp;
    if (slot->ty->size) {
        cs->vsp = (cs->vsp - slot->ty->size) & (~(size_t)0<<_l2(slot->ty->size));
        _emit_instr_w_opr(0x0f, slot->end - cs->vsp);
    }
    slot->loc = cs->vsp;
}

void _cancel_slot(compile_state ref cs, struct slot cref slot) {
    cs->res.len = slot->codeat;
}

void _rewind_slot(compile_state ref cs, struct slot cref slot) {
    if (slot->end != cs->vsp)
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

void _emit_write(compile_state ref cs, size_t ptr_dst_slt, struct slot cref slot_src) {
    _emit_instr_w_opr(0x2f, ptr_dst_slt, slot_src->ty->size, at(slot_src));
}

void _emit_read(compile_state ref cs, size_t ptr_src_slt, struct slot cref slot_dst) {
    _emit_instr_w_opr(0x2f, ptr_src_slt, slot_dst->ty->size, at(slot_dst));
}

//void _emit_slot_value(compile_state ref cs, struct slot cref slot) {
//    _emit_data(cs, at(slot), slot->ty->size, (unsigned char*)&slot->as.value.ul);
//}

void _emit_call_base(compile_state ref cs, unsigned argc, size_t ret, size_t fun) {
    _emit_instr_w_opr(argc<<4 | 0xc, ret, fun);
}

void _emit_call_arg(compile_state ref cs, size_t argv) {
    unsigned count = 0;
    if (argv) for (size_t it = argv; it; count++) it>>= 7;
    else count = 1;

    unsigned char* w = dyarr_insert(&cs->res, cs->res.len, count);
    if (!w) exitf("OOM");

    do {
        unsigned char l = argv&127;
        *(w++) = !!(argv>>= 7)<<7 | l;
    } while (argv);
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
    switch (slot->ty->tyty) {
    case TYPE_CHAR:   return _oprw_8;
    case TYPE_UCHAR:  return _oprw_8;
    case TYPE_SCHAR:  return _oprw_8;
    case TYPE_SHORT:  return _oprw_16;
    case TYPE_INT:    return _oprw_32;
    case TYPE_LONG:   return _oprw_64;
    case TYPE_USHORT: return _oprw_16;
    case TYPE_UINT:   return _oprw_32;
    case TYPE_ULONG:  return _oprw_64;
    case TYPE_FLOAT:  return _oprw_f;
    case TYPE_DOUBLE: return _oprw_d;
    default: return 0;
    }
}

size_t _slot_arith_v(struct slot cref slot) {
    switch (slot->ty->tyty) {
    case TYPE_CHAR:   return slot->as.value.c;
    case TYPE_UCHAR:  return slot->as.value.uc;
    case TYPE_SCHAR:  return slot->as.value.sc;
    case TYPE_SHORT:  return slot->as.value.ss;
    case TYPE_INT:    return slot->as.value.si;
    case TYPE_LONG:   return slot->as.value.sl;
    case TYPE_USHORT: return slot->as.value.us;
    case TYPE_UINT:   return slot->as.value.ui;
    case TYPE_ULONG:  return slot->as.value.ul;
    case TYPE_FLOAT:  return slot->as.value.f;
    case TYPE_DOUBLE: return slot->as.value.d;
    default: return 0;
    }
}

void _emit_extend(compile_state ref cs, struct slot cref big_dst, struct slot cref small_src) {
    // unsigned -> unsigned: zero extend
    // signed   -> signed:   sign extend
    // unsigned -> signed:   zero extend
    // signed   -> unsigned: sign extend

    unsigned char ref b = dyarr_insert(&cs->res, cs->res.len, 3);

    bool const issigned = TYPE_SCHAR == small_src->ty->tyty || TYPE_SHORT == small_src->ty->tyty || TYPE_INT == small_src->ty->tyty || TYPE_LONG == small_src->ty->tyty;

    b[0] = _l2(small_src->ty->size)<<4 | _l2(big_dst->ty->size) | !issigned<<3;
    b[1] = at(big_dst);
    b[2] = at(small_src);
}

#define _cfold_arith_ints(dst, lhs, op, rhs)  \
    do switch ((dst)->ty->tyty) {  \
    case TYPE_CHAR:   (dst)->as.value.c  = (lhs)->as.value.c  op (rhs)->as.value.c;  break;  \
    case TYPE_UCHAR:  (dst)->as.value.uc = (lhs)->as.value.uc op (rhs)->as.value.uc; break;  \
    case TYPE_SCHAR:  (dst)->as.value.sc = (lhs)->as.value.sc op (rhs)->as.value.sc; break;  \
    case TYPE_SHORT:  (dst)->as.value.ss = (lhs)->as.value.ss op (rhs)->as.value.ss; break;  \
    case TYPE_INT:    (dst)->as.value.si = (lhs)->as.value.si op (rhs)->as.value.si; break;  \
    case TYPE_LONG:   (dst)->as.value.sl = (lhs)->as.value.sl op (rhs)->as.value.sl; break;  \
    case TYPE_USHORT: (dst)->as.value.us = (lhs)->as.value.us op (rhs)->as.value.us; break;  \
    case TYPE_UINT:   (dst)->as.value.ui = (lhs)->as.value.ui op (rhs)->as.value.ui; break;  \
    case TYPE_ULONG:  (dst)->as.value.ul = (lhs)->as.value.ul op (rhs)->as.value.ul; break;  \
    default:;  \
    } while (0)

#define _cfold_arith(dst, lhs, op, rhs)  \
    do switch ((dst)->ty->tyty) {  \
    case TYPE_CHAR:   (dst)->as.value.c  = (lhs)->as.value.c  op (rhs)->as.value.c;  break;  \
    case TYPE_UCHAR:  (dst)->as.value.uc = (lhs)->as.value.uc op (rhs)->as.value.uc; break;  \
    case TYPE_SCHAR:  (dst)->as.value.sc = (lhs)->as.value.sc op (rhs)->as.value.sc; break;  \
    case TYPE_SHORT:  (dst)->as.value.ss = (lhs)->as.value.ss op (rhs)->as.value.ss; break;  \
    case TYPE_INT:    (dst)->as.value.si = (lhs)->as.value.si op (rhs)->as.value.si; break;  \
    case TYPE_LONG:   (dst)->as.value.sl = (lhs)->as.value.sl op (rhs)->as.value.sl; break;  \
    case TYPE_USHORT: (dst)->as.value.us = (lhs)->as.value.us op (rhs)->as.value.us; break;  \
    case TYPE_UINT:   (dst)->as.value.ui = (lhs)->as.value.ui op (rhs)->as.value.ui; break;  \
    case TYPE_ULONG:  (dst)->as.value.ul = (lhs)->as.value.ul op (rhs)->as.value.ul; break;  \
    case TYPE_FLOAT:  (dst)->as.value.f  = (lhs)->as.value.f  op (rhs)->as.value.f;  break;  \
    case TYPE_DOUBLE: (dst)->as.value.d  = (lhs)->as.value.d  op (rhs)->as.value.d;  break;  \
    default:;  \
    } while (0)

/// makes a slot "pysical", ie if it was a value, it will be inserted as data
/// and marked as used (but if it was a variable, it will stay as is)
void _materialize_slot(compile_state ref cs, struct slot ref slot) {
    if (_slot_value == slot->usage) {
        _emit_data(cs, at(slot), slot->ty->size, slot->as.value.bytes); // (yyy: endianness)
        slot->usage = _slot_used;
    }
}

/// fits an expression to a slot by compiling into it with any necessary
/// conversion step (allocates and de-allocates a slot if needed)
void _fit_expr_to_slot(compile_state ref cs, expression cref expr, struct slot ref slot) {
    // so we have `slot` and `expr` of two types
    // compatibility should already be ensured by `check_expression`

    // ideal case when they have the same type is just compile the expr w/ res into the slot
    // otherwise ptr and num conversions must take place:
    // - ptr -> ptr is whever
    // - int -> int:
    //   - if the slot is wider then it can still be emit into it, conversion in-place (zero/sign extend)
    //   - otherwise truncating cvt doesn't requier an other slot, conversion in-place (because little endian)
    // - float -> int, int -> float, float -> float: ...

    // after compiling the expression, either
    // - the slot is used in which case conversion steps in-place
    // - the slot is a variable, a widening conversion will need to copy
    // - the slot is a value, compile-time conversion

    bool const is_to_int = TYPE_CHAR <= slot->ty->tyty && slot->ty->tyty <= TYPE_ULONG;
    bool const is_to_flt = TYPE_FLOAT <= slot->ty->tyty && slot->ty->tyty <= TYPE_DOUBLE;

    // not a number, nothing to do about it, all should be already type-sound
    if (!is_to_int && !is_to_flt) {
        compile_expression(cs, expr, slot);
        return;
    }

    struct adpt_type cref expr_ty = expr->usr;
    bool const is_from_int = TYPE_CHAR <= expr_ty->tyty && expr_ty->tyty <= TYPE_ULONG;
    bool const is_from_flt = TYPE_FLOAT <= expr_ty->tyty && expr_ty->tyty <= TYPE_DOUBLE;

    // same size -> nothing to do
    if (slot->ty->size == expr_ty->size && is_from_int == is_to_int) {
        compile_expression(cs, expr, slot);
        return;
    }

    bool const is_to_signed = TYPE_SCHAR == slot->ty->tyty || TYPE_SHORT == slot->ty->tyty || TYPE_INT == slot->ty->tyty || TYPE_LONG == slot->ty->tyty;
    bool const is_to_unsigned = TYPE_UCHAR == slot->ty->tyty || TYPE_USHORT == slot->ty->tyty || TYPE_UINT == slot->ty->tyty || TYPE_ULONG == slot->ty->tyty;
    bool const is_from_signed = TYPE_SCHAR == expr_ty->tyty || TYPE_SHORT == expr_ty->tyty || TYPE_INT == expr_ty->tyty || TYPE_LONG == expr_ty->tyty;
    bool const is_from_unsigned = TYPE_UCHAR == expr_ty->tyty || TYPE_USHORT == expr_ty->tyty || TYPE_UINT == expr_ty->tyty || TYPE_ULONG == expr_ty->tyty;

    struct slot tmp = {.ty= expr_ty};

    // if slot too small, make 'physical' temporary, else it'll used the same a `slot`
    if (slot->ty->size < expr_ty->size) {
        _alloc_slot(cs, &tmp);
        compile_expression(cs, expr, &tmp);

        switch (tmp.usage) {
        case _slot_value:
            slot->usage = _slot_value;
            _cancel_slot(cs, &tmp);
            switch (expr_ty->tyty <<8| slot->ty->tyty) {
            case TYPE_LONG <<8| TYPE_FLOAT:  slot->as.value.f = tmp.as.value.sl; break;
            case TYPE_ULONG <<8| TYPE_FLOAT: slot->as.value.f = tmp.as.value.ul; break;
            case TYPE_FLOAT <<8| TYPE_CHAR:   slot->as.value.c = tmp.as.value.f; break;
            case TYPE_FLOAT <<8| TYPE_UCHAR:  slot->as.value.uc = tmp.as.value.f; break;
            case TYPE_FLOAT <<8| TYPE_SCHAR:  slot->as.value.sc = tmp.as.value.f; break;
            case TYPE_FLOAT <<8| TYPE_SHORT:  slot->as.value.ss = tmp.as.value.f; break;
            case TYPE_FLOAT <<8| TYPE_USHORT: slot->as.value.us = tmp.as.value.f; break;
            case TYPE_DOUBLE <<8| TYPE_CHAR:   slot->as.value.c = tmp.as.value.d; break;
            case TYPE_DOUBLE <<8| TYPE_UCHAR:  slot->as.value.uc = tmp.as.value.d; break;
            case TYPE_DOUBLE <<8| TYPE_SCHAR:  slot->as.value.sc = tmp.as.value.d; break;
            case TYPE_DOUBLE <<8| TYPE_SHORT:  slot->as.value.ss = tmp.as.value.d; break;
            case TYPE_DOUBLE <<8| TYPE_INT:    slot->as.value.si = tmp.as.value.d; break;
            case TYPE_DOUBLE <<8| TYPE_USHORT: slot->as.value.us = tmp.as.value.d; break;
            case TYPE_DOUBLE <<8| TYPE_UINT:   slot->as.value.ui = tmp.as.value.d; break;
            case TYPE_DOUBLE <<8| TYPE_FLOAT: slot->as.value.f = tmp.as.value.d; break;
            default: memcpy(slot->as.value.bytes, tmp.as.value.bytes, slot->ty->size); // (yyy: endianness)
            }
            break;

        case _slot_used:
            slot->usage = _slot_used;
            // TODO: this is the int case
            _emit_move(cs, at(slot), slot->ty->size, at(&tmp)); // (yyy: endianness)
            _rewind_slot(cs, &tmp);
            break;

        case _slot_variable:
            slot->usage = _slot_variable;
            // TODO: this is the int case
            slot->as.variable = tmp.as.variable; // (yyy: endianness)
            _cancel_slot(cs, &tmp);
            break;
        }
    }

    // slot large enough for the expr result
    else {
        tmp.loc = slot->loc;
        compile_expression(cs, expr, &tmp);

        switch (tmp.usage) {
        case _slot_value:
            slot->usage = _slot_value;
            switch (expr_ty->tyty <<8| slot->ty->tyty) {
            case TYPE_CHAR   <<8| TYPE_FLOAT: slot->as.value.f = tmp.as.value.c; break;
            case TYPE_UCHAR  <<8| TYPE_FLOAT: slot->as.value.f = tmp.as.value.uc; break;
            case TYPE_SCHAR  <<8| TYPE_FLOAT: slot->as.value.f = tmp.as.value.sc; break;
            case TYPE_SHORT  <<8| TYPE_FLOAT: slot->as.value.f = tmp.as.value.ss; break;
            case TYPE_INT    <<8| TYPE_FLOAT: slot->as.value.f = tmp.as.value.si; break;
            case TYPE_USHORT <<8| TYPE_FLOAT: slot->as.value.f = tmp.as.value.us; break;
            case TYPE_UINT   <<8| TYPE_FLOAT: slot->as.value.f = tmp.as.value.ui; break;
            case TYPE_CHAR   <<8| TYPE_DOUBLE: slot->as.value.d = tmp.as.value.c; break;
            case TYPE_UCHAR  <<8| TYPE_DOUBLE: slot->as.value.d = tmp.as.value.uc; break;
            case TYPE_SCHAR  <<8| TYPE_DOUBLE: slot->as.value.d = tmp.as.value.sc; break;
            case TYPE_SHORT  <<8| TYPE_DOUBLE: slot->as.value.d = tmp.as.value.ss; break;
            case TYPE_INT    <<8| TYPE_DOUBLE: slot->as.value.d = tmp.as.value.si; break;
            case TYPE_LONG   <<8| TYPE_DOUBLE: slot->as.value.d = tmp.as.value.sl; break;
            case TYPE_USHORT <<8| TYPE_DOUBLE: slot->as.value.d = tmp.as.value.us; break;
            case TYPE_UINT   <<8| TYPE_DOUBLE: slot->as.value.d = tmp.as.value.ui; break;
            case TYPE_ULONG  <<8| TYPE_DOUBLE: slot->as.value.d = tmp.as.value.ul; break;
            case TYPE_FLOAT <<8| TYPE_INT:   slot->as.value.si = tmp.as.value.f; break;
            case TYPE_FLOAT <<8| TYPE_UINT:  slot->as.value.ui = tmp.as.value.f; break;
            case TYPE_FLOAT <<8| TYPE_LONG:  slot->as.value.sl = tmp.as.value.f; break;
            case TYPE_FLOAT <<8| TYPE_ULONG: slot->as.value.ul = tmp.as.value.f; break;
            case TYPE_FLOAT <<8| TYPE_DOUBLE: slot->as.value.d = tmp.as.value.f; break;
            case TYPE_DOUBLE <<8| TYPE_LONG:  slot->as.value.sl = tmp.as.value.d; break;
            case TYPE_DOUBLE <<8| TYPE_ULONG: slot->as.value.ul = tmp.as.value.d; break;
            default:
                if (is_from_unsigned)
                    memcpy(slot->as.value.bytes, memcpy((unsigned char[sizeof slot->as.value.bytes]){0}, tmp.as.value.bytes, expr_ty->size), slot->ty->size);
                else {
                    unsigned char x[sizeof slot->as.value.bytes] = {0};
                    bool const sign_bit = tmp.as.value.bytes[expr_ty->size-1] & 0x80; // (yyy: endianness)
                    if (sign_bit) memset(x, 0xff, sizeof x);
                    memcpy(slot->as.value.bytes, memcpy(x, tmp.as.value.bytes, expr_ty->size), slot->ty->size);
                }
            }
            break;

        case _slot_used:
            slot->usage = _slot_used;
            // TODO: this is the int case
            _emit_extend(cs, slot, &(struct slot){
                .ty= expr_ty,
                .loc= tmp.loc,
                .usage= _slot_used,
            });
            break;

        case _slot_variable:
            slot->usage = _slot_used;
            // TODO: this is the int case
            _emit_extend(cs, slot, &(struct slot){
                .ty= expr_ty,
                .loc= tmp.as.variable,
                .usage= _slot_used,
            });
            break;
        }
    }
}
// }}}

void compile_expression(compile_state ref cs, expression cref expr, struct slot ref slot) {
    size_t plen = cs->res.len;
    size_t pvsp = cs->vsp;

    // work variables that are better function-scoped and initialized early
    struct slot tmp = {0};
    enum _arith_op op = 0;

    switch (expr->kind) {
    case ATOM:;
        size_t const len = expr->info.atom.len;
        char cref ptr = expr->info.atom.ptr;
        if ('"' == ptr[0]) {
            slot->as.variable = -1;
            slot->usage = _slot_variable;
            exitf("NYI: string literal (should have been replaced with its slot during checking)");
            // XXX: nah- it's gonna need to be a pointer proper, be it on the stack or not
            //      SO, will eventually need an instruction to write sp to a slot
            return;
        }

        if ('\'' == ptr[0]) {
            char v = ptr[1];
            if ('\\' == v) switch (ptr[2]) {
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
            slot->as.value.c = v;
            slot->usage = _slot_value;
            return;
        }

        if (('0' <= ptr[0] && ptr[0] <= '9') || '.' == ptr[0]) {
            enum adpt_type_tag const tyty = ((struct adpt_type const*)expr->usr)->tyty;
            switch (tyty) {
            case TYPE_INT:
            case TYPE_LONG:
            case TYPE_UINT:
            case TYPE_ULONG:
                if (len <3) {
                    if ('9' < ptr[len-1]) slot->as.value.ul = ptr[0]-'0';
                    else slot->as.value.ul = ptr[len-1]-'0' + (2 == len ? (ptr[len-2]-'0')*10 : 0);
                } else {
                    unsigned long r = 0;
                    size_t k = 0;

                    unsigned shft = 0;
                    char const* dgts = "0123456789";
                    if ('0' == ptr[0]) switch (ptr[1]) {
                    case 'B': case 'b': k = 2; shft = 1; dgts = "01";               break;
                    case 'O': case 'o': k = 2; shft = 3; dgts = "01234567";         break;
                    case 'X': case 'x': k = 2; shft = 4; dgts = "0123456789abcdef"; break;
                    }
                    char const* v = strchr(dgts, ptr[k]|32);
                    do if ('\'' != ptr[k]) r = (!shft ? r*10 : r<<shft) + (v-dgts);
                    while (++k < len && ('\'' == ptr[k] || (v = strchr(dgts, ptr[k]|32))));

                    slot->as.value.ul = r;
                }
                break;

            case TYPE_FLOAT:
            case TYPE_DOUBLE:;
                double r = 0;

                if (len <3 || 'x' != (ptr[1]|32)) {
                    size_t k = 0;

                    while (k < len && '.' != ptr[k] && 'e' != (ptr[k]|32) && 'f' != (ptr[k]|32))
                        r = r*10 + (ptr[k++]-'0');
                    if (k < len && '.' == ptr[k]) {
                        double d = 1;
                        while (++k < len && 'e' != (ptr[k]|32) && 'f' != (ptr[k]|32))
                            r+= (ptr[k]-'0')*(d/= 10);
                    }
                    if (k < len && 'e' == (ptr[k]|32)) {
                        unsigned long d = ptr[++k]-'0';
                        while (++k < len && 'f' != (ptr[k]|32)) d = d*10 + (ptr[k]-'0');
                        for (; d; d--) r*= 10;
                    }
                } else {
                    size_t k = 2;
                    static char cref dgts = "0123456789abcdef";

                    while (k < len && '.' != ptr[k] && 'p' != (ptr[k]|32))
                        r = r*16 + (strchr(dgts, ptr[k++]|32)-dgts);
                    if (k < len && '.' == ptr[k]) {
                        double d = 1;
                        while (++k < len && 'p' != (ptr[k]|32))
                            r+= (strchr(dgts, ptr[k]|32)-dgts)*(d/= 16);
                    }
                    if (k < len && 'p' == (ptr[k]|32)) {
                        unsigned long d = ptr[++k]-'0';
                        while (++k < len && 'f' != (ptr[k]|32)) d = d*2 + (ptr[k]-'0');
                        for (; d; d--) r*= 2;
                    }
                }

                if (TYPE_FLOAT == tyty) slot->as.value.f = r;
                else slot->as.value.d = r;
                break;

            default:;
            }

            slot->usage = _slot_value;
            return;
        }

        struct adpt_item const* found = cs->lookup(cs->usr, expr->info.atom);
        if (ITEM_VALUE == found->kind) {
            slot->as.value.ul = (size_t)found->as.object;
            slot->usage = _slot_value;
        } else {
            slot->as.variable = found->as.variable;
            slot->usage = _slot_variable;
        }
        return;

    case BINOP_SUBSCR: { // TODO: wip
            struct slot base = {.ty= expr->info.subscr.base->usr};
            _alloc_slot(cs, &base);

            compile_expression(cs, expr->info.subscr.base, &base);
            // can be:
            // - variable: stack, to ptr (far)
            // - value: ptr (far), but outside of NULL..!?
            // - used: -

            struct slot off = {0};
            compile_expression(cs, expr->info.subscr.off, &off);
            slot->usage = _slot_variable;
        }
        return;

    case BINOP_CALL: {
            struct slot fun = {.ty= expr->info.call.base->usr};
            _alloc_slot(cs, &fun);

            compile_expression(cs, expr->info.call.base, &fun);
            if (_slot_variable == fun.usage) _cancel_slot(cs, &fun);
            else _materialize_slot(cs, &fun);

            struct slot args[15] = {0};
            expression const* cons = expr->info.call.args;
            size_t count = fun.ty->info.fun.count;
            for (size_t k = 0; k < count; k++) {
                args[k].ty = fun.ty->info.fun.params[count-1-k].type;
                _alloc_slot(cs, args+k);

                expression cref arg_expr = k < count ? cons->info.binary.rhs : cons;
                if (k < count) cons = cons->info.binary.lhs;

                _fit_expr_to_slot(cs, arg_expr, args+k);
                if (_slot_variable == args[k].usage) {
                    _cancel_slot(cs, args+k);
                    args[k].loc = args[k].as.variable;
                } else _materialize_slot(cs, args+k);
            }

            _emit_call_base(cs, count, at(slot), _slot_variable == fun.usage ? atv(&fun) : at(&fun));
            for (size_t k = count; k; k--) _emit_call_arg(cs, at(args+k-1));

            slot->usage = _slot_used;
            // call rewind even if the slot for fun itself isn't used because
            // there might have been arguments; rewind itself is smart enough
            // to not emit a 0 pop anyways
            _rewind_slot(cs, &fun);
        }
        return;

    case BINOP_TERNCOND:
    case BINOP_TERNBRANCH:
        exitf("NIY: branches");
        return;

    case BINOP_COMMA: {
            struct slot drp = {.ty= expr->info.binary.lhs->usr};
            _alloc_slot(cs, &drp);
            compile_expression(cs, expr->info.binary.lhs, &drp);
            if (_slot_used != drp.usage) _cancel_slot(cs, &drp);
            compile_expression(cs, expr->info.binary.rhs, slot);
            if (_slot_used == drp.usage) _rewind_slot(cs, &drp);
        }
        return;

        {
    case BINOP_ASGN_BOR:  op = _ops_bori;  goto sw_asgn;
    case BINOP_ASGN_BXOR: op = _ops_bxori; goto sw_asgn;
    case BINOP_ASGN_BAND: op = _ops_bandi; goto sw_asgn;
    case BINOP_ASGN_BSHL: op = _ops_bshli; goto sw_asgn;
    case BINOP_ASGN_BSHR: op = _ops_bshri; goto sw_asgn;
    case BINOP_ASGN_SUB:  op = _ops_subi;  goto sw_asgn;
    case BINOP_ASGN_ADD:  op = _ops_addi;  goto sw_asgn;
    case BINOP_ASGN_REM:  op = _ops_remi;  goto sw_asgn;
    case BINOP_ASGN_DIV:  op = _ops_divi;  goto sw_asgn;
    case BINOP_ASGN_MUL:  op = _ops_muli;  goto sw_asgn;
    case BINOP_ASGN:
        sw_asgn:;

            expression cref dst_ex = expr->info.binary.lhs;
            expression cref src_ex = expr->info.binary.rhs;

            _fit_expr_to_slot(cs, src_ex, slot);

            struct slot dst = {.ty= dst_ex->usr};
            switch (dst_ex->kind) {
            case ATOM:
                compile_expression(cs, dst_ex, &dst);
                // can be stack variable or static global object;
                // for now only support stack variable

                if (op) {
                    tmp = dst;
                    goto sw_asgn_binop;
                }

                switch (slot->usage) {
                case _slot_value:
                    _emit_data(cs, atv(&dst), dst.ty->size, slot->as.value.bytes); // (yyy: endianness)
                    break;
                case _slot_used:
                    _emit_move(cs, atv(&dst), dst.ty->size, at(slot));
                    break;
                case _slot_variable:
                    _emit_move(cs, atv(&dst), dst.ty->size, atv(slot));
                    break;
                }
                break; // return;

            case BINOP_SUBSCR:
            case UNOP_DEREF:
            case UNOP_PMEMBER:
            case UNOP_MEMBER:
                exitf("NIY: assigning to this kind of lvalue");
                break;

            default:;
            }
        }
        return;

        {
    case BINOP_BOR:  op = _ops_bori;  goto sw_binop;
    case BINOP_BXOR: op = _ops_bxori; goto sw_binop;
    case BINOP_BAND: op = _ops_bandi; goto sw_binop;
    case BINOP_BSHL: op = _ops_bshli; goto sw_binop; // asy
    case BINOP_BSHR: op = _ops_bshri; goto sw_binop; // asy
    case BINOP_SUB:  op = _ops_subi;  goto sw_binop; // asy
    case BINOP_ADD:  op = _ops_addi;  goto sw_binop;
    case BINOP_REM:  op = _ops_remi;  goto sw_binop; // asy
    case BINOP_DIV:  op = _ops_divi;  goto sw_binop; // asy
    case BINOP_MUL:  op = _ops_muli;  goto sw_binop;
        sw_binop:;

            struct slot lhs = *slot, rhs;
            _fit_expr_to_slot(cs, expr->info.binary.lhs, &lhs);
            if (_slot_used != lhs.usage)
                rhs = *slot;
            else {
                rhs.ty = slot->ty;
                _alloc_slot(cs, &rhs);
            }
            _fit_expr_to_slot(cs, expr->info.binary.rhs, &rhs);
            if (_slot_used == lhs.usage && _slot_used != rhs.usage) _cancel_slot(cs, &rhs);

            bool was_asgn = false;
            if (0) {
        sw_asgn_binop:
                was_asgn = true;

                rhs = *slot;
                lhs = *slot = tmp;
                // wip: only comming from case ATOM in assignment
                // so for now the destination is a stack variable
            }

            // TODO: pointer arithmetic (is only correct for size 1 ie char)
            switch (lhs.usage <<8| rhs.usage) {
                // neither is "physical"
            case _slot_value <<8| _slot_value:
                switch (op) {
                case _ops_bori:  _cfold_arith_ints(slot, &lhs, |,  &rhs); break;
                case _ops_bxori: _cfold_arith_ints(slot, &lhs, ^,  &rhs); break;
                case _ops_bandi: _cfold_arith_ints(slot, &lhs, &,  &rhs); break;
                case _ops_bshli: _cfold_arith_ints(slot, &lhs, <<, &rhs); break;
                case _ops_bshri: _cfold_arith_ints(slot, &lhs, >>, &rhs); break;
                case _ops_subi:  _cfold_arith     (slot, &lhs, -,  &rhs); break;
                case _ops_addi:  _cfold_arith     (slot, &lhs, +,  &rhs); break;
                case _ops_remi:  _cfold_arith_ints(slot, &lhs, %,  &rhs); break;
                case _ops_divi:  _cfold_arith     (slot, &lhs, /,  &rhs); break;
                case _ops_muli:  _cfold_arith     (slot, &lhs, *,  &rhs); break;
                default:;
                }
                slot->usage = _slot_value;
                break;

                // one is "physical"
                struct slot const* val, * xhs;
            case _slot_used     <<8| _slot_value:
            case _slot_variable <<8| _slot_value:
                // rhs is value, use the r[..]i form
                val = &rhs, xhs = &lhs;
                switch (op) {
                case _ops_bshli: op = _ops_rbshli; break;
                case _ops_bshri: op = _ops_rbshri; break;
                case _ops_subi:  op = _ops_rsubi;  break;
                case _ops_remi:  op = _ops_rremi;  break;
                case _ops_divi:  op = _ops_rdivi;  break;
                default:;
                }
                if (0)
            case _slot_value    <<8| _slot_used:
            case _slot_value    <<8| _slot_variable:
                    // lhs is value, use the [..]i form
                    val = &lhs, xhs = &rhs;
                _emit_arith(cs, op, _slot_arith_w(slot),
                        _slot_variable == slot->usage ? atv(slot) : at(slot),
                        _slot_arith_v(val),
                        _slot_variable == xhs->usage ? atv(xhs) : at(xhs));
                slot->usage = _slot_used;
                break;

                // both are "physical"
            case _slot_used     <<8| _slot_used:
            case _slot_used     <<8| _slot_variable:
            case _slot_variable <<8| _slot_used:
            case _slot_variable <<8| _slot_variable:
                switch (op) {
                case _ops_bori:  op = _ops_bor;  break;
                case _ops_bxori: op = _ops_bxor; break;
                case _ops_bandi: op = _ops_band; break;
                case _ops_bshli: op = _ops_bshl; break;
                case _ops_bshri: op = _ops_bshr; break;
                case _ops_subi:  op = _ops_sub;  break;
                case _ops_addi:  op = _ops_add;  break;
                case _ops_remi:  op = _ops_rem;  break;
                case _ops_divi:  op = _ops_div;  break;
                case _ops_muli:  op = _ops_mul;  break;
                default:;
                }
                _emit_arith(cs, op, _slot_arith_w(slot),
                        _slot_variable == slot->usage ? atv(slot) : at(slot),
                        _slot_variable == lhs.usage ? atv(&lhs) : at(&lhs),
                        _slot_variable == rhs.usage ? atv(&rhs) : at(&rhs));
                slot->usage = _slot_used;
                break;
            } // switch both usages

            if (was_asgn) {
                *slot = rhs;
                _emit_move(cs, at(slot), slot->ty->size, atv(&lhs));
                slot->usage = _slot_used;
            } else {
                if (_slot_used == lhs.usage && _slot_used == rhs.usage) _rewind_slot(cs, &rhs);
            }
        }
        return;

    case UNOP_ADDR:
        exitf("NIY: address of");
        return;

    case UNOP_DEREF: {
            struct slot ptr = {.ty= expr->usr};
            _alloc_slot(cs, &ptr);
            compile_expression(cs, expr->info.unary.opr, &ptr);

            switch (ptr.usage) {
            case _slot_value:
                exitf("deref on a compile time - this is surely very wrong");

            case _slot_used:
                _emit_read(cs, at(&ptr), slot);
                _rewind_slot(cs, &ptr);
                break;

            case _slot_variable:
                _cancel_slot(cs, &ptr);
                _emit_read(cs, atv(&ptr), slot);
                break;
            }

            slot->usage = _slot_used;
        }
        return;

    case UNOP_PMEMBER:
    case UNOP_MEMBER:
        exitf("NIY: member access");
        return;

    case BINOP_LOR:
    case BINOP_LAND:
        exitf("NIY: branches");
        return;

    case BINOP_EQ:
    case BINOP_NE:
    case BINOP_LT:
    case BINOP_GT:
    case BINOP_LE:
    case BINOP_GE:
        exitf("NIY: comparisons");
        return;

    case UNOP_BNOT:
    case UNOP_LNOT:
    case UNOP_MINUS:
    case UNOP_PLUS:
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

            size_t at_slot;
        case _slot_used:     at_slot = at(slot); if (0)
        case _slot_variable: at_slot = atv(slot);
            switch (expr->kind) {
            case UNOP_BNOT: _emit_arith(cs, _ops_bxori, _slot_arith_w(slot), at(slot), -1/* xxx: should be of the type's size, this is larger */, at_slot); break;
            case UNOP_LNOT: exitf("NIY emit lnot"); break;
            case UNOP_MINUS: _emit_arith(cs, _ops_subi, _slot_arith_w(slot), at(slot), 0, at_slot); break;
            case UNOP_PLUS: _emit_move(cs, at(slot), slot->ty->size, atv(slot)); break;
            default:;
            }
            slot->usage = _slot_used;
        }
        return;

        {
            bool post;
    case UNOP_PRE_DEC: post = false; op = _ops_rsubi; goto sw_crement;
    case UNOP_PRE_INC: post = false; op = _ops_addi;  goto sw_crement;
    case UNOP_POST_DEC: post = true; op = _ops_rsubi; goto sw_crement;
    case UNOP_POST_INC: post = true; op = _ops_addi;  goto sw_crement;
        sw_crement:

            switch (expr->info.unary.opr->kind) {
            case ATOM:
                compile_expression(cs, expr->info.unary.opr, slot);
                // can be stack variable or static global object;
                // for now only support stack variable
                if (post) {
                    _emit_move(cs, at(slot), slot->ty->size, atv(slot));
                    slot->usage = _slot_used;
                }
                _emit_arith(cs, op, _slot_arith_w(slot), atv(slot),
                        TYPE_PTR == slot->ty->tyty ? slot->ty->info.ptr->size : 1,
                        atv(slot));
                break;

            case BINOP_SUBSCR:
            case UNOP_DEREF:
            case UNOP_PMEMBER:
            case UNOP_MEMBER:
                exitf("NIY: assigning to this kind of lvalue");
                break;

            default:;
            }
            return;
        }
    }

} // compile_expression

#undef atv
#undef at

#endif // CINTRE_COMPILER_H
