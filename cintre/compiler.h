/// Bytecode compiler on top of the parser; example:
/// (interface is being worked on..)
/// ```c
/// ct_compile_state cs = {0};
///
/// struct ct_slot slot = {.ty= ct_check_expression(&cs, expr)};
/// if (slot.ty) {
///     _ct_alloc_slot(&cs, &ct_slot);
///
///     ct_compile_expression(&cs, expr, &ct_slot);
///     switch (slot.usage) { ... }
///
///     cs->res; // this is the ct_bytecode
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

// yyy: the `ct_compile_state` is actually defined in "checker.h"

struct ct_slot {
  struct ct_adpt_type const* /*const*/ ty;

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
          void* p;
          unsigned char bytes[8]; // (yyy: sizeof @This() -- assumes 64b)
      } value;
      size_t variable;
  } as;
};

/// the expression should have been sieved through `ct_check_expression` first
void ct_compile_expression(ct_compile_state ref cs, ct_expression cref expr, struct ct_slot ref slot);

// ---

// compile utils {{{
#define at(__slt)  ((__slt)->loc - cs->vsp)
#define atv(__slt) ((__slt)->as.variable - cs->vsp)

unsigned _ct_l2(size_t n)
{
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

void _ct_alloc_slot(ct_compile_state ref cs, struct ct_slot ref slot)
{
    slot->codeat = cs->res.len;
    slot->end = cs->vsp;
    if (slot->ty->size) {
        cs->vsp = (cs->vsp - slot->ty->size) & (~(size_t)0<<_ct_l2(slot->ty->align));
        _emit_instr_w_opr(0x0f, slot->end - cs->vsp);
    }
    slot->loc = cs->vsp;
}

void _ct_cancel_slot(ct_compile_state ref cs, struct ct_slot cref slot)
{
    cs->res.len = slot->codeat;
    cs->vsp = slot->end;
}

void _ct_rewind_slot(ct_compile_state ref cs, struct ct_slot cref slot)
{
    if (slot->end != cs->vsp)
        _emit_instr_w_opr(0x0d, slot->end - cs->vsp);
    cs->vsp = slot->end;
}

void _ct_emit_data(ct_compile_state ref cs, size_t const dst, size_t const width, unsigned char const* data)
{
    _emit_instr_w_opr(0x1d, dst, width);
    unsigned char* dt = dyarr_insert(&cs->res, cs->res.len, width);
    if (!dt) exitf("OOM");
    memcpy(dt, data, width);
}

void _ct_emit_move(ct_compile_state ref cs, size_t const dst, size_t const width, size_t const src)
{
    _emit_instr_w_opr(0x1f, dst, width, src);
}

void _ct_emit_write(ct_compile_state ref cs, size_t const ptr_dst_slt, struct ct_slot cref slot_src)
{
    _emit_instr_w_opr(0x2f, ptr_dst_slt, slot_src->ty->size, at(slot_src));
}

void _ct_emit_read(ct_compile_state ref cs, size_t const ptr_src_slt, struct ct_slot cref slot_dst)
{
    _emit_instr_w_opr(0x2f, ptr_src_slt, slot_dst->ty->size, at(slot_dst));
}

void _ct_emit_lea(ct_compile_state ref cs, size_t const dst, size_t const off)
{
    _emit_instr_w_opr(0x20, dst, off);
}

//void _emit_slot_value(ct_compile_state ref cs, struct ct_slot cref slot) {
//    _ct_emit_data(cs, at(slot), slot->ty->size, (unsigned char*)&slot->as.value.ul);
//}

void _ct_emit_call_base(ct_compile_state ref cs, unsigned const argc, size_t const ret, size_t const fun)
{
    _emit_instr_w_opr(argc<<4 | 0xc, ret, fun);
}

void _ct_emit_call_arg(ct_compile_state ref cs, size_t argv)
{
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

enum _ct_arith_op {
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

enum _ct_arith_w {
    _oprw_8,
    _oprw_16,
    _oprw_32,
    _oprw_64,
    _oprw_f= 0xf,
    _oprw_d= 0xd
};

void _ct_emit_arith(ct_compile_state ref cs, enum _ct_arith_op aop, enum _ct_arith_w w, size_t const dst, size_t const a, size_t const b)
{
    _emit_instr_w_opr(aop+w, dst, a, b);
}

enum _ct_arith_w _ct_slot_arith_w(struct ct_slot cref slot)
{
    switch (slot->ty->tyty) {
    case CT_TYPE_CHAR:   return _oprw_8;
    case CT_TYPE_UCHAR:  return _oprw_8;
    case CT_TYPE_SCHAR:  return _oprw_8;
    case CT_TYPE_SHORT:  return _oprw_16;
    case CT_TYPE_INT:    return _oprw_32;
    case CT_TYPE_LONG:   return _oprw_64;
    case CT_TYPE_USHORT: return _oprw_16;
    case CT_TYPE_UINT:   return _oprw_32;
    case CT_TYPE_ULONG:  return _oprw_64;
    case CT_TYPE_FLOAT:  return _oprw_f;
    case CT_TYPE_DOUBLE: return _oprw_d;
    default: return 0;
    }
}

size_t _ct_slot_arith_v(struct ct_slot cref slot)
{
    switch (slot->ty->tyty) {
    case CT_TYPE_CHAR:   return slot->as.value.c;
    case CT_TYPE_UCHAR:  return slot->as.value.uc;
    case CT_TYPE_SCHAR:  return slot->as.value.sc;
    case CT_TYPE_SHORT:  return slot->as.value.ss;
    case CT_TYPE_INT:    return slot->as.value.si;
    case CT_TYPE_LONG:   return slot->as.value.sl;
    case CT_TYPE_USHORT: return slot->as.value.us;
    case CT_TYPE_UINT:   return slot->as.value.ui;
    case CT_TYPE_ULONG:  return slot->as.value.ul;
    case CT_TYPE_FLOAT:  return slot->as.value.f;
    case CT_TYPE_DOUBLE: return slot->as.value.d;
    default: return 0;
    }
}

void _ct_emit_extend(ct_compile_state ref cs, struct ct_slot cref big_dst, struct ct_slot cref small_src)
{
    // unsigned -> unsigned: zero extend
    // signed   -> signed:   sign extend
    // unsigned -> signed:   zero extend
    // signed   -> unsigned: sign extend

    bool const is_src_signed =
        CT_TYPE_CHAR  == small_src->ty->tyty ||
        CT_TYPE_SCHAR == small_src->ty->tyty ||
        CT_TYPE_SHORT == small_src->ty->tyty ||
        CT_TYPE_INT   == small_src->ty->tyty ||
        CT_TYPE_LONG  == small_src->ty->tyty;

    _emit_instr_w_opr(_ct_l2(small_src->ty->size)<<4 | _ct_l2(big_dst->ty->size) | !is_src_signed<<2,
            at(big_dst), at(small_src));
}

#define _cfold_arith_ints(dst, lhs, op, rhs)  \
    do switch ((dst)->ty->tyty) {  \
    case CT_TYPE_CHAR:   (dst)->as.value.c  = (lhs)->as.value.c  op (rhs)->as.value.c;  break;  \
    case CT_TYPE_UCHAR:  (dst)->as.value.uc = (lhs)->as.value.uc op (rhs)->as.value.uc; break;  \
    case CT_TYPE_SCHAR:  (dst)->as.value.sc = (lhs)->as.value.sc op (rhs)->as.value.sc; break;  \
    case CT_TYPE_SHORT:  (dst)->as.value.ss = (lhs)->as.value.ss op (rhs)->as.value.ss; break;  \
    case CT_TYPE_INT:    (dst)->as.value.si = (lhs)->as.value.si op (rhs)->as.value.si; break;  \
    case CT_TYPE_LONG:   (dst)->as.value.sl = (lhs)->as.value.sl op (rhs)->as.value.sl; break;  \
    case CT_TYPE_USHORT: (dst)->as.value.us = (lhs)->as.value.us op (rhs)->as.value.us; break;  \
    case CT_TYPE_UINT:   (dst)->as.value.ui = (lhs)->as.value.ui op (rhs)->as.value.ui; break;  \
    case CT_TYPE_ULONG:  (dst)->as.value.ul = (lhs)->as.value.ul op (rhs)->as.value.ul; break;  \
    default:;  \
    } while (0)

#define _cfold_arith(dst, lhs, op, rhs)  \
    do switch ((dst)->ty->tyty) {  \
    case CT_TYPE_CHAR:   (dst)->as.value.c  = (lhs)->as.value.c  op (rhs)->as.value.c;  break;  \
    case CT_TYPE_UCHAR:  (dst)->as.value.uc = (lhs)->as.value.uc op (rhs)->as.value.uc; break;  \
    case CT_TYPE_SCHAR:  (dst)->as.value.sc = (lhs)->as.value.sc op (rhs)->as.value.sc; break;  \
    case CT_TYPE_SHORT:  (dst)->as.value.ss = (lhs)->as.value.ss op (rhs)->as.value.ss; break;  \
    case CT_TYPE_INT:    (dst)->as.value.si = (lhs)->as.value.si op (rhs)->as.value.si; break;  \
    case CT_TYPE_LONG:   (dst)->as.value.sl = (lhs)->as.value.sl op (rhs)->as.value.sl; break;  \
    case CT_TYPE_USHORT: (dst)->as.value.us = (lhs)->as.value.us op (rhs)->as.value.us; break;  \
    case CT_TYPE_UINT:   (dst)->as.value.ui = (lhs)->as.value.ui op (rhs)->as.value.ui; break;  \
    case CT_TYPE_ULONG:  (dst)->as.value.ul = (lhs)->as.value.ul op (rhs)->as.value.ul; break;  \
    case CT_TYPE_FLOAT:  (dst)->as.value.f  = (lhs)->as.value.f  op (rhs)->as.value.f;  break;  \
    case CT_TYPE_DOUBLE: (dst)->as.value.d  = (lhs)->as.value.d  op (rhs)->as.value.d;  break;  \
    default:;  \
    } while (0)

/// makes a slot "pysical", ie if it was a value, it will be inserted as data
/// and marked as used (but if it was a variable, it will stay as is)
void _ct_materialize_slot(ct_compile_state ref cs, struct ct_slot ref slot)
{
    if (_slot_value == slot->usage) {
        _ct_emit_data(cs, at(slot), slot->ty->size, slot->as.value.bytes); // (yyy: endianness)
        slot->usage = _slot_used;
    }
}

/// fits an expression to a slot by compiling into it with any necessary
/// conversion step (allocates and de-allocates a slot if needed)
void _ct_fit_expr_to_slot(ct_compile_state ref cs, ct_expression cref expr, struct ct_slot ref slot)
{
    // so we have `slot` and `expr` of two types
    // compatibility should already be ensured by `ct_check_expression`

    // ideal case when they have the same type is just compile the expr w/ res into the slot
    // otherwise ptr and num conversions must take place:
    // - ptr -> ptr is whever
    // - arr -> ptr is what it is
    // - int -> int:
    //   - if the slot is wider then it can still be emit into it, conversion in-place (zero/sign extend)
    //   - otherwise truncating cvt doesn't requier an other slot, conversion in-place (because little endian)
    // - float -> int, int -> float, float -> float: ...

    // after compiling the expression, either
    // - the slot is used in which case conversion steps in-place
    // - the slot is a variable, a widening conversion will need to copy
    // - the slot is a value, compile-time conversion

    struct ct_adpt_type cref expr_ty = expr->usr;
    if (CT_TYPE_ARR == expr_ty->tyty && CT_TYPE_PTR == slot->ty->tyty) {
        struct ct_slot tmp = {.ty= expr_ty};
        ct_compile_expression(cs, expr, &tmp);

        if (_slot_variable != tmp.usage) exitf("unreachable: array should have usage _slot_variable");

        _ct_emit_lea(cs, at(slot), atv(&tmp));
        slot->usage = _slot_used;
        return;
    }

    bool const is_to_int = CT_TYPE_CHAR <= slot->ty->tyty && slot->ty->tyty <= CT_TYPE_ULONG;
    bool const is_to_flt = CT_TYPE_FLOAT <= slot->ty->tyty && slot->ty->tyty <= CT_TYPE_DOUBLE;

    // not a number, nothing to do about it, all should be already type-sound
    if (!is_to_int && !is_to_flt) {
        ct_compile_expression(cs, expr, slot);
        return;
    }

    bool const is_from_int = CT_TYPE_CHAR <= expr_ty->tyty && expr_ty->tyty <= CT_TYPE_ULONG;
    //bool const is_from_flt = CT_TYPE_FLOAT <= expr_ty->tyty && expr_ty->tyty <= CT_TYPE_DOUBLE;

    // same size -> nothing to do
    if (slot->ty->size == expr_ty->size && is_from_int == is_to_int) {
        ct_compile_expression(cs, expr, slot);
        return;
    }

    //bool const is_to_signed = CT_TYPE_SCHAR == ct_slot->ty->tyty || CT_TYPE_SHORT == ct_slot->ty->tyty || CT_TYPE_INT == ct_slot->ty->tyty || CT_TYPE_LONG == ct_slot->ty->tyty;
    //bool const is_to_unsigned = CT_TYPE_UCHAR == ct_slot->ty->tyty || CT_TYPE_USHORT == ct_slot->ty->tyty || CT_TYPE_UINT == ct_slot->ty->tyty || CT_TYPE_ULONG == ct_slot->ty->tyty;
    //bool const is_from_signed = CT_TYPE_SCHAR == expr_ty->tyty || CT_TYPE_SHORT == expr_ty->tyty || CT_TYPE_INT == expr_ty->tyty || CT_TYPE_LONG == expr_ty->tyty;
    bool const is_from_unsigned = CT_TYPE_UCHAR == expr_ty->tyty || CT_TYPE_USHORT == expr_ty->tyty || CT_TYPE_UINT == expr_ty->tyty || CT_TYPE_ULONG == expr_ty->tyty;

    struct ct_slot tmp = {.ty= expr_ty};

    // if slot too small, make 'physical' temporary, else it'll used the same a `ct_slot`
    if (slot->ty->size < expr_ty->size) {
        _ct_alloc_slot(cs, &tmp);
        ct_compile_expression(cs, expr, &tmp);

        switch (tmp.usage) {
        case _slot_value:
            slot->usage = _slot_value;
            _ct_cancel_slot(cs, &tmp);
            switch (expr_ty->tyty <<8| slot->ty->tyty) {
            case CT_TYPE_LONG <<8| CT_TYPE_FLOAT:  slot->as.value.f = tmp.as.value.sl; break;
            case CT_TYPE_ULONG <<8| CT_TYPE_FLOAT: slot->as.value.f = tmp.as.value.ul; break;
            case CT_TYPE_FLOAT <<8| CT_TYPE_CHAR:   slot->as.value.c = tmp.as.value.f; break;
            case CT_TYPE_FLOAT <<8| CT_TYPE_UCHAR:  slot->as.value.uc = tmp.as.value.f; break;
            case CT_TYPE_FLOAT <<8| CT_TYPE_SCHAR:  slot->as.value.sc = tmp.as.value.f; break;
            case CT_TYPE_FLOAT <<8| CT_TYPE_SHORT:  slot->as.value.ss = tmp.as.value.f; break;
            case CT_TYPE_FLOAT <<8| CT_TYPE_USHORT: slot->as.value.us = tmp.as.value.f; break;
            case CT_TYPE_DOUBLE <<8| CT_TYPE_CHAR:   slot->as.value.c = tmp.as.value.d; break;
            case CT_TYPE_DOUBLE <<8| CT_TYPE_UCHAR:  slot->as.value.uc = tmp.as.value.d; break;
            case CT_TYPE_DOUBLE <<8| CT_TYPE_SCHAR:  slot->as.value.sc = tmp.as.value.d; break;
            case CT_TYPE_DOUBLE <<8| CT_TYPE_SHORT:  slot->as.value.ss = tmp.as.value.d; break;
            case CT_TYPE_DOUBLE <<8| CT_TYPE_INT:    slot->as.value.si = tmp.as.value.d; break;
            case CT_TYPE_DOUBLE <<8| CT_TYPE_USHORT: slot->as.value.us = tmp.as.value.d; break;
            case CT_TYPE_DOUBLE <<8| CT_TYPE_UINT:   slot->as.value.ui = tmp.as.value.d; break;
            case CT_TYPE_DOUBLE <<8| CT_TYPE_FLOAT: slot->as.value.f = tmp.as.value.d; break;
            default: memcpy(slot->as.value.bytes, tmp.as.value.bytes, slot->ty->size); // (yyy: endianness)
            }
            break;

        case _slot_used:
            slot->usage = _slot_used;
            // TODO: this is the int case
            _ct_emit_move(cs, at(slot), slot->ty->size, at(&tmp)); // (yyy: endianness)
            _ct_rewind_slot(cs, &tmp);
            break;

        case _slot_variable:
            slot->usage = _slot_variable;
            // TODO: this is the int case
            slot->as.variable = tmp.as.variable; // (yyy: endianness)
            _ct_cancel_slot(cs, &tmp);
            break;
        }
    }

    // slot large enough for the expr result
    else {
        tmp.loc = slot->loc;
        ct_compile_expression(cs, expr, &tmp);

        switch (tmp.usage) {
        case _slot_value:
            slot->usage = _slot_value;
            switch (expr_ty->tyty <<8| slot->ty->tyty) {
            case CT_TYPE_CHAR   <<8| CT_TYPE_FLOAT: slot->as.value.f = tmp.as.value.c; break;
            case CT_TYPE_UCHAR  <<8| CT_TYPE_FLOAT: slot->as.value.f = tmp.as.value.uc; break;
            case CT_TYPE_SCHAR  <<8| CT_TYPE_FLOAT: slot->as.value.f = tmp.as.value.sc; break;
            case CT_TYPE_SHORT  <<8| CT_TYPE_FLOAT: slot->as.value.f = tmp.as.value.ss; break;
            case CT_TYPE_INT    <<8| CT_TYPE_FLOAT: slot->as.value.f = tmp.as.value.si; break;
            case CT_TYPE_USHORT <<8| CT_TYPE_FLOAT: slot->as.value.f = tmp.as.value.us; break;
            case CT_TYPE_UINT   <<8| CT_TYPE_FLOAT: slot->as.value.f = tmp.as.value.ui; break;
            case CT_TYPE_CHAR   <<8| CT_TYPE_DOUBLE: slot->as.value.d = tmp.as.value.c; break;
            case CT_TYPE_UCHAR  <<8| CT_TYPE_DOUBLE: slot->as.value.d = tmp.as.value.uc; break;
            case CT_TYPE_SCHAR  <<8| CT_TYPE_DOUBLE: slot->as.value.d = tmp.as.value.sc; break;
            case CT_TYPE_SHORT  <<8| CT_TYPE_DOUBLE: slot->as.value.d = tmp.as.value.ss; break;
            case CT_TYPE_INT    <<8| CT_TYPE_DOUBLE: slot->as.value.d = tmp.as.value.si; break;
            case CT_TYPE_LONG   <<8| CT_TYPE_DOUBLE: slot->as.value.d = tmp.as.value.sl; break;
            case CT_TYPE_USHORT <<8| CT_TYPE_DOUBLE: slot->as.value.d = tmp.as.value.us; break;
            case CT_TYPE_UINT   <<8| CT_TYPE_DOUBLE: slot->as.value.d = tmp.as.value.ui; break;
            case CT_TYPE_ULONG  <<8| CT_TYPE_DOUBLE: slot->as.value.d = tmp.as.value.ul; break;
            case CT_TYPE_FLOAT <<8| CT_TYPE_INT:   slot->as.value.si = tmp.as.value.f; break;
            case CT_TYPE_FLOAT <<8| CT_TYPE_UINT:  slot->as.value.ui = tmp.as.value.f; break;
            case CT_TYPE_FLOAT <<8| CT_TYPE_LONG:  slot->as.value.sl = tmp.as.value.f; break;
            case CT_TYPE_FLOAT <<8| CT_TYPE_ULONG: slot->as.value.ul = tmp.as.value.f; break;
            case CT_TYPE_FLOAT <<8| CT_TYPE_DOUBLE: slot->as.value.d = tmp.as.value.f; break;
            case CT_TYPE_DOUBLE <<8| CT_TYPE_LONG:  slot->as.value.sl = tmp.as.value.d; break;
            case CT_TYPE_DOUBLE <<8| CT_TYPE_ULONG: slot->as.value.ul = tmp.as.value.d; break;
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
            _ct_emit_extend(cs, slot, &(struct ct_slot){
                .ty= expr_ty,
                .loc= tmp.loc,
                .usage= _slot_used,
            });
            break;

        case _slot_variable:
            slot->usage = _slot_used;
            // TODO: this is the int case
            _ct_emit_extend(cs, slot, &(struct ct_slot){
                .ty= expr_ty,
                .loc= tmp.as.variable,
                .usage= _slot_used,
            });
            break;
        }
    }
}
// }}}

void ct_compile_expression(ct_compile_state ref cs, ct_expression cref expr, struct ct_slot ref slot)
{
    // work variables that are better function-scoped and initialized early
    struct ct_slot tmp = {0};
    enum _ct_arith_op op = 0;

    switch (expr->kind) {
    case CT_ATOM:;
        size_t const len = expr->info.atom.len;
        char cref ptr = expr->info.atom.ptr;
        if ('"' == ptr[0]) {
            slot->as.variable = len;
            slot->usage = _slot_variable;
            return;
        }

        if ('\'' == ptr[0]) {
            size_t k = 1;
            // xxx: not supposed to be of type `char` but `int`
            slot->as.value.c = '\\' == ptr[1] ? ct_unescape(ptr, len, &k) : ptr[1];
            slot->usage = _slot_value;
            return;
        }

        if (('0' <= ptr[0] && ptr[0] <= '9') || '.' == ptr[0]) {
            enum ct_adpt_type_tag const tyty = ((struct ct_adpt_type const*)expr->usr)->tyty;
            switch (tyty) {
            case CT_TYPE_INT:
            case CT_TYPE_LONG:
            case CT_TYPE_UINT:
            case CT_TYPE_ULONG:
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

            case CT_TYPE_FLOAT:
            case CT_TYPE_DOUBLE:;
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

                if (CT_TYPE_FLOAT == tyty) slot->as.value.f = r;
                else slot->as.value.d = r;
                break;

            default:;
            }

            slot->usage = _slot_value;
            return;
        }

        struct ct_adpt_item const* found = cs->lookup(cs->usr, expr->info.atom);
        switch (found->kind) {
        case CT_ITEM_VALUE:
            slot->as.value.sl = found->as.value;
            slot->usage = _slot_value;
            break;

        case CT_ITEM_OBJECT:
            slot->as.value.ul = (size_t)found->as.object; // xxx: is a C pointer
            slot->usage = _slot_value;
            break;

        case CT_ITEM_TYPEDEF:
            // unreachable
            break;

        case CT_ITEM_VARIABLE:
            slot->as.variable = found->as.variable;
            slot->usage = _slot_variable;
            break;
        }
        return;

    case CT_BINOP_SUBSCR: {
            struct ct_slot base = {.ty= expr->info.subscr.base->usr};

            if (CT_TYPE_ARR == base.ty->tyty) {
                ct_compile_expression(cs, expr->info.subscr.base, &base);

                if (_slot_variable != base.usage) exitf("unreachable: array should have usage _slot_variable");

                struct ct_slot off = {.ty= &ct_adptb_long_type};
                _ct_alloc_slot(cs, &off);
                _ct_fit_expr_to_slot(cs, expr->info.subscr.off, &off);

                if (_slot_value == off.usage) {
                    _ct_cancel_slot(cs, &off);
                    slot->as.variable = base.as.variable + off.as.value.sl*base.ty->info.arr.item->size;
                    slot->usage = _slot_variable;
                } else {
                    if (1 != base.ty->info.arr.item->size) {
                        _ct_emit_arith(cs, _ops_muli, _ct_slot_arith_w(&off),
                                at(&off),
                                base.ty->info.arr.item->size,
                                _slot_variable == off.usage ? atv(&off) : at(&off));
                        off.usage = _slot_used;
                    } else if (_slot_variable == off.usage) _ct_cancel_slot(cs, &off);

                    struct ct_slot ptr = {.ty= &(struct ct_adpt_type){
                            .size= sizeof(void*), .align= sizeof(void*),
                            .tyty= CT_TYPE_PTR, // xxx: USL
                            .info.ptr= base.ty, // xxx: USL
                        }};
                    _ct_alloc_slot(cs, &ptr);
                    _ct_emit_lea(cs, at(&ptr), atv(&base));

                    _ct_emit_arith(cs, _ops_add, _ct_slot_arith_w(&off),
                            at(&ptr),
                            at(&ptr),
                            _slot_variable == off.usage ? atv(&off) : at(&off));

                    _ct_emit_read(cs, at(&ptr), slot);
                    slot->usage = _slot_used;

                    _ct_rewind_slot(cs, &ptr);
                    if (_slot_used == off.usage) _ct_rewind_slot(cs, &off);
                }
            }

            else {
                _ct_alloc_slot(cs, &base);
                ct_compile_expression(cs, expr->info.subscr.base, &base);

                if (_slot_value == base.usage) exitf("subscr on a compile time - this is surely very wrong");

                if (_slot_variable == base.usage) _ct_emit_move(cs, at(&base), base.ty->size, atv(&base));

                // on the stack is the ptr (base)
                // _fit off to sl
                // add res onto base
                // read from at base into slot over slot size

                struct ct_slot off = {.ty= &ct_adptb_long_type};
                _ct_alloc_slot(cs, &off);
                _ct_fit_expr_to_slot(cs, expr->info.subscr.off, &off);

                if (_slot_value == off.usage) {
                    _ct_cancel_slot(cs, &off);
                    _ct_emit_arith(cs, _ops_addi, _ct_slot_arith_w(&off),
                            at(&base),
                            _ct_slot_arith_v(&off)*base.ty->info.ptr->size,
                            at(&base));
                } else {
                    if (1 != base.ty->info.arr.item->size) {
                        _ct_emit_arith(cs, _ops_muli, _ct_slot_arith_w(&off),
                                at(&off),
                                base.ty->info.ptr->size,
                                _slot_variable == off.usage ? atv(&off) : at(&off));
                        off.usage = _slot_used;
                    } else if (_slot_variable == off.usage) _ct_cancel_slot(cs, &off);

                    _ct_emit_arith(cs, _ops_add, _ct_slot_arith_w(&off),
                            at(&base),
                            at(&base),
                            _slot_variable == off.usage ? atv(&off) : at(&off));

                    if (_slot_used == off.usage) _ct_rewind_slot(cs, &off);
                }

                _ct_emit_read(cs, at(&base), slot);
                slot->usage = _slot_used;

                _ct_rewind_slot(cs, &base);
            }
        }
        return;

    case CT_BINOP_CALL: {
            struct ct_slot fun = {.ty= expr->info.call.base->usr};
            _ct_alloc_slot(cs, &fun);

            ct_compile_expression(cs, expr->info.call.base, &fun);
            if (_slot_variable == fun.usage) _ct_cancel_slot(cs, &fun);
            else _ct_materialize_slot(cs, &fun);

            struct ct_slot args[15] = {0};
            struct ct_expr_call_arg const* cons = expr->info.call.first;
            size_t count = fun.ty->info.fun.count;
            for (size_t k = 0; k < count; k++, cons = cons->next) {
                args[k].ty = fun.ty->info.fun.params[k].type;
                _ct_alloc_slot(cs, args+k);

                _ct_fit_expr_to_slot(cs, cons->expr, args+k);
                if (_slot_variable == args[k].usage) _ct_cancel_slot(cs, args+k);
                else _ct_materialize_slot(cs, args+k);
            }

            _ct_emit_call_base(cs, count, at(slot), _slot_variable == fun.usage ? atv(&fun) : at(&fun));
            for (size_t k = 0; k < count; k++) _ct_emit_call_arg(cs, _slot_variable == args[k].usage ? atv(args+k) : at(args+k));

            for (size_t k = count; k; k--) if (_slot_used == args[k-1].usage) _ct_rewind_slot(cs, args+k-1);
            if (_slot_used == fun.usage) _ct_rewind_slot(cs, &fun);
            slot->usage = _slot_used;
        }
        return;

    case CT_BINOP_TERNCOND:
    case CT_BINOP_TERNBRANCH: {
            ct_expression cref condition = expr->info.binary.lhs;
            ct_expression cref consequence = expr->info.binary.rhs->info.binary.lhs;
            ct_expression cref alternative = expr->info.binary.rhs->info.binary.rhs;

            struct ct_slot cdt = {.ty= &ct_adptb_int_type};
            _ct_alloc_slot(cs, &cdt);
            _ct_fit_expr_to_slot(cs, condition, &cdt);

            if (_slot_value == cdt.usage) {
                _ct_cancel_slot(cs, &cdt);
                _ct_fit_expr_to_slot(cs, cdt.as.value.si ? consequence : alternative, slot);
            } else {
                // cdt
                // cmp1
                // breq -> csq
                // alt
                // jmp -> fall
                // csq
                // fall

                if (_slot_used != cdt.usage) _ct_cancel_slot(cs, &cdt);

                // wip
                _emit_instr_w_opr(0x14, _slot_value == cdt.usage ? atv(&cdt) : at(&cdt));
                _emit_instr_w_opr(0x0b, 0);
                {
                    struct ct_slot tmp = *slot;
                    _ct_fit_expr_to_slot(cs, alternative, &tmp);
                    switch (tmp.usage) {
                    case _slot_value: _ct_emit_data(cs, at(&tmp), tmp.ty->size, tmp.as.value.bytes); break; // (yyy: endianness)
                    case _slot_used: break;
                    case _slot_variable: _ct_emit_move(cs, at(&tmp), tmp.ty->size, atv(&tmp)); break;
                    }
                }
                _emit_instr_w_opr(0x0a, 0);
                {
                    struct ct_slot tmp = *slot;
                    _ct_fit_expr_to_slot(cs, consequence, &tmp);
                    switch (tmp.usage) {
                    case _slot_value: _ct_emit_data(cs, at(&tmp), tmp.ty->size, tmp.as.value.bytes); break; // (yyy: endianness)
                    case _slot_used: break;
                    case _slot_variable: _ct_emit_move(cs, at(&tmp), tmp.ty->size, atv(&tmp)); break;
                    }
                }
                slot->usage = _slot_used;

                if (_slot_used == cdt.usage) _ct_rewind_slot(cs, &cdt);
            }
        }
        return;

    case CT_BINOP_COMMA: {
            struct ct_slot drp = {.ty= expr->info.binary.lhs->usr};
            _ct_alloc_slot(cs, &drp);
            ct_compile_expression(cs, expr->info.binary.lhs, &drp);
            if (_slot_used != drp.usage) _ct_cancel_slot(cs, &drp);
            ct_compile_expression(cs, expr->info.binary.rhs, slot);
            if (_slot_used == drp.usage) _ct_rewind_slot(cs, &drp);
        }
        return;

        {
    case CT_BINOP_ASGN_BOR:  op = _ops_bori;  goto sw_asgn;
    case CT_BINOP_ASGN_BXOR: op = _ops_bxori; goto sw_asgn;
    case CT_BINOP_ASGN_BAND: op = _ops_bandi; goto sw_asgn;
    case CT_BINOP_ASGN_BSHL: op = _ops_bshli; goto sw_asgn;
    case CT_BINOP_ASGN_BSHR: op = _ops_bshri; goto sw_asgn;
    case CT_BINOP_ASGN_SUB:  op = _ops_subi;  goto sw_asgn;
    case CT_BINOP_ASGN_ADD:  op = _ops_addi;  goto sw_asgn;
    case CT_BINOP_ASGN_REM:  op = _ops_remi;  goto sw_asgn;
    case CT_BINOP_ASGN_DIV:  op = _ops_divi;  goto sw_asgn;
    case CT_BINOP_ASGN_MUL:  op = _ops_muli;  goto sw_asgn;
    case CT_BINOP_ASGN:
        sw_asgn:;

            ct_expression cref dst_ex = expr->info.binary.lhs;
            ct_expression cref src_ex = expr->info.binary.rhs;

            _ct_fit_expr_to_slot(cs, src_ex, slot);

            struct ct_slot dst = {.ty= dst_ex->usr};
            switch (dst_ex->kind) {
            case CT_ATOM:
            case CT_UNOP_MEMBER:
                ct_compile_expression(cs, dst_ex, &dst);
                // can be stack variable or static global object;
                // for now only support stack variable
                if (_slot_variable != dst.usage) exitf("NIY: only supports stack variables for now");

                if (op) {
                    tmp = dst;
                    goto sw_asgn_binop;
                }

                switch (slot->usage) {
                case _slot_value:
                    _ct_emit_data(cs, atv(&dst), dst.ty->size, slot->as.value.bytes); // (yyy: endianness)
                    break;
                case _slot_used:
                    _ct_emit_move(cs, atv(&dst), dst.ty->size, at(slot));
                    break;
                case _slot_variable:
                    _ct_emit_move(cs, atv(&dst), dst.ty->size, atv(slot));
                    break;
                }
                break; // return;

            case CT_BINOP_SUBSCR:
            case CT_UNOP_DEREF:
            case CT_UNOP_PMEMBER:
                exitf("NIY: assigning to this kind of lvalue");
                break;

            default:;
            }
        }
        return;

        {
    case CT_BINOP_BOR:  op = _ops_bori;  goto sw_binop;
    case CT_BINOP_BXOR: op = _ops_bxori; goto sw_binop;
    case CT_BINOP_BAND: op = _ops_bandi; goto sw_binop;
    case CT_BINOP_BSHL: op = _ops_bshli; goto sw_binop; // asy
    case CT_BINOP_BSHR: op = _ops_bshri; goto sw_binop; // asy
    case CT_BINOP_SUB:  op = _ops_subi;  goto sw_binop; // asy
    case CT_BINOP_ADD:  op = _ops_addi;  goto sw_binop;
    case CT_BINOP_REM:  op = _ops_remi;  goto sw_binop; // asy
    case CT_BINOP_DIV:  op = _ops_divi;  goto sw_binop; // asy
    case CT_BINOP_MUL:  op = _ops_muli;  goto sw_binop;
        sw_binop:;

            struct ct_slot lhs = *slot, rhs = {0};

            if (CT_TYPE_PTR == slot->ty->tyty) {
                // one is a pointer(/array), the other is promoted to long and mult by sizeof item
                ct_expression const* ptr, * off;
                struct ct_adpt_type const* ty = expr->info.binary.lhs->usr;
                if (CT_TYPE_PTR == ty->tyty || CT_TYPE_ARR == ty->tyty)
                    ptr = expr->info.binary.lhs, off = expr->info.binary.rhs;
                else ty = expr->info.binary.rhs->usr,
                    ptr = expr->info.binary.rhs, off = expr->info.binary.lhs;

                lhs = *slot;
                _ct_fit_expr_to_slot(cs, ptr, &lhs);
                if (_slot_value == lhs.usage) exitf("ptr arith on a compile time - this is surely very wrong");
                if (_slot_variable == lhs.usage) {
                    _ct_emit_move(cs, at(&lhs), slot->ty->size, atv(&lhs));
                    lhs.usage = _slot_used;
                }
                // YYY: to make things simpler, the slot is forcefully used
                // (with the lea in _fit or the move above); only case where
                // this is unnecessary is with CT_TYPE_PTR and _slot_variable
                // which, granted, is likely the most common case, but not
                // worth 'optimizing'

                rhs.ty = &ct_adptb_long_type;
                _ct_alloc_slot(cs, &rhs);
                _ct_fit_expr_to_slot(cs, off, &rhs);
                if (_slot_value == rhs.usage) {
                    _ct_cancel_slot(cs, &rhs);
                    rhs.as.value.sl*= slot->ty->info.ptr->size;
                } else {
                    _ct_emit_arith(cs, _ops_muli, _ct_slot_arith_w(&rhs),
                            at(&rhs),
                            slot->ty->info.ptr->size,
                            _slot_variable == rhs.usage ? atv(&rhs) : at(&rhs));
                    rhs.usage = _slot_used;
                }
            }

            else {
                lhs = *slot;
                _ct_fit_expr_to_slot(cs, expr->info.binary.lhs, &lhs);
                if (_slot_used != lhs.usage)
                    rhs = *slot;
                else {
                    rhs.ty = slot->ty;
                    _ct_alloc_slot(cs, &rhs);
                }
                _ct_fit_expr_to_slot(cs, expr->info.binary.rhs, &rhs);
                if (_slot_used == lhs.usage && _slot_used != rhs.usage) _ct_cancel_slot(cs, &rhs);
            }

            bool was_asgn = false;
            if (0) {
        sw_asgn_binop:
                was_asgn = true;

                rhs = *slot;
                lhs = *slot = tmp;
                // wip: only comming from case CT_ATOM in assignment
                // so for now the destination is a stack variable
            }

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
                struct ct_slot const* val, * xhs;
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
                _ct_emit_arith(cs, op, _ct_slot_arith_w(&rhs), // yyy: case ptr arith, is long, case arith, sizes of lhs/rhs/slot all same
                        _slot_variable == slot->usage ? atv(slot) : at(slot),
                        _ct_slot_arith_v(val),
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
                _ct_emit_arith(cs, op, _ct_slot_arith_w(&rhs), // yyy: case ptr arith, is long, case arith, sizes of lhs/rhs/slot all same
                        _slot_variable == slot->usage ? atv(slot) : at(slot),
                        _slot_variable == lhs.usage ? atv(&lhs) : at(&lhs),
                        _slot_variable == rhs.usage ? atv(&rhs) : at(&rhs));
                slot->usage = _slot_used;
                break;
            } // switch both usages

            if (was_asgn) {
                *slot = rhs;
                _ct_emit_move(cs, at(slot), slot->ty->size, atv(&lhs));
                slot->usage = _slot_used;
            } else {
                if (_slot_used == lhs.usage && _slot_used == rhs.usage) _ct_rewind_slot(cs, &rhs);
            }
        }
        return;

    case CT_UNOP_ADDR:
        exitf("NIY: address of");
        return;

    case CT_UNOP_DEREF: {
            struct ct_slot ptr = {.ty= expr->info.unary.opr->usr};
            _ct_alloc_slot(cs, &ptr);
            ct_compile_expression(cs, expr->info.unary.opr, &ptr);

            switch (ptr.usage) {
            case _slot_value:
                exitf("deref on a compile time - this is surely very wrong");

            case _slot_used:
                _ct_emit_read(cs, at(&ptr), slot);
                _ct_rewind_slot(cs, &ptr);
                slot->usage = _slot_used;
                break;

            case _slot_variable:
                _ct_cancel_slot(cs, &ptr);
                if (CT_TYPE_ARR == ptr.ty->tyty) {
                    slot->as.variable = ptr.as.variable;
                    slot->usage = _slot_variable;
                } else {
                    _ct_emit_read(cs, atv(&ptr), slot);
                    slot->usage = _slot_used;
                }
                break;
            }
        }
        return;

    case CT_UNOP_CAST:
        _ct_fit_expr_to_slot(cs, expr->info.cast.opr, slot);
        return;

    case CT_UNOP_PMEMBER:
        exitf("NIY: pmember");
    case CT_UNOP_MEMBER: {
            ct_expression cref base_ex = expr->info.member.base;
            ct_bufsl const name = *expr->info.member.name;
            struct ct_adpt_comp_desc cref comp = &((struct ct_adpt_type*)base_ex->usr)->info.comp;

            size_t off = 0;
            for (size_t k = 0; k < comp->count; k++) if (bufis(name, comp->fields[k].name)) {
                off = comp->fields[k].offset;
                break;
            }

            struct ct_slot base = {.ty= base_ex->usr};
            ct_compile_expression(cs, base_ex, &base);
            // can be stack variable or static global object or a function's return;
            // for now only support stack variable
            if (_slot_variable != base.usage) exitf("NIY: only supports stack variables for now");

            slot->as.variable = base.as.variable+off;
            slot->usage = _slot_variable;
        }
        return;

    case CT_BINOP_LOR:
    case CT_BINOP_LAND:
        exitf("NIY: branches");
        return;

    case CT_BINOP_EQ:
    case CT_BINOP_NE:
    case CT_BINOP_LT:
    case CT_BINOP_GT:
    case CT_BINOP_LE:
    case CT_BINOP_GE:
        exitf("NIY: comparisons");
        return;

    case CT_UNOP_BNOT:
    case CT_UNOP_LNOT:
    case CT_UNOP_MINUS:
    case CT_UNOP_PLUS:
        ct_compile_expression(cs, expr->info.unary.opr, slot);

        switch (slot->usage) {
        case _slot_value:
            switch (expr->kind) {
            case CT_UNOP_BNOT: _cfold_arith_ints(slot, &(struct ct_slot){0}, |~, slot); break;
            case CT_UNOP_LNOT: _cfold_arith(slot, &(struct ct_slot){0}, ||!, slot); break;
            case CT_UNOP_MINUS: _cfold_arith(slot, &(struct ct_slot){0}, -, slot); break;
            case CT_UNOP_PLUS: break;
            default:;
            }
            break;

            size_t at_slot;
        case _slot_used:     at_slot = at(slot); if (0)
        case _slot_variable: at_slot = atv(slot);
            switch (expr->kind) {
            case CT_UNOP_BNOT: _ct_emit_arith(cs, _ops_bxori, _ct_slot_arith_w(slot), at(slot), -1/* xxx: should be of the type's size, this is larger */, at_slot); break;
            case CT_UNOP_LNOT: exitf("NIY emit lnot"); break;
            case CT_UNOP_MINUS: _ct_emit_arith(cs, _ops_subi, _ct_slot_arith_w(slot), at(slot), 0, at_slot); break;
            case CT_UNOP_PLUS: _ct_emit_move(cs, at(slot), slot->ty->size, atv(slot)); break;
            default:;
            }
            slot->usage = _slot_used;
        }
        return;

        {
            bool post;
    case CT_UNOP_PRE_DEC: post = false; op = _ops_rsubi; goto sw_crement;
    case CT_UNOP_PRE_INC: post = false; op = _ops_addi;  goto sw_crement;
    case CT_UNOP_POST_DEC: post = true; op = _ops_rsubi; goto sw_crement;
    case CT_UNOP_POST_INC: post = true; op = _ops_addi;  goto sw_crement;
        sw_crement:

            switch (expr->info.unary.opr->kind) {
            case CT_ATOM:
            case CT_UNOP_MEMBER:
                ct_compile_expression(cs, expr->info.unary.opr, slot);
                // can be stack variable or static global object;
                // for now only support stack variable
                if (_slot_variable != slot->usage) exitf("NIY: only supports stack variables for now");

                if (post) {
                    _ct_emit_move(cs, at(slot), slot->ty->size, atv(slot));
                    slot->usage = _slot_used;
                }
                _ct_emit_arith(cs, op, _ct_slot_arith_w(slot), atv(slot),
                        CT_TYPE_PTR == slot->ty->tyty ? slot->ty->info.ptr->size : 1,
                        atv(slot));
                break;

            case CT_BINOP_SUBSCR:
            case CT_UNOP_DEREF:
            case CT_UNOP_PMEMBER:
                exitf("NIY: assigning to this kind of lvalue");
                break;

            default:;
            }
            return;
        }
    }

} // ct_compile_expression

#undef atv
#undef at

#endif // CINTRE_COMPILER_H
