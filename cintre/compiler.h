/// Bytecode compiler on top of the parser; example:
/// (interface is being worked on..)
/// ```c
/// compile_state cs = {0};
///
/// struct slot slot = {.ty= check_expression(&cs, expr)};
/// if (slot.ty) {
///     slot.ty = _truetype(slot.ty);
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
          char const* p;
          unsigned char bytes[8]; // (yyy: sizeof @This() -- assumes 64b)
      } value;
      size_t variable;
  } as;
};

/// the expression should have been sieved through `check_expression` first
void compile_expression(compile_state ref cs, expression cref expr, struct slot ref slot);

// ---

#define cstokn(__at) (cs->ls->tokens.ptr+(__at))

// compile utils {{{
#define at(__slt)  ((__slt)->loc - cs->vsp)
#define atv(__slt) ((__slt)->as.variable - cs->vsp)

unsigned _l2(size_t n)
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

void _alloc_slot(compile_state ref cs, struct slot ref slot)
{
    slot->codeat = cs->res.len;
    slot->end = cs->vsp;
    if (slot->ty->size) {
        cs->vsp = (cs->vsp - slot->ty->size) & (~(size_t)0<<_l2(slot->ty->align));
        _emit_instr_w_opr(0x0f, slot->end - cs->vsp);
    }
    slot->loc = cs->vsp;
}

void _cancel_slot(compile_state ref cs, struct slot cref slot)
{
    cs->res.len = slot->codeat;
    cs->vsp = slot->end;
}

void _rewind_slot(compile_state ref cs, struct slot cref slot)
{
    if (slot->end != cs->vsp)
        _emit_instr_w_opr(0x0d, slot->end - cs->vsp);
    cs->vsp = slot->end;
}

void _emit_data(compile_state ref cs, size_t const dst, size_t const width, unsigned char const* data)
{
    _emit_instr_w_opr(0x1d, dst, width);
    memcpy(dyarr_insert(&cs->res, cs->res.len, width), data, width);
}

void _emit_move(compile_state ref cs, size_t const dst, size_t const width, size_t const src)
{
    _emit_instr_w_opr(0x1f, dst, width, src);
}

void _emit_write(compile_state ref cs, size_t const ptr_dst_slt, struct slot cref slot_src)
{
    //_emit_instr_w_opr(0x2d, ptr_dst_slt, slot_src->ty->size, at(slot_src));
    _emit_instr_w_opr(0x2d, ptr_dst_slt, slot_src->ty->size, _slot_variable == slot_src->usage ? atv(slot_src) : at(slot_src));
}

void _emit_read(compile_state ref cs, size_t const ptr_src_slt, struct slot cref slot_dst)
{
    //_emit_instr_w_opr(0x2f, ptr_src_slt, slot_dst->ty->size, at(slot_dst));
    _emit_instr_w_opr(0x2f, ptr_src_slt, slot_dst->ty->size, _slot_variable == slot_dst->usage ? atv(slot_dst) : at(slot_dst));
}

void _emit_lea(compile_state ref cs, size_t const dst, size_t const off)
{
    _emit_instr_w_opr(0x20, dst, off);
}

//void _emit_slot_value(compile_state ref cs, struct slot cref slot) {
//    _emit_data(cs, at(slot), slot->ty->size, (unsigned char*)&slot->as.value.ul);
//}

void _emit_call_base(compile_state ref cs, unsigned const argc, size_t const ret, size_t const fun)
{
    _emit_instr_w_opr(argc<<4 | 0xc, ret, fun);
}

void _emit_call_arg(compile_state ref cs, size_t argv)
{
    unsigned count = 0;
    if (argv) for (size_t it = argv; it; count++) it>>= 7;
    else count = 1;

    unsigned char* w = dyarr_insert(&cs->res, cs->res.len, count);

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

void _emit_arith(compile_state ref cs, enum _arith_op aop, enum _arith_w w, size_t const dst, size_t const a, size_t const b)
{
    _emit_instr_w_opr(aop+w, dst, a, b);
}

enum _arith_w _slot_arith_w(struct slot cref slot)
{
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
        // unreachable cases
    case TYPE_VOID: case TYPE_STRUCT: case TYPE_UNION: case TYPE_FUN: case TYPE_PTR: case TYPE_ARR: case TYPE_NAMED:;
    }
    return 0;
}

size_t _slot_arith_v(struct slot cref slot)
{
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
        // unreachable cases
    case TYPE_VOID: case TYPE_STRUCT: case TYPE_UNION: case TYPE_FUN: case TYPE_PTR: case TYPE_ARR: case TYPE_NAMED:;
    }
    return 0;
}

void _emit_extend(compile_state ref cs, struct slot cref big_dst, struct slot cref small_src)
{
    // unsigned -> unsigned: zero extend
    // signed   -> signed:   sign extend
    // unsigned -> signed:   zero extend
    // signed   -> unsigned: sign extend

    bool const is_src_signed =
        TYPE_CHAR  == small_src->ty->tyty ||
        TYPE_SCHAR == small_src->ty->tyty ||
        TYPE_SHORT == small_src->ty->tyty ||
        TYPE_INT   == small_src->ty->tyty ||
        TYPE_LONG  == small_src->ty->tyty;

    _emit_instr_w_opr(_l2(small_src->ty->size)<<4 | _l2(big_dst->ty->size) | !is_src_signed<<2,
            //at(big_dst), at(small_src));
            _slot_variable == big_dst->usage ? atv(big_dst) : at(big_dst),
            _slot_variable == small_src->usage ? atv(small_src) : at(small_src));
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
        /* unreachable cases */  \
    case TYPE_VOID: case TYPE_FLOAT: case TYPE_DOUBLE: case TYPE_STRUCT: case TYPE_UNION: case TYPE_FUN: case TYPE_PTR: case TYPE_ARR: case TYPE_NAMED:;  \
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
        /* unreachable cases */  \
    case TYPE_VOID: case TYPE_STRUCT: case TYPE_UNION: case TYPE_FUN: case TYPE_PTR: case TYPE_ARR: case TYPE_NAMED:;  \
    } while (0)

/// makes a slot "pysical", ie if it was a value, it will be inserted as data
/// and marked as used (but if it was a variable, it will stay as is)
void _materialize_slot(compile_state ref cs, struct slot ref slot)
{
    if (_slot_value == slot->usage) {
        _emit_data(cs, at(slot), slot->ty->size, slot->as.value.bytes); // (yyy: endianness)
        slot->usage = _slot_used;
    }
}

//void _write_object_from_stack(compile_state ref cs, struct slot cref from, void cref as_object)

void _read_object_to_stack(compile_state ref cs, struct slot cref into, void cref as_object)
{
    struct slot ptr = {.ty= &(struct adpt_type){
        .size= sizeof(void*), .align= sizeof(void*),
        .tyty= TYPE_PTR, // xxx: USL
        .info.ptr= into->ty, // xxx: USL
    }};
    _alloc_slot(cs, &ptr);
    _emit_data(cs, at(&ptr), ptr.ty->size, (unsigned char*)&as_object); // (yyy: endianness)
    _emit_read(cs, at(&ptr), into);
    _rewind_slot(cs, &ptr);
}

void _value_cvt_matrix(struct slot ref dst, struct slot cref src)
{
#   define _as_CHAR(__s)    ((__s)->as.value.c)
#   define _as_UCHAR(__s)   ((__s)->as.value.uc)
#   define _as_SCHAR(__s)   ((__s)->as.value.sc)
#   define _as_SHORT(__s)   ((__s)->as.value.ss)
#   define _as_INT(__s)     ((__s)->as.value.si)
#   define _as_LONG(__s)    ((__s)->as.value.sl)
#   define _as_USHORT(__s)  ((__s)->as.value.us)
#   define _as_UINT(__s)    ((__s)->as.value.ui)
#   define _as_ULONG(__s)   ((__s)->as.value.ul)
#   define _as_FLOAT(__s)   ((__s)->as.value.f)
#   define _as_DOUBLE(__s)  ((__s)->as.value.d)
#   define _as(__t, __s) _as_##__t(__s)

#   define _list1(__do     ) __do(CHAR     ) __do(UCHAR     ) __do(SCHAR     ) __do(SHORT     ) __do(INT     ) __do(LONG     ) __do(USHORT     ) __do(UINT     ) __do(ULONG     ) __do(FLOAT     ) __do(DOUBLE     )
#   define _list2(__do, __x) __do(__x, CHAR) __do(__x, UCHAR) __do(__x, SCHAR) __do(__x, SHORT) __do(__x, INT) __do(__x, LONG) __do(__x, USHORT) __do(__x, UINT) __do(__x, ULONG) __do(__x, FLOAT) __do(__x, DOUBLE)

#   define _do1(__tdst        ) case TYPE_##__tdst: switch (src->ty->tyty) { _list2(_do2, __tdst) /* unreachable cases */ case TYPE_VOID: case TYPE_STRUCT: case TYPE_UNION: case TYPE_FUN: case TYPE_PTR: case TYPE_ARR: case TYPE_NAMED:; } break;
#   define _do2(__tdst, __tsrc) case TYPE_##__tsrc: _as(__tdst, dst) = _as(__tsrc, src); break;
                                                    switch (dst->ty->tyty) { _list1(_do1        ) /* unreachable cases */ case TYPE_VOID: case TYPE_STRUCT: case TYPE_UNION: case TYPE_FUN: case TYPE_PTR: case TYPE_ARR: case TYPE_NAMED:; }
#   undef _do2
#   undef _do1

#   undef _list2
#   undef _list1

#   undef _as
#   undef _as_DOUBLE
#   undef _as_FLOAT
#   undef _as_ULONG
#   undef _as_UINT
#   undef _as_USHORT
#   undef _as_LONG
#   undef _as_INT
#   undef _as_SHORT
#   undef _as_SCHAR
#   undef _as_UCHAR
#   undef _as_CHAR
}

/// fits an expression to a slot by compiling into it with any necessary
/// conversion step (allocates and de-allocates a slot if needed)
void _fit_expr_to_slot(compile_state ref cs, expression cref expr, struct slot ref slot)
{
    // so we have `slot` and `expr` of two types
    // compatibility should already be ensured by `check_expression`

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

    struct adpt_type cref expr_ty = _truetype(expr->usr);
    if (TYPE_ARR == expr_ty->tyty && TYPE_PTR == slot->ty->tyty) {
        struct slot tmp = {.ty= expr_ty};
        compile_expression(cs, expr, &tmp);

        if (_slot_variable != tmp.usage) notif("IDK: array should have usage _slot_variable");

        _emit_lea(cs, at(slot), atv(&tmp));
        slot->usage = _slot_used;
        return;
    }

    bool const is_to_int = TYPE_CHAR <= slot->ty->tyty && slot->ty->tyty <= TYPE_ULONG;
    bool const is_to_flt = TYPE_FLOAT <= slot->ty->tyty && slot->ty->tyty <= TYPE_DOUBLE;

    // TODO: more properly handle ptr <-> int

    // not a number, nothing to do about it, all should be already type-sound
    if (!is_to_int && !is_to_flt) {
        compile_expression(cs, expr, slot);
        return;
    }

    bool const is_from_int = TYPE_CHAR <= expr_ty->tyty && expr_ty->tyty <= TYPE_ULONG;
    //bool const is_from_flt = TYPE_FLOAT <= expr_ty->tyty && expr_ty->tyty <= TYPE_DOUBLE;

    // same size -> nothing to do
    if (slot->ty->size == expr_ty->size && is_from_int == is_to_int) {
        compile_expression(cs, expr, slot);
        return;
    }

    //bool const is_to_signed = TYPE_SCHAR == slot->ty->tyty || TYPE_SHORT == slot->ty->tyty || TYPE_INT == slot->ty->tyty || TYPE_LONG == slot->ty->tyty;
    //bool const is_to_unsigned = TYPE_UCHAR == slot->ty->tyty || TYPE_USHORT == slot->ty->tyty || TYPE_UINT == slot->ty->tyty || TYPE_ULONG == slot->ty->tyty;
    //bool const is_from_signed = TYPE_SCHAR == expr_ty->tyty || TYPE_SHORT == expr_ty->tyty || TYPE_INT == expr_ty->tyty || TYPE_LONG == expr_ty->tyty;
    //bool const is_from_unsigned = TYPE_UCHAR == expr_ty->tyty || TYPE_USHORT == expr_ty->tyty || TYPE_UINT == expr_ty->tyty || TYPE_ULONG == expr_ty->tyty;

    struct slot tmp = {.ty= expr_ty};

    // if slot too small, make 'physical' temporary, else it'll used the same a `slot`
    if (slot->ty->size < expr_ty->size) {
        _alloc_slot(cs, &tmp);
        compile_expression(cs, expr, &tmp);

        switch (tmp.usage) {
        case _slot_value:
            _cancel_slot(cs, &tmp);
            _value_cvt_matrix(slot, &tmp);
            slot->usage = _slot_value;
            break;

        case _slot_used:
            // TODO: this is the int case
            _emit_move(cs, at(slot), slot->ty->size, at(&tmp)); // (yyy: endianness)
            _rewind_slot(cs, &tmp);
            slot->usage = _slot_used;
            break;

        case _slot_variable:
            // TODO: this is the int case
            slot->as.variable = tmp.as.variable; // (yyy: endianness)
            _cancel_slot(cs, &tmp);
            slot->usage = _slot_variable;
            break;
        }
    }

    // slot large enough for the expr result
    else {
        tmp.loc = slot->loc;
        compile_expression(cs, expr, &tmp);

        switch (tmp.usage) {
        case _slot_value:
            _value_cvt_matrix(slot, &tmp);
            slot->usage = _slot_value;
            break;

        case _slot_used:
            // TODO: this is the int case
            _emit_extend(cs, slot, &(struct slot){
                .ty= expr_ty,
                .loc= tmp.loc,
                .usage= _slot_used,
            });
            slot->usage = _slot_used;
            break;

        case _slot_variable:
            // TODO: this is the int case
            _emit_extend(cs, slot, &(struct slot){
                .ty= expr_ty,
                .loc= tmp.as.variable,
                .usage= _slot_used,
            });
            slot->usage = _slot_used;
            break;
        }
    }
} // _fit_expr_to_slot

/// returned slot should be
/// - value, and it's an object (C pointer in as.value.p) (don't cancel)
/// - used, and it's a pointer put on the stack; slot's ty is not correct (as in it is not the pointer type)! (slot should be rewinded afterward)
/// - variable (don't cancel)
struct slot _compile_lvalue(compile_state ref cs, expression cref expr)
{
    struct slot r = {.ty= _truetype(expr->usr)};

    switch (expr->kind) {
    case ATOM: {
            // can only be ident
            struct adpt_item cref found = cs->lookup(cs->usr, cstokn(expr->info.atom));
            switch (found->kind) {
            case ITEM_OBJECT:
                r.as.value.p = found->as.object;
                r.usage = _slot_value;
                break;

            case ITEM_VARIABLE:
                r.as.variable = found->as.variable;
                r.usage = _slot_variable;
                break;

                // unreachable cases
            case ITEM_VALUE: case ITEM_TYPEDEF:;
            }
        }
        break;

    case UNOP_MEMBER: {
            expression cref base_ex = expr->info.member.base;

            char cref name = cstokn(expr->info.member.name);
            struct adpt_type cref base_ty = _truetype(base_ex->usr);
            struct adpt_comp_desc cref comp = &base_ty->info.comp;
            size_t off = 0;
            for (size_t k = 0; k < comp->count; k++) if (!strcmp(name, comp->fields[k].name)) {
                off = comp->fields[k].offset;
                break;
            }

            struct slot base = {.ty= base_ty};
            _alloc_slot(cs, &base);
            compile_expression(cs, base_ex, &base);

            switch (base.usage) {
            case _slot_value:
                _cancel_slot(cs, &base);
                r.as.value.p = base.as.value.p+off;
                r.usage = _slot_value;
                break;

            case _slot_used:
                r.as.variable = base.loc+off;
                r.usage = _slot_variable;
                // caller doesn't know about `base` being on the stack
                // but it works out because:
                // - if caller allocs another one, it will be correctly put atop `base`
                // - when caller rewind/cancel its own, it will encompase `base`
                // this is not great, quite hackish even, and I will not be
                // surprised when this eventually generates unexpected things
                break;

            case _slot_variable:
                _cancel_slot(cs, &base);
                r.as.variable = base.as.variable+off;
                r.usage = _slot_variable;
                break;
            }
        }
        break;

    case BINOP_SUBSCR:
    case UNOP_DEREF:
    case UNOP_PMEMBER:
        notif("NIY: this kind of lvalue");
        r.usage = _slot_value;
        break;

        // unreachable cases
    default:; // (43 cases ><'')
    }

    return r;
}
// }}}

void compile_expression(compile_state ref cs, expression cref expr, struct slot ref slot)
{
    // work variables that are better function-scoped and initialized early
    struct slot tmp = {0};
    enum _arith_op op = 0;

    switch (expr->kind) {
    case ATOM:;
        char cref atom = cstokn(expr->info.atom);
        if ('"' == atom[0]) {
            exitf("FIXME: slot->as.variable = len"); // (from checker)
            slot->usage = _slot_variable;
            return;
        }

        if ('\'' == atom[0]) {
            union { long l; char b[sizeof(long)]; } r = {0};
            lex_struqo(r.b, sizeof r.b, atom);
            slot->as.value.c = r.l; // xxx: little endian
            slot->usage = _slot_value;
            return;
        }

        if (('0' <= atom[0] && atom[0] <= '9') || '.' == atom[0]) {
            size_t const len = strlen(atom);

            // no _truetype because it's an adptb_.._type from checker
            enum adpt_type_tag const tyty = ((struct adpt_type const*)expr->usr)->tyty;
            switch (tyty) {
            case TYPE_INT:
            case TYPE_LONG:
            case TYPE_UINT:
            case TYPE_ULONG:
                if (len <3) {
                    if ('9' < atom[len-1]) slot->as.value.ul = atom[0]-'0';
                    else slot->as.value.ul = atom[len-1]-'0' + (2 == len ? (atom[len-2]-'0')*10 : 0);
                } else {
                    unsigned long r = 0;
                    size_t k = 0;

                    unsigned shft = 0;
                    char const* dgts = "0123456789";
                    if ('0' == atom[0]) switch (atom[1]) {
                    case 'B': case 'b': k = 2; shft = 1; dgts = "01";               break;
                    case 'O': case 'o': k = 2; shft = 3; dgts = "01234567";         break;
                    case 'X': case 'x': k = 2; shft = 4; dgts = "0123456789abcdef"; break;
                    }
                    char const* v = strchr(dgts, atom[k]|32);
                    do if ('\'' != atom[k]) r = (!shft ? r*10 : r<<shft) + (v-dgts);
                    while (++k < len && ('\'' == atom[k] || (v = strchr(dgts, atom[k]|32))));

                    slot->as.value.ul = r;
                }
                break;

            case TYPE_FLOAT:
            case TYPE_DOUBLE:;
                double r = 0;

                if (len <3 || 'x' != (atom[1]|32)) {
                    size_t k = 0;

                    while (k < len && '.' != atom[k] && 'e' != (atom[k]|32) && 'f' != (atom[k]|32))
                        r = r*10 + (atom[k++]-'0');
                    if (k < len && '.' == atom[k]) {
                        double d = 1;
                        while (++k < len && 'e' != (atom[k]|32) && 'f' != (atom[k]|32))
                            r+= (atom[k]-'0')*(d/= 10);
                    }
                    if (k < len && 'e' == (atom[k]|32)) {
                        unsigned long d = atom[++k]-'0';
                        while (++k < len && 'f' != (atom[k]|32)) d = d*10 + (atom[k]-'0');
                        for (; d; d--) r*= 10;
                    }
                } else {
                    size_t k = 2;
                    static char cref dgts = "0123456789abcdef";

                    while (k < len && '.' != atom[k] && 'p' != (atom[k]|32))
                        r = r*16 + (strchr(dgts, atom[k++]|32)-dgts);
                    if (k < len && '.' == atom[k]) {
                        double d = 1;
                        while (++k < len && 'p' != (atom[k]|32))
                            r+= (strchr(dgts, atom[k]|32)-dgts)*(d/= 16);
                    }
                    if (k < len && 'p' == (atom[k]|32)) {
                        unsigned long d = atom[++k]-'0';
                        while (++k < len && 'f' != (atom[k]|32)) d = d*2 + (atom[k]-'0');
                        for (; d; d--) r*= 2;
                    }
                }

                if (TYPE_FLOAT == tyty) slot->as.value.f = r;
                else slot->as.value.d = r;
                break;

            default:; // (12 cases ><'')
            }

            slot->usage = _slot_value;
            return;
        }

        struct adpt_item cref found = cs->lookup(cs->usr, atom);
        switch (found->kind) {
        case ITEM_VALUE:
            slot->as.value.sl = found->as.value;
            slot->usage = _slot_value;
            break;

        case ITEM_OBJECT:
            _read_object_to_stack(cs, slot, found->as.object);
            slot->usage = _slot_used;
            break;

        case ITEM_TYPEDEF:
            // unreachable
            break;

        case ITEM_VARIABLE:
            slot->as.variable = found->as.variable;
            slot->usage = _slot_variable;
            break;
        }
        return;

    case BINOP_SUBSCR: {
            struct slot base = {.ty= _truetype(expr->info.subscr.base->usr)};

            if (TYPE_ARR == base.ty->tyty) {
                compile_expression(cs, expr->info.subscr.base, &base);

                if (_slot_variable != base.usage) {
                    notif("IDK: array should have usage _slot_variable");
                    slot->usage = _slot_used;
                    return;
                }

                struct slot off = {.ty= &adptb_long_type};
                _alloc_slot(cs, &off);
                _fit_expr_to_slot(cs, expr->info.subscr.off, &off);

                if (_slot_value == off.usage) {
                    _cancel_slot(cs, &off);
                    slot->as.variable = base.as.variable + off.as.value.sl*base.ty->info.arr.item->size;
                    slot->usage = _slot_variable;
                } else {
                    if (1 != base.ty->info.arr.item->size) {
                        _emit_arith(cs, _ops_muli, _slot_arith_w(&off),
                                at(&off),
                                base.ty->info.arr.item->size,
                                _slot_variable == off.usage ? atv(&off) : at(&off));
                        off.usage = _slot_used;
                    } else if (_slot_variable == off.usage) _cancel_slot(cs, &off);

                    struct slot ptr = {.ty= &(struct adpt_type){
                        .size= sizeof(void*), .align= sizeof(void*),
                        .tyty= TYPE_PTR, // xxx: USL
                        .info.ptr= base.ty, // xxx: USL
                    }};
                    _alloc_slot(cs, &ptr);
                    _emit_lea(cs, at(&ptr), atv(&base));

                    _emit_arith(cs, _ops_add, _slot_arith_w(&off),
                            at(&ptr),
                            at(&ptr),
                            _slot_variable == off.usage ? atv(&off) : at(&off));

                    _emit_read(cs, at(&ptr), slot);
                    slot->usage = _slot_used;

                    _rewind_slot(cs, &ptr);
                    if (_slot_used == off.usage) _rewind_slot(cs, &off);
                }
            }

            else {
                _alloc_slot(cs, &base);
                compile_expression(cs, expr->info.subscr.base, &base);

                if (_slot_value == base.usage) {
                    notif("IDK: subscr on a compile time - this is surely very wrong");
                    _cancel_slot(cs, &base);
                    slot->usage = _slot_used;
                    return;
                }

                if (_slot_variable == base.usage) _emit_move(cs, at(&base), base.ty->size, atv(&base));

                // on the stack is the ptr (base)
                // _fit off to sl
                // add res onto base
                // read from at base into slot over slot size

                struct slot off = {.ty= &adptb_long_type};
                _alloc_slot(cs, &off);
                _fit_expr_to_slot(cs, expr->info.subscr.off, &off);

                if (_slot_value == off.usage) {
                    _cancel_slot(cs, &off);
                    _emit_arith(cs, _ops_addi, _slot_arith_w(&off),
                            at(&base),
                            _slot_arith_v(&off)*base.ty->info.ptr->size,
                            at(&base));
                } else {
                    if (1 != base.ty->info.arr.item->size) {
                        _emit_arith(cs, _ops_muli, _slot_arith_w(&off),
                                at(&off),
                                base.ty->info.ptr->size,
                                _slot_variable == off.usage ? atv(&off) : at(&off));
                        off.usage = _slot_used;
                    } else if (_slot_variable == off.usage) _cancel_slot(cs, &off);

                    _emit_arith(cs, _ops_add, _slot_arith_w(&off),
                            at(&base),
                            at(&base),
                            _slot_variable == off.usage ? atv(&off) : at(&off));

                    if (_slot_used == off.usage) _rewind_slot(cs, &off);
                }

                _emit_read(cs, at(&base), slot);
                slot->usage = _slot_used;

                _rewind_slot(cs, &base);
            }
        }
        return;

    case BINOP_CALL: {
            struct slot fun = {.ty= _truetype(expr->info.call.base->usr)};
            _alloc_slot(cs, &fun);

            compile_expression(cs, expr->info.call.base, &fun);
            if (_slot_variable == fun.usage) _cancel_slot(cs, &fun);
            else _materialize_slot(cs, &fun);

            struct slot args[15] = {0};
            struct expr_call_arg const* cons = expr->info.call.first;
            size_t count = fun.ty->info.fun.count;
            for (size_t k = 0; k < count; k++, cons = cons->next) {
                args[k].ty = fun.ty->info.fun.params[k].type;
                _alloc_slot(cs, args+k);

                _fit_expr_to_slot(cs, cons->expr, args+k);
                if (_slot_variable == args[k].usage) _cancel_slot(cs, args+k);
                else _materialize_slot(cs, args+k);
            }

            _emit_call_base(cs, count, at(slot), _slot_variable == fun.usage ? atv(&fun) : at(&fun));
            for (size_t k = 0; k < count; k++) _emit_call_arg(cs, _slot_variable == args[k].usage ? atv(args+k) : at(args+k));

            for (size_t k = count; k; k--) if (_slot_used == args[k-1].usage) _rewind_slot(cs, args+k-1);
            if (_slot_used == fun.usage) _rewind_slot(cs, &fun);
            slot->usage = _slot_used;
        }
        return;

    case BINOP_TERNCOND:
    case BINOP_TERNBRANCH: {
            expression cref condition = expr->info.binary.lhs;
            expression cref consequence = expr->info.binary.rhs->info.binary.lhs;
            expression cref alternative = expr->info.binary.rhs->info.binary.rhs;

            struct slot cdt = {.ty= &adptb_int_type};
            _alloc_slot(cs, &cdt);
            _fit_expr_to_slot(cs, condition, &cdt);

            if (_slot_value == cdt.usage) {
                _cancel_slot(cs, &cdt);
                _fit_expr_to_slot(cs, cdt.as.value.si ? consequence : alternative, slot);
            } else {
                // cdt
                // cmp1
                // breq -> csq
                // alt
                // jmp -> fall
                // csq
                // fall

                if (_slot_used != cdt.usage) _cancel_slot(cs, &cdt);

                // wip
                _emit_instr_w_opr(0x14, _slot_value == cdt.usage ? atv(&cdt) : at(&cdt));
                _emit_instr_w_opr(0x0b, 0);
                {
                    struct slot tmp = *slot;
                    _fit_expr_to_slot(cs, alternative, &tmp);
                    switch (tmp.usage) {
                    case _slot_value: _emit_data(cs, at(&tmp), tmp.ty->size, tmp.as.value.bytes); break; // (yyy: endianness)
                    case _slot_used: break;
                    case _slot_variable: _emit_move(cs, at(&tmp), tmp.ty->size, atv(&tmp)); break;
                    }
                }
                _emit_instr_w_opr(0x0a, 0);
                {
                    struct slot tmp = *slot;
                    _fit_expr_to_slot(cs, consequence, &tmp);
                    switch (tmp.usage) {
                    case _slot_value: _emit_data(cs, at(&tmp), tmp.ty->size, tmp.as.value.bytes); break; // (yyy: endianness)
                    case _slot_used: break;
                    case _slot_variable: _emit_move(cs, at(&tmp), tmp.ty->size, atv(&tmp)); break;
                    }
                }
                slot->usage = _slot_used;

                if (_slot_used == cdt.usage) _rewind_slot(cs, &cdt);
            }
        }
        return;

    case BINOP_COMMA: {
            struct slot drp = {.ty= _truetype(expr->info.binary.lhs->usr)};
            _alloc_slot(cs, &drp);
            compile_expression(cs, expr->info.binary.lhs, &drp);
            if (_slot_used != drp.usage) _cancel_slot(cs, &drp);
            else _rewind_slot(cs, &drp);
            compile_expression(cs, expr->info.binary.rhs, slot);
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

            struct slot dst = _compile_lvalue(cs, dst_ex);

            switch (dst.usage) {
            case _slot_value:;
                void cref as_object = dst.as.value.p;
                dst.ty = &(struct adpt_type){
                    .size= sizeof(void*), .align= sizeof(void*),
                    .tyty= TYPE_PTR, // xxx: USL
                    .info.ptr= dst.ty, // xxx: USL
                };
                _alloc_slot(cs, &dst);
                _emit_data(cs, at(&dst), dst.ty->size, (unsigned char*)&as_object); // (yyy: endianness)
                break;

            case _slot_used:
                break;

            case _slot_variable:
                if (op) {
                    tmp = dst;
                    goto sw_asgn_binop;
                }

                switch (slot->usage) {
                case _slot_value:    _emit_data(cs, atv(&dst), dst.ty->size, slot->as.value.bytes); break; // (yyy: endianness)
                case _slot_used:     _emit_move(cs, atv(&dst), dst.ty->size, at(slot));             break;
                case _slot_variable: _emit_move(cs, atv(&dst), dst.ty->size, atv(slot));            break;
                }
                return;
            }
            // if falling out: dst is a pointer to far, put on the stack

            if (op) {
                notif("NIY: using this kind of lvalue here (compound assignment through pointer or to global variable)");
                slot->usage = _slot_used;
                return;
            }

            _materialize_slot(cs, slot);
            _emit_write(cs, at(&dst), slot);
            _rewind_slot(cs, &dst);
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

            struct slot lhs = *slot, rhs = {0};

            if (TYPE_PTR == slot->ty->tyty) {
                // one is a pointer(/array), the other is promoted to long and mult by sizeof item
                expression const* ptr, * off;
                struct adpt_type const* ty = _truetype(expr->info.binary.lhs->usr);
                if (TYPE_PTR == ty->tyty || TYPE_ARR == ty->tyty)
                    ptr = expr->info.binary.lhs, off = expr->info.binary.rhs;
                else ty = _truetype(expr->info.binary.rhs->usr),
                    ptr = expr->info.binary.rhs, off = expr->info.binary.lhs;

                lhs = *slot;
                _fit_expr_to_slot(cs, ptr, &lhs);
                if (_slot_value == lhs.usage) {
                    notif("IDK: ptr arith on a compile time - this is surely very wrong");
                    slot->usage = _slot_used;
                    return;
                }
                if (_slot_variable == lhs.usage) {
                    _emit_move(cs, at(&lhs), slot->ty->size, atv(&lhs));
                    lhs.usage = _slot_used;
                }
                // YYY: to make things simpler, the slot is forcefully used
                // (with the lea in _fit or the move above); only case where
                // this is unnecessary is with TYPE_PTR and _slot_variable
                // which, granted, is likely the most common case, but not
                // worth 'optimizing'

                rhs.ty = &adptb_long_type;
                _alloc_slot(cs, &rhs);
                _fit_expr_to_slot(cs, off, &rhs);
                if (_slot_value == rhs.usage) {
                    _cancel_slot(cs, &rhs);
                    rhs.as.value.sl*= slot->ty->info.ptr->size;
                } else {
                    _emit_arith(cs, _ops_muli, _slot_arith_w(&rhs),
                            at(&rhs),
                            slot->ty->info.ptr->size,
                            _slot_variable == rhs.usage ? atv(&rhs) : at(&rhs));
                    rhs.usage = _slot_used;
                }
            }

            else {
                lhs = *slot;
                _fit_expr_to_slot(cs, expr->info.binary.lhs, &lhs);
                if (_slot_used != lhs.usage)
                    rhs = *slot;
                else {
                    rhs.ty = slot->ty;
                    _alloc_slot(cs, &rhs);
                }
                _fit_expr_to_slot(cs, expr->info.binary.rhs, &rhs);
                if (_slot_used == lhs.usage && _slot_used != rhs.usage) _cancel_slot(cs, &rhs);
            }

            bool was_asgn = false;
            if (0) {
        sw_asgn_binop:
                was_asgn = true;

                rhs = *slot;
                lhs = *slot = tmp;
                // wip: only comming from case ATOM in assignment
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
                    // unreachable cases
                default:; // (15 cases ><'')
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
                    // unreachable or noop cases
                default:; // (20 cases ><'')
                }
                if (0)
            case _slot_value    <<8| _slot_used:
            case _slot_value    <<8| _slot_variable:
                    // lhs is value, use the [..]i form
                    val = &lhs, xhs = &rhs;
                _emit_arith(cs, op, _slot_arith_w(&rhs), // yyy: case ptr arith, is long, case arith, sizes of lhs/rhs/slot all same
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
                    // unreachable cases
                default:; // (15 cases ><'')
                }
                _emit_arith(cs, op, _slot_arith_w(&rhs), // yyy: case ptr arith, is long, case arith, sizes of lhs/rhs/slot all same
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

    case UNOP_ADDR: {
            //if (..FUN..) slot->usage = _slot_var.. idk;

            struct slot obj = _compile_lvalue(cs, expr->info.unary.opr);

            switch (obj.usage) {
            case _slot_value:
                slot->as.value.p = obj.as.value.p;
                slot->usage = _slot_value;
                break;

            case _slot_used:
                //notif("NIY: only supports stack variables for now");
                //slot->usage = _slot_used;
                //_rewind_slot(cs, &obj);
                // XXX: hackish atop hackish
                slot->as.variable = obj.loc;
                slot->usage = _slot_variable;
                return;

            case _slot_variable:
                _emit_lea(cs, at(slot), atv(&obj));
                slot->usage = _slot_used;
                break;
            }
        }
        return;

    case UNOP_DEREF: {
            struct slot ptr = {.ty= _truetype(expr->info.unary.opr->usr)};
            _alloc_slot(cs, &ptr);
            compile_expression(cs, expr->info.unary.opr, &ptr);

            switch (ptr.usage) {
            case _slot_value:
                notif("IDK: deref on a compile time - this is surely very wrong");
                slot->usage = _slot_used;
                return;

            case _slot_used:
                _emit_read(cs, at(&ptr), slot);
                _rewind_slot(cs, &ptr);
                slot->usage = _slot_used;
                break;

            case _slot_variable:
                _cancel_slot(cs, &ptr);
                if (TYPE_ARR == ptr.ty->tyty) {
                    slot->as.variable = ptr.as.variable;
                    slot->usage = _slot_variable;
                } else {
                    _emit_read(cs, atv(&ptr), slot);
                    slot->usage = _slot_used;
                }
                break;
            }
        }
        return;

    case UNOP_CAST:
        _fit_expr_to_slot(cs, expr->info.cast.opr, slot);
        return;

    case UNOP_PMEMBER:
        notif("NIY: pmember");
        slot->usage = _slot_used;
        return;
    case UNOP_MEMBER: {
            expression cref base_ex = expr->info.member.base;
            char cref name = cstokn(expr->info.member.name);
            struct adpt_type cref base_ty = _truetype(base_ex->usr);
            struct adpt_comp_desc cref comp = &base_ty->info.comp;

            size_t off = 0;
            for (size_t k = 0; k < comp->count; k++) if (!strcmp(name, comp->fields[k].name)) {
                off = comp->fields[k].offset;
                break;
            }

            struct slot base = {.ty= base_ty};
            compile_expression(cs, base_ex, &base);
            // can be stack variable or static global object or a function's return;
            // for now only support stack variable
            if (_slot_variable != base.usage) {
                notif("NIY: only supports stack variables for now");
                slot->usage = _slot_used;
                return;
            }

            slot->as.variable = base.as.variable+off;
            slot->usage = _slot_variable;
        }
        return;

    case BINOP_LOR:
    case BINOP_LAND:
        notif("NIY: branches");
        slot->usage = _slot_used;
        return;

    case BINOP_EQ:
    case BINOP_NE:
    case BINOP_LT:
    case BINOP_GT:
    case BINOP_LE:
    case BINOP_GE:
        notif("NIY: comparisons");
        slot->usage = _slot_used;
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
                // unreachable cases
            default:; // (44 cases ><'')
            }
            break;

            size_t at_slot;
        case _slot_used:     at_slot = at(slot); if (0)
        case _slot_variable: at_slot = atv(slot);
            switch (expr->kind) {
            case UNOP_BNOT: _emit_arith(cs, _ops_bxori, _slot_arith_w(slot), at(slot), -1/* xxx: should be of the type's size, this is larger */, at_slot); break;
            case UNOP_LNOT: notif("NIY: emit lnot"); break;
            case UNOP_MINUS: _emit_arith(cs, _ops_subi, _slot_arith_w(slot), at(slot), 0, at_slot); break;
            case UNOP_PLUS: _emit_move(cs, at(slot), slot->ty->size, atv(slot)); break;
                // unreachable cases
            default:; // (44 cases ><'')
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
        sw_crement:;

            struct slot dst = _compile_lvalue(cs, expr->info.unary.opr);
            switch (dst.usage) {
            case _slot_value:
                notif("NIY: using this kind of lvalue here (increment/decrement global variable)");
                slot->usage = _slot_used;
                return;

            case _slot_used:
                notif("NIY: using this kind of lvalue here (increment/decrement through pointer or global variable)");
                slot->usage = _slot_used;
                _rewind_slot(cs, &dst);
                return;

            case _slot_variable:
                break;
            }

            if (post) {
                _emit_move(cs, at(slot), slot->ty->size, atv(slot));
                slot->usage = _slot_used;
            }
            _emit_arith(cs, op, _slot_arith_w(slot), atv(slot),
                    TYPE_PTR == slot->ty->tyty ? slot->ty->info.ptr->size : 1,
                    atv(slot));

            return;
        }
    }

} // compile_expression

#undef atv
#undef at

#undef cstokn

#endif // CINTRE_COMPILER_H
