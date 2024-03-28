/// compile to some bytecode

#ifndef CINTRE_COMPILER_H
#define CINTRE_COMPILER_H

#include "common.h"
#include "parser.h"
#include "adapter.h"

typedef dyarr(unsigned char) bytecode;

struct slot {
  struct adpt_type const* ty;
  size_t loc; // yyy
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
      } or_value;
  } info;
};

bool compile_expression(bytecode ref res, expression ref expr, struct slot ref slot, struct adpt_item const* lookup(bufsl const name), void typehole(struct adpt_type cref expect));

enum _fops {
    _fops_add =3,
    _fops_sub,
    _fops_mul,
    _fops_div,
    _fops_rem,
    _fops_addi,
    _fops_subi,
    _fops_muli,
    _fops_divi,
    _fops_remi,
    _fops_rsubi,
    _fops_rdivi,
    _fops_rremi
};
enum _iops {
    _iops_bor =3,
    _iops_bxor,
    _iops_bshl,
    _iops_bshr,
    _iops_band,
    _iops_bori,
    _iops_bxori,
    _iops_bshli,
    _iops_bshri,
    _iops_bandi,
    _iops_rbshli= 0xe,
    _iops_rbshri
};
enum _oprw {
    _oprw_8,
    _oprw_16,
    _oprw_32,
    _oprw_64,
    _oprw_float= 0xf,
    _oprw_double= 0xd
};
bool _emit_fop(bytecode ref res, enum _fops fop, enum _oprw w, size_t d, size_t a, size_t b) {
    unsigned c = 1;
    if (d) for (size_t k = d; k; c++) k>>= 7; else c++;
    if (a) for (size_t k = a; k; c++) k>>= 7; else c++;
    if (b) for (size_t k = b; k; c++) k>>= 7; else c++;
    unsigned char* op = dyarr_insert(res, res->len, c);
    if (!op) return notif("OOM"), false;
    *op = fop<<4 | w;
    do { unsigned char k = d&127; *++op = !!(d>>= 7)<<7 | k; } while (d);
    do { unsigned char k = a&127; *++op = !!(a>>= 7)<<7 | k; } while (a);
    do { unsigned char k = b&127; *++op = !!(b>>= 7)<<7 | k; } while (b);
    return true;
}

// yyy: USL?
unsigned _l2(size_t n) {
    for (unsigned k = 0; k < 8*sizeof n; k++) {
        if (n&1) return k;
        n>>= 1;
    }
    return -1;
}

bool compile_expression(bytecode ref res, expression ref expr, struct slot ref slot, struct adpt_item const* lookup(bufsl const name), void typehole(struct adpt_type cref expect)) {
#   define fail(...)  do { slot->ty = NULL; notif(__VA_ARGS__); return false; } while (1)
#   define failforward(id, from)  for (compile_expression(res, from, &id, lookup, typehole); !id; ) fail("here")
#   define isint(__ty)  (ADPT_KIND_CHAR <= (__ty)->kind && (__ty)->kind <= ADPT_KIND_ENUM)
#   define isflt(__ty)  (ADPT_KIND_FLOAT <= (__ty)->kind && (__ty)->kind <= ADPT_KIND_LONGDOUBLE)
#   define isnum(__ty)  (isint(__ty) || isflt(__ty))
#   define isptr(__ty)  (ADPT_KIND_PTR == (__ty)->kind)
#   define isfun(__ty)  (ADPT_KIND_FUN == (__ty)->kind)

    static char const* const ty_kind_names[] = {"void", "char", "uchar", "schar", "short", "int", "long", "longlong", "ushort", "uint", "ulong", "ulonglong", "enum", "float", "double", "longdouble", "struct", "union", "fun", "ptr"};
    struct slot opr, lhs, rhs, base, off;

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
            notif("NIY: put string under and push string's slot here");
            slot->ty = &string;
            return false;
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
            slot->ty = &adptb_char_type;
            return false;
        }

        if ('0' <= c && c <= '9') {
            if (!isint(slot->ty)) {
                notif("Expected %s, found integer literal", ty_kind_names[slot->ty->kind]);
                return false;
            }

            slot->info.or_value.si = atoi(expr->info.atom.ptr);
            return true;

            //int v = atoi(expr->info.atom.ptr);
            //// data <dst> <sze> <...b>
            //unsigned char* r = dyarr_insert(res, res->len, 1+1+1+4);
            //if (!r) fail("OOM");
            //r[0] = 0x1d;
            //r[1] = slot->loc; // yyy: obviously wrong
            //r[2] = slot->ty->size; // yyy: conv if slot type doesn't match (which may require a temp slot on the stack)
            //memcpy(r+3, (char*)&v, slot->ty->size); // yyy: endianness shortcut
            //slot->info.used = true;
            //return true;
        }

        if (1 == expr->info.atom.len && '_' == c) {
            typehole(slot->ty);
            return false;
        }

        struct adpt_item const* found = lookup(expr->info.atom);
        if (!found) fail("Unknown name: '%.*s'", bufmt(expr->info.atom));

        void* v = found->as.object;
        //while (isptr(base)) base = base->info.to; // XXX: automatic fun to ptr something something..?
        slot->ty = found->type;
        return false;

    case BINOP_SUBSCR:
        //failforward(base, expr->info.subscr.base);
        //failforward(off, expr->info.subscr.off);
        //if (!isptr(base)) fail("Base of subscript expression is not of a pointer type");
        //if (!isint(off)) fail("Offset of subscript expression is not of an integral type");
        //slot->ty = base->info.to;
        return false;

    case BINOP_CALL:
        //failforward(base, expr->info.call.base);
        //if (!isfun(base)) fail("Base of call expression is not of a function type");
        //expression* cons = expr->info.call.args;
        //size_t k, count = base->info.fun.count;
        //for (k = 0; k < count && cons; k++) {
        //    struct adpt_type const* arg = base->info.fun.params[count-1-k].type;
        //    if (k+1 == count) {
        //        if (BINOP_COMMA == cons->kind) {
        //            while (k++, BINOP_COMMA == cons->kind) cons = cons->info.binary.lhs;
        //            fail("Too many arguments: %zu provided, expected %zu", k, count);
        //        }
        //        failforward(arg, cons);
        //    } else {
        //        if (BINOP_COMMA != cons->kind) break;
        //        failforward(arg, cons->info.binary.rhs);
        //        cons = cons->info.binary.lhs;
        //    }
        //    // TODO: assignment
        //}
        //if (k < count) fail("Not enough arguments: %zu provided, expected %zu", k+!!cons, base->info.fun.count);
        //*dyarr_push(res) = (count<<4) | 0xc;
        //slot->ty = base->info.fun.ret;
        return false;

    case BINOP_TERNCOND:
    case BINOP_TERNBRANCH:
        fail("NIY: ternary");

    case BINOP_COMMA:
        //failforward(lhs, expr->info.binary.lhs);
        //failforward(rhs, expr->info.binary.rhs);
        //slot->ty = rhs;
        return false;

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
        //failforward(lhs, expr->info.binary.lhs);
        //failforward(rhs, expr->info.binary.rhs);
        //// TODO: assignment
        fail("NIY: assignment");

    case BINOP_LOR:
    case BINOP_LAND:
        slot->ty = &adptb_int_type; // yyy
        return false;

    case BINOP_EQ:
    case BINOP_NE:
    case BINOP_LT:
    case BINOP_GT:
    case BINOP_LE:
    case BINOP_GE:
        //failforward(lhs, expr->info.binary.lhs);
        //failforward(rhs, expr->info.binary.rhs);
        //if ((isnum(lhs) && isnum(rhs)) || (isptr(lhs) && isptr(rhs))) {
        //    slot->ty = &adptb_int_type; // yyy
        //    return false;
        //}
        fail("Values are not comparable");

    case BINOP_BOR:
    case BINOP_BXOR:
    case BINOP_BAND:
    case BINOP_BSHL:
    case BINOP_BSHR:
        //failforward(lhs, expr->info.binary.lhs);
        //failforward(rhs, expr->info.binary.rhs);
        //if (isint(lhs) && isint(rhs)) {
        //    slot->ty = lhs; // yyy
        //    return false;
        //}
        fail("Both operands are not of an integral type");

    case BINOP_SUB:
    case BINOP_ADD:
        lhs = rhs = *slot;
        if (!compile_expression(res, expr->info.binary.lhs, &lhs, lookup, typehole)) return false;
        if (lhs.info.used) {
            // align 4
            // push 4
            rhs.loc+= 4; // yyy: obviously wrong
        }
        if (!compile_expression(res, expr->info.binary.rhs, &rhs, lookup, typehole)) return false;
        if (!rhs.info.used) {
            // undo push 4 and align 4
        }
        slot->info.used = lhs.info.used || rhs.info.used;
        switch (lhs.info.used << 1 | rhs.info.used) {
        case 3: // both used
            return _emit_fop(res, BINOP_SUB ? _fops_sub : _fops_add, _oprw_32, slot->loc, lhs.loc, rhs.loc);
        case 2: // only lhs used
            return _emit_fop(res, BINOP_SUB ? _fops_subi : _fops_addi, _oprw_32, slot->loc, rhs.info.or_value.si, lhs.loc);
        case 1: // only rhs used
            return _emit_fop(res, BINOP_SUB ? _fops_rsubi : _fops_addi, _oprw_32, slot->loc, lhs.info.or_value.si, rhs.loc);
        case 0:
            slot->info.or_value.si = BINOP_SUB == expr->kind
                ? lhs.info.or_value.si - rhs.info.or_value.si
                : lhs.info.or_value.si + rhs.info.or_value.si;
        }
        return true;

    case BINOP_REM:
    case BINOP_DIV:
    case BINOP_MUL:
        //failforward(lhs, expr->info.binary.lhs);
        //failforward(rhs, expr->info.binary.rhs);
        //if (isnum(lhs) && isnum(rhs)) {
        //    slot->ty = lhs; // yyy
        //    return false;
        //}
        fail("Both operands are not of an arithmetic type");

    case UNOP_ADDR:
        fail("NIY: address of");
    case UNOP_DEREF:
        //failforward(opr, expr->info.unary.opr);
        //if (isptr(opr)) {
        //    slot->ty = opr->info.to;
        //    return false;
        //}
        fail("Operand is not of a pointer type");

    case UNOP_PMEMBER:
        //failforward(opr, expr->info.unary.opr);
        //if (!isptr(opr)) fail("Operand is not of a pointer type");
        //if (0)
    case UNOP_MEMBER:
        //    failforward(opr, expr->info.unary.opr);
        fail("NIY: member access");

    case UNOP_BNOT:
    case UNOP_LNOT:
        //failforward(opr, expr->info.unary.opr);
        //if (isint(opr)) {
        //    slot->ty = opr; // yyy (lnot)
        //    return false;
        //}
        fail("Operand is not of an integral type");

    case UNOP_MINUS:
    case UNOP_PLUS:
        //failforward(opr, expr->info.unary.opr);
        //if (isnum(opr)) {
        //    slot->ty = opr;
        //    return false;
        //}
        fail("Operand is not of an arithmetic type");

    case UNOP_PRE_DEC:
    case UNOP_PRE_INC:
    case UNOP_POST_DEC:
    case UNOP_POST_INC:
        //failforward(opr, expr->info.unary.opr);
        //if (isnum(opr) || isptr(opr)) {
        //    slot->ty = opr;
        //    return false;
        //}
        fail("Operand is not of an arithmetic type");
    }

    fail("Broken tree: expression kind %d", expr->kind);

#   undef isfun
#   undef isptr
#   undef isnum
#   undef isflt
#   undef isint
#   undef failforward
#   undef fail
}

#endif // CINTRE_COMPILER_H
