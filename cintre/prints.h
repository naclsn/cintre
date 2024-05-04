/// Collection of colorful print utils for various types:
///
/// `declaration` and `expression` from the parser
///   declarations does not reflect the C-style declarations
///   expressions are shown as a plain boring tree
///
/// `struct adpt_type` and `struct adpt_item` from the adapter
///   items are printed with their type if depth is 0 and with their value
///   (which may be taken from the stack)
///
/// `bytecode` from the compiler
///   traditional assembly-looking, also works a decent reference for the
///   instruction set as long as you can read it
///
/// `run_state` for the top of its stack at runtime

#ifndef CINTRE_PRINTS_H
#define CINTRE_PRINTS_H

#include "common.h"
#include "parser.h"
#include "adapter.h"
#include "compiler.h"
#include "runner.h"

void print_decl(FILE ref strm, declaration cref decl);
void print_expr(FILE ref strm, expression cref expr, unsigned const depth);
void print_type(FILE ref strm, struct adpt_type cref ty);
void print_code(FILE ref strm, bytecode const code);
void print_item(FILE ref strm, struct adpt_item cref it, char cref stack, unsigned const depth);
void print_tops(FILE ref strm, run_state cref rs, struct adpt_item cref items, size_t const count);
void print_slot(FILE ref strm, struct slot cref slt);

// ---

void _print_decl_type(FILE ref strm, struct decl_type cref ty) {
    for (size_t k = 0; QUAL_END != ty->quals[k]; k++) switch (ty->quals[k]) {
    case QUAL_END:                                                  break;
    case QUAL_CONST:     fprintf(strm, "\x1b[34mconst\x1b[m ");     break;
    case QUAL_RESTRICT:  fprintf(strm, "\x1b[34mrestrict\x1b[m ");  break;
    case QUAL_VOLATILE:  fprintf(strm, "\x1b[34mvolatile\x1b[m ");  break;
    case QUAL_INLINE:    fprintf(strm, "\x1b[34minline\x1b[m ");    break;
    case QUAL_SIGNED:    fprintf(strm, "\x1b[34msigned\x1b[m ");    break;
    case QUAL_UNSIGNED:  fprintf(strm, "\x1b[34munsigned\x1b[m ");  break;
    case QUAL_SHORT:     fprintf(strm, "\x1b[34mshort\x1b[m ");     break;
    case QUAL_LONG:      fprintf(strm, "\x1b[34mlong\x1b[m ");      break;
    case QUAL_COMPLEX:   fprintf(strm, "\x1b[34mcomplex\x1b[m ");   break;
    case QUAL_IMAGINARY: fprintf(strm, "\x1b[34mimaginary\x1b[m "); break;
    }

    switch (ty->kind) {
    case KIND_NOTAG: fprintf(strm, "\x1b[32m%.*s\x1b[m", bufmt(ty->name)); break;

    case KIND_STRUCT: fprintf(strm, "\x1b[34mstruct\x1b[m"); if (0)
    case KIND_UNION:  fprintf(strm, "\x1b[34munion\x1b[m");
        if (ty->name.len) fprintf(strm, " %.*s", bufmt(ty->name));
        if ((size_t)-1 != ty->info.comp.count) {
            fprintf(strm, " {");
            for (struct decl_type_field const* it = ty->info.comp.first; it; it = it->next) {
                print_decl(strm, it->decl);
                // todo: same as an array's length, this is incorrect but
                //       avoids unneeded complication
                if (it->bitw) ATOM == it->bitw->kind
                        ? fprintf(strm, ":%.*s", bufmt(it->bitw->info.atom))
                        : fprintf(strm, "<expr>");
                if (it->next) fprintf(strm, ", ");
            }
            fprintf(strm, "}");
        }
        break;

    case KIND_ENUM:
        fprintf(strm, "\x1b[34menum\x1b[m");
        if (ty->name.len) fprintf(strm, " %.*s", bufmt(ty->name));
        if ((size_t)-1 != ty->info.enu.count) {
            fprintf(strm, " {");
            for (struct decl_type_enumer const* it = ty->info.enu.first; it; it = it->next) {
                fprintf(strm, "%.*s", bufmt(it->name));
                if (it->next) fprintf(strm, ", ");
            }
            fprintf(strm, "}");
        }
        break;

    case KIND_PTR:
        fprintf(strm, "\x1b[34mptr\x1b[m[");
        _print_decl_type(strm, &ty->info.ptr->type);
        fprintf(strm, "]");
        break;

    case KIND_FUN:
        fprintf(strm, "\x1b[34mfun\x1b[m(");
        if ((size_t)-1 == ty->info.fun.count) ;
        else if (!ty->info.fun.count) fprintf(strm, "\x1b[36mvoid\x1b[m");
        else for (struct decl_type_param const* it = ty->info.fun.first; it; it = it->next) {
            print_decl(strm, it->decl);
            if (it->next) fprintf(strm, ", ");
        }
        fprintf(strm, ") -> ");
        _print_decl_type(strm, &ty->info.fun.ret->type);
        break;

    case KIND_ARR:
        fprintf(strm, "\x1b[34marr\x1b[m[");
        if (!ty->info.arr.count) fprintf(strm, "*, ");
        // todo: this was temporary to generate the test tapes, but because
        //       we're not going to just compile and run the expression here
        //       we'll need to print the expression which gets complicated and
        //       more importantly unneeded for now and before a long time
        else ATOM == ty->info.arr.count->kind
                ? fprintf(strm, "%.*s, ", bufmt(ty->info.arr.count->info.atom))
                : fprintf(strm, "<expr>");
        _print_decl_type(strm, &ty->info.arr.item->type);
        fprintf(strm, "]");
        break;
    }
}

void print_decl(FILE ref strm, declaration cref decl) {
    switch (decl->spec) {
    case SPEC_NONE:                                               break;
    case SPEC_TYPEDEF:  fprintf(strm, "\x1b[36mtypedef\x1b[m ");  break;
    case SPEC_EXTERN:   fprintf(strm, "\x1b[36mextern\x1b[m ");   break;
    case SPEC_STATIC:   fprintf(strm, "\x1b[36mstatic\x1b[m ");   break;
    case SPEC_AUTO:     fprintf(strm, "\x1b[36mauto\x1b[m ");     break;
    case SPEC_REGISTER: fprintf(strm, "\x1b[36mregister\x1b[m "); break;
    }
    fprintf(strm, "%.*s: ", bufmt(decl->name));
    _print_decl_type(strm, &decl->type);
}

void print_expr(FILE ref strm, expression cref expr, unsigned const depth) {
    static char cref op_kind_names[] = {"ATOM", "BINOP_SUBSCR", "BINOP_CALL", "BINOP_TERNCOND", "BINOP_TERNBRANCH", "BINOP_COMMA", "BINOP_ASGN", "BINOP_ASGN_BOR", "BINOP_ASGN_BXOR", "BINOP_ASGN_BAND", "BINOP_ASGN_BSHL", "BINOP_ASGN_BSHR", "BINOP_ASGN_SUB", "BINOP_ASGN_ADD", "BINOP_ASGN_REM", "BINOP_ASGN_DIV", "BINOP_ASGN_MUL", "BINOP_LOR", "BINOP_LAND", "BINOP_BOR", "BINOP_BXOR", "BINOP_BAND", "BINOP_EQ", "BINOP_NE", "BINOP_LT", "BINOP_GT", "BINOP_LE", "BINOP_GE", "BINOP_BSHL", "BINOP_BSHR", "BINOP_SUB", "BINOP_ADD", "BINOP_REM", "BINOP_DIV", "BINOP_MUL", "UNOP_ADDR", "UNOP_DEREF", "UNOP_CAST", "UNOP_BNOT", "UNOP_LNOT", "UNOP_MINUS", "UNOP_PLUS", "UNOP_PRE_DEC", "UNOP_PRE_INC", "UNOP_PMEMBER", "UNOP_MEMBER", "UNOP_POST_DEC", "UNOP_POST_INC"};
    for (unsigned k = 0; k < depth; k++) fprintf(strm, "|  ");

    if (!expr) {
        fprintf(strm, "\x1b[31m(nil)\x1b[m\n");
        return;
    }

    if (ATOM == expr->kind) {
        char const c = *expr->info.atom.ptr;
        fprintf(strm, "\x1b[%dm%.*s\x1b[m\n", '"' == c ? 36 : ('0' <= c && c <= '9') || '\'' == c || '.' == c ? 33 : 0, bufmt(expr->info.atom));
        return;
    }

    fprintf(strm, "\x1b[34m%s\x1b[m\n", op_kind_names[expr->kind]);

    switch (expr->kind) {
    case UNOP_CAST:
        for (unsigned k = 0; k < depth+1; k++) fprintf(strm, "|  ");
        _print_decl_type(strm, expr->info.cast.type);
        fprintf(strm, "\n");
        print_expr(strm, expr->info.cast.opr, depth+1);
        break;

    case UNOP_MEMBER:
    case UNOP_PMEMBER:
        print_expr(strm, expr->info.member.base, depth+1);
        for (unsigned k = 0; k < depth+1; k++) fprintf(strm, "|  ");
        fprintf(strm, "%s%.*s\n", UNOP_MEMBER == expr->kind ? "." : "->", bufmt(*expr->info.member.name));
        break;

    case UNOP_ADDR:     case UNOP_DEREF:
    case UNOP_BNOT:     case UNOP_LNOT:
    case UNOP_MINUS:    case UNOP_PLUS:
    case UNOP_PRE_DEC:  case UNOP_PRE_INC:
    case UNOP_POST_DEC: case UNOP_POST_INC:
        print_expr(strm, expr->info.unary.opr, depth+1);
        break;

    case BINOP_CALL:
        print_expr(strm, expr->info.call.base, depth+1);
        size_t count = 0;
        for (struct expr_call_arg const* it = expr->info.call.first; it; it = it->next)
            count++;
        for (unsigned k = 0; k < depth; k++) fprintf(strm, "|  ");
        fprintf(strm, "|  \x1b[32m(%zu)\x1b[m\n", count);
        for (struct expr_call_arg const* it = expr->info.call.first; it; it = it->next)
            print_expr(strm, it->expr, depth+2);
        break;

    case BINOP_SUBSCR:
        print_expr(strm, expr->info.subscr.base, depth+1);
        print_expr(strm, expr->info.subscr.off, depth+1);
        break;

    default:
        print_expr(strm, expr->info.binary.lhs, depth+1);
        print_expr(strm, expr->info.binary.rhs, depth+1);
        break;
    }
}

void print_type(FILE ref strm, struct adpt_type cref ty) {
    if (!ty) {
        fprintf(strm, "\x1b[31m(nil)\x1b[m");
        return;
    }

    switch (ty->tyty) {
    case TYPE_VOID:   fprintf(strm, "\x1b[32mvoid\x1b[m");   break;
    case TYPE_CHAR:   fprintf(strm, "\x1b[32mchar\x1b[m");   break;
    case TYPE_UCHAR:  fprintf(strm, "\x1b[32muchar\x1b[m");  break;
    case TYPE_SCHAR:  fprintf(strm, "\x1b[32mschar\x1b[m");  break;
    case TYPE_SHORT:  fprintf(strm, "\x1b[32mshort\x1b[m");  break;
    case TYPE_INT:    fprintf(strm, "\x1b[32mint\x1b[m");    break;
    case TYPE_LONG:   fprintf(strm, "\x1b[32mlong\x1b[m");   break;
    case TYPE_USHORT: fprintf(strm, "\x1b[32mushort\x1b[m"); break;
    case TYPE_UINT:   fprintf(strm, "\x1b[32muint\x1b[m");   break;
    case TYPE_ULONG:  fprintf(strm, "\x1b[32mulong\x1b[m");  break;
    case TYPE_FLOAT:  fprintf(strm, "\x1b[32mfloat\x1b[m");  break;
    case TYPE_DOUBLE: fprintf(strm, "\x1b[32mdouble\x1b[m"); break;

    case TYPE_STRUCT: fprintf(strm, "\x1b[34mstruct\x1b[m{"); if (0)
    case TYPE_UNION:  fprintf(strm, "\x1b[34munion\x1b[m{");
        for (size_t k = 0; k < ty->info.comp.count; k++) {
            struct adpt_comp_field const* it = ty->info.comp.fields+k;
            fprintf(strm, k ? ", %s@%zu: " : "%s@%zu: ", it->name, it->offset);
            print_type(strm, it->type);
        }
        fprintf(strm, "}");
        break;

    case TYPE_FUN:
        fprintf(strm, "\x1b[34mfun\x1b[m(");
        for (size_t k = 0; k < ty->info.fun.count; k++) {
            struct adpt_fun_param const* it = ty->info.fun.params+k;
            fprintf(strm, k ? ", %s: " : "%s: ", it->name);
            print_type(strm, it->type);
        }
        fprintf(strm, ") -> ");
        print_type(strm, ty->info.fun.ret);
        break;

    case TYPE_PTR:
        fprintf(strm, "\x1b[34mptr\x1b[m[");
        print_type(strm, ty->info.ptr);
        fprintf(strm, "]");
        break;

    case TYPE_ARR:
        fprintf(strm, "\x1b[34marr\x1b[m[%zu, ", ty->info.arr.count);
        print_type(strm, ty->info.arr.item);
        fprintf(strm, "]");
        break;
    }
}

void print_code(FILE ref strm, bytecode const code) {
    for (size_t k = 0; k < code.len; k++) {
        size_t pk = k;
        fprintf(strm, "%5zu   ", k);
        unsigned char c = code.ptr[k];
        unsigned hi = c>>4&0xf, lo = c&0xf, w = c&3;

        size_t src, dst, lhs, rhs, opr, sze, val, ret, fun, arg, slt, ptr, off;
#       define imm(nm) for (                                          \
            unsigned xx = (nm = code.ptr[++k], 0);                    \
            code.ptr[k]&0x80                                          \
                ? true                                                \
                : ( fprintf(strm, "\t" #nm ":\x1b[33m%zu\x1b[m", nm)  \
                  , false);                                           \
            nm = nm | (code.ptr[++k]&0x7f)<<(xx+= 7))

        if (2 < hi && (lo < 8 || 0xd == lo || 0xf == lo)) {
            static char const* const fops[] = {"add", "sub", "mul", "div", "rem", "addi", "subi", "muli", "divi", "remi", "rsubi", "rdivi", "rremi"};
            static char const* const iops[] = {"bor", "bxor", "bshl", "bshr", "band", "bori", "bxori", "bshli", "bshri", "bandi", "rbshli", "rbshri", "???"};
            char const* name = (lo < 4 || 7 < lo ? fops : iops)[hi-3];
            if (0x7d == c || 0x7f == c || 0xcd == c || 0xcf == c || 0xfd == c || 0xff == c) name = "???";
            fprintf(strm, "\x1b[34m%s%c\x1b[m", name, (lo < 8 ? "0123" : " d f")[w]);
            imm(dst);
            if (hi < 8) {
                imm(lhs);
                imm(rhs);
            } else {
                imm(val);
                if (hi < 0xd) imm(rhs);
                else imm(lhs);
            }
        } // arith

        else if (0xc == lo) {
            fprintf(strm, "\x1b[34mcall%u\x1b[m", hi);
            imm(ret);
            imm(fun);
            for (unsigned yy = 0; yy < hi; yy++) imm(arg);
        } // call

        else if (hi < w && lo < 8) {
            fprintf(strm, "\x1b[34m%cx%dto%d\x1b[m", lo < 4 ? 's' : 'z', hi, w);
            imm(dst);
            imm(src);
        } // cvt

        else switch (c) {
        case 0x11: fprintf(strm, "\x1b[34mftoi\x1b[m"); goto sw_cvt;
        case 0x22: fprintf(strm, "\x1b[34mdtol\x1b[m"); goto sw_cvt;
        case 0x15: fprintf(strm, "\x1b[34mitof\x1b[m"); goto sw_cvt;
        case 0x26: fprintf(strm, "\x1b[34mltod\x1b[m"); goto sw_cvt;
        case 0x21: fprintf(strm, "\x1b[34mdtof\x1b[m"); goto sw_cvt;
        case 0x25: fprintf(strm, "\x1b[34mftod\x1b[m"); goto sw_cvt;
        sw_cvt:
            imm(dst);
            imm(src);
            break;

        case 0x00: fprintf(strm, "\x1b[34mnop\x1b[m"); break;
        case 0x2a: fprintf(strm, "\x1b[34mdebug\x1b[m"); break;

        case 0x04:
            fprintf(strm, "\x1b[34mnot\x1b[m");
            break;
        case 0x14:
            fprintf(strm, "\x1b[34mcmp1\x1b[m");
            imm(opr);
            break;
        case 0x24:
            fprintf(strm, "\x1b[34mcmp2\x1b[m");
            imm(lhs);
            imm(rhs);
            break;

        case 0x0a: fprintf(strm, "\x1b[34mjmp\x1b[m");  goto sw_jmp;
        case 0x1a: fprintf(strm, "\x1b[34mjmb\x1b[m");  goto sw_jmp;
        case 0x0b: fprintf(strm, "\x1b[34mbreq\x1b[m"); goto sw_jmp;
        case 0x1b: fprintf(strm, "\x1b[34mbrlt\x1b[m"); goto sw_jmp;
        case 0x2b: fprintf(strm, "\x1b[34mbrle\x1b[m"); goto sw_jmp;
        sw_jmp:
            imm(sze);
            break;

        case 0x0d: fprintf(strm, "\x1b[34mpop\x1b[m"); if (0)
        case 0x0f: fprintf(strm, "\x1b[34mpush\x1b[m");
            imm(sze);
            break;

        case 0x1d:
            fprintf(strm, "\x1b[34mdata\x1b[m");
            imm(dst);
            imm(sze);
            fprintf(strm, "\tbytes:\x1b[33m\"");
            for (unsigned yy = 0; yy < sze; yy++) fprintf(strm, "\\x%02x", code.ptr[++k]);
            fprintf(strm, "\"\x1b[m");
            break;
        case 0x1f:
            fprintf(strm, "\x1b[34mmove\x1b[m");
            imm(dst);
            imm(sze);
            imm(src);
            break;

        case 0x2d: fprintf(strm, "\x1b[34mwrite\x1b[m"); if (0)
        case 0x2f: fprintf(strm, "\x1b[34mread\x1b[m");
            imm(ptr);
            imm(sze);
            imm(slt);
            break;

        case 0x20:
            fprintf(strm, "\x1b[34mlea\x1b[m");
            imm(dst);
            imm(off);
            break;

        default: fprintf(strm, "\x1b[31merrop\x1b[m");
        } // other

        fprintf(strm, "\t\x1b[32m;");
        while (pk <= k) fprintf(strm, " 0x%02x", code.ptr[pk++]);
        fprintf(strm, "\x1b[m\n");
#       undef imm
    }
}

void print_item(FILE ref strm, struct adpt_item cref it, char cref stack, unsigned const depth) {
    if (!depth) {
        fprintf(strm, "%s: ", it->name);
        print_type(strm, it->type);
        fprintf(strm, "\n   = ");
    }

    void cref p = ITEM_VALUE == it->kind
        ? it->as.object
        : stack+it->as.variable;

    switch (it->type->tyty) {
    case TYPE_VOID: fprintf(strm, "()"); break;

    case TYPE_CHAR: switch (*(char*)p) {
        case '\0': fprintf(strm, "'\\0'"); break;
        case '\'': fprintf(strm, "'\\''"); break;
        case '\"': fprintf(strm,"'\\\"'"); break;
        case '\?': fprintf(strm, "'\\?'"); break;
        case '\\': fprintf(strm,"'\\\\'"); break;
        case '\a': fprintf(strm, "'\\a'"); break;
        case '\b': fprintf(strm, "'\\b'"); break;
        case '\f': fprintf(strm, "'\\f'"); break;
        case '\n': fprintf(strm, "'\\n'"); break;
        case '\r': fprintf(strm, "'\\r'"); break;
        case '\t': fprintf(strm, "'\\t'"); break;
        case '\v': fprintf(strm, "'\\v'"); break;
        default: fprintf(strm, ' ' <= *(char*)p && *(char*)p <= '~' ? "'%c'" : "'\\x%02x'", *(char*)p);
    } break;

    case TYPE_UCHAR:  fprintf(strm, "0x%02hhx", *(unsigned char*)p);  break;
    case TYPE_SCHAR:  fprintf(strm, "%hhi",     *(signed char*)p);    break;
    case TYPE_SHORT:  fprintf(strm, "%hi",      *(short*)p);          break;
    case TYPE_INT:    fprintf(strm, "%i",       *(int*)p);            break;
    case TYPE_LONG:   fprintf(strm, "%li",      *(long*)p);           break;
    case TYPE_USHORT: fprintf(strm, "%hu",      *(unsigned short*)p); break;
    case TYPE_UINT:   fprintf(strm, "%u",       *(unsigned int*)p);   break;
    case TYPE_ULONG:  fprintf(strm, "%lu",      *(unsigned long*)p);  break;
    case TYPE_FLOAT:  fprintf(strm, "%f",       *(float*)p);          break;
    case TYPE_DOUBLE: fprintf(strm, "%lf",      *(double*)p);         break;

    case TYPE_STRUCT:
    case TYPE_UNION:
        fprintf(strm, "{\n");
        for (size_t k = 0; k < it->type->info.comp.count; k++) {
            struct adpt_comp_field cref f = it->type->info.comp.fields+k;
            fprintf(strm, "%*s.%s= ", (depth+1)*3, "", f->name);
            print_item(strm, &(struct adpt_item){
                    .type= f->type,
                    .as.object= (char*)p+f->offset, // xxx: discards const
                }, stack, depth+1);
            fprintf(strm, "\n");
        }
        fprintf(strm, "%*s}", depth*3, "");
        break;

    case TYPE_FUN: fprintf(strm, "(%p)", p); break;

    case TYPE_PTR:
        fprintf(strm, "(%p) ", p);
        //print_item(strm, &(struct adpt_item){
        //        .type= it->type->info.ptr,
        //        .as.object= *(void**)p,
        //    }, stack, depth);
        break;

    case TYPE_ARR:
        fprintf(strm, "[\n");
        for (size_t k = 0; k < it->type->info.arr.count; k++) {
            fprintf(strm, "%*s[%zu]= ", (depth+1)*3, "", k);
            print_item(strm, &(struct adpt_item){
                    .type= it->type->info.arr.item,
                    .as.object= (char*)p+k*it->type->info.arr.item->size, // xxx: discards const
                }, stack, depth+1);
            fprintf(strm, "\n");
        }
        fprintf(strm, "%*s]", depth*3, "");
        break;
    }

    fprintf(strm, "\n");
}

void print_tops(FILE ref strm, run_state cref rs, struct adpt_item cref items, size_t const count) {
    size_t const sz = sizeof rs->stack; // (xxx: sizeof stack)

    size_t in_var_size = 0;
    unsigned cur_var_color = 1;
#   define col_n(_n) (((_n)&3)+31)

    for (size_t at = sz; 16 <= at && rs->sp < at; at-= 16) {
        fprintf(strm, "0x%08zx @-%-5zu | ", at, sz-at);

        unsigned off_var_color = 0;
        for (unsigned k = 0; k < 16; k++) {
            if (rs->sp >= at-k) {
                fprintf(strm, "   ");
                continue;
            }
            fprintf(strm, " ");

            if (!in_var_size) {
                for (size_t n = 0; n < count; n++) if (ITEM_VARIABLE == items[n].kind && items[n].as.variable == at-items[n].type->size-k) {
                    in_var_size = items[n].type->size;
                    break;
                }
                if (in_var_size) fprintf(strm, "\x1b[%u;4m", col_n(cur_var_color+off_var_color++));
            }

            fprintf(strm, "%02hhX", rs->stack[at-1-k]);

            if (in_var_size && !--in_var_size) fprintf(strm, "\x1b[m");
        }

        fprintf(strm, "  |");
        if (off_var_color)
            for (unsigned k = 0; k < 16 && rs->sp < at-k; k++)
                for (size_t n = 0; n < count; n++) if (ITEM_VARIABLE == items[n].kind && items[n].as.variable == at-items[n].type->size-k)
                    fprintf(strm, "  \x1b[%um%s\x1b[m(%zu)", col_n(cur_var_color++), items[n].name, items[n].type->size);

        fprintf(strm, "\n");
    }

#   undef col_n
}

void print_slot(FILE ref strm, struct slot cref slot) {
    fprintf(strm, "slot ");
    print_type(strm, slot->ty);
    switch (slot->usage) {
    case _slot_value:
        fprintf(strm, " \x1b[36mvalue\x1b[m = ");
        switch (slot->ty->tyty) {
        case TYPE_CHAR:   fprintf(strm, "'%c'",     slot->as.value.c);  break;
        case TYPE_UCHAR:  fprintf(strm, "0x%02hhx", slot->as.value.uc); break;
        case TYPE_SCHAR:  fprintf(strm, "%hhi",     slot->as.value.sc); break;
        case TYPE_SHORT:  fprintf(strm, "%hi",      slot->as.value.ss); break;
        case TYPE_INT:    fprintf(strm, "%i",       slot->as.value.si); break;
        case TYPE_LONG:   fprintf(strm, "%li",      slot->as.value.sl); break;
        case TYPE_USHORT: fprintf(strm, "%hu",      slot->as.value.us); break;
        case TYPE_UINT:   fprintf(strm, "%u",       slot->as.value.ui); break;
        case TYPE_ULONG:  fprintf(strm, "%lu",      slot->as.value.ul); break;
        case TYPE_FLOAT:  fprintf(strm, "%f",       slot->as.value.f);  break;
        case TYPE_DOUBLE: fprintf(strm, "%lf",      slot->as.value.d);  break;
        default:          fprintf(strm, "%p",       slot->as.value.p);  break;
        }
        break;

    case _slot_used:
        fprintf(strm, " \x1b[36mused\x1b[m (runtime)");
        break;

    case _slot_variable:
        fprintf(strm, " \x1b[36mvariable\x1b[m @ %zu", slot->as.variable);
        break;
    }
}

#endif // CINTRE_PRINTS_H
