#ifndef CINTRE_PRINTS_H
#define CINTRE_PRINTS_H

#include "common.h"
#include "parser.h"
#include "adapter.h"
#include "compiler.h"

void print_expr(FILE ref strm, expression cref expr, unsigned depth);
void print_type(FILE ref strm, struct adpt_type cref ty);
void print_code(FILE ref strm, bytecode const code);

void print_expr(FILE ref strm, expression cref expr, unsigned depth) {
    static char const* const op_kind_names[] = {"ATOM", "BINOP_SUBSCR", "BINOP_CALL", "BINOP_TERNCOND", "BINOP_TERNBRANCH", "BINOP_COMMA", "BINOP_ASGN", "BINOP_ASGN_BOR", "BINOP_ASGN_BXOR", "BINOP_ASGN_BAND", "BINOP_ASGN_BSHL", "BINOP_ASGN_BSHR", "BINOP_ASGN_SUB", "BINOP_ASGN_ADD", "BINOP_ASGN_REM", "BINOP_ASGN_DIV", "BINOP_ASGN_MUL", "BINOP_LOR", "BINOP_LAND", "BINOP_BOR", "BINOP_BXOR", "BINOP_BAND", "BINOP_EQ", "BINOP_NE", "BINOP_LT", "BINOP_GT", "BINOP_LE", "BINOP_GE", "BINOP_BSHL", "BINOP_BSHR", "BINOP_SUB", "BINOP_ADD", "BINOP_REM", "BINOP_DIV", "BINOP_MUL", "UNOP_ADDR", "UNOP_DEREF", "UNOP_BNOT", "UNOP_LNOT", "UNOP_MINUS", "UNOP_PLUS", "UNOP_PRE_DEC", "UNOP_PRE_INC", "UNOP_PMEMBER", "UNOP_MEMBER", "UNOP_POST_DEC", "UNOP_POST_INC"};
    for (unsigned k = 0; k < depth; k++) fprintf(strm, "|  ");

    if (!expr) {
        fprintf(strm, "\x1b[31m(nil)\x1b[m\n");
        return;
    }
    char const* const name = op_kind_names[expr->kind];

    if (ATOM == expr->kind) {
        char c = *expr->info.atom.ptr;
        fprintf(strm, "\x1b[%dm%.*s\x1b[m\n", '"' == c ? 36 : ('0' <= c && c <= '9') || '\'' == c || '.' == c ? 33 : 0, bufmt(expr->info.atom));
    }

    else if (!memcmp("UNOP", name, 4)) {
        fprintf(strm, "\x1b[34m%s\x1b[m\n", name);
        print_expr(strm, expr->info.unary.opr, depth+1);
    }

    else if (!memcmp("BINO", name, 4)) {
        fprintf(strm, "\x1b[34m%s\x1b[m\n", name);
        print_expr(strm, expr->info.binary.lhs, depth+1);
        print_expr(strm, expr->info.binary.rhs, depth+1);
    }
}

void print_type(FILE ref strm, struct adpt_type cref ty) {
    switch (ty->kind) {
    case ADPT_KIND_VOID:   fprintf(strm, "\x1b[32mvoid\x1b[m");   break;
    case ADPT_KIND_CHAR:   fprintf(strm, "\x1b[32mchar\x1b[m");   break;
    case ADPT_KIND_UCHAR:  fprintf(strm, "\x1b[32muchar\x1b[m");  break;
    case ADPT_KIND_SCHAR:  fprintf(strm, "\x1b[32mschar\x1b[m");  break;
    case ADPT_KIND_SHORT:  fprintf(strm, "\x1b[32mshort\x1b[m");  break;
    case ADPT_KIND_INT:    fprintf(strm, "\x1b[32mint\x1b[m");    break;
    case ADPT_KIND_LONG:   fprintf(strm, "\x1b[32mlong\x1b[m");   break;
    case ADPT_KIND_USHORT: fprintf(strm, "\x1b[32mushort\x1b[m"); break;
    case ADPT_KIND_UINT:   fprintf(strm, "\x1b[32muint\x1b[m");   break;
    case ADPT_KIND_ULONG:  fprintf(strm, "\x1b[32mulong\x1b[m");  break;
    case ADPT_KIND_ENUM:   fprintf(strm, "\x1b[32menum\x1b[m");   break;
    case ADPT_KIND_FLOAT:  fprintf(strm, "\x1b[32mfloat\x1b[m");  break;
    case ADPT_KIND_DOUBLE: fprintf(strm, "\x1b[32mdouble\x1b[m"); break;

    case ADPT_KIND_STRUCT: fprintf(strm, "\x1b[34mstruct\x1b[m{"); if (0)
    case ADPT_KIND_UNION:  fprintf(strm, "\x1b[34munion\x1b[m{");
        for (size_t k = 0; k < ty->info.comp.count; k++) {
            struct adpt_comp_field const* it = ty->info.comp.fields+k;
            fprintf(strm, k ? ", [%zu]%s: " : "[%zu]%s: ", it->offset, it->name);
            print_type(strm, it->type);
        }
        fprintf(strm, "}");
        break;

    case ADPT_KIND_FUN:
        fprintf(strm, "\x1b[34mfun\x1b[m(");
        for (size_t k = 0; k < ty->info.fun.count; k++) {
            struct adpt_fun_param const* it = ty->info.fun.params+k;
            fprintf(strm, k ? ", %s: " : "%s: ", it->name);
            print_type(strm, it->type);
        }
        fprintf(strm, ") -> ");
        print_type(strm, ty->info.fun.ret);
        break;

    case ADPT_KIND_PTR:
        fprintf(strm, "\x1b[34mptr\x1b[m[");
        print_type(strm, ty->info.to);
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

        size_t src, dst, lhs, rhs, opr, sze, val, ret, fun, arg, slt, ptr;
#       define imm(nm) for (                        \
            unsigned xx = (nm = code.ptr[++k], 0);  \
            code.ptr[k]&0x80  \
                ? true  \
                : (fprintf(strm, "\t" #nm ":\x1b[33m%zu\x1b[m", nm), false);                   \
            nm = nm | (code.ptr[++k]&0x7f)<<(xx+= 7))

        if (2 < hi && (lo < 8 || 0xd == lo || 0xf == lo)) {
            static char const* const fops[] = {"add", "sub", "mul", "div", "rem", "addi", "subi", "muli", "divi", "remi", "rsubi", "rdivi", "rremi"};
            static char const* const iops[] = {"bor", "bxor", "bshl", "bshr", "band", "bori", "bxori", "bshli", "bshri", "bandi", "", "rbshli", "rbshri"};
            char const* name = (lo < 4 || 7 < lo ? fops : iops)[hi-3];
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
            for (unsigned yy = 0; yy < lo; yy++) imm(arg);
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
        case 0x20: fprintf(strm, "\x1b[34mdebug\x1b[m"); break;

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

        case 0x0b: fprintf(strm, "\x1b[34mjmp\x1b[m");  goto sw_jmp;
        case 0x1b: fprintf(strm, "\x1b[34mbreq\x1b[m"); goto sw_jmp;
        case 0x2b: fprintf(strm, "\x1b[34mbrlt\x1b[m"); goto sw_jmp;
        case 0x3b: fprintf(strm, "\x1b[34mbrle\x1b[m"); goto sw_jmp;
        case 0x4b: fprintf(strm, "\x1b[34mjmb\x1b[m");  goto sw_jmp;
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

        default: fprintf(strm, "\x1b[31merrop\x1b[m");
        } // other

        fprintf(strm, "\t\x1b[32m;");
        while (pk <= k) fprintf(strm, " 0x%02x", code.ptr[pk++]);
        fprintf(strm, "\x1b[m\n");
#       undef imm
    }
}

#endif // CINTRE_PRINTS_H
