/// Collection of colorful print utils for various types:
///
/// `ct_declaration` and `ct_expression` from the parser
///   declarations does not reflect the C-style declarations
///   expressions are shown as a plain boring tree
///
/// `struct ct_adpt_type` and `struct ct_adpt_item` from the adapter
///   items are printed with their type if depth is 0 and with their value
///   (which may be taken from the stack)
///
/// `ct_bytecode` from the compiler
///   traditional assembly-looking, also works a decent reference for the
///   instruction set as long as you can read it
///
/// `ct_run_state` for the top of its stack at runtime

#ifndef CINTRE_PRINTS_H
#define CINTRE_PRINTS_H

#include "common.h"
#include "parser.h"
#include "adapter.h"
#include "compiler.h"
#include "runner.h"

void ct_print_decl(FILE ref strm, ct_declaration cref decl);
void ct_print_expr(FILE ref strm, ct_expression cref expr, unsigned const depth);
void ct_print_type(FILE ref strm, struct ct_adpt_type cref ty, bool const top);
void ct_print_code(FILE ref strm, ct_bytecode const code);
void ct_print_item(FILE ref strm, struct ct_adpt_item cref it, char cref stack, unsigned const depth);
void ct_print_tops(FILE ref strm, ct_run_state cref rs, struct ct_adpt_item cref items, size_t const count);
void ct_print_slot(FILE ref strm, struct ct_slot cref slt);

// ---

void _ct_print_decl_type(FILE ref strm, struct ct_decl_type cref ty)
{
    for (size_t k = 0; CT_QUAL_END != ty->quals[k]; k++) switch (ty->quals[k]) {
    case CT_QUAL_END:                                                  break;
    case CT_QUAL_CONST:     fprintf(strm, "\x1b[34mconst\x1b[m ");     break;
    case CT_QUAL_RESTRICT:  fprintf(strm, "\x1b[34mrestrict\x1b[m ");  break;
    case CT_QUAL_VOLATILE:  fprintf(strm, "\x1b[34mvolatile\x1b[m ");  break;
    case CT_QUAL_SIGNED:    fprintf(strm, "\x1b[34msigned\x1b[m ");    break;
    case CT_QUAL_UNSIGNED:  fprintf(strm, "\x1b[34munsigned\x1b[m ");  break;
    case CT_QUAL_SHORT:     fprintf(strm, "\x1b[34mshort\x1b[m ");     break;
    case CT_QUAL_LONG:      fprintf(strm, "\x1b[34mlong\x1b[m ");      break;
    case CT_QUAL_COMPLEX:   fprintf(strm, "\x1b[34mcomplex\x1b[m ");   break;
    case CT_QUAL_IMAGINARY: fprintf(strm, "\x1b[34mimaginary\x1b[m "); break;
    }

    switch (ty->kind) {
    case CT_KIND_NOTAG: fprintf(strm, "\x1b[32m%.*s\x1b[m", bufmt(ty->name)); break;

    case CT_KIND_STRUCT: fprintf(strm, "\x1b[34mstruct\x1b[m"); if (0)
    case CT_KIND_UNION:  fprintf(strm, "\x1b[34munion\x1b[m");
        if (ty->name.len) fprintf(strm, " %.*s", bufmt(ty->name));
        if ((size_t)-1 != ty->info.comp.count) {
            fprintf(strm, " {");
            for (struct ct_decl_type_field const* it = ty->info.comp.first; it; it = it->next) {
                ct_print_decl(strm, it->decl);
                // todo: same as an array's length, this is incorrect but
                //       avoids unneeded complication
                if (it->bitw) CT_ATOM == it->bitw->kind
                        ? fprintf(strm, ":%.*s", bufmt(it->bitw->info.atom))
                        : fprintf(strm, "<expr>");
                if (it->next) fprintf(strm, ", ");
            }
            fprintf(strm, "}");
        }
        break;

    case CT_KIND_ENUM:
        fprintf(strm, "\x1b[34menum\x1b[m");
        if (ty->name.len) fprintf(strm, " %.*s", bufmt(ty->name));
        if ((size_t)-1 != ty->info.enu.count) {
            fprintf(strm, " {");
            for (struct ct_decl_type_enumer const* it = ty->info.enu.first; it; it = it->next) {
                fprintf(strm, "%.*s", bufmt(it->name));
                if (it->next) fprintf(strm, ", ");
            }
            fprintf(strm, "}");
        }
        break;

    case CT_KIND_PTR:
        fprintf(strm, "\x1b[34mptr\x1b[m[");
        _ct_print_decl_type(strm, &ty->info.ptr->type);
        fprintf(strm, "]");
        break;

    case CT_KIND_FUN:
        fprintf(strm, "\x1b[34mfun\x1b[m(");
        if ((size_t)-1 == ty->info.fun.count) ;
        else if (!ty->info.fun.count) fprintf(strm, "\x1b[36mvoid\x1b[m");
        else for (struct ct_decl_type_param const* it = ty->info.fun.first; it; it = it->next) {
            if (!it->decl) fprintf(strm, "...");
            else ct_print_decl(strm, it->decl);
            if (it->next) fprintf(strm, ", ");
        }
        fprintf(strm, ") -> ");
        _ct_print_decl_type(strm, &ty->info.fun.ret->type);
        break;

    case CT_KIND_ARR:
        fprintf(strm, "\x1b[34marr\x1b[m[");
        if (ty->info.arr.is_static) fprintf(strm, "\x1b[36mstatic\x1b[m ");
        if (!ty->info.arr.count) fprintf(strm, "*, ");
        // todo: this was temporary to generate the test tapes, but because
        //       we're not going to just compile and run the expression here
        //       we'll need to print the expression which gets complicated and
        //       more importantly unneeded for now and before a long time
        else CT_ATOM == ty->info.arr.count->kind
                ? fprintf(strm, "%.*s, ", bufmt(ty->info.arr.count->info.atom))
                : fprintf(strm, "<expr>");
        _ct_print_decl_type(strm, &ty->info.arr.item->type);
        fprintf(strm, "]");
        break;
    }
}

void ct_print_decl(FILE ref strm, ct_declaration cref decl)
{
    switch (decl->spec) {
    case CT_SPEC_NONE:                                               break;
    case CT_SPEC_TYPEDEF:  fprintf(strm, "\x1b[36mtypedef\x1b[m ");  break;
    case CT_SPEC_EXTERN:   fprintf(strm, "\x1b[36mextern\x1b[m ");   break;
    case CT_SPEC_STATIC:   fprintf(strm, "\x1b[36mstatic\x1b[m ");   break;
    case CT_SPEC_AUTO:     fprintf(strm, "\x1b[36mauto\x1b[m ");     break;
    case CT_SPEC_REGISTER: fprintf(strm, "\x1b[36mregister\x1b[m "); break;
    }
    if (decl->is_inline) fprintf(strm, "\x1b[34minline\x1b[m ");
    fprintf(strm, "%.*s: ", bufmt(decl->name));
    _ct_print_decl_type(strm, &decl->type);
}

void ct_print_expr(FILE ref strm, ct_expression cref expr, unsigned const depth)
{
    static char cref op_kind_names[] = {"CT_ATOM", "CT_BINOP_SUBSCR", "CT_BINOP_CALL", "CT_BINOP_TERNCOND", "CT_BINOP_TERNBRANCH", "CT_BINOP_COMMA", "CT_BINOP_ASGN", "CT_BINOP_ASGN_BOR", "CT_BINOP_ASGN_BXOR", "CT_BINOP_ASGN_BAND", "CT_BINOP_ASGN_BSHL", "CT_BINOP_ASGN_BSHR", "CT_BINOP_ASGN_SUB", "CT_BINOP_ASGN_ADD", "CT_BINOP_ASGN_REM", "CT_BINOP_ASGN_DIV", "CT_BINOP_ASGN_MUL", "CT_BINOP_LOR", "CT_BINOP_LAND", "CT_BINOP_BOR", "CT_BINOP_BXOR", "CT_BINOP_BAND", "CT_BINOP_EQ", "CT_BINOP_NE", "CT_BINOP_LT", "CT_BINOP_GT", "CT_BINOP_LE", "CT_BINOP_GE", "CT_BINOP_BSHL", "CT_BINOP_BSHR", "CT_BINOP_SUB", "CT_BINOP_ADD", "CT_BINOP_REM", "CT_BINOP_DIV", "CT_BINOP_MUL", "CT_UNOP_ADDR", "CT_UNOP_DEREF", "CT_UNOP_CAST", "CT_UNOP_BNOT", "CT_UNOP_LNOT", "CT_UNOP_MINUS", "CT_UNOP_PLUS", "CT_UNOP_PRE_DEC", "CT_UNOP_PRE_INC", "CT_UNOP_PMEMBER", "CT_UNOP_MEMBER", "CT_UNOP_POST_DEC", "CT_UNOP_POST_INC"};
    for (unsigned k = 0; k < depth; k++) fprintf(strm, "|  ");

    if (!expr) {
        fprintf(strm, "\x1b[31m(nil)\x1b[m\n");
        return;
    }

    if (CT_ATOM == expr->kind) {
        char const c = *expr->info.atom.ptr;
        fprintf(strm, "\x1b[%dm%.*s\x1b[m\n", '"' == c ? 36 : ('0' <= c && c <= '9') || '\'' == c || '.' == c ? 33 : 0, bufmt(expr->info.atom));
        return;
    }

    fprintf(strm, "\x1b[34m%s\x1b[m\n", op_kind_names[expr->kind]);

    switch (expr->kind) {
    case CT_UNOP_CAST:
        for (unsigned k = 0; k < depth+1; k++) fprintf(strm, "|  ");
        _ct_print_decl_type(strm, expr->info.cast.type);
        fprintf(strm, "\n");
        ct_print_expr(strm, expr->info.cast.opr, depth+1);
        break;

    case CT_UNOP_MEMBER:
    case CT_UNOP_PMEMBER:
        ct_print_expr(strm, expr->info.member.base, depth+1);
        for (unsigned k = 0; k < depth+1; k++) fprintf(strm, "|  ");
        fprintf(strm, "%s%.*s\n", CT_UNOP_MEMBER == expr->kind ? "." : "->", bufmt(*expr->info.member.name));
        break;

    case CT_UNOP_ADDR:     case CT_UNOP_DEREF:
    case CT_UNOP_BNOT:     case CT_UNOP_LNOT:
    case CT_UNOP_MINUS:    case CT_UNOP_PLUS:
    case CT_UNOP_PRE_DEC:  case CT_UNOP_PRE_INC:
    case CT_UNOP_POST_DEC: case CT_UNOP_POST_INC:
        ct_print_expr(strm, expr->info.unary.opr, depth+1);
        break;

    case CT_BINOP_CALL:
        ct_print_expr(strm, expr->info.call.base, depth+1);
        size_t count = 0;
        for (struct ct_expr_call_arg const* it = expr->info.call.first; it; it = it->next)
            count++;
        for (unsigned k = 0; k < depth; k++) fprintf(strm, "|  ");
        fprintf(strm, "|  \x1b[32m(%zu)\x1b[m\n", count);
        for (struct ct_expr_call_arg const* it = expr->info.call.first; it; it = it->next)
            ct_print_expr(strm, it->expr, depth+2);
        break;

    case CT_BINOP_SUBSCR:
        ct_print_expr(strm, expr->info.subscr.base, depth+1);
        ct_print_expr(strm, expr->info.subscr.off, depth+1);
        break;

    default: // (33 cases ><'')
        ct_print_expr(strm, expr->info.binary.lhs, depth+1);
        ct_print_expr(strm, expr->info.binary.rhs, depth+1);
        break;
    }
}

void ct_print_type(FILE ref strm, struct ct_adpt_type cref ty, bool const top)
{
    if (!ty) {
        fprintf(strm, "\x1b[31m(nil)\x1b[m");
        return;
    }

    struct ct_adpt_type cref tty = top ? _ct_truetype(ty) : ty;
    switch (tty->tyty) {
    case CT_TYPE_VOID:   fprintf(strm, "\x1b[32mvoid\x1b[m");   break;
    case CT_TYPE_CHAR:   fprintf(strm, "\x1b[32mchar\x1b[m");   break;
    case CT_TYPE_UCHAR:  fprintf(strm, "\x1b[32muchar\x1b[m");  break;
    case CT_TYPE_SCHAR:  fprintf(strm, "\x1b[32mschar\x1b[m");  break;
    case CT_TYPE_SHORT:  fprintf(strm, "\x1b[32mshort\x1b[m");  break;
    case CT_TYPE_INT:    fprintf(strm, "\x1b[32mint\x1b[m");    break;
    case CT_TYPE_LONG:   fprintf(strm, "\x1b[32mlong\x1b[m");   break;
    case CT_TYPE_USHORT: fprintf(strm, "\x1b[32mushort\x1b[m"); break;
    case CT_TYPE_UINT:   fprintf(strm, "\x1b[32muint\x1b[m");   break;
    case CT_TYPE_ULONG:  fprintf(strm, "\x1b[32mulong\x1b[m");  break;
    case CT_TYPE_FLOAT:  fprintf(strm, "\x1b[32mfloat\x1b[m");  break;
    case CT_TYPE_DOUBLE: fprintf(strm, "\x1b[32mdouble\x1b[m"); break;

    case CT_TYPE_STRUCT: fprintf(strm, "\x1b[34mstruct\x1b[m"); if (0)
    case CT_TYPE_UNION:  fprintf(strm, "\x1b[34munion\x1b[m");
        if (tty->info.comp.named) {
            fprintf(strm, " %s", tty->info.comp.named);
            if (!top) break;
        }
        fprintf(strm, "{");
        for (size_t k = 0; k < tty->info.comp.count; k++) {
            struct ct_adpt_comp_field const* it = tty->info.comp.fields+k;
            fprintf(strm, k ? ", %s@%zu: " : "%s@%zu: ", it->name, it->offset);
            ct_print_type(strm, it->type, false);
        }
        fprintf(strm, "}");
        break;

    case CT_TYPE_FUN:
        fprintf(strm, "\x1b[34mfun\x1b[m(");
        for (size_t k = 0; k < tty->info.fun.count; k++) {
            struct ct_adpt_fun_param const* it = tty->info.fun.params+k;
            fprintf(strm, k ? ", %s: " : "%s: ", it->name);
            ct_print_type(strm, it->type, false);
        }
        fprintf(strm, ") -> ");
        ct_print_type(strm, tty->info.fun.ret, false);
        break;

    case CT_TYPE_PTR:
        fprintf(strm, "\x1b[34mptr\x1b[m[");
        ct_print_type(strm, tty->info.ptr, false);
        fprintf(strm, "]");
        break;

    case CT_TYPE_ARR:
        fprintf(strm, "\x1b[34marr\x1b[m[%zu, ", tty->info.arr.count);
        ct_print_type(strm, tty->info.arr.item, false);
        fprintf(strm, "]");
        break;

    case CT_TYPE_NAMED:
        fprintf(strm, "\x1b[32m%s\x1b[m", tty->info.named.name);
        break;
    }
}

void ct_print_code(FILE ref strm, ct_bytecode const code)
{
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

void ct_print_item(FILE ref strm, struct ct_adpt_item cref it, char cref stack, unsigned const depth)
{
    if (!depth) {
        fprintf(strm, "%s: ", it->name);
        ct_print_type(strm, it->type, true);
        fprintf(strm, "\n   = ");
    }

    if (CT_ITEM_TYPEDEF == it->kind) {
        fprintf(strm, "???");
        return;
    }
    if (CT_ITEM_VALUE == it->kind) {
        fprintf(strm, "%li", it->as.value);
        return;
    }

    void cref p = CT_ITEM_OBJECT == it->kind
        ? it->as.object
        : stack+it->as.variable;

    struct ct_adpt_type cref tty = _ct_truetype(it->type);
    switch (tty->tyty) {
    case CT_TYPE_VOID: fprintf(strm, "()"); break;

    case CT_TYPE_CHAR:;
        switch (*(char*)p) {
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
            default: fprintf(strm, ' ' <= *(char*)p && *(char*)p <= '~' ? "'%c'" : "'\\x%02hhx'", *(char*)p);
        } break;

    case CT_TYPE_UCHAR:  fprintf(strm, "0x%02hhx", *(unsigned char*)p);  break;
    case CT_TYPE_SCHAR:  fprintf(strm, "%hhi",     *(signed char*)p);    break;
    case CT_TYPE_SHORT:  fprintf(strm, "%hi",      *(short*)p);          break;
    case CT_TYPE_INT:    fprintf(strm, "%i",       *(int*)p);            break;
    case CT_TYPE_LONG:   fprintf(strm, "%li",      *(long*)p);           break;
    case CT_TYPE_USHORT: fprintf(strm, "%hu",      *(unsigned short*)p); break;
    case CT_TYPE_UINT:   fprintf(strm, "%u",       *(unsigned int*)p);   break;
    case CT_TYPE_ULONG:  fprintf(strm, "%lu",      *(unsigned long*)p);  break;
    case CT_TYPE_FLOAT:  fprintf(strm, "%f",       *(float*)p);          break;
    case CT_TYPE_DOUBLE: fprintf(strm, "%lf",      *(double*)p);         break;

    case CT_TYPE_STRUCT:
    case CT_TYPE_UNION:
        fprintf(strm, "{\n");
        for (size_t k = 0; k < tty->info.comp.count; k++) {
            struct ct_adpt_comp_field cref f = tty->info.comp.fields+k;
            fprintf(strm, "%*s.%s= ", (depth+1)*3, "", f->name);
            ct_print_item(strm, &(struct ct_adpt_item){
                    .type= f->type,
                    .as.object= (char*)p+f->offset, // xxx: discards const
                }, stack, depth+1);
            //fprintf(strm, "\n");
        }
        fprintf(strm, "%*s}", depth*3, "");
        break;

    case CT_TYPE_FUN: fprintf(strm, "(%p)", p); break;

    case CT_TYPE_PTR:
        fprintf(strm, "(%p) ", p);
        //ct_print_item(strm, &(struct ct_adpt_item){
        //        .type= tty->info.ptr,
        //        .as.object= *(void**)p,
        //    }, stack, depth);
        break;

    case CT_TYPE_ARR:
        fprintf(strm, "[\n");
        for (size_t k = 0; k < tty->info.arr.count; k++) {
            fprintf(strm, "%*s[%zu]= ", (depth+1)*3, "", k);
            ct_print_item(strm, &(struct ct_adpt_item){
                    .type= tty->info.arr.item,
                    .as.object= (char*)p+k*tty->info.arr.item->size, // xxx: discards const
                }, stack, depth+1);
            //fprintf(strm, "\n");
        }
        fprintf(strm, "%*s]", depth*3, "");
        break;

        // unreachable case
    case CT_TYPE_NAMED:;
    }

    fprintf(strm, "\n");
}

void ct_print_tops(FILE ref strm, ct_run_state cref rs, struct ct_adpt_item cref items, size_t const count)
{
    size_t const sz = sizeof rs->stack; // (xxx: sizeof stack)

    size_t in_var_size = 0;
#   define col_n(_n) ((( (_n)[0]^(_n)[1] )&3)+31)

    fprintf(strm, "\x1b[1m                   . ");
    for (int k = 0; k < 16; k++) fprintf(strm, " %02x", k);
    fprintf(strm, "  .\x1b[m\n");

    for (size_t at = sz; 16 <= at && rs->sp < at; at-= 16) {
        fprintf(strm, "\x1b[1m0x%08zx @-%-5zu |\x1b[m ", at-16, sz-at+16);

        for (int k = 15; k >= 0; k--) {
            if (rs->sp >= at-k) {
                fprintf(strm, "   ");
                continue;
            }
            fprintf(strm, " ");

            if (!in_var_size) {
                size_t n;
                for (n = 0; n < count; n++) if (CT_ITEM_VARIABLE == items[n].kind && items[n].as.variable <= at-1-k && at-1-k < items[n].as.variable+items[n].type->size) {
                    in_var_size = items[n].type->size - ((at-1-k)-items[n].as.variable);
                    break;
                }
                if (in_var_size) fprintf(strm, "\x1b[%um\x1b[4m", col_n(items[n].name));
            }

            fprintf(strm, "%02hhX", rs->stack[at-1-k]);

            if (in_var_size && !--in_var_size) fprintf(strm, "\x1b[m");
        }
        if (in_var_size) fprintf(strm, "\x1b[m"), in_var_size = 0;

        fprintf(strm, "  \x1b[1m|\x1b[m");
        for (int k = 15; k >= 0; k--)
            for (size_t n = 0; n < count; n++) if (CT_ITEM_VARIABLE == items[n].kind && items[n].as.variable == at-1-k)
                fprintf(strm, "  \x1b[%um%s\x1b[m(%zu)", col_n(items[n].name), items[n].name, items[n].type->size);

        fprintf(strm, "\n");
    }

#   undef col_n
}

void ct_print_slot(FILE ref strm, struct ct_slot cref slot)
{
    fprintf(strm, "slot ");
    ct_print_type(strm, slot->ty, true);
    switch (slot->usage) {
    case _slot_value:
        fprintf(strm, " \x1b[36mvalue\x1b[m = ");
        switch (slot->ty->tyty) {
        case CT_TYPE_CHAR:   fprintf(strm, "'%c'",     slot->as.value.c);  break;
        case CT_TYPE_UCHAR:  fprintf(strm, "0x%02hhx", slot->as.value.uc); break;
        case CT_TYPE_SCHAR:  fprintf(strm, "%hhi",     slot->as.value.sc); break;
        case CT_TYPE_SHORT:  fprintf(strm, "%hi",      slot->as.value.ss); break;
        case CT_TYPE_INT:    fprintf(strm, "%i",       slot->as.value.si); break;
        case CT_TYPE_LONG:   fprintf(strm, "%li",      slot->as.value.sl); break;
        case CT_TYPE_USHORT: fprintf(strm, "%hu",      slot->as.value.us); break;
        case CT_TYPE_UINT:   fprintf(strm, "%u",       slot->as.value.ui); break;
        case CT_TYPE_ULONG:  fprintf(strm, "%lu",      slot->as.value.ul); break;
        case CT_TYPE_FLOAT:  fprintf(strm, "%f",       slot->as.value.f);  break;
        case CT_TYPE_DOUBLE: fprintf(strm, "%lf",      slot->as.value.d);  break;
        case CT_TYPE_FUN:
        case CT_TYPE_PTR:
        case CT_TYPE_ARR:    fprintf(strm, "%p",       slot->as.value.p);  break;

            // unreachable cases
        case CT_TYPE_VOID: case CT_TYPE_STRUCT: case CT_TYPE_UNION: case CT_TYPE_NAMED:;
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
