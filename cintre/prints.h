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

void print_decl(FILE ref strm, struct lex_state cref ls, declaration cref decl);
void print_expr(FILE ref strm, struct lex_state cref ls, expression cref expr, unsigned const depth);
void print_cxpr(FILE ref strm, struct lex_state cref ls, expression cref expr);
void print_stmt(FILE ref strm, struct lex_state cref ls, struct statement cref stmt, unsigned const depth);
void print_type(FILE ref strm, struct adpt_type cref ty, bool const top);
void print_code(FILE ref strm, bytecode const code);
void print_item(FILE ref strm, struct adpt_item cref it, char cref stack, unsigned const depth);
void print_tops(FILE ref strm, run_state cref rs, struct adpt_item cref items, size_t const count);
void print_slot(FILE ref strm, struct slot cref slt);

// ---

void _print_decl_type(FILE ref strm, lex_state cref ls, struct decl_type cref ty)
{
    for (size_t k = 0; DECL_QUAL_END != ty->quals[k]; k++) switch (ty->quals[k]) {
    case DECL_QUAL_END:                                                  break;
    case DECL_QUAL_CONST:     fprintf(strm, "\x1b[34mconst\x1b[m ");     break;
    case DECL_QUAL_RESTRICT:  fprintf(strm, "\x1b[34mrestrict\x1b[m ");  break;
    case DECL_QUAL_VOLATILE:  fprintf(strm, "\x1b[34mvolatile\x1b[m ");  break;
    case DECL_QUAL_SIGNED:    fprintf(strm, "\x1b[34msigned\x1b[m ");    break;
    case DECL_QUAL_UNSIGNED:  fprintf(strm, "\x1b[34munsigned\x1b[m ");  break;
    case DECL_QUAL_SHORT:     fprintf(strm, "\x1b[34mshort\x1b[m ");     break;
    case DECL_QUAL_LONG:      fprintf(strm, "\x1b[34mlong\x1b[m ");      break;
    case DECL_QUAL_COMPLEX:   fprintf(strm, "\x1b[34mcomplex\x1b[m ");   break;
    case DECL_QUAL_IMAGINARY: fprintf(strm, "\x1b[34mimaginary\x1b[m "); break;
    }

    switch (ty->kind) {
    case DECL_KIND_NOTAG: fprintf(strm, "\x1b[32m%s\x1b[m", tokn(ty->name)); break;

    case DECL_KIND_STRUCT: fprintf(strm, "\x1b[34mstruct\x1b[m"); if (0)
    case DECL_KIND_UNION:  fprintf(strm, "\x1b[34munion\x1b[m");
        if (*tokn(ty->name)) fprintf(strm, " %s", tokn(ty->name));
        if ((size_t)-1 != ty->info.comp.count) {
            fprintf(strm, " {");
            for (struct decl_type_field const* it = ty->info.comp.first; it; it = it->next) {
                print_decl(strm, ls, it->decl);
                if (it->bitw) fprintf(strm, " :"), print_cxpr(strm, ls, it->bitw);
                if (it->next) fprintf(strm, ", ");
            }
            fprintf(strm, "}");
        }
        break;

    case DECL_KIND_ENUM:
        fprintf(strm, "\x1b[34menum\x1b[m");
        if (*tokn(ty->name)) fprintf(strm, " %s", tokn(ty->name));
        if ((size_t)-1 != ty->info.enu.count) {
            fprintf(strm, " {");
            for (struct decl_type_enumer const* it = ty->info.enu.first; it; it = it->next) {
                fprintf(strm, "%s", tokn(it->name));
                if (it->expr) fprintf(strm, "= "), print_cxpr(strm, ls, it->expr);
                if (it->next) fprintf(strm, ", ");
            }
            fprintf(strm, "}");
        }
        break;

    case DECL_KIND_PTR:
        fprintf(strm, "\x1b[34mptr\x1b[m[");
        _print_decl_type(strm, ls, &ty->info.ptr->type);
        fprintf(strm, "]");
        break;

    case DECL_KIND_FUN:
        fprintf(strm, "\x1b[34mfun\x1b[m(");
        if ((size_t)-1 == ty->info.fun.count) ;
        else if (!ty->info.fun.count) fprintf(strm, "\x1b[36mvoid\x1b[m");
        else for (struct decl_type_param const* it = ty->info.fun.first; it; it = it->next) {
            if (!it->decl) fprintf(strm, "...");
            else print_decl(strm, ls, it->decl);
            if (it->next) fprintf(strm, ", ");
        }
        fprintf(strm, ") -> ");
        _print_decl_type(strm, ls, &ty->info.fun.ret->type);
        break;

    case DECL_KIND_ARR:
        fprintf(strm, "\x1b[34marr\x1b[m[");
        if (ty->info.arr.is_static) fprintf(strm, "\x1b[36mstatic\x1b[m ");
        if (!ty->info.arr.count) fprintf(strm, "*");
        else print_cxpr(strm, ls, ty->info.arr.count);
        fprintf(strm, ", ");
        _print_decl_type(strm, ls, &ty->info.arr.item->type);
        fprintf(strm, "]");
        break;
    }
}

void print_decl(FILE ref strm, lex_state cref ls, declaration cref decl)
{
    switch (decl->spec) {
    case DECL_SPEC_NONE:                                               break;
    case DECL_SPEC_TYPEDEF:  fprintf(strm, "\x1b[36mtypedef\x1b[m ");  break;
    case DECL_SPEC_EXTERN:   fprintf(strm, "\x1b[36mextern\x1b[m ");   break;
    case DECL_SPEC_STATIC:   fprintf(strm, "\x1b[36mstatic\x1b[m ");   break;
    case DECL_SPEC_AUTO:     fprintf(strm, "\x1b[36mauto\x1b[m ");     break;
    case DECL_SPEC_REGISTER: fprintf(strm, "\x1b[36mregister\x1b[m "); break;
    }
    if (decl->is_inline) fprintf(strm, "\x1b[34minline\x1b[m ");
    fprintf(strm, "%s: ", tokn(decl->name));
    _print_decl_type(strm, ls, &decl->type);
}

void print_expr(FILE ref strm, lex_state cref ls, expression cref expr, unsigned const depth)
{
    static char cref op_kind_names[] = {"ATOM", "COMPLIT", "BINOP_SUBSCR", "BINOP_CALL", "BINOP_TERNCOND", "BINOP_TERNBRANCH", "BINOP_COMMA", "BINOP_ASGN", "BINOP_ASGN_BOR", "BINOP_ASGN_BXOR", "BINOP_ASGN_BAND", "BINOP_ASGN_BSHL", "BINOP_ASGN_BSHR", "BINOP_ASGN_SUB", "BINOP_ASGN_ADD", "BINOP_ASGN_REM", "BINOP_ASGN_DIV", "BINOP_ASGN_MUL", "BINOP_LOR", "BINOP_LAND", "BINOP_BOR", "BINOP_BXOR", "BINOP_BAND", "BINOP_EQ", "BINOP_NE", "BINOP_LT", "BINOP_GT", "BINOP_LE", "BINOP_GE", "BINOP_BSHL", "BINOP_BSHR", "BINOP_SUB", "BINOP_ADD", "BINOP_REM", "BINOP_DIV", "BINOP_MUL", "UNOP_ADDR", "UNOP_DEREF", "UNOP_CAST", "UNOP_BNOT", "UNOP_LNOT", "UNOP_MINUS", "UNOP_PLUS", "UNOP_PRE_DEC", "UNOP_PRE_INC", "UNOP_PMEMBER", "UNOP_MEMBER", "UNOP_POST_DEC", "UNOP_POST_INC"};
    for (unsigned k = 0; k < depth; k++) fprintf(strm, "|  ");

    if (!expr) {
        fprintf(strm, "\x1b[31m(nil)\x1b[m\n");
        return;
    }

    if (EXPR_ATOM == expr->kind) {
        char const c = *tokn(expr->info.atom);
        fprintf(strm, "\x1b[%dm%s\x1b[m\n", '"' == c ? 36 : ('0' <= c && c <= '9') || '\'' == c || '.' == c ? 33 : 0, tokn(expr->info.atom));
        return;
    }

    fprintf(strm, "\x1b[34m%s\x1b[m\n", op_kind_names[expr->kind]);

    switch (expr->kind) {
    case EXPR_COMPLIT:
        for (unsigned k = 0; k < depth+1; k++) fprintf(strm, "|  ");
        if (expr->info.comp.type) _print_decl_type(strm, ls, expr->info.comp.type);
        else fprintf(strm, "(no type)");
        fprintf(strm, "\n");
        for (struct expr_comp_entry* en = expr->info.comp.first; en; en = en->next) {
            for (unsigned k = 0; k < depth+1; k++) fprintf(strm, "|  ");
            if (en->desig) for (struct expr_comp_desig* de = en->desig; de; de = de->next) {
                if (de->is_field) {
                    fprintf(strm, ".%s", tokn(de->info.field));
                    continue;
                }
                if (de->is_subscript) {
                    fprintf(strm, "[");
                    print_cxpr(strm, ls, de->info.subscript);
                    fprintf(strm, "]");
                    continue;
                }
            } else fprintf(strm, "(no desig)");
            fprintf(strm, "\n");
            print_expr(strm, ls, en->value, depth+2);
        }
        break;

    case EXPR_UNOP_CAST:
        for (unsigned k = 0; k < depth+1; k++) fprintf(strm, "|  ");
        _print_decl_type(strm, ls, expr->info.cast.type);
        fprintf(strm, "\n");
        print_expr(strm, ls, expr->info.cast.opr, depth+1);
        break;

    case EXPR_UNOP_MEMBER:
    case EXPR_UNOP_PMEMBER:
        print_expr(strm, ls, expr->info.member.base, depth+1);
        for (unsigned k = 0; k < depth+1; k++) fprintf(strm, "|  ");
        fprintf(strm, "%s%s\n", EXPR_UNOP_MEMBER == expr->kind ? "." : "->", tokn(expr->info.member.name));
        break;

    case EXPR_UNOP_ADDR:     case EXPR_UNOP_DEREF:
    case EXPR_UNOP_BNOT:     case EXPR_UNOP_LNOT:
    case EXPR_UNOP_MINUS:    case EXPR_UNOP_PLUS:
    case EXPR_UNOP_PRE_DEC:  case EXPR_UNOP_PRE_INC:
    case EXPR_UNOP_POST_DEC: case EXPR_UNOP_POST_INC:
        print_expr(strm, ls, expr->info.unary.opr, depth+1);
        break;

    case EXPR_BINOP_CALL:
        print_expr(strm, ls, expr->info.call.base, depth+1);
        size_t count = 0;
        for (struct expr_call_arg const* it = expr->info.call.first; it; it = it->next)
            count++;
        for (unsigned k = 0; k < depth; k++) fprintf(strm, "|  ");
        fprintf(strm, "|  \x1b[32m(%zu)\x1b[m\n", count);
        for (struct expr_call_arg const* it = expr->info.call.first; it; it = it->next)
            print_expr(strm, ls, it->expr, depth+2);
        break;

    case EXPR_BINOP_SUBSCR:
        print_expr(strm, ls, expr->info.subscr.base, depth+1);
        print_expr(strm, ls, expr->info.subscr.off, depth+1);
        break;

    default: // (33 cases ><'')
        print_expr(strm, ls, expr->info.binary.lhs, depth+1);
        print_expr(strm, ls, expr->info.binary.rhs, depth+1);
        break;
    }
}

void print_cxpr(FILE ref strm, struct lex_state cref ls, expression cref expr)
{
    fprintf(strm, "("); // xxx: needs to not if complit
    switch (expr->kind) {
    case EXPR_ATOM:
        fprintf(strm, "%s", tokn(expr->info.atom));
        break;

    case EXPR_COMPLIT:
        if (expr->info.comp.type) {
            fprintf(strm, "(");
            _print_decl_type(strm, ls, expr->info.comp.type);
            fprintf(strm, ")");
        }
        fprintf(strm, "{");
        for (struct expr_comp_entry* en = expr->info.comp.first; en; en = en->next) {
            if (en->desig) {
                for (struct expr_comp_desig* de = en->desig; de; de = de->next) {
                    if (de->is_field) {
                        fprintf(strm, ".%s", tokn(de->info.field));
                        continue;
                    }
                    if (de->is_subscript) {
                        fprintf(strm, "[");
                        print_cxpr(strm, ls, de->info.subscript);
                        fprintf(strm, "]");
                        continue;
                    }
                }
                fprintf(strm, "= ");
            }
            print_cxpr(strm, ls, en->value);
        }
        fprintf(strm, "}");
        break;

    case EXPR_BINOP_SUBSCR:
        print_cxpr(strm, ls, expr->info.subscr.base);
        fprintf(strm, "[");
        print_cxpr(strm, ls, expr->info.subscr.off);
        fprintf(strm, "]");
        break;

    case EXPR_BINOP_CALL:
        print_cxpr(strm, ls, expr->info.call.base);
        fprintf(strm, "(");
        for (struct expr_call_arg* it = expr->info.call.first; it; it = it->next) {
            print_cxpr(strm, ls, it->expr);
            if (it->next) fprintf(strm, ", ");
        }
        fprintf(strm, ")");
        break;

    case EXPR_BINOP_TERNBRANCH:
        fprintf(strm, "0");
        break;
    case EXPR_BINOP_TERNCOND:
        print_cxpr(strm, ls, expr->info.binary.lhs); // condition
        fprintf(strm, " ? ");
        print_cxpr(strm, ls, expr->info.binary.rhs->info.binary.lhs); // consequence
        fprintf(strm, " : ");
        print_cxpr(strm, ls, expr->info.binary.rhs->info.binary.rhs); // alternative
        break;

        char const* binop;
    case EXPR_BINOP_COMMA:     binop = ", ";   goto binop;
    case EXPR_BINOP_ASGN:      binop = " = ";  goto binop;
    case EXPR_BINOP_ASGN_BOR:  binop = "|= ";  goto binop;
    case EXPR_BINOP_ASGN_BXOR: binop = "^= ";  goto binop;
    case EXPR_BINOP_ASGN_BAND: binop = "&= ";  goto binop;
    case EXPR_BINOP_ASGN_BSHL: binop = "<<= "; goto binop;
    case EXPR_BINOP_ASGN_BSHR: binop = ">>= "; goto binop;
    case EXPR_BINOP_ASGN_SUB:  binop = "-= ";  goto binop;
    case EXPR_BINOP_ASGN_ADD:  binop = "+= ";  goto binop;
    case EXPR_BINOP_ASGN_REM:  binop = "%= ";  goto binop;
    case EXPR_BINOP_ASGN_DIV:  binop = "/= ";  goto binop;
    case EXPR_BINOP_ASGN_MUL:  binop = "*= ";  goto binop;
    case EXPR_BINOP_LOR:       binop = " || "; goto binop;
    case EXPR_BINOP_LAND:      binop = " && "; goto binop;
    case EXPR_BINOP_BOR:       binop = " | ";  goto binop;
    case EXPR_BINOP_BXOR:      binop = " ^ ";  goto binop;
    case EXPR_BINOP_BAND:      binop = " & ";  goto binop;
    case EXPR_BINOP_EQ:        binop = " == "; goto binop;
    case EXPR_BINOP_NE:        binop = " != "; goto binop;
    case EXPR_BINOP_LT:        binop = " < ";  goto binop;
    case EXPR_BINOP_GT:        binop = " > ";  goto binop;
    case EXPR_BINOP_LE:        binop = " <= "; goto binop;
    case EXPR_BINOP_GE:        binop = " >= "; goto binop;
    case EXPR_BINOP_BSHL:      binop = "<<";   goto binop;
    case EXPR_BINOP_BSHR:      binop = ">>";   goto binop;
    case EXPR_BINOP_SUB:       binop = "-";    goto binop;
    case EXPR_BINOP_ADD:       binop = "+";    goto binop;
    case EXPR_BINOP_REM:       binop = "%";    goto binop;
    case EXPR_BINOP_DIV:       binop = "/";    goto binop;
    case EXPR_BINOP_MUL:       binop = "*";    goto binop;
    binop:
        print_cxpr(strm, ls, expr->info.binary.lhs);
        fprintf(strm, "%s", binop);
        print_cxpr(strm, ls, expr->info.binary.rhs);
        break;

    case EXPR_UNOP_CAST:
        fprintf(strm, "(");
        // FIXME: this is wrong btw
        _print_decl_type(strm, ls, expr->info.cast.type);
        fprintf(strm, ")");
        print_cxpr(strm, ls, expr->info.cast.opr);
        break;

        char const* unop;
    case EXPR_UNOP_ADDR:    unop = "&";  goto unop;
    case EXPR_UNOP_DEREF:   unop = "*";  goto unop;
    case EXPR_UNOP_BNOT:    unop = "~";  goto unop;
    case EXPR_UNOP_LNOT:    unop = "!";  goto unop;
    case EXPR_UNOP_MINUS:   unop = "-";  goto unop;
    case EXPR_UNOP_PLUS:    unop = "+";  goto unop;
    case EXPR_UNOP_PRE_DEC: unop = "--"; goto unop;
    case EXPR_UNOP_PRE_INC: unop = "++"; goto unop;
    unop:
        fprintf(strm, "%s", unop);
        print_cxpr(strm, ls, expr->info.unary.opr);
        break;

    case EXPR_UNOP_POST_DEC:
        print_cxpr(strm, ls, expr->info.unary.opr);
        fprintf(strm, "--");
        break;
    case EXPR_UNOP_POST_INC:
        print_cxpr(strm, ls, expr->info.unary.opr);
        fprintf(strm, "++");
        break;

    case EXPR_UNOP_PMEMBER:
        print_cxpr(strm, ls, expr->info.member.base);
        fprintf(strm, "->%s", tokn(expr->info.member.name));
        break;
    case EXPR_UNOP_MEMBER:
        print_cxpr(strm, ls, expr->info.member.base);
        fprintf(strm, ".%s", tokn(expr->info.member.name));
        break;
    }
    fprintf(strm, ")");
}

void print_stmt(FILE ref strm, struct lex_state cref ls, struct statement cref stmt, unsigned const depth)
{
    if (!stmt) {
        fprintf(strm, "%*s\x1b[31m(void);\x1b[m\n", (depth+1)*4, "");
        return;
    }

    switch (stmt->kind) {
    case STMT_KIND_EMPTY:
        fprintf(strm, "%*s;\n", (depth+1)*4, "");
        break;

    case STMT_KIND_COMP:
        fprintf(strm, "%*s{\n", depth*4, "");
        for (struct stmt_comp_one* curr = stmt->info.comp; curr; curr = curr->next)
            print_stmt(strm, ls, curr->stmt, depth+1);
        fprintf(strm, "%*s}\n", depth*4, "");
        break;

    case STMT_KIND_EXPR:
        fprintf(strm, "%*s", (depth+1)*4, "");
        print_cxpr(strm, ls, stmt->info.expr);
        fprintf(strm, ";\n");
        break;

    case STMT_KIND_DECL:
        fprintf(strm, "%*s", (depth+1)*4, "");
        print_decl(strm, ls, stmt->info.decl->decl);
        if (stmt->info.decl->expr) {
            fprintf(strm, " = ");
            print_cxpr(strm, ls, stmt->info.decl->expr);
        }
        fprintf(strm, ";\n");
        break;

    case STMT_KIND_IF:
        fprintf(strm, "%*s\x1b[34mif\x1b[m (", (depth+1)*4, "");
        print_cxpr(strm, ls, stmt->info.if_.ctrl);
        fprintf(strm, ")\n");
        print_stmt(strm, ls, stmt->info.if_.body, depth+1);
        if (stmt->info.if_.else_) {
            fprintf(strm, "%*s\x1b[34melse\x1b[m\n", (depth+1)*4, "");
            print_stmt(strm, ls, stmt->info.if_.body, depth+1);
        }
        break;

    case STMT_KIND_SWITCH:
        fprintf(strm, "%*s\x1b[34mswitch\x1b[m (", (depth+1)*4, "");
        print_cxpr(strm, ls, stmt->info.switch_.ctrl);
        fprintf(strm, ")\n");
        print_stmt(strm, ls, stmt->info.switch_.body, depth+1);
        break;

    case STMT_KIND_WHILE:
        fprintf(strm, "%*s\x1b[34mwhile\x1b[m (", (depth+1)*4, "");
        print_cxpr(strm, ls, stmt->info.while_.ctrl);
        fprintf(strm, ")\n");
        print_stmt(strm, ls, stmt->info.while_.body, depth+1);
        break;

    case STMT_KIND_DOWHILE:
        fprintf(strm, "%*s\x1b[34mdo\x1b[m", (depth+1)*4, "");
        print_stmt(strm, ls, stmt->info.dowhile.body, depth+1);
        fprintf(strm, "%*s\x1b[34mwhile (", depth, "");
        print_cxpr(strm, ls, stmt->info.dowhile.ctrl);
        fprintf(strm, ");\n");
        break;

    case STMT_KIND_FOR:
        fprintf(strm, "%*s\x1b[34mfor\x1b[m (", (depth+1)*4, "");
        switch (stmt->info.for_.init->kind) {
        case STMT_KIND_EXPR:
            print_cxpr(strm, ls, stmt->info.expr);
            break;
        case STMT_KIND_DECL:
            print_decl(strm, ls, stmt->info.decl->decl);
            if (stmt->info.decl->expr) {
                fprintf(strm, " = ");
                print_cxpr(strm, ls, stmt->info.decl->expr);
            }
            break;
        case STMT_KIND_EMPTY:    fprintf(strm, "\x1b[31mSTMT_KIND_EMPTY\x1b[m");    break;
        case STMT_KIND_COMP:     fprintf(strm, "\x1b[31mSTMT_KIND_COMP\x1b[m");     break;
        case STMT_KIND_IF:       fprintf(strm, "\x1b[31mSTMT_KIND_IF\x1b[m");       break;
        case STMT_KIND_SWITCH:   fprintf(strm, "\x1b[31mSTMT_KIND_SWITCH\x1b[m");   break;
        case STMT_KIND_WHILE:    fprintf(strm, "\x1b[31mSTMT_KIND_WHILE\x1b[m");    break;
        case STMT_KIND_DOWHILE:  fprintf(strm, "\x1b[31mSTMT_KIND_DOWHILE\x1b[m");  break;
        case STMT_KIND_FOR:      fprintf(strm, "\x1b[31mSTMT_KIND_FOR\x1b[m");      break;
        case STMT_KIND_BREAK:    fprintf(strm, "\x1b[31mSTMT_KIND_BREAK\x1b[m");    break;
        case STMT_KIND_CONTINUE: fprintf(strm, "\x1b[31mSTMT_KIND_CONTINUE\x1b[m"); break;
        case STMT_KIND_RETURN:   fprintf(strm, "\x1b[31mSTMT_KIND_RETURN\x1b[m");   break;
        case STMT_KIND_GOTO:     fprintf(strm, "\x1b[31mSTMT_KIND_GOTO\x1b[m");     break;
        }
        fprintf(strm, "; ");
        if (stmt->info.for_.ctrl) print_cxpr(strm, ls, stmt->info.for_.ctrl);
        fprintf(strm, "; ");
        if (stmt->info.for_.iter) print_cxpr(strm, ls, stmt->info.for_.iter);
        break;

    case STMT_KIND_BREAK:
        fprintf(strm, "%*s\x1b[34mbreak\x1b[m;\n", (depth+1)*4, "");
        break;

    case STMT_KIND_CONTINUE:
        fprintf(strm, "%*s\x1b[34mcontinue\x1b[m;\n", (depth+1)*4, "");
        break;

    case STMT_KIND_RETURN:
        fprintf(strm, "%*s\x1b[34mreturn\x1b[m", (depth+1)*4, "");
        if (stmt->info.return_) {
            fprintf(strm, " ");
            print_cxpr(strm, ls, stmt->info.return_);
        }
        fprintf(strm, ";");
        break;

    case STMT_KIND_GOTO:
        fprintf(strm, "%*s\x1b[34mgoto %s\x1b[m;\n", (depth+1)*4, "", tokn(stmt->info.goto_));
        break;
    }
}

void print_type(FILE ref strm, struct adpt_type cref ty, bool const top)
{
    if (!ty) {
        fprintf(strm, "\x1b[31m(nil)\x1b[m");
        return;
    }

    struct adpt_type cref tty = top ? _truetype(ty) : ty;
    switch (tty->tyty) {
    case ADPT_TYPE_VOID:   fprintf(strm, "\x1b[32mvoid\x1b[m");   break;
    case ADPT_TYPE_CHAR:   fprintf(strm, "\x1b[32mchar\x1b[m");   break;
    case ADPT_TYPE_UCHAR:  fprintf(strm, "\x1b[32muchar\x1b[m");  break;
    case ADPT_TYPE_SCHAR:  fprintf(strm, "\x1b[32mschar\x1b[m");  break;
    case ADPT_TYPE_SHORT:  fprintf(strm, "\x1b[32mshort\x1b[m");  break;
    case ADPT_TYPE_INT:    fprintf(strm, "\x1b[32mint\x1b[m");    break;
    case ADPT_TYPE_LONG:   fprintf(strm, "\x1b[32mlong\x1b[m");   break;
    case ADPT_TYPE_USHORT: fprintf(strm, "\x1b[32mushort\x1b[m"); break;
    case ADPT_TYPE_UINT:   fprintf(strm, "\x1b[32muint\x1b[m");   break;
    case ADPT_TYPE_ULONG:  fprintf(strm, "\x1b[32mulong\x1b[m");  break;
    case ADPT_TYPE_FLOAT:  fprintf(strm, "\x1b[32mfloat\x1b[m");  break;
    case ADPT_TYPE_DOUBLE: fprintf(strm, "\x1b[32mdouble\x1b[m"); break;

    case ADPT_TYPE_STRUCT: fprintf(strm, "\x1b[34mstruct\x1b[m"); if (0)
    case ADPT_TYPE_UNION:  fprintf(strm, "\x1b[34munion\x1b[m");
        if (tty->info.comp.named) {
            fprintf(strm, " %s", tty->info.comp.named);
            if (!top) break;
        }
        fprintf(strm, "{");
        for (size_t k = 0; k < tty->info.comp.count; k++) {
            struct adpt_comp_field const* it = tty->info.comp.fields+k;
            fprintf(strm, k ? ", %s@%zu: " : "%s@%zu: ", it->name, it->offset);
            print_type(strm, it->type, false);
        }
        fprintf(strm, "}");
        break;

    case ADPT_TYPE_FUN:
        fprintf(strm, "\x1b[34mfun\x1b[m(");
        for (size_t k = 0; k < tty->info.fun.count; k++) {
            struct adpt_fun_param const* it = tty->info.fun.params+k;
            fprintf(strm, k ? ", %s: " : "%s: ", it->name);
            print_type(strm, it->type, false);
        }
        fprintf(strm, ") -> ");
        print_type(strm, tty->info.fun.ret, false);
        break;

    case ADPT_TYPE_PTR:
        fprintf(strm, "\x1b[34mptr\x1b[m[");
        print_type(strm, tty->info.ptr, false);
        fprintf(strm, "]");
        break;

    case ADPT_TYPE_ARR:
        fprintf(strm, "\x1b[34marr\x1b[m[%zu, ", tty->info.arr.count);
        print_type(strm, tty->info.arr.item, false);
        fprintf(strm, "]");
        break;

    case ADPT_TYPE_NAMED:
        fprintf(strm, "\x1b[32m%s\x1b[m", tty->info.named.name);
        if (top) {
            fprintf(strm, " = ");
            print_type(strm, tty->info.named.def, false);
        }
        break;
    }
}

void print_code(FILE ref strm, bytecode const code)
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
        case 0x2a:
            fprintf(strm, "\x1b[34mdebug\x1b[m");
            imm(sze);
            fprintf(strm, "\tbytes:\x1b[33m\"%.*s\"\x1b[m", (unsigned)sze, code.ptr+k);
            k+= sze;
            break;

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
        size_t const tk = 7 < k-pk ? pk+7 : k;
        while (pk <= tk) fprintf(strm, " 0x%02x", code.ptr[pk++]);
        if (tk != k) fprintf(strm, "...");
        fprintf(strm, "\x1b[m\n");
#       undef imm
    }
}

void print_item(FILE ref strm, struct adpt_item cref it, char cref stack, unsigned const depth)
{
    if (!depth) {
        fprintf(strm, "%s: ", it->name);
        print_type(strm, it->type, false);
        fprintf(strm, "\n   = ");
    }

    if (ADPT_ITEM_TYPEDEF == it->kind) {
        fprintf(strm, "???");
        return;
    }
    if (ADPT_ITEM_VALUE == it->kind) {
        fprintf(strm, "%li", it->as.value);
        return;
    }

    void cref p = ADPT_ITEM_OBJECT == it->kind
        ? it->as.object
        : stack+it->as.variable;

    struct adpt_type cref tty = _truetype(it->type);
    switch (tty->tyty) {
    case ADPT_TYPE_VOID: fprintf(strm, "()"); break;

    case ADPT_TYPE_CHAR:
        fprintf(strm, "\x1b[33m");
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
        }
        fprintf(strm, "\x1b[m");
        break;

    case ADPT_TYPE_UCHAR:  fprintf(strm, "\x1b[33m0x%02hhx\x1b[m", *(unsigned char*)p);  break;
    case ADPT_TYPE_SCHAR:  fprintf(strm, "\x1b[33m%hhi\x1b[m",     *(signed char*)p);    break;
    case ADPT_TYPE_SHORT:  fprintf(strm, "\x1b[33m%hi\x1b[m",      *(short*)p);          break;
    case ADPT_TYPE_INT:    fprintf(strm, "\x1b[33m%i\x1b[m",       *(int*)p);            break;
    case ADPT_TYPE_LONG:   fprintf(strm, "\x1b[33m%li\x1b[m",      *(long*)p);           break;
    case ADPT_TYPE_USHORT: fprintf(strm, "\x1b[33m%hu\x1b[m",      *(unsigned short*)p); break;
    case ADPT_TYPE_UINT:   fprintf(strm, "\x1b[33m%u\x1b[m",       *(unsigned int*)p);   break;
    case ADPT_TYPE_ULONG:  fprintf(strm, "\x1b[33m%lu\x1b[m",      *(unsigned long*)p);  break;
    case ADPT_TYPE_FLOAT:  fprintf(strm, "\x1b[33m%f\x1b[m",       *(float*)p);          break;
    case ADPT_TYPE_DOUBLE: fprintf(strm, "\x1b[33m%lf\x1b[m",      *(double*)p);         break;

    case ADPT_TYPE_STRUCT: fprintf(strm, "\x1b[34mstruct\x1b[m"); if (0)
    case ADPT_TYPE_UNION:  fprintf(strm, "\x1b[34munion\x1b[m");
        fprintf(strm, " {\n");
        for (size_t k = 0; k < tty->info.comp.count; k++) {
            struct adpt_comp_field cref f = tty->info.comp.fields+k;
            fprintf(strm, "%*s.%s= ", (depth+1)*3, "", f->name);
            print_item(strm, &(struct adpt_item){
                    .type= f->type,
                    .kind= ADPT_ITEM_OBJECT,
                    .as.object= (char*)p+f->offset, // xxx: discards const
                }, stack, depth+1);
            fprintf(strm, "\n");
        }
        fprintf(strm, "%*s}", depth*3, "");
        break;

    case ADPT_TYPE_FUN: fprintf(strm, "\x1b[32m(%p)\x1b[m", p); break;

    case ADPT_TYPE_PTR:
        fprintf(strm, "\x1b[32m(%p)\x1b[m", p);
        //print_item(strm, &(struct adpt_item){
        //        .type= tty->info.ptr,
        //        .as.object= *(void**)p,
        //    }, stack, depth);
        break;

    case ADPT_TYPE_ARR:
        fprintf(strm, "[- x%zu -]", tty->info.arr.count);
        //fprintf(strm, "[\n");
        //for (size_t k = 0; k < tty->info.arr.count; k++) {
        //    fprintf(strm, "%*s[%zu]= ", (depth+1)*3, "", k);
        //    print_item(strm, &(struct adpt_item){
        //            .type= tty->info.arr.item,
        //            .as.object= (char*)p+k*tty->info.arr.item->size, // xxx: discards const
        //        }, stack, depth+1);
        //    fprintf(strm, "\n");
        //}
        //fprintf(strm, "%*s]", depth*3, "");
        break;

        // unreachable case
    case ADPT_TYPE_NAMED:;
    }

    if (!depth) fprintf(strm, "\n");
}

void print_tops(FILE ref strm, run_state cref rs, struct adpt_item cref items, size_t const count)
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
                for (n = 0; n < count; n++) if (ADPT_ITEM_VARIABLE == items[n].kind && items[n].as.variable <= at-1-k) {
                    struct adpt_type cref tty = _truetype(items[n].type);
                    if (at-1-k < items[n].as.variable+tty->size) {
                        in_var_size = tty->size - ((at-1-k)-items[n].as.variable);
                        break;
                    }
                }
                if (in_var_size) fprintf(strm, "\x1b[%um\x1b[4m", col_n(items[n].name));
            }

            fprintf(strm, "%02hhX", rs->stack[at-1-k]);

            if (in_var_size && !--in_var_size) fprintf(strm, "\x1b[m");
        }
        if (in_var_size) fprintf(strm, "\x1b[m"), in_var_size = 0;

        fprintf(strm, "  \x1b[1m|\x1b[m");
        for (int k = 15; k >= 0; k--)
            for (size_t n = 0; n < count; n++) if (ADPT_ITEM_VARIABLE == items[n].kind && items[n].as.variable == at-1-k)
                fprintf(strm, "  \x1b[%um%s\x1b[m(%zu)", col_n(items[n].name), items[n].name, _truetype(items[n].type)->size);

        fprintf(strm, "\n");
    }

#   undef col_n
}

void print_slot(FILE ref strm, struct slot cref slot)
{
    fprintf(strm, "slot ");
    print_type(strm, slot->ty, false);
    switch (slot->usage) {
    case _slot_value:
        fprintf(strm, " \x1b[36mvalue\x1b[m = ");
        switch (slot->ty->tyty) {
        case ADPT_TYPE_CHAR:   fprintf(strm, "'%c'",     slot->as.value.c);  break;
        case ADPT_TYPE_UCHAR:  fprintf(strm, "0x%02hhx", slot->as.value.uc); break;
        case ADPT_TYPE_SCHAR:  fprintf(strm, "%hhi",     slot->as.value.sc); break;
        case ADPT_TYPE_SHORT:  fprintf(strm, "%hi",      slot->as.value.ss); break;
        case ADPT_TYPE_INT:    fprintf(strm, "%i",       slot->as.value.si); break;
        case ADPT_TYPE_LONG:   fprintf(strm, "%li",      slot->as.value.sl); break;
        case ADPT_TYPE_USHORT: fprintf(strm, "%hu",      slot->as.value.us); break;
        case ADPT_TYPE_UINT:   fprintf(strm, "%u",       slot->as.value.ui); break;
        case ADPT_TYPE_ULONG:  fprintf(strm, "%lu",      slot->as.value.ul); break;
        case ADPT_TYPE_FLOAT:  fprintf(strm, "%f",       slot->as.value.f);  break;
        case ADPT_TYPE_DOUBLE: fprintf(strm, "%lf",      slot->as.value.d);  break;
        case ADPT_TYPE_FUN:
        case ADPT_TYPE_PTR:
        case ADPT_TYPE_ARR:    fprintf(strm, "%p",       slot->as.value.p);  break;

            // unreachable cases
        case ADPT_TYPE_VOID: case ADPT_TYPE_STRUCT: case ADPT_TYPE_UNION: case ADPT_TYPE_NAMED:;
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

void print_lexr(FILE ref strm, struct lex_state cref ls)
{
    fprintf(strm, "ls= {\n");

    fprintf(strm, "    macros= [\n");
    for (size_t k = 0; k < ls->macros.len; ++k) {
        struct lex_macro const* const it = ls->macros.ptr+k;
        fprintf(strm, "        {   name= \"%s\"\n", ls->work.ptr+it->name);
        fprintf(strm, "            value= ");
        for (size_t kk = 0; kk < it->length; kk+= strlen(ls->work.ptr+it->value+kk)+1)
            fprintf(strm, "%s@", ls->work.ptr+it->value+kk);
        fprintf(strm, "\n");
        fprintf(strm, "            length= %zu\n", it->length);
        fprintf(strm, "            params= %d\n", it->params);
        fprintf(strm, "            marked= %s\n", it->marked ? "true" : "false");
        fprintf(strm, "        }\n");
    }
    fprintf(strm, "    ]\n");

    fprintf(strm, "    paths= [\n");
    for (size_t k = 0; k < ls->paths.len; ++k)
        fprintf(strm, "        %s\n", ls->paths.ptr[k]);
    fprintf(strm, "    ]\n");

    fprintf(strm, "    sources= [\n");
    for (size_t k = 0; k < ls->sources.len; ++k)
        fprintf(strm, "        %s:%zu (%s)\n", ls->work.ptr+ls->sources.ptr[k].file, ls->sources.ptr[k].line, ls->sources.ptr[k].stream ? "open" : "close");
    fprintf(strm, "    ]\n");
    fprintf(strm, "    gotc= 0x%02hhX '%c'\n", ls->gotc, ' ' <= ls->gotc && ls->gotc <= '~' ? ls->gotc : ' ');

    fprintf(strm, "    nlend= %s\n", ls->nlend ? "true" : "false");
    fprintf(strm, "    noxid= %s\n", ls->noxid ? "true" : "false");
    fprintf(strm, "    nomrg= %s\n", ls->nomrg ? "true" : "false");
    fprintf(strm, "    nodir= %s\n", ls->nodir ? "true" : "false");
    fprintf(strm, "    dumgc= %s\n", ls->dumgc ? "true" : "false");
    fprintf(strm, "    cstream= %s\n", ls->cstream ? ls->cstream : "(null)");

    fprintf(strm, "    work= ");
    for (size_t k = 0; k < ls->work.len; k+= strlen(ls->work.ptr+k)+1)
        fprintf(strm, "%s@", ls->work.ptr+k);
    fprintf(strm, "\n");

    fprintf(strm, "    ahead= %zu\n", ls->ahead);
    fprintf(strm, "    tokens= ");
    for (size_t k = 0; k < ls->tokens.len; k+= strlen(ls->tokens.ptr+k)+1)
        fprintf(strm, "%s@", ls->tokens.ptr+k);
    fprintf(strm, "\n            %*s^\n", (unsigned)(ls->tokens.len - ls->ahead), "");

    fprintf(strm, "}\n");
}

#endif // CINTRE_PRINTS_H
