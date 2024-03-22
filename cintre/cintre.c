#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "parser.h"

#ifndef STACK_SIZE
#define STACK_SIZE 1024*1024
#endif

#ifdef USE_READLINE
#include <readline/readline.h>
#include <readline/history.h>
bool prompt(char const* prompt, char** res) {
    free(*res);
    if (!(*res = readline(prompt))) return false;
    add_history(*res);
    return true;
}
#else
bool prompt(char const* prompt, char** res) {
    static char r[1024];
    *res = r;
    printf("%s", prompt);
    if (!fgets(r, sizeof r, stdin)) return false;
    r[strlen(r)-1] = '\0';
    return true;
}
#endif

/*
#define search_namespace(__n, __ns)          \
    for (size_t k = 0; (__ns)[k].name; k++)  \
        if (!strcmp((__ns)[k].name, (__n)))
*/

//char stack[STACK_SIZE];
//size_t sp = sizeof stack;
//#define top() (stack+sp)
//#define topas(ty) ((ty*)top())
//#define stalloc(size, align, count) (&stack[sp = (sp-size*count)/align*align])

void print_expr(FILE* strm, expression cref expr) {
    fprintf(strm, "(");
    switch (expr->kind) {
    case ATOM: fprintf(strm, "%.*s", (unsigned)expr->info._tok.len, expr->info._tok.ptr); break;
    case BINOP_COMMA:      print_expr(strm, expr->info._opr[0]); fprintf(strm, ", "); print_expr(strm, expr->info._opr[1]); break;
    case BINOP_ASGN:       print_expr(strm, expr->info._opr[0]); fprintf(strm, " = "); print_expr(strm, expr->info._opr[1]); break;
    case BINOP_ASGN_SUB:   print_expr(strm, expr->info._opr[0]); fprintf(strm, "-= "); print_expr(strm, expr->info._opr[1]); break;
    case BINOP_ASGN_ADD:   print_expr(strm, expr->info._opr[0]); fprintf(strm, "+= "); print_expr(strm, expr->info._opr[1]); break;
    case BINOP_ASGN_REM:   print_expr(strm, expr->info._opr[0]); fprintf(strm, "%%= "); print_expr(strm, expr->info._opr[1]); break;
    case BINOP_ASGN_DIV:   print_expr(strm, expr->info._opr[0]); fprintf(strm, "/= "); print_expr(strm, expr->info._opr[1]); break;
    case BINOP_ASGN_MUL:   print_expr(strm, expr->info._opr[0]); fprintf(strm, "*= "); print_expr(strm, expr->info._opr[1]); break;
    case BINOP_ASGN_BOR:   print_expr(strm, expr->info._opr[0]); fprintf(strm, "|= "); print_expr(strm, expr->info._opr[1]); break;
    case BINOP_ASGN_BXOR:  print_expr(strm, expr->info._opr[0]); fprintf(strm, "^= "); print_expr(strm, expr->info._opr[1]); break;
    case BINOP_ASGN_BAND:  print_expr(strm, expr->info._opr[0]); fprintf(strm, "&= "); print_expr(strm, expr->info._opr[1]); break;
    case BINOP_ASGN_BSHL:  print_expr(strm, expr->info._opr[0]); fprintf(strm, "<<= "); print_expr(strm, expr->info._opr[1]); break;
    case BINOP_ASGN_BSHR:  print_expr(strm, expr->info._opr[0]); fprintf(strm, ">>= "); print_expr(strm, expr->info._opr[1]); break;
    case BINOP_TERNBRANCH: print_expr(strm, expr->info._opr[0]); fprintf(strm, " : "); print_expr(strm, expr->info._opr[1]); break;
    case BINOP_TERNCOND:   print_expr(strm, expr->info._opr[0]); fprintf(strm, " ? "); print_expr(strm, expr->info._opr[1]); break;
    case BINOP_LOR:        print_expr(strm, expr->info._opr[0]); fprintf(strm, " || "); print_expr(strm, expr->info._opr[1]); break;
    case BINOP_LAND:       print_expr(strm, expr->info._opr[0]); fprintf(strm, " && "); print_expr(strm, expr->info._opr[1]); break;
    case BINOP_BOR:        print_expr(strm, expr->info._opr[0]); fprintf(strm, "|"); print_expr(strm, expr->info._opr[1]); break;
    case BINOP_BXOR:       print_expr(strm, expr->info._opr[0]); fprintf(strm, "^"); print_expr(strm, expr->info._opr[1]); break;
    case BINOP_BAND:       print_expr(strm, expr->info._opr[0]); fprintf(strm, "&"); print_expr(strm, expr->info._opr[1]); break;
    case BINOP_EQ:         print_expr(strm, expr->info._opr[0]); fprintf(strm, " == "); print_expr(strm, expr->info._opr[1]); break;
    case BINOP_NE:         print_expr(strm, expr->info._opr[0]); fprintf(strm, " != "); print_expr(strm, expr->info._opr[1]); break;
    case BINOP_LT:         print_expr(strm, expr->info._opr[0]); fprintf(strm, " < "); print_expr(strm, expr->info._opr[1]); break;
    case BINOP_GT:         print_expr(strm, expr->info._opr[0]); fprintf(strm, " > "); print_expr(strm, expr->info._opr[1]); break;
    case BINOP_LE:         print_expr(strm, expr->info._opr[0]); fprintf(strm, " <= "); print_expr(strm, expr->info._opr[1]); break;
    case BINOP_GE:         print_expr(strm, expr->info._opr[0]); fprintf(strm, " >= "); print_expr(strm, expr->info._opr[1]); break;
    case BINOP_BSHL:       print_expr(strm, expr->info._opr[0]); fprintf(strm, "<<"); print_expr(strm, expr->info._opr[1]); break;
    case BINOP_BSHR:       print_expr(strm, expr->info._opr[0]); fprintf(strm, ">>"); print_expr(strm, expr->info._opr[1]); break;
    case BINOP_SUB:        print_expr(strm, expr->info._opr[0]); fprintf(strm, "-"); print_expr(strm, expr->info._opr[1]); break;
    case BINOP_ADD:        print_expr(strm, expr->info._opr[0]); fprintf(strm, "+"); print_expr(strm, expr->info._opr[1]); break;
    case BINOP_REM:        print_expr(strm, expr->info._opr[0]); fprintf(strm, "%%"); print_expr(strm, expr->info._opr[1]); break;
    case BINOP_DIV:        print_expr(strm, expr->info._opr[0]); fprintf(strm, "/"); print_expr(strm, expr->info._opr[1]); break;
    case BINOP_MUL:        print_expr(strm, expr->info._opr[0]); fprintf(strm, "*"); print_expr(strm, expr->info._opr[1]); break;
    case UNOP_ADDR:        fprintf(strm, "&"); print_expr(strm, expr->info._opr[0]); break;
    case UNOP_DEREF:       fprintf(strm, "*"); print_expr(strm, expr->info._opr[0]); break;
    case UNOP_BNOT:        fprintf(strm, "~"); print_expr(strm, expr->info._opr[0]); break;
    case UNOP_LNOT:        fprintf(strm, "!"); print_expr(strm, expr->info._opr[0]); break;
    case UNOP_MINUS:       fprintf(strm, "-"); print_expr(strm, expr->info._opr[0]); break;
    case UNOP_PLUS:        fprintf(strm, "+"); print_expr(strm, expr->info._opr[0]); break;
    case UNOP_PRE_DEC:     fprintf(strm, "--"); print_expr(strm, expr->info._opr[0]); break;
    case UNOP_PRE_INC:     fprintf(strm, "++"); print_expr(strm, expr->info._opr[0]); break;
    case BINOP_SUBSCR:     print_expr(strm, expr->info._opr[0]); fprintf(strm, " []{ "); print_expr(strm, expr->info._opr[1]); fprintf(strm, " }"); break;
    case BINOP_CALL:       print_expr(strm, expr->info._opr[0]); fprintf(strm, " (){ "); if (expr->info.call.args) print_expr(strm, expr->info._opr[1]); fprintf(strm, " }"); break;
    case UNOP_PMEMBER:     print_expr(strm, expr->info.member.base); fprintf(strm, "->%.*s", bufmt((*expr->info.member.name))); break;
    case UNOP_MEMBER:      print_expr(strm, expr->info.member.base); fprintf(strm, ".%.*s", bufmt((*expr->info.member.name))); break;
    case UNOP_POST_DEC:    print_expr(strm, expr->info._opr[0]); fprintf(strm, "--"); break;
    case UNOP_POST_INC:    print_expr(strm, expr->info._opr[0]); fprintf(strm, "++"); break;
    }
    fprintf(strm, ")");
}

void show(void ref _, expression cref expr, bufsl ref tok) {
    (void)_;
    print_expr(stdout, expr);
    printf(" -- tok: '%.*s'\n", (unsigned)tok->len, tok->ptr);
}

int main(void) {
    lex_state ls = {.file= "<input>"};

    static char const* const ps[] = {
        "          _D ",
        "(*^^),u~~ _D ",
        "   (/o.o)/_D ",
        "   (( p.o)_D ",
        "(<.< )-~  _D ",
        " (#>A<)=B _D ",
        "  (o~n`)  _D ",
        "  (\"o3o)& _D ",
        " (~o.o)~  _D ",
        " (q.q )   _D ",
        "   ( .-.) _D ",
        " (x^x)    _D ",
        "  (-#.#)\\ _D ",
        "  (@.@ )  _D ",
        "    __``  _D ",
        "  (*-*)   _D ",
    };
    //srand(time(NULL));

    char* line = NULL;
    while (prompt("(*^^),u~~ " /*ps[rand()%countof(ps)]*/, &line)) {
        ls.line++;
        ls.slice.len = strlen(ls.slice.ptr = line);

        parse_expr_state ps = {.ls= &ls, .on= show};
        parse_expression(&ps, lext(ps.ls));
        puts("");
    }

    return EXIT_SUCCESS;
}
