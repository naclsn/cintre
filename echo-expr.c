#include "cintre/prints.h"
#include "cintre/parser.h"
#include "cintre/lexer.h"
#include <readline/readline.h>

void accept(void ref usr, expression ref expr, tokt ref tok)
{
    (void)tok;
    printf("->  ");
    print_expr(stdout, usr, expr, false);
    printf(";\n");
}

int main(void)
{
    parse_expr_state ps = {
        .ls= &(lex_state){0},
        .on= accept,
        .usr= ps.ls,
    };
    lex_entry(ps.ls, NULL, "<input>");
    for (char* l; (l = readline("?= ")); free(l)) {
        ps.ls->cstream = l;
        parse_expression(&ps, lext(ps.ls));
    }
}
