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

char stack[STACK_SIZE];
size_t sp = sizeof stack;
#define top() (stack+sp)
#define topas(ty) ((ty*)top())
#define stalloc(size, align, count) (&stack[sp = (sp-size*count)/align*align])

void show(void ref _, expression cref expr, bufsl ref tok) {
    (void)_;

    char const* xcmd = "";
    if (tok->len && ';' == *tok->ptr)
        xcmd = tok->ptr+1;
    fprintf(stderr, " -- xcmd: '%s'\n", xcmd);

    //alloc_pass(expr);
    //exec_pass(expr);
}

int main(void) {
    lex_state ls = {.file= "<input>"};

    char* line = NULL;
    while (prompt("(*^^),u~~ ", &line)) {
        ls.line++;
        ls.slice.len = strlen(ls.slice.ptr = line);

        parse_expr_state ps = {.ls= &ls, .on= show};
        parse_expression(&ps, lext(ps.ls));
        puts("");
    }

    return EXIT_SUCCESS;
}
