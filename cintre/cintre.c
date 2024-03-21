#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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

#define search_namespace(__n, __ns)          \
    for (size_t k = 0; (__ns)[k].name; k++)  \
        if (!strcmp((__ns)[k].name, (__n)))

char stack[STACK_SIZE];
size_t sp = sizeof stack;
#define top() (stack+sp)
#define topas(ty) ((ty*)top())
#define stalloc(size, align, count) (&stack[sp = (sp-size*count)/align*align])

int main(void) {
    char* line = NULL;
    while (prompt(":3 ", &line)) {
        printf("echo: %s\n", line);
    }
    return EXIT_SUCCESS;
}
