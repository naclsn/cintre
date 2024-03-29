#include "common.h"
#include "parser.h"
#include "adapter.h"
#include "compiler.h"
#include "prints.h"

#ifndef STACK_SIZE
#define STACK_SIZE 1024*1024
#endif

#ifdef USE_READLINE
#include <readline/readline.h>
#include <readline/history.h>
#include <signal.h>
#define hist_file ".ignore/history"
void _prompt_cc(int sigint) {
    rl_crlf();
    rl_end_of_history(0, 0);
    rl_beg_of_line(0, 0);
    rl_kill_line(0, 0);
    rl_forced_update_display();
    signal(sigint, _prompt_cc);
}
bool prompt(char const* prompt, char** res) {
    if (!*res) {
        read_history(hist_file);
        signal(SIGINT, _prompt_cc);
    }
    free(*res);
    if (!(*res = readline(prompt))) {
        write_history(hist_file);
        clear_history();
        return false;
    }
    if (strspn(*res, " \t\n") != strlen(*res)) {
        HIST_ENTRY* h = history_get(history_length);
        if (!h || strcmp(h->line, *res)) add_history(*res);
    }
    return true;
}
#undef hist_file
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

#ifndef CINTRE_NAMESPACES_DEFINED
static struct {
    char const* const name;
    size_t const count;
    struct adpt_item const* const items;
} const namespaces[1] = {
    {.name= "(placeholder)", .count= 0, .items= NULL},
};
#endif

struct adpt_item const* search_namespaces(bufsl const name, size_t ref out_ns) {
    for (size_t ns = 0; ns < countof(namespaces); ns++)
        for (size_t k = 0; k < namespaces[ns].count; k++) {
            struct adpt_item const* const it = &namespaces[ns].items[k];
            if (!memcmp(it->name, name.ptr, name.len)) {
                if (out_ns) *out_ns = ns;
                return it;
            }
        }
    return NULL;
}

char stack[STACK_SIZE] = {0};
size_t sp = sizeof stack;
#define top(ty) ((ty*)(stack+sp))
//#define stalloc(size, align, count) (&stack[sp = (sp-size*count)/align*align])
bytecode code = {0};

void run(bytecode const code) {
    notif("NIY: run");
    //for (size_t k = 0; k < code.len; k++) {
    //    unsigned char c = code.ptr[k];
    //}
}

struct adpt_item const* lookup(bufsl const name) {
    return search_namespaces(name, NULL);
}

void typehole(struct adpt_type cref expect) {
    printf("type hold found, expecting: ");
    print_type(stdout, expect);
    printf("\n");
}

void accept(void ref _, expression ref expr, bufsl ref tok) {
    (void)_;

    char const* const xcmd = tok->len && ';' == *tok->ptr ? tok->ptr+1+strspn(tok->ptr+1, " \t\n") : "";
#   define xcmdis(s)  (!memcmp(s, xcmd, strlen(s)))

    if (xcmdis("h")) {
        printf("List of commands:\n");
        printf("   names[paces] or ns  -  list names in namespaces\n");
        printf("   ast  -  ast of the expression\n");
        printf("   ty[pe]  -  type of the expression, eg. `strlen; ty`\n");
        printf("   bytec[ode] or bc  -  internal bytecode from compilation\n");
        printf("no command after the ; (or no ;) will simply execute the expression\n");
        return;
    }

    if (xcmdis("names") || xcmdis("ns")) {
        printf("List of names:\n");
        for (size_t ns = 0; ns < countof(namespaces); ns++)
            for (size_t k = 0; k < namespaces[ns].count; k++) {
                struct adpt_item const* const it = &namespaces[ns].items[k];
                printf("   [%p] %s::%-8s\t", it->as.object, namespaces[ns].name, it->name);
                print_type(stdout, it->type);
                printf("\n");
            }
        return;
    }

    if (xcmdis("ast")) {
        printf("AST of the expression:\n");
        print_expr(stdout, expr, 0);
        return;
    }

    code.len = 0;
    if (!expr) return;
    compile_state cs = {.res= code, .lookup= lookup, .typehole= typehole};

    if (xcmdis("ty")) {
        struct adpt_type const* ty = check_expression(&cs, expr);
        if (!ty) return;
        printf("Expression is of type: ");
        print_type(stdout, ty);
        printf("\n");
        return;
    }

    bool r = compile_expression_tmp_wrap(&cs, expr);
    code = cs.res;
    if (!r) return;

    if (xcmdis("bytec") || xcmdis("bc")) {
        printf("Resulting bytecode (%zuB):\n", code.len);
        print_code(stdout, code);
        return;
    }

    run(code);
    printf("Result: %u\n", top(unsigned)[0]); // yyy: type
}

int main(void) {
    printf("Type `;help` for a list of command\n");

    lex_state ls = {.file= "<input>"};
    parse_expr_state ps = {.ls= &ls, .on= accept};

    char* line = NULL;
    while (prompt("\1\x1b[35m\2(*^^),u~~\1\x1b[m\2 ", &line)) {
        ls.line++;
        ls.slice.len = strlen(ls.slice.ptr = line);

        bufsl tok = lext(&ls);
        if (!tok.len || ';' == *tok.ptr) accept(NULL, NULL, &tok);
        else parse_expression(&ps, tok);
    }

    return EXIT_SUCCESS;
}
