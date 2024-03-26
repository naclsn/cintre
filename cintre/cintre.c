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
    add_history(*res);
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

char stack[STACK_SIZE];
size_t sp = sizeof stack;
#define top(ty) ((ty*)(stack+sp))
//#define stalloc(size, align, count) (&stack[sp = (sp-size*count)/align*align])
bytecode code = {0};

void run(bytecode const code) {
    for (size_t k = 0; k < code.len; k++) {
        unsigned char c = code.ptr[k]; // ........
        unsigned w = c&3;              // ......[]
        unsigned x = c>>2&3;           // ....[]..
        enum _bc_op_un u = c>>2&7;     // ...[-]..
        enum _bc_op_bin b = c>>2&15;   // ..[--]..
#       define rsxc()        \
            size_t sxc = 0;  \
            for (unsigned xx = 1<<x; xx; sxc = sxc | (code.ptr[++k]<<(--xx*8)))

        if (1 == c) notif("NIY: debug (probably just show 32b stack for now)");

        else if (c < 32) {
            bool i = c>>4&1;
            rsxc();
            sp-= sxc; // alloc sxc
            sp&= ~(size_t)0<<w; // align to w
            if (i) {
                memcpy(top(char), code.ptr+k+1, sxc);
                k+= sxc;
            }
        }

        else if (c < 64) {
            bool f = c>>4&1;
            if (x == w) {
                rsxc();
                if (f) notif("NIY copy"); //memmove(,, sxc); // copy
                else sp+= sxc;
            } else if (!(f && (x < 2 || w < 2))) {
                notif("NIY: cvt x to w");
            }
        }

        else if (c < 128) {
            bool f = c>>5&1;
            if (f && 0 == w) {
                rsxc();
                //if (c>>4&1) k+= (signed)sxc;
                //else { .. }
                notif("NIY: %s by sxc", c>>4&1 ? "jmp" : "brz");
            } else if (!(f && w < 2)) {
                // yyy: w
                switch (u) {
                case BC_UNOP_BNOT:  top(unsigned)[0] =  ~top(unsigned)[0]; break;
                case BC_UNOP_LNOT:  top(unsigned)[0] =  !top(unsigned)[0]; break;
                case BC_UNOP_MINUS: top(unsigned)[0] =  -top(unsigned)[0]; break;
                case BC_UNOP_BANYS: top(unsigned)[0] = !!top(unsigned)[0]; break;
                case BC_UNOP_DEC:   top(unsigned)[0]--;               break;
                case BC_UNOP_INC:   top(unsigned)[0]++;               break;
                }
            }
        }

        else {
            bool f = c>>6&1;
            if (f && 0 == w) {
                notif("NIY: call");
            } else if (!(f && w < 2)) {
                // yyy: w
                switch (b) {
                case BC_BINOP_EQ:   top(unsigned)[1] = top(unsigned)[1] == top(unsigned)[0]; break;
                case BC_BINOP_NE:   top(unsigned)[1] = top(unsigned)[1] != top(unsigned)[0]; break;
                case BC_BINOP_LT:   top(unsigned)[1] = top(unsigned)[1] <  top(unsigned)[0]; break;
                case BC_BINOP_GT:   top(unsigned)[1] = top(unsigned)[1] >  top(unsigned)[0]; break;
                case BC_BINOP_LE:   top(unsigned)[1] = top(unsigned)[1] <= top(unsigned)[0]; break;
                case BC_BINOP_GE:   top(unsigned)[1] = top(unsigned)[1] >= top(unsigned)[0]; break;
                case BC_BINOP_BOR:  top(unsigned)[1] = top(unsigned)[1] |  top(unsigned)[0]; break;
                case BC_BINOP_BXOR: top(unsigned)[1] = top(unsigned)[1] ^  top(unsigned)[0]; break;
                case BC_BINOP_BAND: top(unsigned)[1] = top(unsigned)[1] &  top(unsigned)[0]; break;
                case BC_BINOP_BSHL: top(unsigned)[1] = top(unsigned)[1] << top(unsigned)[0]; break;
                case BC_BINOP_BSHR: top(unsigned)[1] = top(unsigned)[1] >> top(unsigned)[0]; break;
                case BC_BINOP_SUB:  top(unsigned)[1] = top(unsigned)[1] -  top(unsigned)[0]; break;
                case BC_BINOP_ADD:  top(unsigned)[1] = top(unsigned)[1] +  top(unsigned)[0]; break;
                case BC_BINOP_REM:  top(unsigned)[1] = top(unsigned)[1] %  top(unsigned)[0]; break;
                case BC_BINOP_DIV:  top(unsigned)[1] = top(unsigned)[1] /  top(unsigned)[0]; break;
                case BC_BINOP_MUL:  top(unsigned)[1] = top(unsigned)[1] *  top(unsigned)[0]; break;
                }
                sp+= 4;
            }
        }
#       undef rsxc
    }
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

    if (xcmdis("ast")) {
        printf("AST of the expression:\n");
        print_expr(stdout, expr, 0);
        return;
    }

    code.len = 0;
    if (!expr) return;
    struct adpt_type const* ty;
    compile_expression(&code, expr, &ty, lookup, typehole);
    if (!ty) return;

    if (xcmdis("ty")) {
        printf("expression is of type: ");
        print_type(stdout, ty);
        printf("\n");
        return;
    }

    if (xcmdis("bytec") || xcmdis("bc")) {
        printf("resulting bytecode (%zuB):\n", code.len);
        print_code(stdout, code);
        return;
    }

    run(code);
    printf("result: %u\n", top(unsigned)[0]); // yyy: type
}

int main(void) {
    printf("namespaces:\n");
    for (size_t k = 0; k < countof(namespaces); k++)
        printf("   %s (%zu items)\n", namespaces[k].name, namespaces[k].count);
    printf("(%zu total)\n\n", countof(namespaces));

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
