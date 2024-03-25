#include "common.h"
#include "parser.h"
#include "adapter.h"
#include "compiler.h"

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

// print helpers {{{
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
    case ADPT_KIND_VOID:       fprintf(strm, "\x1b[32mvoid\x1b[m");       break;
    case ADPT_KIND_CHAR:       fprintf(strm, "\x1b[32mchar\x1b[m");       break;
    case ADPT_KIND_UCHAR:      fprintf(strm, "\x1b[32muchar\x1b[m");      break;
    case ADPT_KIND_SCHAR:      fprintf(strm, "\x1b[32mschar\x1b[m");      break;
    case ADPT_KIND_SHORT:      fprintf(strm, "\x1b[32mshort\x1b[m");      break;
    case ADPT_KIND_INT:        fprintf(strm, "\x1b[32mint\x1b[m");        break;
    case ADPT_KIND_LONG:       fprintf(strm, "\x1b[32mlong\x1b[m");       break;
    case ADPT_KIND_LONGLONG:   fprintf(strm, "\x1b[32mlonglong\x1b[m");   break;
    case ADPT_KIND_USHORT:     fprintf(strm, "\x1b[32mushort\x1b[m");     break;
    case ADPT_KIND_UINT:       fprintf(strm, "\x1b[32muint\x1b[m");       break;
    case ADPT_KIND_ULONG:      fprintf(strm, "\x1b[32mulong\x1b[m");      break;
    case ADPT_KIND_ULONGLONG:  fprintf(strm, "\x1b[32mulonglong\x1b[m");  break;
    case ADPT_KIND_ENUM:       fprintf(strm, "\x1b[32menum\x1b[m");       break;
    case ADPT_KIND_FLOAT:      fprintf(strm, "\x1b[32mfloat\x1b[m");      break;
    case ADPT_KIND_DOUBLE:     fprintf(strm, "\x1b[32mdouble\x1b[m");     break;
    case ADPT_KIND_LONGDOUBLE: fprintf(strm, "\x1b[32mlongdouble\x1b[m"); break;

    case ADPT_KIND_STRUCT:
        fprintf(strm, "\x1b[34mstruct\x1b[m{");
        if (0)
    case ADPT_KIND_UNION:
            fprintf(strm, "\x1b[34munion\x1b[m{");
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
        unsigned char c = code.ptr[k]; // ........
        unsigned w = c&3;              // ......[]
        unsigned x = c>>2&3;           // ....[]..
        unsigned u = c>>2&7;           // ...[-]..
        unsigned b = c>>2&15;          // ..[--]..
#       define rsxc() \
            size_t sxc = 0;  \
            unsigned xx = 1<<x;  \
            while (xx) sxc = sxc | (code.ptr[++k]<<(--xx*8));

        if (0 == c) fprintf(strm, "\x1b[34mnop\x1b[m");
        else if (1 == c) fprintf(strm, "\x1b[34mdebug\x1b[m");
        else if (2 == c) fprintf(strm, "\x1b[34m(exit0)\x1b[m"); // yyy
        else if (3 == c) fprintf(strm, "\x1b[34m(exit1)\x1b[m"); // yyy

        else if (c < 32) {
            bool i = c>>4&1;
            rsxc();
            fprintf(strm, "\x1b[34mpush%s%u\x1b[m \x1b[33m%zu\x1b[m", i?"i":"", (1<<2)*8, sxc);
            if (i) while (sxc) fprintf(strm, ", \x1b[33m0x%02x\x1b[m", code.ptr[sxc--, ++k]);
        }

        else if (c < 64) {
            bool f = c>>4&1;
            if (x == w) {
                rsxc();
                fprintf(strm, f ? "\x1b[34mcopy\x1b[m \x1b[33m%zu\x1b[m" : "\x1b[34mpop\x1b[m \x1b[33m%zu\x1b[m", sxc);
            } else if (!(f && (x < 2 || w < 2))) {
                fprintf(strm, "\x1b[34m%s%uto%u\x1b[m", f?"f":"", (1<<x)*8, (1<<w)*8);
            }
        }

        else if (c < 128) {
            static char const* const names[] = {"bnot", "lnot", "minus", "banys", "dec", "inc", "xun", "yun"};
            bool f = c>>5&1;
            if (f && 0 == w) {
                rsxc();
                fprintf(strm, c>>4&1 ? "\x1b[34mjmp\x1b[m \x1b[33m%zu\x1b[m" : "\x1b[34mbrz\x1b[m \x1b[33m%zu\x1b[m", sxc);
            } else if (!(f && w < 2)) {
                fprintf(strm, "\x1b[34m%s%s%u\x1b[m", f?"f":"", names[u], (1<<w)*8);
            }
        }

        else {
            static char const* const names[] = {"eq", "ne", "lt", "gt", "le", "ge", "bor", "bxor", "band", "bshl", "bshr", "sub", "add", "rem", "div", "mul"};
            bool f = c>>6&1;
            if (f && 0 == w) { // call
                fprintf(strm, "\x1b[34mcall%u\x1b[m", b);
            } else if (!(f && w < 2)) {
                fprintf(strm, "\x1b[34m%s%s%u\x1b[m", f?"f":"", names[b], (1<<w)*8);
            }
        }

        //else fprintf(strm, "\x1b[31merrop\x1b[m"); // yyy: everywhere else

        fprintf(strm, "\t\x1b[32m;");
        while (pk <= k) fprintf(strm, " 0x%02x", code.ptr[pk++]);
        fprintf(strm, "\x1b[m\n");
    }
}
// }}}

char stack[STACK_SIZE];
size_t sp = sizeof stack;
#define top() (stack+sp)
#define topas(ty) ((ty*)top())
#define stalloc(size, align, count) (&stack[sp = (sp-size*count)/align*align])
bytecode code = {0};

struct adpt_item const* lookup(bufsl const name) {
    return search_namespaces(name, NULL);
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
    struct adpt_type cref ty = compile_expression(&code, expr, lookup);
    if (!ty) return;

    if (xcmdis("ty")) {
        printf("expression is of type: ");
        print_type(stdout, ty);
        printf("\n");
        return;
    }

    if (xcmdis("bytec") || xcmdis("bc")) {
        printf("resulting bytecode (%zub):\n", code.len);
        print_code(stdout, code);
        return;
    }

    notif("NIY: run code");
    //run(&code);
}

int main(void) {
    printf("namespaces:\n");
    for (size_t k = 0; k < countof(namespaces); k++)
        printf("   %s (%zu items)\n", namespaces[k].name, namespaces[k].count);
    printf("(%zu total)\n\n", countof(namespaces));

    lex_state ls = {.file= "<input>"};
    parse_expr_state ps = {.ls= &ls, .on= accept};

    char* line = NULL;
    while (prompt("\x1b[35m(*^^),u~~\x1b[m ", &line)) {
        ls.line++;
        ls.slice.len = strlen(ls.slice.ptr = line);

        bufsl tok = lext(&ls);
        if (!tok.len || ';' == *tok.ptr) accept(NULL, NULL, &tok);
        else parse_expression(&ps, tok);
    }

    return EXIT_SUCCESS;
}
