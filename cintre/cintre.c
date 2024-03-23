#include "common.h"
#include "parser.h"
#include "adapter.h"

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

void print_type(FILE* strm, struct adpt_type cref ty) {
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

char stack[STACK_SIZE];
size_t sp = sizeof stack;
#define top() (stack+sp)
#define topas(ty) ((ty*)top())
#define stalloc(size, align, count) (&stack[sp = (sp-size*count)/align*align])

// check and alloc pass {{{
struct adpt_type const* typeof(expression ref expr) {
#   define fail(...)  return notif(__VA_ARGS__), NULL
#   define failforward(id, from)  for (id = typeof(from); !id; ) fail("here")
#   define isint(__ty)  (ADPT_KIND_CHAR <= (__ty)->kind && (__ty)->kind <= ADPT_KIND_ENUM)
#   define isflt(__ty)  (ADPT_KIND_FLOAT <= (__ty)->kind && (__ty)->kind <= ADPT_KIND_LONGDOUBLE)
#   define isnum(__ty)  (isint(__ty) || isflt(__ty))
#   define isptr(__ty)  (ADPT_KIND_PTR == (__ty)->kind)
#   define isfun(__ty)  (ADPT_KIND_FUN == (__ty)->kind)

    struct adpt_type const *opr, *lhs, *rhs, *base, *off;

    switch (expr->kind) {
    case ATOM:;
        char c = *expr->info.atom.ptr;
        if ('"' == c) {
            static struct adpt_type string = {
                .size= sizeof(void*),
                .align= sizeof(void*),
                .kind= ADPT_KIND_PTR,
                .info.to= &adptb_char_type,
            };
            return &string;
        }
        if ('\'' == c) return &adptb_char_type;
        if ('0' <= c && c <= '9') return &adptb_int_type; // yyy
        struct adpt_item const* found = search_namespaces(expr->info.atom, NULL);
        if (!found) fail("Unknown name: '%.*s'", bufmt(expr->info.atom));
        return found->type;

    case BINOP_SUBSCR:
        failforward(base, expr->info.subscr.base);
        failforward(off, expr->info.subscr.off);
        if (!isptr(base)) fail("Base of subscript expression is not of a pointer type");
        if (!isint(off)) fail("Offset of subscript expression is not of an integral type");
        return base->info.to;

    case BINOP_CALL:
        failforward(base, expr->info.call.base);
        if (!isfun(base)) fail("Base of call expression is not of a function type");
        expression* cons = expr->info.call.args;
        size_t k;
        for (k = 0; k < base->info.fun.count && cons; k++) {
            struct adpt_type const* arg;
            if (k+1 == base->info.fun.count) {
                failforward(arg, cons);
                cons = NULL;
            } else {
                if (BINOP_COMMA != cons->kind) fail("Not enough arguments: %zu provided, expected %zu", k+1, base->info.fun.count);
                failforward(arg, cons->info.binary.rhs);
                cons = cons->info.binary.lhs;
            }
            fprintf(stderr, "  arg %zu: ", base->info.fun.count - k);
            print_type(stderr, arg);
            fprintf(stderr, "\n");
        }
        if (cons) fail("Too many arguments: %zu provided, expected %zu", k, base->info.fun.count);
        return base->info.fun.ret;

    case BINOP_TERNBRANCH:
    case BINOP_TERNCOND:
        fail("NIY: ternary");

    case BINOP_COMMA:
        failforward(lhs, expr->info.binary.lhs);
        failforward(rhs, expr->info.binary.rhs);
        return rhs;

    case BINOP_ASGN:
    case BINOP_ASGN_BOR:
    case BINOP_ASGN_BXOR:
    case BINOP_ASGN_BAND:
    case BINOP_ASGN_BSHL:
    case BINOP_ASGN_BSHR:
    case BINOP_ASGN_SUB:
    case BINOP_ASGN_ADD:
    case BINOP_ASGN_REM:
    case BINOP_ASGN_DIV:
    case BINOP_ASGN_MUL:
        fail("NIY: assignment");

    case BINOP_LOR:
    case BINOP_LAND:
        return &adptb_int_type; // yyy

    case BINOP_EQ:
    case BINOP_NE:
    case BINOP_LT:
    case BINOP_GT:
    case BINOP_LE:
    case BINOP_GE:
        failforward(lhs, expr->info.binary.lhs);
        failforward(rhs, expr->info.binary.rhs);
        if ((isnum(lhs) && isnum(rhs)) || (isptr(lhs) && isptr(rhs))) return &adptb_int_type; // yyy
        fail("Values are not comparable");

    case BINOP_BOR:
    case BINOP_BXOR:
    case BINOP_BAND:
    case BINOP_BSHL:
    case BINOP_BSHR:
        failforward(lhs, expr->info.binary.lhs);
        failforward(rhs, expr->info.binary.rhs);
        if (isint(lhs) && isint(rhs)) return lhs; // yyy
        fail("Both operands are not of an integral type");

    case BINOP_SUB:
    case BINOP_ADD:
        failforward(lhs, expr->info.binary.lhs);
        failforward(rhs, expr->info.binary.rhs);
        bool lnum = isnum(lhs), rnum = isnum(rhs);
        if (lnum && rnum) return lhs; // yyy
        if (lnum && isptr(rhs)) return rhs;
        if (rnum && isptr(lhs)) return lhs;
        fail("Both operands are not of an arithmetic type");

    case BINOP_REM:
    case BINOP_DIV:
    case BINOP_MUL:
        failforward(lhs, expr->info.binary.lhs);
        failforward(rhs, expr->info.binary.rhs);
        if (isnum(lhs) && isnum(rhs)) return lhs; // yyy
        fail("Both operands are not of an arithmetic type");

    case UNOP_ADDR:
        fail("NIY: address of");
    case UNOP_DEREF:
        failforward(opr, expr->info.unary.opr);
        if (isptr(opr)) return opr->info.to;
        fail("Operand is not of a pointer type");

    case UNOP_PMEMBER:
        failforward(opr, expr->info.unary.opr);
        if (!isptr(opr)) fail("Operand is not of a pointer type");
        if (0)
    case UNOP_MEMBER:
            failforward(opr, expr->info.unary.opr);
        fail("NIY: member access");

    case UNOP_BNOT:
    case UNOP_LNOT:
        failforward(opr, expr->info.unary.opr);
        if (isint(opr)) return opr;
        fail("Operand is not of an integral type");

    case UNOP_MINUS:
    case UNOP_PLUS:
        failforward(opr, expr->info.unary.opr);
        if (isnum(opr)) return opr;
        fail("Operand is not of an arithmetic type");

    case UNOP_PRE_DEC:
    case UNOP_PRE_INC:
    case UNOP_POST_DEC:
    case UNOP_POST_INC:
        failforward(opr, expr->info.unary.opr);
        if (isnum(opr) || isptr(opr)) return opr;
        fail("Operand is not of an arithmetic type");
    }

    return NULL;

#   undef isfun
#   undef isptr
#   undef isnum
#   undef isflt
#   undef isint
#   undef failforward
#   undef fail
} // typeof

bool check_and_alloc_pass(expression ref expr) {
    // TODO:
    // - check if names exists
    // - check if types matches
    // - stack allocate and copy string literal
    // - stack allocate and fill compound literals
    // - track the allocations and found names somehow -> easiest is to modify the ast itself

    struct adpt_type cref ty = typeof(expr);
    if (!ty) return false;

    printf("expression is of type: ");
    print_type(stdout, ty);
    printf("\n");
    return true;
}
// }}}

// exec pass {{{
void exec_pass(expression ref expr) {
    switch (expr->kind) {
    case ATOM:
    case BINOP_SUBSCR:
        break;

    case BINOP_CALL:
        ;
        bufsl name = expr->info.call.base->info.atom;
        printf("call to: %.*s\n", bufmt(name));
        size_t ns;
        struct adpt_item cref found = search_namespaces(name, &ns);

        if (found) printf("-> found in namespace '%s'\n", namespaces[ns].name);
        else printf("-> not found in any namespaces\n");
        break;

    case BINOP_TERNBRANCH:
    case BINOP_TERNCOND:
    case BINOP_COMMA:
    case BINOP_ASGN:
    case BINOP_ASGN_BOR:
    case BINOP_ASGN_BXOR:
    case BINOP_ASGN_BAND:
    case BINOP_ASGN_BSHL:
    case BINOP_ASGN_BSHR:
    case BINOP_ASGN_SUB:
    case BINOP_ASGN_ADD:
    case BINOP_ASGN_REM:
    case BINOP_ASGN_DIV:
    case BINOP_ASGN_MUL:
    case BINOP_LOR:
    case BINOP_LAND:
    case BINOP_BOR:
    case BINOP_BXOR:
    case BINOP_BAND:
    case BINOP_EQ:
    case BINOP_NE:
    case BINOP_LT:
    case BINOP_GT:
    case BINOP_LE:
    case BINOP_GE:
    case BINOP_BSHL:
    case BINOP_BSHR:
    case BINOP_SUB:
    case BINOP_ADD:
    case BINOP_REM:
    case BINOP_DIV:
    case BINOP_MUL:
    case UNOP_ADDR:
    case UNOP_DEREF:
    case UNOP_BNOT:
    case UNOP_LNOT:
    case UNOP_MINUS:
    case UNOP_PLUS:
    case UNOP_PRE_DEC:
    case UNOP_PRE_INC:
    case UNOP_PMEMBER:
    case UNOP_MEMBER:
    case UNOP_POST_DEC:
    case UNOP_POST_INC:
        break;
    }
}
// }}}

void accept(void ref _, expression ref expr, bufsl ref tok) {
    (void)_;

    size_t psp = sp;
    if (!check_and_alloc_pass(expr)) {
        sp = psp;
        return;
    }

    char const* xcmd = "";
    if (tok->len && ';' == *tok->ptr)
        xcmd = tok->ptr+1;
    fprintf(stderr, " -- xcmd: '%s'\n", xcmd);

    exec_pass(expr);
}

int main(void) {
    printf("namespaces:\n");
    for (size_t k = 0; k < countof(namespaces); k++)
        printf("   %s (%zu items)\n", namespaces[k].name, namespaces[k].count);
    printf("(%zu total)\n\n", countof(namespaces));

    lex_state ls = {.file= "<input>"};

    char* line = NULL;
    while (prompt("\x1b[35m(*^^),u~~\x1b[m ", &line)) {
        ls.line++;
        ls.slice.len = strlen(ls.slice.ptr = line);

        parse_expr_state ps = {.ls= &ls, .on= accept};
        parse_expression(&ps, lext(ps.ls));
    }

    return EXIT_SUCCESS;
}
