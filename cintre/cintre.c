#include "common.h"
#include "parser.h"
#include "adapter.h"
#include "compiler.h"
#include "prints.h"

#ifndef STACK_SIZE
#define STACK_SIZE 1024*1024
#endif

typedef struct cintre_state {
    lex_state lexr;
    parse_decl_state decl;
    parse_expr_state expr;

    struct adpt_item const* (*lookup)(bufsl const name);

    char stack[STACK_SIZE];
    size_t sp;
    bytecode code;
} cintre_state;

// readline {{{
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
// }}}

// compile time (lookup and random helpers) {{{
#ifndef CINTRE_NAMESPACES_DEFINED
static struct adpt_namespace const namespaces[1] = {{.name= "(placeholder)", .count= 0, .items= NULL}};
#endif

dyarr(struct adpt_item) locals;

struct adpt_item const* lookup(bufsl const name) {
    for (size_t k = 0; k < locals.len; k++) {
        struct adpt_item const* const it = locals.ptr+k;
        if (!memcmp(it->name, name.ptr, name.len)) return it;
    }
    for (size_t ns = 0; ns < countof(namespaces); ns++)
        for (size_t k = 0; k < namespaces[ns].count; k++) {
            struct adpt_item const* const it = namespaces[ns].items+k;
            if (!memcmp(it->name, name.ptr, name.len)) return it;
        }
    return NULL;
}

bool is_decl_keyword(bufsl const tok) {
#   define tokis(l) (strlen(l) == tok.len && !memcmp(l, tok.ptr, strlen(l)))
    return tokis("char")
        || tokis("short")
        || tokis("int")
        || tokis("long")
        || tokis("signed")
        || tokis("unsigned")
        || tokis("float")
        || tokis("double")
        || tokis("struct")
        || tokis("union")
        || tokis("enum")
        || tokis("typedef")
        || tokis("const")
        ;
#   undef tokis
}

void free_adpt_type(struct adpt_type cref ty) {
    switch (ty->tyty) {
    default: break;

    case TYPE_STRUCT:
    case TYPE_UNION:
        for (size_t k = 0; k < ty->info.comp.count; k++) {
            free((void*)ty->info.comp.fields[k].name);
            free_adpt_type(ty->info.comp.fields[k].type);
        }
        free((void*)ty->info.comp.fields);
        break;

    case TYPE_FUN:
        free_adpt_type(ty->info.fun.ret);
        for (size_t k = 0; k < ty->info.fun.count; k++) {
            free((void*)ty->info.fun.params[k].name);
            free_adpt_type(ty->info.fun.params[k].type);
        }
        free((void*)ty->info.fun.params);
        break;

    case TYPE_PTR:
        free_adpt_type(ty->info.ptr);
        break;

    case TYPE_ARR:
        free_adpt_type(ty->info.arr.item);
        break;
    }

    free((void*)ty);
}

struct adpt_type const* alloc_adpt_type(cintre_state cref gs, struct decl_type cref ty) {
    struct adpt_type ref r = malloc(sizeof *r);
    if (!r) exitf("OOM");
#   define cpyreturn(...) return memcpy(r, (__VA_ARGS__), sizeof*r)

    switch (ty->kind) {
    case KIND_NOTAG:;
        bool _signed = false, _unsigned = false, _short = false, _long = false;
        for (size_t k = 0; QUAL_END != ty->quals[k]; k++) switch (ty->quals[k]) {
            default: break;
            case QUAL_SIGNED:    _signed = true;          break;
            case QUAL_UNSIGNED:  _unsigned = true;        break;
            case QUAL_SHORT:     _short = true;           break;
            case QUAL_LONG:      _long = true;            break;
            case QUAL_COMPLEX:   exitf("NIY: complex");   break;
            case QUAL_IMAGINARY: exitf("NIY: imaginary"); break;
        }

#       define nameis(s)  (strlen(s) == ty->name.len && !memcmp(s, ty->name.ptr, strlen(s)))
        if (nameis("char"))
            cpyreturn(_signed ? &adptb_schar_type
                    : _unsigned ? &adptb_uchar_type
                    : &adptb_char_type);
        if (nameis("int") || !ty->name.len)
            cpyreturn(_short ? (_unsigned ? &adptb_ushort_type : &adptb_short_type)
                    : _long ? (_unsigned ? &adptb_ulong_type : &adptb_long_type)
                    : _unsigned ? &adptb_uint_type : &adptb_int_type);
        if (nameis("float"))  cpyreturn(&adptb_float_type);
        if (nameis("double")) cpyreturn(&adptb_double_type);
#       undef nameis

        struct adpt_item cref it = gs->lookup(ty->name);
        if (it && ITEM_TYPEDEF == it->kind) cpyreturn(it->type);
        break;

    case KIND_STRUCT:
    case KIND_UNION:
        // XXX: for now only support via looking up the tag
        //      (because otherwise will have to do the layout for size/align)
        if ((size_t)-1 == ty->info.comp.count) {
            // XXX: wrong lookup space
            //struct adpt_item cref it = gs->lookup(ty->name);
            //if (it && ITEM_TYPEDEF == it->kind) cpyreturn(it->type);
            //break;
        }

        return NULL;
        cpyreturn(&(struct adpt_type){
                .size= 0, // TODO: uuuhh :' didn't think about thaaat
                .align= 0, // (same)
                .tyty= KIND_STRUCT == ty->kind ? TYPE_STRUCT : TYPE_UNION,
                .info.comp = {
                    .fields= NULL,
                    .count= 0,
                },
            });

    case KIND_ENUM: cpyreturn(&adptb_int_type);

    case KIND_PTR:;
        struct adpt_type cref ptr = alloc_adpt_type(gs, &ty->info.ptr->type);
        if (!ptr) break;
        cpyreturn(&(struct adpt_type){
                .size= sizeof(void*),
                .align= sizeof(void*), // yyy: approximation
                .tyty= TYPE_PTR,
                .info.ptr= ptr,
            });

    case KIND_FUN:
        if ((size_t)-1 == ty->info.fun.count) break;

        struct adpt_type cref ret = alloc_adpt_type(gs, &ty->info.fun.ret->type);
        if (!ret) break;

        struct adpt_fun_param* const params = malloc(ty->info.fun.count*sizeof*params);
        if (!params) exitf("OOM");

        size_t k = 0;
        for (struct decl_type_param const* curr = ty->info.fun.first; curr; curr = curr->next, k++) {
            struct adpt_type cref type = alloc_adpt_type(gs, &curr->decl->type);
            if (!type) break;

            char* name = NULL;
            if (curr->decl->name.len) {
                name = malloc(curr->decl->name.len+1);
                if (!name) exitf("OOM");
                name[curr->decl->name.len] = '\0';
                memcpy(name, curr->decl->name.ptr, curr->decl->name.len);
            }

            memcpy(params+k, &(struct adpt_fun_param){
                    .name= name,
                    .type= type,
                }, 0);
        }
        // early break (alloc_adpt_type returned NULL)
        if (k < ty->info.fun.count) {
            while (k--) {
                free((void*)params[k].name);
                free_adpt_type(params[k].type);
            }
            free(params);
            free_adpt_type(ret);
            break;
        }

        cpyreturn(&(struct adpt_type){
                .size= sizeof(void(*)()),
                .align= sizeof(void(*)()),
                .tyty= TYPE_FUN,
                .info.fun= {
                    .ret= ret,
                    .params= params,
                    .count= ty->info.fun.count,
                },
            });

    case KIND_ARR:;
        struct adpt_type cref item = alloc_adpt_type(gs, &ty->info.arr.item->type);
        if (!item) break;
        size_t const count = ty->info.arr.count
            ? atoi(ty->info.arr.count->info.atom.ptr) // XXX/TODO: very wrong of course
            : 0;
        cpyreturn(&(struct adpt_type){
                .size= item->size*count,
                .align= item->align,
                .tyty= TYPE_ARR,
                .info.arr= {
                    .item= item,
                    .count= count,
                },
            });
    }

#   undef cpyreturn
    return free(r), NULL;
}
// }}}

// runtime {{{
void run(cintre_state ref gs, bytecode const code) {
    size_t a, b, c;
#   define imm(nm) for (                           \
        unsigned xx = (nm = code.ptr[++k], 0);     \
        code.ptr[k]&0x80;                          \
        nm = nm | (code.ptr[++k]&0x7f)<<(xx+= 7))
#   define at(off, ty) ((ty*)(gs->stack+gs->sp+off))

    for (size_t k = 0; k < code.len; k++) switch (code.ptr[k]) {
    case 0x2a:
        notif("yyy: debug");
        // fall through
    case 0x00:
        continue;

    case 0x0d: imm(a); gs->sp+= a; continue; // pop
    case 0x0f: imm(a); gs->sp-= a; continue; // push
    case 0x1d: // data
        imm(a);
        imm(b);
        memcpy(at(a, char), code.ptr+k+1, b);
        k+= b;
        continue;
    case 0x1f: // move
        imm(a);
        imm(b);
        imm(c);
        memmove(at(a, char), at(c, char), b);
        continue;
    case 0x2d: exitf("NIY: (run) write");
    case 0x2f: exitf("NIY: (run) read");
        continue;

#       define cvt(code, from, to)  \
    case code:                      \
        imm(a);                     \
        imm(b);                     \
        *at(a, from) = *at(b, to);  \
        continue;
#       define extend_cvt(from_w, from, to_w, to)       \
        cvt(from_w<<4 | to_w, signed from, signed to);  \
        cvt(from_w<<4 | to_w | 0x4, unsigned from, unsigned to);
        extend_cvt(0, char, 1, short)
        extend_cvt(0, char, 2, int)
        extend_cvt(0, char, 3, long)
        extend_cvt(1, short, 2, int)
        extend_cvt(1, short, 3, long)
        extend_cvt(2, int, 3, long)
#       undef extend_cvt
        cvt(0x11, float, int)
        cvt(0x22, double, long)
        cvt(0x15, int, float)
        cvt(0x26, long, double)
        cvt(0x21, double, float)
        cvt(0x25, float, double)
#       undef cvt

    case 0x04: exitf("NIY: (run) not");
    case 0x14: exitf("NIY: (run) cmp1");
    case 0x24: exitf("NIY: (run) cmp2");
    case 0x0a: exitf("NIY: (run) jmp");
    case 0x1a: exitf("NIY: (run) jmb");
    case 0x0b: exitf("NIY: (run) breq");
    case 0x1b: exitf("NIY: (run) brlt");
    case 0x2b: exitf("NIY: (run) brle");
        continue;

    default:;
        unsigned const hi = code.ptr[k]>>4 & 0xf, lo = code.ptr[k] & 0xf;

        if (0xc == lo) {
            imm(a);
            char* ret = at(a, char);
            imm(b);
            typedef void (*fun_t)(char* ret, char** args);
            fun_t fun = *at(b, fun_t);
            char* args[15];
            for (unsigned l = 0; l < hi; l++) {
                imm(c);
                args[l] = at(c, char);
            }
            fun(ret, args);
            continue;
        }

        imm(a);
        imm(b);
        imm(c);
        switch (hi) {
#           define  _x( op, ty) *at(a, ty) = *at(b, ty) op *at(c, ty); continue;
#           define  _xi(op, ty) *at(a, ty) = (ty)b      op *at(c, ty); continue;
#           define _rxi(op, ty) *at(a, ty) = *at(c, ty) op (ty)b     ; continue;
#           define dox(n, ...) n(__VA_ARGS__)
#           define bop_u(x, op)  \
                case 0x0: dox(x, op, unsigned char)   \
                case 0x1: dox(x, op, unsigned short)  \
                case 0x2: dox(x, op, unsigned int)    \
                case 0x3: dox(x, op, unsigned long)
#           define bop_b(x, op)  \
                case 0x4: dox(x, op, unsigned char)   \
                case 0x5: dox(x, op, unsigned short)  \
                case 0x6: dox(x, op, unsigned int)    \
                case 0x7: dox(x, op, unsigned long)
#           define bop_f(x, op)  \
                case 0xd: dox(x, op, double)  \
                case 0xf: dox(x, op, float)
        case 0x3: switch (lo) { bop_u( _x , +) bop_b( _x , |)  bop_f( _x , +) } break;
        case 0x4: switch (lo) { bop_u( _x , -) bop_b( _x , ^)  bop_f( _x , -) } break;
        case 0x5: switch (lo) { bop_u( _x , *) bop_b( _x , <<) bop_f( _x , *) } break;
        case 0x6: switch (lo) { bop_u( _x , /) bop_b( _x , >>) bop_f( _x , /) } break;
        case 0x7: switch (lo) { bop_u( _x , %) bop_b( _x , &)                 } break;
        case 0x8: switch (lo) { bop_u( _xi, +) bop_b( _xi, |)  bop_f( _xi, +) } break;
        case 0x9: switch (lo) { bop_u( _xi, -) bop_b( _xi, ^)  bop_f( _xi, -) } break;
        case 0xa: switch (lo) { bop_u( _xi, *) bop_b( _xi, <<) bop_f( _xi, *) } break;
        case 0xb: switch (lo) { bop_u( _xi, /) bop_b( _xi, >>) bop_f( _xi, /) } break;
        case 0xc: switch (lo) { bop_u( _xi, %) bop_b( _xi, &)                 } break;
        case 0xd: switch (lo) { bop_u(_rxi, -) bop_b(_rxi, <<) bop_f(_rxi, -) } break;
        case 0xe: switch (lo) { bop_u(_rxi, /) bop_b(_rxi, >>) bop_f(_rxi, /) } break;
        case 0xf: switch (lo) { bop_u(_rxi, %)                                } break;
#           undef bop_f
#           undef bop_b
#           undef bop_u
#           undef dox
#           undef _rxi
#           undef _xi
#           undef _x
        }

        notif("unknown op code 0x%02x", code.ptr[k]);
        return;
    }

#   undef at
#   undef imm
} // run
// }}}

// accept parsed input {{{
void accept_decl(void ref usr, declaration cref decl, bufsl ref tok) {
    cintre_state ref gs = usr;

    //printf("decl: ");
    //print_decl(stdout, decl);
    //printf("\n");
    struct adpt_type cref ty = alloc_adpt_type(gs, &decl->type);

    //size_t end = gs->sp;
    gs->sp = ((gs->sp-ty->size) / ty->align) * ty->align;

    struct adpt_item* it = dyarr_push(&locals);
    if (!it) exitf("OOM");
    char* name = malloc(decl->name.len+1);
    if (!name) free(it), exitf("OOM");
    name[decl->name.len] = '\0';
    memcpy(it, &(struct adpt_item){
            .name= memcpy(name, decl->name.ptr, decl->name.len),
            .type= ty,
            .kind= ITEM_VARIABLE,
            .as.variable= gs->sp,
        }, sizeof *it);
}

void accept_expr(void ref usr, expression ref expr, bufsl ref tok) {
    cintre_state ref gs = usr;

    // NOTE: thinking about moving this xcmd stuff in its own function, in
    //       a way it could be called from accept_decl (and maybe even
    //       accept_sttm, idk that doesn't make sense but all this isn't
    //       devised yest)
    char const* const xcmd = tok->len && ';' == *tok->ptr ? tok->ptr+1+strspn(tok->ptr+1, " \t\n") : "";
#   define xcmdis(s)  (!memcmp(s, xcmd, strlen(s)))

    if (xcmdis("h")) {
        printf("List of commands:\n");
        printf("   h[elp]  - print this help and no more\n");
        printf("   loc[ales]  -  list local names\n");
        printf("   names[paces] or ns  -  list names in namespaces\n");
        printf("   ast  -  ast of the expression\n");
        printf("   ty[pe]  -  type of the expression, eg. `strlen; ty`\n");
        printf("   bytec[ode] or bc  -  internal bytecode from compilation\n");
        printf("no command after the ; (or no ;) will simply execute the expression\n");
        return;
    }

    if (xcmdis("loc")) {
        printf("List of locals:\n");
        for (size_t k = 0; k < locals.len; k++) {
            struct adpt_item const* it = locals.ptr+k;
            if (ITEM_TYPEDEF == it->kind) printf("   [typedef] %-8s\t", it->name);
            else printf("   [top-%zu] %-8s\t", STACK_SIZE-it->as.variable, it->name);
            print_type(stdout, it->type);
            printf("\n");
        }
        return;
    }

    if (xcmdis("names") || xcmdis("ns")) {
        printf("List of names:\n");
        for (size_t ns = 0; ns < countof(namespaces); ns++) {
            printf("%s::\n", namespaces[ns].name);
            for (size_t k = 0; k < namespaces[ns].count; k++) {
                struct adpt_item const* it = namespaces[ns].items+k;
                if (ITEM_TYPEDEF == it->kind) printf("   [typedef] %-8s\t", it->name);
                else printf("   [%p] %-8s\t", it->as.object, it->name);
                print_type(stdout, it->type);
                printf("\n");
            }
        }
        return;
    }

    if (xcmdis("ast")) {
        printf("AST of the expression:\n");
        print_expr(stdout, expr, 0);
        return;
    }

    gs->code.len = 0;
    if (!expr) return;
    compile_state cs = {.vsp= gs->sp, .res= gs->code, .lookup= gs->lookup};

    if (xcmdis("ty")) {
        struct adpt_type const* ty = check_expression(&cs, expr);
        if (!ty) return;
        printf("Expression is of type: ");
        print_type(stdout, ty);
        printf("\n");
        return;
    }

    bool r = compile_expression_tmp_wrap(&cs, expr);
    gs->code = cs.res;
    if (!r) return;

    if (xcmdis("bytec") || xcmdis("bc")) {
        printf("Resulting bytecode (%zuB):\n", gs->code.len);
        print_code(stdout, gs->code);
        return;
    }

    run(gs, gs->code);
    struct adpt_item res = {
        .name= "_",
        .type= expr->usr,
        .kind= ITEM_VARIABLE,
        .as.variable= gs->sp,
    };

    printf("Result:\n");
    print_item(stdout, &res, gs->stack, 0);

    gs->sp+= res.type->size;
} // accept_expr
// }}}

int main(void) {
    printf("Type `;help` for a list of command\n");

    cintre_state gs = {
        .lexr= {.file= "<input>"},
        .decl= {.ls= &gs.lexr, .usr= &gs, .on= accept_decl},
        .expr= {.ls= &gs.lexr, .usr= &gs, .on= accept_expr},
        .lookup= lookup,
        .sp= STACK_SIZE,
    };

    char* line = NULL;
    while (prompt("\1\x1b[35m\2(*^^),u~~\1\x1b[m\2 ", &line)) {
        gs.lexr.line++;
        gs.lexr.slice.len = strlen(gs.lexr.slice.ptr = line);

        bufsl tok = lext(&gs.lexr);
        if (!tok.len || ';' == *tok.ptr) accept_expr(NULL, NULL, &tok);

        else if (is_decl_keyword(tok)) {
            do {
                tok = parse_declaration(&gs.decl, tok);

                if (1 == tok.len && '=' == *tok.ptr) {
                    gs.lexr.slice.ptr--, gs.lexr.slice.len++;
                    char cref name = dyarr_bot(&locals)->name;
                    tok = parse_expression(&gs.expr, (bufsl){.ptr= name, .len= strlen(name)});
                }
                // FIXME: sometime incorrect until parsing of the comma operator is done properly
                //        (if there was an '=')
            } while (1 == tok.len && ',' == *tok.ptr ? tok = lext(&gs.lexr), true : false);

            gs.decl.base = (declaration){0};
        }

        else parse_expression(&gs.expr, tok);
    }

    ldel(&gs.lexr);
    for (size_t k = 0; k < locals.len; k++) {
        free((void*)locals.ptr[k].name);
        free_adpt_type(locals.ptr[k].type);
    }
    free(locals.ptr);

    return EXIT_SUCCESS;
}
