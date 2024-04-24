/// `int main()` of the REPL; typically included as the entry point in the
/// <c-main.c> file generated from `$ preparer -m`:
/// ```c
/// #include "build/a-standard.h"
///
/// static struct adpt_namespace const namespaces[] = {
///     {.name= "standard", .count= countof(adptns_standard), .items= adptns_standard},
/// };
///
/// #define CINTRE_NAMESPACES_DEFINED
/// #include "cintre.c"
/// ```
///
/// see `$ preparer -h`
///
/// if `CINTRE_NAMESPACES_DEFINED` is defined, then a declaration like the
/// following must have already been fully defined:
/// ```c
/// static struct adpt_namespace const namespaces[] = { /*-*/ };
/// ```
///
/// if `USE_READLINE` is defined, readline will be used

#include "common.h"
#include "parser.h"
#include "adapter.h"
#include "compiler.h"
#include "runner.h"
#include "prints.h"

typedef struct cintre_state {
    lex_state lexr;
    parse_decl_state decl;
    parse_expr_state expr;
    compile_state comp;
    run_state runr;
} cintre_state;

bool _compile_expression_tmp_wrap(compile_state ref cs, expression ref expr) {
    struct slot slot = {.ty= check_expression(cs, expr)};
    if (!slot.ty) return false;
    _alloc_slot(cs, &slot);

    compile_expression(cs, expr, &slot);
    switch (slot.usage) {
    case _slot_value:
        _emit_data(cs, slot.loc-cs->vsp, slot.ty->size, slot.as.value.bytes);
        break;

    case _slot_used:
        break;

    case _slot_variable:
        _emit_move(cs, slot.loc-cs->vsp, slot.ty->size, slot.as.variable-cs->vsp);
        // yyy: result is a variable, show it and not "_"?
        break;
    }
    slot.usage = _slot_used;

    return true;
}

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

struct adpt_type const* alloc_adpt_type(cintre_state ref gs, struct decl_type cref ty) {
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

        struct adpt_item cref it = gs->comp.lookup(ty->name);
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

        size_t count = 0;
        if (ty->info.arr.count) {
            size_t const plen = gs->comp.res.len;
            size_t const psp = gs->runr.sp;

            // TODO: once UNOP_CAST is added, use it by wrapping `ty->info.arr.count`
            struct adpt_type cref count_expr = check_expression(&gs->comp, ty->info.arr.count);
            if (!count_expr) break;
            if (!(TYPE_CHAR <= count_expr->tyty && count_expr->tyty <= TYPE_ULONG)) {
                notif("Array size is not of an integral type");
                break;
            }

            struct slot slot = {.ty= &adptb_ulong_type};
            _alloc_slot(&gs->comp, &slot);
            _fit_expr_to_slot(&gs->comp, ty->info.arr.count, &slot);

            switch (slot.usage) {
            case _slot_value:
                count = slot.as.value.ul;
                break;

            case _slot_used:
            case _slot_variable:
                run(&gs->runr, gs->comp.res);
                count = *(size_t*)(gs->runr.stack+gs->runr.sp);
                gs->comp.res.len = plen;
                gs->runr.sp = psp;
                break;
            }
        }

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

// accept parsed input {{{
void accept_decl(void ref usr, declaration cref decl, bufsl ref tok) {
    cintre_state ref gs = usr;

    //printf("decl: ");
    //print_decl(stdout, decl);
    //printf("\n");
    struct adpt_type cref ty = alloc_adpt_type(gs, &decl->type);
    if (!ty) return;

    //size_t end = gs->sp;
    gs->runr.sp = ((gs->runr.sp-ty->size) / ty->align) * ty->align;

    struct adpt_item* it = dyarr_push(&locals);
    if (!it) exitf("OOM");
    char* name = malloc(decl->name.len+1);
    if (!name) free(it), exitf("OOM");
    name[decl->name.len] = '\0';
    memcpy(it, &(struct adpt_item){
            .name= memcpy(name, decl->name.ptr, decl->name.len),
            .type= ty,
            .kind= ITEM_VARIABLE,
            .as.variable= gs->runr.sp,
        }, sizeof *it);
}

void accept_expr(void ref usr, expression ref expr, bufsl ref tok) {
    cintre_state ref gs = usr;

    // NOTE: thinking about moving this xcmd stuff in its own function, in
    //       a way it could be called from accept_decl (and maybe even
    //       accept_sttm, idk that doesn't make sense but all this isn't
    //       devised yest)
    char cref xcmd = tok->len && ';' == *tok->ptr ? tok->ptr+1+strspn(tok->ptr+1, " \t\n") : "";
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
            struct adpt_item cref it = locals.ptr+k;
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
                struct adpt_item cref it = namespaces[ns].items+k;
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

    if (!expr) return;
    gs->comp.vsp = gs->runr.sp;
    gs->comp.res.len = 0;

    if (xcmdis("ty")) {
        struct adpt_type cref ty = check_expression(&gs->comp, expr);
        if (!ty) return;
        printf("Expression is of type: ");
        print_type(stdout, ty);
        printf("\n");
        return;
    }

    bool const r = _compile_expression_tmp_wrap(&gs->comp, expr);
    if (!r) return;

    if (xcmdis("bytec") || xcmdis("bc")) {
        printf("Resulting bytecode (%zuB):\n", gs->comp.res.len);
        print_code(stdout, gs->comp.res);
        return;
    }

    run(&gs->runr, gs->comp.res);
    struct adpt_item const res = {
        .name= "_",
        .type= expr->usr,
        .kind= ITEM_VARIABLE,
        .as.variable= gs->runr.sp,
    };

    printf("Result:\n");
    print_item(stdout, &res, gs->runr.stack, 0);

    gs->runr.sp+= res.type->size; // yyy: free
} // accept_expr
// }}}

int main(void) {
    printf("Type `;help` for a list of command\n");

    static cintre_state _gs = {
        .lexr= {.file= "<input>"},
        .decl= {.ls= &_gs.lexr, .usr= &_gs, .on= accept_decl},
        .expr= {.ls= &_gs.lexr, .usr= &_gs, .on= accept_expr},
        .comp= {.lookup= lookup},
        .runr= {.sp= sizeof _gs.runr.stack}, // xxx: don't like having to do that
    };
    cintre_state ref gs = &_gs;

    char* line = NULL;
    while (prompt("\1\x1b[35m\2(*^^),u~~\1\x1b[m\2 ", &line)) {
        gs->lexr.line++;
        gs->lexr.slice.len = strlen(gs->lexr.slice.ptr = line);

        bufsl tok = lext(&gs->lexr);
        if (!tok.len || ';' == *tok.ptr) accept_expr(NULL, NULL, &tok);

        else if (is_decl_keyword(tok)) {
            do {
                tok = parse_declaration(&gs->decl, tok);

                if (1 == tok.len && '=' == *tok.ptr) {
                    gs->lexr.slice.ptr--, gs->lexr.slice.len++;
                    char cref name = dyarr_bot(&locals)->name;
                    tok = parse_expression(&gs->expr, (bufsl){.ptr= name, .len= strlen(name)});
                }
                // FIXME: sometime incorrect until parsing of the comma operator is done properly
                //        (if there was an '=')
            } while (1 == tok.len && ',' == *tok.ptr ? tok = lext(&gs->lexr), true : false);

            gs->decl.base = (declaration){0};
        }

        else parse_expression(&gs->expr, tok);
    }

    ldel(&gs->lexr);
    for (size_t k = 0; k < locals.len; k++) {
        free((void*)locals.ptr[k].name);
        free_adpt_type(locals.ptr[k].type);
    }
    free(locals.ptr);

    return EXIT_SUCCESS;
}
