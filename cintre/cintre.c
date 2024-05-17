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
    dyarr(struct adpt_item) locs;

    // needed for `decl_to_adpt_type`
    dyarr(struct adpt_type) ty_work;
} cintre_state;

#ifndef CINTRE_NAMESPACES_DEFINED
static struct adpt_namespace const namespaces[1] = {{.name= "(0 namespaces)", .count= 0, .items= NULL}};
#endif

void free_cintre_state(cintre_state ref gs)
{
    ldel(&gs->lexr);

    for (size_t k = 0; k < gs->locs.len; k++)
        free((void*)gs->locs.ptr[k].name);
    free(gs->locs.ptr);

    for (size_t k = 0; k < gs->ty_work.len; k++) switch (gs->ty_work.ptr[k].tyty) {
    case TYPE_STRUCT:
    case TYPE_UNION:;
        struct adpt_comp_desc cref comp = &gs->ty_work.ptr[k].info.comp;
        for (size_t kk = 0; kk < comp->count; kk++) free((void*)comp->fields[kk].name);
        free((void*)comp->fields);
        break;
    case TYPE_FUN:;
        struct adpt_fun_desc cref fun = &gs->ty_work.ptr[k].info.fun;
        for (size_t kk = 0; kk < fun->count; kk++) free((void*)fun->params[kk].name);
        free((void*)fun->params);
        break;
    default:;
    }
    free(gs->ty_work.ptr);

    free(gs->comp.chk_work.ptr); // xxx: annoying
}

bool _compile_expression_tmp_wrap(compile_state ref cs, expression ref expr)
{
    cs->chk_work.len = 0; // xxx: annoying
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
void _prompt_cc(int sigint)
{
    rl_crlf();
    rl_end_of_history(0, 0);
    rl_beg_of_line(0, 0);
    rl_kill_line(0, 0);
    rl_forced_update_display();
    signal(sigint, _prompt_cc);
}
char* _prompt_compl(char cref text, int const state)
{
    rl_completion_suppress_append = 1; // yyy: the automatic trailing space
    // TODO: could
    //if (in string) return rl_filename_completion_function(text, state);
    //if (TYPE_STRUCT || TYPE_UNION) .. rl_point, rl_line_buffer ..;
    size_t const len = strlen(text);
    static size_t ns, k;
    if (0 == state) ns = k = 0;
    // FIXME: not having locals in completion is dumb :/
    //cintre_state cref gs = usr;
    //for (size_t k = 0; k < gs->locs.len; k++) {
    //    struct adpt_item const* const it = gs->locs.ptr+k;
    //    if (bufis(name, it->name)) return it;
    //}
    for (; ns < countof(namespaces); ns++, k = 0)
        for (; k < namespaces[ns].count; k++) {
            struct adpt_item const* const it = namespaces[ns].items+k;
            if (!memcmp(it->name, text, len)) {
                size_t const len = strlen(it->name);
                bool const tdf = ITEM_TYPEDEF == it->kind;
                bool const fun = TYPE_FUN == it->type->tyty || (TYPE_PTR == it->type->tyty && TYPE_FUN == it->type->info.ptr->tyty); // xxx: || ...
                bool const arr = TYPE_ARR == it->type->tyty || (TYPE_PTR == it->type->tyty && TYPE_ARR == it->type->info.ptr->tyty); // xxx: || ...
                char ref r = malloc(len + (tdf|fun|arr));
                strcpy(r, it->name);
                if (tdf|fun|arr) r[len] =
                    tdf ? ' ' :
                    fun ? '(' :
                    arr ? '[' :
                        '?', r[len+1] = '\0';
                k++;
                return r;
            }
        }
    return NULL;
}
void _prompt_list_compl(char** const matches, int const num_matches, int const max_length)
{
    int term_width = 0; {
        printf("\x1b[9999G\x1b[6n\r\n");
        while (';' != getchar());
        char c;
        while ('R' != (c = getchar())) term_width = term_width*10 + c-'0';
    }
    int const cols_count = term_width/(max_length+2) - 1;
    int const rows_count = num_matches < cols_count ? 1 : num_matches/cols_count;
    for (int j = 0; j < rows_count; j++) {
        for (int i = 0; i < cols_count+1; i++) {
            if (i*rows_count+j >= num_matches) break;
            char cref it = matches[i*rows_count+j+1];
            unsigned const len = strlen(it);
            if ('(' == it[len-1]) printf("\x1b[33m%.*s\x1b[m(%*s", len-1, it, max_length+2-len, "");
            else if (' ' == it[len-1]) printf("\x1b[32m%-*s\x1b[m", max_length+2, it);
            else printf("%-*s", max_length+2, it);
        }
        printf("\r\n");
    }
    rl_forced_update_display();
}
bool prompt(char const* prompt, char** res)
{
    if (!*res) {
        rl_readline_name = "Cintre";
        rl_completer_word_break_characters = " 0123456789{}[]()<>%:;.?*+-/^&|~!=,";
        rl_completion_entry_function = _prompt_compl;
        rl_completion_display_matches_hook = _prompt_list_compl;
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
bool prompt(char const* prompt, char** res)
{
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
struct adpt_item const* lookup(void* usr, bufsl const name)
{
    cintre_state cref gs = usr;
    for (size_t k = 0; k < gs->locs.len; k++) {
        struct adpt_item const* const it = gs->locs.ptr+k;
        if (bufis(name, it->name)) return it;
    }
    for (size_t ns = 0; ns < countof(namespaces); ns++)
        for (size_t k = 0; k < namespaces[ns].count; k++) {
            struct adpt_item const* const it = namespaces[ns].items+k;
            if (bufis(name, it->name)) return it;
        }
    return NULL;
}

bool is_decl_keyword(cintre_state cref gs, bufsl const tok)
{
    if (bufis(tok, "char")     ||
        bufis(tok, "short")    ||
        bufis(tok, "int")      ||
        bufis(tok, "long")     ||
        bufis(tok, "signed")   ||
        bufis(tok, "unsigned") ||
        bufis(tok, "float")    ||
        bufis(tok, "double")   ||
        bufis(tok, "void")     ||
        bufis(tok, "struct")   ||
        bufis(tok, "union")    ||
        bufis(tok, "enum")     ||
        bufis(tok, "typedef")  ||
        bufis(tok, "extern")   ||
        bufis(tok, "static")   ||
        bufis(tok, "auto")     ||
        bufis(tok, "register") ||
        bufis(tok, "const")    ) return true;

    struct adpt_item cref it = gs->comp.lookup(gs->comp.usr, tok);
    return it && ITEM_TYPEDEF == it->kind;
}

/// allocates using `gs->ty_work`; on failure, it is safe to set the length of
/// it back to what it was to free wip temporaries, it should not cause a leak
struct adpt_type const* decl_to_adpt_type(cintre_state ref gs, struct decl_type cref ty)
{
    switch (ty->kind) {
        struct adpt_type* r;

    case KIND_NOTAG:;
        bool _signed = false, _unsigned = false, _short = false, _long = false;
        for (size_t k = 0; QUAL_END != ty->quals[k]; k++) switch (ty->quals[k]) {
            case QUAL_SIGNED:    _signed = true;          break;
            case QUAL_UNSIGNED:  _unsigned = true;        break;
            case QUAL_SHORT:     _short = true;           break;
            case QUAL_LONG:      _long = true;            break;
            case QUAL_COMPLEX:   exitf("NIY: complex");   break;
            case QUAL_IMAGINARY: exitf("NIY: imaginary"); break;
            default:;
        }

#       define nameis(s)  (strlen(s) == ty->name.len && !memcmp(s, ty->name.ptr, strlen(s)))
        if (nameis("char"))
            return _signed ? &adptb_schar_type
                 : _unsigned ? &adptb_uchar_type
                 : &adptb_char_type;
        if (nameis("int") || !ty->name.len)
            return _short ? (_unsigned ? &adptb_ushort_type : &adptb_short_type)
                 : _long ? (_unsigned ? &adptb_ulong_type : &adptb_long_type)
                 : _unsigned ? &adptb_uint_type : &adptb_int_type;
        if (nameis("float"))  return &adptb_float_type;
        if (nameis("double")) return &adptb_double_type;
        if (nameis("void"))   return &adptb_void_type;
#       undef nameis

        struct adpt_item cref it = gs->comp.lookup(gs->comp.usr, ty->name);
        if (it && ITEM_TYPEDEF == it->kind) return it->type;
        return NULL;

    case KIND_STRUCT:
    case KIND_UNION:
        if (-1ul == ty->info.comp.count) {
            char copy[ty->name.len+1]; // yyy: va
            memcpy(copy+1, ty->name.ptr, ty->name.len);
            copy[0] = '@';
            struct adpt_item cref it = gs->comp.lookup(gs->comp.usr, (bufsl){.ptr= copy, .len= ty->name.len});
            if (it && ITEM_TYPEDEF == it->kind) return it->type;
            return NULL;
        }

        // XXX: for now only support via looking up the tag
        //      (because otherwise will have to do the layout for size/align)
        return notif("NIY: struct/union from annon"), NULL;
        // NOTE: the layout of the structure can be devised arbitrarly (as in,
        // sane, but doesn't have to follow the rules of platform's cc) because
        // runtime-defined structures would not (should not) be eg. passed to
        // functions and such; they only live in the REPL and forcing them
        // outside is just "UB, idc, you did this"

        if (!(r = dyarr_push(&gs->ty_work))) exitf("OOM");
        return memcpy(r, &(struct adpt_type){
                .size= 0,
                .align= 0,
                .tyty= KIND_STRUCT == ty->kind ? TYPE_STRUCT : TYPE_UNION,
                .info.comp = {
                    .fields= NULL,
                    .count= 0,
                },
            }, sizeof*r);

    case KIND_ENUM: return &adptb_int_type;

    case KIND_PTR:;
        struct adpt_type cref ptr = decl_to_adpt_type(gs, &ty->info.ptr->type);
        if (!ptr) return NULL;

        if (!(r = dyarr_push(&gs->ty_work))) exitf("OOM");
        return memcpy(r, &(struct adpt_type){
                .size= sizeof(void*),
                .align= sizeof(void*),
                .tyty= TYPE_PTR,
                .info.ptr= ptr,
            }, sizeof*r);

    case KIND_FUN:
        if (-1ul == ty->info.fun.count) return NULL;

        struct adpt_type cref ret = decl_to_adpt_type(gs, &ty->info.fun.ret->type);
        if (!ret) return NULL;

        struct adpt_fun_param* const params = malloc(ty->info.fun.count*sizeof*params);
        if (!params) exitf("OOM");

        size_t k = 0;
        for (struct decl_type_param const* curr = ty->info.fun.first; curr; curr = curr->next, k++) {
            struct adpt_type cref type = decl_to_adpt_type(gs, &curr->decl->type);
            if (!type) {
                while (k--) free((void*)params[k].name);
                free(params);
                return NULL;
            }

            char* name = NULL;
            if (curr->decl->name.len) {
                name = malloc(curr->decl->name.len+1);
                if (!name) exitf("OOM");
                name[curr->decl->name.len] = '\0';
                memcpy(name, curr->decl->name.ptr, curr->decl->name.len);
            }

            memcpy(params+k, &(struct adpt_fun_param){.name= name, .type= type}, sizeof*params);
        }

        if (!(r = dyarr_push(&gs->ty_work))) exitf("OOM");
        return memcpy(r, &(struct adpt_type){
                .size= sizeof(void(*)()),
                .align= sizeof(void(*)()),
                .tyty= TYPE_FUN,
                .info.fun= {
                    .ret= ret,
                    .params= params,
                    .count= ty->info.fun.count,
                },
            }, sizeof*r);

    case KIND_ARR:;
        struct adpt_type cref item = decl_to_adpt_type(gs, &ty->info.arr.item->type);
        if (!item) return NULL;

        size_t count = 0;
        if (ty->info.arr.count) {
            size_t const plen = gs->comp.res.len;
            size_t const psp = gs->runr.sp;

            // TODO: once UNOP_CAST is added, use it by wrapping `ty->info.arr.count`
            struct adpt_type cref count_expr = check_expression(&gs->comp, ty->info.arr.count);
            if (!count_expr) return NULL;
            if (!(TYPE_CHAR <= count_expr->tyty && count_expr->tyty <= TYPE_ULONG)) {
                notif("Array size is not of an integral type");
                return NULL;
            }

            struct slot slot = {.ty= &adptb_ulong_type};
            _alloc_slot(&gs->comp, &slot);
            _fit_expr_to_slot(&gs->comp, ty->info.arr.count, &slot);

            switch (slot.usage) {
            case _slot_value:
                count = slot.as.value.ul;
                break;

            case _slot_used:
                run(&gs->runr, gs->comp.res);
                count = *(size_t*)(gs->runr.stack+gs->runr.sp);
                gs->comp.res.len = plen;
                gs->runr.sp = psp;
                break;

            case _slot_variable:
                run(&gs->runr, gs->comp.res);
                count = *(size_t*)(gs->runr.stack+slot.as.variable);
                gs->comp.res.len = plen;
                gs->runr.sp = psp;
                break;
            }
        } else exitf("NIY: inferred length (or it's a function param, but those are not handled correctly at many more level anyways)");

        if (!(r = dyarr_push(&gs->ty_work))) exitf("OOM");
        return memcpy(r, &(struct adpt_type){
                .size= item->size*count,
                .align= item->align,
                .tyty= TYPE_ARR,
                .info.arr= {
                    .item= item,
                    .count= count,
                },
            }, sizeof*r);
    } // switch kind

    return NULL;
}
// }}}

// accept parsed input {{{
void accept_decl(void ref usr, declaration cref decl, bufsl ref tok)
{
    cintre_state ref gs = usr;
    (void)tok;

    if (!decl->name.len) {
        notif("Declaration does not declare anything");
        return;
    }
    if (gs->comp.lookup(gs->comp.usr, decl->name)) {
        notif("Name already exits, no shadowing for now");
        return;
    }

    size_t const pwork = gs->ty_work.len;
    struct adpt_type cref ty = decl_to_adpt_type(gs, &decl->type);
    if (!ty || !ty->size) {
        if (!ty) notif("Could not understand type");
        else notif("Zero-sized variable type");
        gs->ty_work.len = pwork;
        return;
    }

    int kind = ITEM_VARIABLE;
    switch (decl->spec) {
    case SPEC_TYPEDEF:
        kind = ITEM_TYPEDEF;
        break;

    case SPEC_EXTERN:
        notif("`extern` in declaration, nothing declared");
        return;

    case SPEC_STATIC:
        notif("`static` ignored in declaration"); if (0)
    case SPEC_AUTO:
        notif("`auto` ignored in declaration"); if (0)
    case SPEC_REGISTER:
        notif("`register` ignored in declaration");
        // fall through
    case SPEC_NONE:
        //size_t end = gs->sp;
        gs->runr.sp = ((gs->runr.sp-ty->size) / ty->align) * ty->align;
    }

    struct adpt_item ref it = dyarr_push(&gs->locs);
    if (!it) exitf("OOM");
    char* name = malloc(decl->name.len+1);
    if (!name) free(it), exitf("OOM");
    name[decl->name.len] = '\0';
    memcpy(it, &(struct adpt_item){
            .name= memcpy(name, decl->name.ptr, decl->name.len),
            .type= ty,
            .kind= kind,
            .as.variable= gs->runr.sp, // (yyy: `.as` not used when `ITEM_TYPEDEF`)
        }, sizeof *it);
}

void accept_expr(void ref usr, expression ref expr, bufsl ref tok)
{
    cintre_state ref gs = usr;

    // NOTE: thinking about moving this xcmd stuff in its own function, in
    //       a way it could be called from accept_decl (and maybe even
    //       accept_sttm, idk that doesn't make sense but all this isn't
    //       devised yest)
    char cref xcmd = tok->len && ';' == *tok->ptr ? tok->ptr+1+strspn(tok->ptr+1, " \t") : "";
#   define xcmdis(s)  (!memcmp(s, xcmd, strlen(s)))

    if (xcmdis("h")) {
        printf("List of commands:\n");
        printf("   h[elp]                  -  print this help and no more\n");
        printf("   loc[ales]               -  list local names\n");
        printf("   names[paces] or ns      -  list names in namespace\n");
        printf("   sta[cktop]              -  top of the stack, ie everything allocated onto it\n");
        printf("   cls[tack]               -  clear the stack (set sp back to top) and locals\n");
        printf("   ast                     -  ast of the expression\n");
        printf("   ty[pe]                  -  type of the expression, eg. `strlen; ty`\n");
        printf("   bytec[ode] or bc        -  internal bytecode from compilation\n");
        printf("no command after the ; (or no ;) will simply execute the expression\n");
        return;
    }

    if (xcmdis("loc")) {
        printf("List of locals:\n");
        size_t const sz = sizeof gs->runr.stack; // (xxx: sizeof stack)
        for (size_t k = 0; k < gs->locs.len; k++) {
            struct adpt_item cref it = gs->locs.ptr+k;
            if (ITEM_TYPEDEF == it->kind) printf("   [typedef] %-8s\t", it->name);
            else printf("   [top-%zu] %-8s\t", sz-it->as.variable, it->name);
            print_type(stdout, it->type);
            printf("\n");
        }
        return;
    }

    if (xcmdis("names") || xcmdis("ns")) {
        char cref end = xcmd+strcspn(xcmd, " \t");
        char cref name = end+strspn(end, " \t");
        if (end == name) {
            printf("List of namespaces:\n");
            for (size_t ns = 0; ns < countof(namespaces); ns++)
                printf("   %s (%zu names)\n", namespaces[ns].name, namespaces[ns].count);
            return;
        }
        for (size_t ns = 0; ns < countof(namespaces); ns++) if (!strcmp(namespaces[ns].name, name)) {
            printf("List of names in %s:\n", namespaces[ns].name);
            for (size_t k = 0; k < namespaces[ns].count; k++) {
                struct adpt_item cref it = namespaces[ns].items+k;
                if (ITEM_TYPEDEF == it->kind) printf("   [typedef] %-8s\t", it->name);
                else printf("   [%p] %-8s\t", it->as.object, it->name);
                print_type(stdout, it->type);
                printf("\n");
            }
            return;
        }
        printf("No such namespace '%s'\n", name);
        return;
    }

    if (xcmdis("sta")) {
        size_t const sz = sizeof gs->runr.stack; // (xxx: sizeof stack)
        printf("Stack top %p (sp= %zx /%zx @-%zu):\n", gs->runr.stack, gs->runr.sp, sz, sz-gs->runr.sp);
        print_tops(stdout, &gs->runr, gs->locs.ptr, gs->locs.len);
        return;
    }

    if (xcmdis("cls")) {
        size_t const sz = sizeof gs->runr.stack; // (xxx: sizeof stack)
        gs->runr.sp = sz;
        for (size_t k = 0; k < gs->locs.len; k++)
            free((void*)gs->locs.ptr[k].name); // yyy: cast const
        gs->locs.len = 0;
        // xxx: annoying, don't like this random thing here, same vibe as chk_work
        for (size_t k = 0; k < gs->comp.chk_interned.len; k++)
            free(gs->comp.chk_interned.ptr[k].ptr);
        gs->comp.chk_interned.len = 0;
        return;
    }

    if (xcmdis("ast")) {
        printf("AST of the expression:\n");
        print_expr(stdout, expr, 0);
        return;
    }

    if (!expr) return;
    size_t const psp = gs->comp.vsp = gs->runr.sp;
    gs->comp.res.len = 0;

    if (xcmdis("ty")) {
        gs->comp.chk_work.len = 0; // xxx: annoying
        struct adpt_type cref ty = check_expression(&gs->comp, expr);
        gs->runr.sp = psp; // yyy: free string/comp literals
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

    gs->runr.sp+= res.type->size; // yyy: free only result (keeps lits, bit loose tho..)
} // accept_expr
// }}}

int main(void)
{
    printf("Type `;help` for a list of command\n");

    static cintre_state _gs = {
        .lexr= {.file= "<input>"},
        .decl= {.ls= &_gs.lexr, .usr= &_gs, .on= accept_decl},
        .expr= {.ls= &_gs.lexr, .usr= &_gs, .on= accept_expr},
        .comp= {.usr= &_gs, .lookup= lookup},
        .runr= {.sp= sizeof _gs.runr.stack}, // (xxx: sizeof stack)
    };
    cintre_state ref gs = &_gs;

    char* line = NULL;
    while (prompt("\1\x1b[35m\2(*^^),u~~\1\x1b[m\2 ", &line)) {
        gs->lexr.line++;
        gs->lexr.slice.len = strlen(gs->lexr.slice.ptr = line);

        bufsl tok = lext(&gs->lexr);
        if (!tok.len || ';' == *tok.ptr) accept_expr(gs->expr.usr, NULL, &tok);

        else if (is_decl_keyword(gs, tok)) {
            gs->decl.base = (declaration){0};

            do {
                tok = parse_declaration(&gs->decl, tok);

                if (1 == tok.len && '=' == *tok.ptr) {
                    // XXX: lexer_recycle
                    gs->lexr.slice.ptr--, gs->lexr.slice.len++;
                    char cref name = dyarr_top(&gs->locs)->name;
                    gs->expr.disallow_comma = true;
                    tok = parse_expression(&gs->expr, (bufsl){.ptr= name, .len= strlen(name)});
                    gs->expr.disallow_comma = false;
                }
            } while (1 == tok.len && ',' == *tok.ptr ? tok = lext(&gs->lexr), true : false);
        }

        else parse_expression(&gs->expr, tok);
    }

    free_cintre_state(gs);

    return EXIT_SUCCESS;
}
