/// `int main()` of the REPL; typically included as the entry point in the
/// <c-main.c> file generated from `$ preparer -m`:
/// ```c
/// #include "build/a-standard.h"
///
/// static struct ct_adpt_namespace const namespaces[] = {
///     {.name= "standard", .count= countof(adptns_standard), .items= adptns_standard},
/// };
///
/// #define CINTRE_NAMESPACES_DEFINED namespaces
/// #include "cintre.c"
/// ```
///
/// see `$ preparer -h`
///
/// if `CINTRE_NAMESPACES_DEFINED` is defined, then the following should hold:
/// - `sizeof(CINTRE_NAMESPACES_DEFINED)/sizeof*(CINTRE_NAMESPACES_DEFINED)`
///   is the number of namespaces
/// - `CINTRE_NAMESPACES_DEFINED` can be cohersed to a `struct ct_adpt_namespace*`
///
/// eg:
/// ```c
/// static struct ct_adpt_namespace const CINTRE_NAMESPACES_DEFINED[] = { /*-*/ };
/// ```
///
/// if `USE_READLINE` is defined, readline will be used

#include "common.h"
#include "parser.h"
#include "adapter.h"
#include "compiler.h"
#include "runner.h"
#include "prints.h"

typedef struct ct_cintre_state {
    ct_lex_state lexr;
    ct_parse_decl_state decl;
    ct_parse_expr_state expr;
    ct_compile_state comp;
    ct_run_state runr;

    ct_dyarr(struct ct_adpt_item) locs;
    struct {
        size_t const count;
        struct ct_adpt_namespace cref spaces;
    } nsps;

    FILE* save;
    ct_dyarr(struct snap {
        char name[8];
        ct_run_state stack;
        ct_dyarr(struct ct_adpt_item) locs;
        ct_dyarr(ct_buf) chk_interned; // xxx: annoying
    }) snaps;

    // needed for `decl_to_adpt_type`
    ct_dyarr(struct ct_adpt_type) ty_work;
} ct_cintre_state;

bool _ct_compile_expression_tmp_wrap(ct_compile_state ref cs, ct_expression ref expr)
{
    cs->chk_work.len = 0; // xxx: annoying
    struct ct_slot slot = {.ty= ct_check_expression(cs, expr)};
    if (!slot.ty) return false;
    _ct_alloc_slot(cs, &slot);

    ct_compile_expression(cs, expr, &slot);
    switch (slot.usage) {
    case _slot_value:
        _ct_emit_data(cs, slot.loc-cs->vsp, slot.ty->size, slot.as.value.bytes);
        break;

    case _slot_used:
        break;

    case _slot_variable:
        _ct_emit_move(cs, slot.loc-cs->vsp, slot.ty->size, slot.as.variable-cs->vsp);
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
static struct ct_cintre_state const* _ct_prompt_rl_gs;
void _ct_prompt_cc(int sigint)
{
    rl_crlf();
    rl_end_of_history(0, 0);
    rl_beg_of_line(0, 0);
    rl_kill_line(0, 0);
    rl_forced_update_display();
    signal(sigint, _ct_prompt_cc);
}
void _ct_prompt_list_compl(char** const matches, int const num_matches, int const max_length)
{
    if (rl_completion_query_items < num_matches) {
        rl_crlf();
        printf("Display all %d possibilities? (y or n)", num_matches);
        fflush(stdout);
        if ('y' != (rl_read_key()|32)) {
            rl_crlf();
            rl_forced_update_display();
            return;
        }
    }
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
            else {
                bool all_cap = true;
                for (unsigned k = 0; k < len; k++) if (!(all_cap = '_' == it[k] || ('A' <= it[k] && it[k] <= 'Z'))) break;
                if (all_cap) printf("\x1b[34m%-*s\x1b[m", max_length+2, it);
                else printf("%-*s", max_length+2, it);
            }
        }
        rl_crlf();
    }
    rl_forced_update_display();
}
char* _ct_prompt_compl(char cref text, int const state)
{
    rl_completion_suppress_append = 1; // yyy: disable the automatic trailing space
    if (rl_completion_found_quote) {
        rl_completion_display_matches_hook = NULL;
        return rl_filename_completion_function(text, state);
    } else rl_completion_display_matches_hook = _ct_prompt_list_compl;
    static size_t ns, k;
    static bool is_xcmd = false;
    if (0 == state) {
        for (int p = rl_point; p && !(is_xcmd = ';' == rl_line_buffer[p-1]); p--);
        ns = k = 0;
    }
    size_t const len = strlen(text);
    // todo: could have completion based on type
    //if (CT_TYPE_STRUCT || CT_TYPE_UNION) .. rl_point, rl_line_buffer ..;
    if (is_xcmd) {
        static char cref cmds[] = {"help", "locales", "namespaces", "stacktop", "clstack", "save \"", "load \"", "ast", "type", "bytecode"};
        while (k < countof(cmds)) if (!memcmp(cmds[k++], text, len)) {
            size_t const len = strlen(cmds[k-1]);
            char ref r = malloc(len+2);
            if (!r) return NULL;
            strcpy(r, cmds[k-1]);
            return r;
        }
        return NULL;
    } // else
    ct_cintre_state cref gs = _ct_prompt_rl_gs;
    for (; ns < gs->nsps.count+1; ns++, k = 0)
        for (; k < (!ns ? gs->locs.len : gs->nsps.spaces[ns-1].count); k++) {
            struct ct_adpt_item cref it = !ns ? gs->locs.ptr+k : gs->nsps.spaces[ns-1].items+k;
            if (!memcmp(it->name, text, len)) {
                size_t const len = strlen(it->name);
                bool const tdf = CT_ITEM_TYPEDEF == it->kind;
                bool const fun = CT_TYPE_FUN == it->type->tyty || (CT_TYPE_PTR == it->type->tyty && CT_TYPE_FUN == it->type->info.ptr->tyty); // xxx: || ...
                bool const arr = CT_TYPE_ARR == it->type->tyty;
                char ref r = malloc(len + (tdf|fun|arr));
                if (!r) return NULL;
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
bool ct_prompt(char const* ct_prompt, char** res, ct_cintre_state cref gs)
{
    if (!*res) {
        rl_readline_name = "Cintre";
        rl_completer_word_break_characters = " 0123456789{}[]()<>%:;.?*+-/^&|~!=,";
        rl_completer_quote_characters = "\"'";
        rl_completion_entry_function = _ct_prompt_compl;
        read_history(hist_file);
        signal(SIGINT, _ct_prompt_cc);
    } else free(*res);
    _ct_prompt_rl_gs = gs;
    if (!(*res = readline(ct_prompt))) {
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
bool ct_prompt(char const* ct_prompt, char** res, ct_cintre_state cref gs)
{
    (void)gs;
    static char r[1024];
    *res = r;
    printf("%s", ct_prompt);
    if (!fgets(r, sizeof r, stdin)) return false;
    r[strlen(r)-1] = '\0';
    return true;
}
#endif
// }}}

void ct_cintre_cleanup(ct_cintre_state ref gs)
{
    ct_ldel(&gs->lexr);

    for (size_t k = 0; k < gs->locs.len; k++)
        free((void*)gs->locs.ptr[k].name);
    free(gs->locs.ptr);

    for (size_t k = 0; k < gs->ty_work.len; k++) switch (gs->ty_work.ptr[k].tyty) {
    case CT_TYPE_STRUCT:
    case CT_TYPE_UNION:;
        struct ct_adpt_comp_desc cref comp = &gs->ty_work.ptr[k].info.comp;
        for (size_t kk = 0; kk < comp->count; kk++) free((void*)comp->fields[kk].name);
        free((void*)comp->fields);
        break;
    case CT_TYPE_FUN:;
        struct ct_adpt_fun_desc cref fun = &gs->ty_work.ptr[k].info.fun;
        for (size_t kk = 0; kk < fun->count; kk++) free((void*)fun->params[kk].name);
        free((void*)fun->params);
        break;
    default:;
    }
    free(gs->ty_work.ptr);

    // xxx: annoying
    free(gs->comp.chk_work.ptr);
    for (size_t k = 0; k < gs->comp.chk_interned.len; k++)
        free(gs->comp.chk_interned.ptr[k].ptr);
    free(gs->comp.chk_interned.ptr);
}

// compile time (lookup and random helpers) {{{
struct ct_adpt_item const* ct_cintre_lookup(void* usr, ct_bufsl const name)
{
    ct_cintre_state cref gs = usr;
    // TODO: same idea as below, so make sure to insert them sorted
    for (size_t k = 0; k < gs->locs.len; k++) {
        struct ct_adpt_item cref it = gs->locs.ptr+k;
        if (bufis(name, it->name)) return it;
    }
    // TODO: names will be sorted (from preparer), use that to go n -> logn
    for (size_t ns = 0; ns < gs->nsps.count; ns++)
        for (size_t k = 0; k < gs->nsps.spaces[ns].count; k++) {
            struct ct_adpt_item cref it = gs->nsps.spaces[ns].items+k;
            if (bufis(name, it->name)) return it;
        }
    return NULL;
}

bool _ct_is_decl_keyword(ct_cintre_state cref gs, ct_bufsl const tok)
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

    struct ct_adpt_item cref it = gs->comp.lookup(gs->comp.usr, tok);
    return it && CT_ITEM_TYPEDEF == it->kind;
}

/// allocates using `gs->ty_work`; on failure, it is safe to set the length of
/// it back to what it was to free wip temporaries, it should not cause a leak
struct ct_adpt_type const* _ct_decl_to_adpt_type(ct_cintre_state ref gs, struct ct_decl_type cref ty)
{
    switch (ty->kind) {
        struct ct_adpt_type* r;

    case CT_KIND_NOTAG:;
        bool _signed = false, _unsigned = false, _short = false, _long = false;
        for (size_t k = 0; CT_QUAL_END != ty->quals[k]; k++) switch (ty->quals[k]) {
            case CT_QUAL_SIGNED:    _signed = true;          break;
            case CT_QUAL_UNSIGNED:  _unsigned = true;        break;
            case CT_QUAL_SHORT:     _short = true;           break;
            case CT_QUAL_LONG:      _long = true;            break;
            case CT_QUAL_COMPLEX:   exitf("NIY: complex");   break;
            case CT_QUAL_IMAGINARY: exitf("NIY: imaginary"); break;
            default:;
        }

#       define nameis(s)  (strlen(s) == ty->name.len && !memcmp(s, ty->name.ptr, strlen(s)))
        if (nameis("char"))
            return _signed ? &ct_adptb_schar_type
                 : _unsigned ? &ct_adptb_uchar_type
                 : &ct_adptb_char_type;
        if (nameis("int") || !ty->name.len)
            return _short ? (_unsigned ? &ct_adptb_ushort_type : &ct_adptb_short_type)
                 : _long ? (_unsigned ? &ct_adptb_ulong_type : &ct_adptb_long_type)
                 : _unsigned ? &ct_adptb_uint_type : &ct_adptb_int_type;
        if (nameis("float"))  return &ct_adptb_float_type;
        if (nameis("double")) return &ct_adptb_double_type;
        if (nameis("void"))   return &ct_adptb_void_type;
#       undef nameis

        struct ct_adpt_item cref it = gs->comp.lookup(gs->comp.usr, ty->name);
        if (it && CT_ITEM_TYPEDEF == it->kind) return it->type;
        return NULL;

    case CT_KIND_STRUCT:
    case CT_KIND_UNION:
        if (-1ul == ty->info.comp.count) {
            char copy[ty->name.len+1]; // yyy: va
            memcpy(copy+1, ty->name.ptr, ty->name.len);
            copy[0] = '@';
            struct ct_adpt_item cref it = gs->comp.lookup(gs->comp.usr, (ct_bufsl){.ptr= copy, .len= sizeof copy});
            if (it && CT_ITEM_TYPEDEF == it->kind) return it->type;
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
        return memcpy(r, &(struct ct_adpt_type){
                .size= 0,
                .align= 0,
                .tyty= CT_KIND_STRUCT == ty->kind ? CT_TYPE_STRUCT : CT_TYPE_UNION,
                .info.comp = {
                    .fields= NULL,
                    .count= 0,
                },
            }, sizeof*r);

    case CT_KIND_ENUM: return &ct_adptb_int_type;

    case CT_KIND_PTR:;
        struct ct_adpt_type cref ptr = _ct_decl_to_adpt_type(gs, &ty->info.ptr->type);
        if (!ptr) return NULL;

        if (!(r = dyarr_push(&gs->ty_work))) exitf("OOM");
        return memcpy(r, &(struct ct_adpt_type){
                .size= sizeof(void*),
                .align= sizeof(void*),
                .tyty= CT_TYPE_PTR,
                .info.ptr= ptr,
            }, sizeof*r);

    case CT_KIND_FUN:
        if (-1ul == ty->info.fun.count) return NULL;

        struct ct_adpt_type cref ret = _ct_decl_to_adpt_type(gs, &ty->info.fun.ret->type);
        if (!ret) return NULL;

        struct ct_adpt_fun_param* const params = malloc(ty->info.fun.count*sizeof*params);
        if (!params) exitf("OOM");

        size_t k = 0;
        for (struct ct_decl_type_param const* curr = ty->info.fun.first; curr; curr = curr->next, k++) {
            struct ct_adpt_type cref type = _ct_decl_to_adpt_type(gs, &curr->decl->type);
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

            memcpy(params+k, &(struct ct_adpt_fun_param){.name= name, .type= type}, sizeof*params);
        }

        if (!(r = dyarr_push(&gs->ty_work))) exitf("OOM");
        return memcpy(r, &(struct ct_adpt_type){
                .size= sizeof(void(*)()),
                .align= sizeof(void(*)()),
                .tyty= CT_TYPE_FUN,
                .info.fun= {
                    .ret= ret,
                    .params= params,
                    .count= ty->info.fun.count,
                },
            }, sizeof*r);

    case CT_KIND_ARR:;
        struct ct_adpt_type cref item = _ct_decl_to_adpt_type(gs, &ty->info.arr.item->type);
        if (!item) return NULL;

        size_t count = 0;
        if (ty->info.arr.count) {
            size_t const plen = gs->comp.res.len;
            size_t const psp = gs->runr.sp;

            // TODO: once CT_UNOP_CAST is added, use it by wrapping `ty->info.arr.count`
            struct ct_adpt_type cref count_expr = ct_check_expression(&gs->comp, ty->info.arr.count);
            if (!count_expr) return NULL;
            if (!(CT_TYPE_CHAR <= count_expr->tyty && count_expr->tyty <= CT_TYPE_ULONG)) {
                notif("Array size is not of an integral type");
                return NULL;
            }

            struct ct_slot slot = {.ty= &ct_adptb_ulong_type};
            _ct_alloc_slot(&gs->comp, &slot);
            _ct_fit_expr_to_slot(&gs->comp, ty->info.arr.count, &slot);

            switch (slot.usage) {
            case _slot_value:
                count = slot.as.value.ul;
                break;

            case _slot_used:
                ct_run(&gs->runr, gs->comp.res);
                count = *(size_t*)(gs->runr.stack+gs->runr.sp);
                gs->comp.res.len = plen;
                gs->runr.sp = psp;
                break;

            case _slot_variable:
                ct_run(&gs->runr, gs->comp.res);
                count = *(size_t*)(gs->runr.stack+slot.as.variable);
                gs->comp.res.len = plen;
                gs->runr.sp = psp;
                break;
            }
        } else exitf("NIY: inferred length (or it's a function param, but those are not handled correctly at many more level anyways)");

        if (!(r = dyarr_push(&gs->ty_work))) exitf("OOM");
        return memcpy(r, &(struct ct_adpt_type){
                .size= item->size*count,
                .align= item->align,
                .tyty= CT_TYPE_ARR,
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
void ct_accept_decl(void ref usr, ct_declaration cref decl, ct_bufsl ref tok)
{
    ct_cintre_state ref gs = usr;
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
    struct ct_adpt_type cref ty = _ct_decl_to_adpt_type(gs, &decl->type);
    if (!ty || !ty->size) {
        if (!ty) notif("Could not understand type");
        else notif("Zero-sized variable type");
        gs->ty_work.len = pwork;
        return;
    }

    int kind = CT_ITEM_VARIABLE;
    switch (decl->spec) {
    case CT_SPEC_TYPEDEF:
        kind = CT_ITEM_TYPEDEF;
        break;

    case CT_SPEC_EXTERN:
        notif("`extern` in declaration, nothing declared");
        return;

    case CT_SPEC_STATIC:
        notif("`static` ignored in declaration"); if (0)
    case CT_SPEC_AUTO:
        notif("`auto` ignored in declaration"); if (0)
    case CT_SPEC_REGISTER:
        notif("`register` ignored in declaration");
        // fall through
    case CT_SPEC_NONE:
        //size_t end = gs->sp;
        gs->runr.sp = ((gs->runr.sp-ty->size) / ty->align) * ty->align;
    }

    struct ct_adpt_item ref it = dyarr_push(&gs->locs);
    if (!it) exitf("OOM");
    char* name = malloc(decl->name.len+1);
    if (!name) free(it), exitf("OOM");
    name[decl->name.len] = '\0';
    memcpy(it, &(struct ct_adpt_item){
            .name= memcpy(name, decl->name.ptr, decl->name.len),
            .type= ty,
            .kind= kind,
            .as.variable= gs->runr.sp, // (yyy: `.as` not used when `CT_ITEM_TYPEDEF`)
        }, sizeof *it);
}

void ct_accept_expr(void ref usr, ct_expression ref expr, ct_bufsl ref tok)
{
    ct_cintre_state ref gs = usr;

    ct_bufsl xcmd = {0};
    if (1 == tok->len && ';' == *tok->ptr) {
        xcmd = ct_lext(&gs->lexr);
        // yyy: hackish, this is for the gs->save (end of main repl)
        //      it assumes the ';' is part of the actual input
        //      (ie. not from macro, etc..)
        *(char*)tok->ptr = '\0';
    }
#   define xcmdis(s)  (xcmd.len && !memcmp(s, xcmd.ptr, strlen(s)))

    if (xcmdis("h")) {
        printf("List of commands:\n");
        printf("   h[elp]                  -  print this help and no more\n");
        printf("   loc[ales]               -  list local names\n");
        printf("   names[paces] or ns      -  list names in namespace\n");
        printf("   sta[cktop]              -  top of the stack, ie everything allocated onto it\n");
        printf("   cls[tack]               -  clear the stack (set sp back to top) and locals\n");
        printf("   save \"file\"           -  save each next lines to the file\n");
        printf("   load \"file\"           -  load the file, running each lines\n");
        printf("   qs[ave]                 -  save a snapshot of the state\n");
        printf("   ql[oad]                 -  load a snapshot of the state\n");
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
            struct ct_adpt_item cref it = gs->locs.ptr+k;
            if (CT_ITEM_TYPEDEF == it->kind) printf("   [typedef] %-8s\t", it->name);
            else printf("   [top-%zu] %-8s\t", sz-it->as.variable, it->name);
            ct_print_type(stdout, it->type, true);
            printf("\n");
        }
        return;
    }

    if (xcmdis("names") || xcmdis("ns")) {
        ct_bufsl const name = ct_lext(&gs->lexr);
        if (!name.len) {
            printf("List of namespaces:\n");
            for (size_t ns = 0; ns < gs->nsps.count; ns++)
                printf("   %s (%zu names)\n", gs->nsps.spaces[ns].name, gs->nsps.spaces[ns].count);
            return;
        }
        for (size_t ns = 0; ns < gs->nsps.count; ns++) if (!memcmp(gs->nsps.spaces[ns].name, name.ptr, name.len)) {
            printf("List of names in %s:\n", gs->nsps.spaces[ns].name);
            for (size_t k = 0; k < gs->nsps.spaces[ns].count; k++) {
                struct ct_adpt_item cref it = gs->nsps.spaces[ns].items+k;
                switch (it->kind) {
                case CT_ITEM_VALUE:   printf("   [=%li] %-8s\t", it->as.value, it->name); break;
                case CT_ITEM_OBJECT:  printf("   [%p] %-8s\t", it->as.object, it->name);  break;
                case CT_ITEM_TYPEDEF: printf("   [typedef] %-8s\t", it->name);            break;
                default: printf("   ??? %-8s\t", it->name);
                }
                ct_print_type(stdout, it->type, true);
                printf("\n");
            }
            return;
        }
        printf("No such namespace '%.*s'\n", bufmt(name));
        return;
    }

    if (xcmdis("sta")) {
        size_t const sz = sizeof gs->runr.stack; // (xxx: sizeof stack)
        printf("Stack top %p (sp= %zx /%zx @-%zu):\n", gs->runr.stack, gs->runr.sp, sz, sz-gs->runr.sp);
        ct_print_tops(stdout, &gs->runr, gs->locs.ptr, gs->locs.len);
        return;
    }

    if (xcmdis("cls")) {
        size_t const sz = sizeof gs->runr.stack; // (xxx: sizeof stack)
        gs->runr.sp = sz;
        for (size_t k = 0; k < gs->locs.len; k++) free((void*)gs->locs.ptr[k].name); // yyy: cast const
        gs->locs.len = 0;
        // xxx: annoying, don't like this random thing here, same vibe as chk_work
        for (size_t k = 0; k < gs->comp.chk_interned.len; k++) free(gs->comp.chk_interned.ptr[k].ptr);
        gs->comp.chk_interned.len = 0;
        return;
    }

    // XXX: i hate it and it doesn't even work
    if (xcmdis("qs") || xcmdis("ql")) {
        ct_bufsl name = ct_lext(&gs->lexr);
        if (!name.len) name.len = 1, name.ptr = "_";
        else if (7 < name.len) name.len = 7;
        struct snap* found = NULL;
        for (size_t k = 0; k < gs->snaps.len; k++) if (memcmp(gs->snaps.ptr[k].name, name.ptr, name.len)) {
            found = gs->snaps.ptr+k;
            break;
        }
        if (!found) {
            if ('s' == xcmd.ptr[1]) {
                found = dyarr_push(&gs->snaps);
                if (!found) exitf("OOM");
                memcpy(found->name, name.ptr, name.len);
                found->name[name.len] = '\0';
                found->locs.ptr = NULL, found->locs.cap = 0;
                found->chk_interned.ptr = NULL, found->chk_interned.cap = 0;
            } else {
                printf("No state named '%.*s'\n", bufmt(name));
                return;
            }
        }
        for (size_t k = 0; k < found->locs.len; k++) free((void*)found->locs.ptr[k].name); // yyy: cast const
        free(found->locs.ptr);
        for (size_t k = 0; k < found->chk_interned.len; k++) free(found->chk_interned.ptr[k].ptr);
        free(found->chk_interned.ptr);
        if ('s' == xcmd.ptr[1]) {
            found->stack = gs->runr;
            if (!((found->locs.len = gs->locs.len) < found->locs.cap || dyarr_resize(&found->locs, gs->locs.len))) exitf("OOM");
            for (size_t k = 0; k < gs->locs.len; k++) *(char**)found->locs.ptr[k].name = strcpy(malloc(strlen(gs->locs.ptr[k].name)+1), gs->locs.ptr[k].name); // yyy: cast const
            if (!((found->chk_interned.len = gs->comp.chk_interned.len) < found->chk_interned.cap || dyarr_resize(&found->chk_interned, gs->comp.chk_interned.len))) exitf("OOM");
            for (size_t k = 0; k < gs->comp.chk_interned.len; k++) dyarr_cpy(&found->chk_interned.ptr[k], &gs->comp.chk_interned.ptr[k]);
        } else {
            gs->runr = found->stack;
            if (!((gs->locs.len = found->locs.len) < gs->locs.cap || dyarr_resize(&gs->locs, found->locs.len))) exitf("OOM");
            for (size_t k = 0; k < found->locs.len; k++) *(char**)gs->locs.ptr[k].name = strcpy(malloc(strlen(found->locs.ptr[k].name)+1), found->locs.ptr[k].name); // yyy: cast const
            if (!((gs->comp.chk_interned.len = found->chk_interned.len) < gs->comp.chk_interned.cap || dyarr_resize(&gs->comp.chk_interned, found->chk_interned.len))) exitf("OOM");
            for (size_t k = 0; k < found->chk_interned.len; k++) dyarr_cpy(&gs->comp.chk_interned.ptr[k], &found->chk_interned.ptr[k]);
        }
    }

    if (xcmdis("save") || xcmdis("load")) {
        ct_bufsl file = ct_lext(&gs->lexr);
        if (!file.len || '"' != *file.ptr) printf("Expected a name for the file to %.4s\n", xcmd.ptr);
        else {
            file.len--, file.ptr++;
            file.len-= '"' == file.ptr[file.len-1];
            char filename[file.len+1]; // xxx: va
            memcpy(filename, file.ptr, file.len);
            filename[file.len] = '\0';
            FILE ref f = fopen(filename, 's' == *xcmd.ptr ? "w" : "r");
            if (!f) printf("Could not open file '%s'\n", filename);
            else {
                if ('s' == *xcmd.ptr) {
                    if (gs->save) fclose(gs->save);
                    gs->save = f;
                } else notif("NIY: run loaded file");
            }
        }
        return;
    }

    if (xcmdis("ast")) {
        printf("AST of the expression:\n");
        ct_print_expr(stdout, expr, 0);
        return;
    }

    if (!expr) return;
    size_t const psp = gs->comp.vsp = gs->runr.sp;
    gs->comp.res.len = 0;

    if (xcmdis("ty")) {
        gs->comp.chk_work.len = 0; // xxx: annoying
        struct ct_adpt_type cref ty = ct_check_expression(&gs->comp, expr);
        gs->runr.sp = psp; // yyy: free string/comp literals
        if (!ty) return;
        printf("Expression is of type: ");
        ct_print_type(stdout, ty, true);
        printf("\n");
        return;
    }

    bool const r = _ct_compile_expression_tmp_wrap(&gs->comp, expr);
    if (!r) return;

    if (xcmdis("bytec") || xcmdis("bc")) {
        printf("Resulting bytecode (%zuB):\n", gs->comp.res.len);
        ct_print_code(stdout, gs->comp.res);
        return;
    }

    ct_run(&gs->runr, gs->comp.res);
    struct ct_adpt_item const res = {
        .name= "_",
        .type= expr->usr,
        .kind= CT_ITEM_VARIABLE,
        .as.variable= gs->runr.sp,
    };

    printf("Result:\n");
    ct_print_item(stdout, &res, gs->runr.stack, 0);

    gs->runr.sp+= res.type->size; // yyy: free only result (keeps lits, bit loose tho, some alignment padding sticks..)
} // ct_accept_expr
// }}}

int main(int argc, char cref* argv)
{
    static ct_cintre_state _gs = {
        .lexr= {.file= "<input>"},
        .decl= {.ls= &_gs.lexr, .usr= &_gs, .on= ct_accept_decl},
        .expr= {.ls= &_gs.lexr, .usr= &_gs, .on= ct_accept_expr},
        .comp= {.usr= &_gs, .lookup= ct_cintre_lookup},
        .runr= {.sp= sizeof _gs.runr.stack}, // (xxx: sizeof stack)
#ifdef CINTRE_NAMESPACES_DEFINED
        .nsps= {.count= countof(CINTRE_NAMESPACES_DEFINED), .spaces= CINTRE_NAMESPACES_DEFINED},
#endif
    };
    ct_cintre_state ref gs = &_gs;

    char cref prog = (argc--, *argv++);
    while (0 < argc) if ('-' == **argv) switch ((argc--, *argv++)[1]) {

    case 'h':
        printf("Usage: %s [-f file]\n", prog);
        return 1;

    case 'f':
        gs->save = fopen((argc--, *argv++), "r+");
        if (!gs->save) {
            gs->save = fopen(argv[-1], "w");
            if (!gs->save) printf("Could not opent file '%s'\n", argv[-1]);
        } else notif("NIY: run loaded file");
        break;

    } else {
        printf("Unexpected argument %s, see -h, continuing", *argv);
        break;
    }

    printf("Type `;help` for a list of command\n");

    char* line = NULL;
    while (ct_prompt("\1\x1b[35m\2(*^^),u~~\1\x1b[m\2 ", &line, gs)) {
        gs->lexr.line++;
        gs->lexr.slice.len = strlen(gs->lexr.slice.ptr = line);

        ct_bufsl tok = ct_lext(&gs->lexr);
        if (!tok.len || ';' == *tok.ptr) ct_accept_expr(gs->expr.usr, NULL, &tok);

        else if (_ct_is_decl_keyword(gs, tok)) {
            gs->decl.base = (ct_declaration){0};

            do {
                tok = ct_parse_declaration(&gs->decl, tok);

                if (1 == tok.len && '=' == *tok.ptr) {
                    // XXX: lexer_recycle
                    gs->lexr.slice.ptr--, gs->lexr.slice.len++;
                    char cref name = dyarr_top(&gs->locs)->name;
                    gs->expr.disallow_comma = true;
                    tok = ct_parse_expression(&gs->expr, (ct_bufsl){.ptr= name, .len= strlen(name)});
                    gs->expr.disallow_comma = false;
                }
            } while (1 == tok.len && ',' == *tok.ptr ? tok = ct_lext(&gs->lexr), true : false);
        }

        else ct_parse_expression(&gs->expr, tok);

        if (gs->save && *line) fprintf(gs->save, "%s;\n", line);
    }

    ct_cintre_cleanup(gs);

    return EXIT_SUCCESS;
}
