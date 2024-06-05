/// `int main()` of the REPL; typically included as the entry point in the
/// <c-main.c> file generated from `$ preparer -m`:
/// ```c
/// #include "build/a-standard.h"
///
/// static struct adpt_namespace const namespaces[] = {
///     {.name= "standard", .count= countof(adptns_standard), .items= adptns_standard},
/// };
///
/// extern struct adpt_namespace cref namespaces_first = &namespaces[0];
/// extern unsigned long const namespaces_count = sizeof namespaces/sizeof*namespaces;
///
/// #include "cintre.c"
/// ```
///
/// see `$ preparer -h`
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
    struct {
        size_t count;
        struct adpt_namespace const* spaces;
    } nsps;

    FILE* save;
    dyarr(struct snap {
        char name[8];
        run_state stack;
        dyarr(struct adpt_item) locs;
        dyarr(buf) chk_interned; // xxx: annoying
    }) snaps;

    // needed for `decl_to_adpt_type`
    dyarr(struct adpt_type) ty_work;
} cintre_state;

#define gstokn(__at) (gs->lexr.tokens.ptr+(__at))

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
static struct cintre_state const* _prompt_rl_gs;
void _prompt_cc(int sigint)
{
    rl_crlf();
    rl_end_of_history(0, 0);
    rl_beg_of_line(0, 0);
    rl_kill_line(0, 0);
    rl_forced_update_display();
    signal(sigint, _prompt_cc);
}
void _prompt_list_compl(char** const matches, int const num_matches, int const max_length)
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
char* _prompt_compl(char cref text, int const state)
{
    rl_completion_suppress_append = 1; // yyy: disable the automatic trailing space
    if (rl_completion_found_quote) {
        rl_completion_display_matches_hook = NULL;
        return rl_filename_completion_function(text, state);
    } else rl_completion_display_matches_hook = _prompt_list_compl;
    static size_t ns, k;
    static bool is_xcmd = false;
    if (0 == state) {
        for (int p = rl_point; p && !(is_xcmd = ';' == rl_line_buffer[p-1]); p--);
        ns = k = 0;
    }
    size_t const len = strlen(text);
    // todo: could have completion based on type
    //if (TYPE_STRUCT || TYPE_UNION) .. rl_point, rl_line_buffer ..;
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
    cintre_state cref gs = _prompt_rl_gs;
    for (; ns < gs->nsps.count+1; ns++, k = 0)
        for (; k < (!ns ? gs->locs.len : gs->nsps.spaces[ns-1].count); k++) {
            struct adpt_item cref it = !ns ? gs->locs.ptr+k : gs->nsps.spaces[ns-1].items+k;
            if (!memcmp(it->name, text, len)) {
                size_t const len = strlen(it->name);
                bool const tdf = ITEM_TYPEDEF == it->kind;
                bool const fun = TYPE_FUN == _tailtype(it->type)->tyty;
                bool const arr = TYPE_ARR == _truetype(it->type)->tyty;
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
bool prompt(char const* prompt, char** res, cintre_state cref gs)
{
    if (!*res) {
        rl_readline_name = "Cintre";
        rl_completer_word_break_characters = " 0123456789{}[]()<>%:;.?*+-/^&|~!=,";
        rl_completer_quote_characters = "\"'";
        rl_completion_entry_function = _prompt_compl;
        read_history(hist_file);
        signal(SIGINT, _prompt_cc);
    } else free(*res);
    _prompt_rl_gs = gs;
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
bool prompt(char const* prompt, char** res, cintre_state cref gs)
{
    (void)gs;
    static char r[1024];
    *res = r;
    printf("%s", prompt);
    if (!fgets(r, sizeof r, stdin)) return false;
    r[strlen(r)-1] = '\0';
    return true;
}
#endif
// }}}

void cintre_cleanup(cintre_state ref gs)
{
    lex_free(&gs->lexr);

    for (size_t k = 0; k < gs->locs.len; k++)
        free((void*)gs->locs.ptr[k].name);
    free(gs->locs.ptr);

    if (gs->save) fclose(gs->save);

    for (size_t k = 0; k < gs->snaps.len; k++) {
        for (size_t kk = 0; kk < gs->snaps.ptr[k].locs.len; kk++)
            free((void*)gs->snaps.ptr[k].locs.ptr[kk].name);
        free(gs->snaps.ptr[k].locs.ptr);
        for (size_t kk = 0; kk < gs->snaps.ptr[k].chk_interned.len; kk++)
            free((void*)gs->snaps.ptr[k].chk_interned.ptr[kk].ptr);
        free(gs->snaps.ptr[k].chk_interned.ptr);
    }

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
        // noop cases
    case TYPE_VOID: case TYPE_CHAR: case TYPE_UCHAR: case TYPE_SCHAR: case TYPE_SHORT: case TYPE_INT: case TYPE_LONG: case TYPE_USHORT: case TYPE_UINT: case TYPE_ULONG: case TYPE_FLOAT: case TYPE_DOUBLE: case TYPE_PTR: case TYPE_ARR: case TYPE_NAMED:;
    }
    free(gs->ty_work.ptr);

    // xxx: annoying
    free(gs->comp.chk_work.ptr);
    for (size_t k = 0; k < gs->comp.chk_interned.len; k++)
        free(gs->comp.chk_interned.ptr[k].ptr);
    free(gs->comp.chk_interned.ptr);
}

// compile time (lookup and random helpers) {{{
struct adpt_item const* cintre_lookup(void* usr, char cref name)
{
    cintre_state cref gs = usr;
    // TODO: same idea as below, so make sure to insert them sorted
    for (size_t k = 0; k < gs->locs.len; k++) {
        struct adpt_item cref it = gs->locs.ptr+k;
        if (!strcmp(name, it->name)) return it;
    }
    // TODO: names will be sorted (from preparer), use that to go n -> logn
    for (size_t ns = 0; ns < gs->nsps.count; ns++)
        for (size_t k = 0; k < gs->nsps.spaces[ns].count; k++) {
            struct adpt_item cref it = gs->nsps.spaces[ns].items+k;
            if (!strcmp(name, it->name)) return it;
        }
    return NULL;
}

bool _is_decl_keyword(cintre_state cref gs, char cref tok)
{
    if (!strcmp("char",     tok) ||
        !strcmp("short",    tok) ||
        !strcmp("int",      tok) ||
        !strcmp("long",     tok) ||
        !strcmp("signed",   tok) ||
        !strcmp("unsigned", tok) ||
        !strcmp("float",    tok) ||
        !strcmp("double",   tok) ||
        !strcmp("void",     tok) ||
        !strcmp("struct",   tok) ||
        !strcmp("union",    tok) ||
        !strcmp("enum",     tok) ||
        !strcmp("typedef",  tok) ||
        !strcmp("extern",   tok) ||
        !strcmp("static",   tok) ||
        !strcmp("auto",     tok) ||
        !strcmp("register", tok) ||
        !strcmp("const",    tok) ) return true;

    struct adpt_item cref it = gs->comp.lookup(gs->comp.usr, tok);
    return it && ITEM_TYPEDEF == it->kind;
}

/// allocates using `gs->ty_work`; on failure, it is safe to set the length of
/// it back to what it was to free wip temporaries, it should not cause a leak
struct adpt_type const* _decl_to_adpt_type(cintre_state ref gs, struct decl_type cref ty)
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
            case QUAL_COMPLEX:   notif("NIY: complex");   return NULL;
            case QUAL_IMAGINARY: notif("NIY: imaginary"); return NULL;

                // noop cases
            case QUAL_END: case QUAL_CONST: case QUAL_RESTRICT: case QUAL_VOLATILE:;
            }

#       define nameis(s)  (!strcmp(s, gstokn(ty->name))) //(strlen(s) == ty->name.len && !memcmp(s, ty->name.ptr, strlen(s)))
        if (nameis("char"))
            return _signed ? &adptb_schar_type
                 : _unsigned ? &adptb_uchar_type
                 : &adptb_char_type;
        if (nameis("int")) // XXX: reachable? || !ty->name.len)
            return _short ? (_unsigned ? &adptb_ushort_type : &adptb_short_type)
                 : _long ? (_unsigned ? &adptb_ulong_type : &adptb_long_type)
                 : _unsigned ? &adptb_uint_type : &adptb_int_type;
        if (nameis("float"))  return &adptb_float_type;
        if (nameis("double")) return &adptb_double_type;
        if (nameis("void"))   return &adptb_void_type;
#       undef nameis

        struct adpt_item cref it = gs->comp.lookup(gs->comp.usr, gstokn(ty->name));
        if (it && ITEM_TYPEDEF == it->kind) return it->type;
        return NULL;

    case KIND_STRUCT:
    case KIND_UNION:
        if (-1ul == ty->info.comp.count) {
            char ref name = gstokn(ty->name);
            // xxx: lexer internal (the fact that we know we can)
            name[-1] = '@';
            struct adpt_item cref it = gs->comp.lookup(gs->comp.usr, name-1);
            name[-1] = '\0';
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

        r = dyarr_push(&gs->ty_work);
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
        struct adpt_type cref ptr = _decl_to_adpt_type(gs, &ty->info.ptr->type);
        if (!ptr) return NULL;

        r = dyarr_push(&gs->ty_work);
        return memcpy(r, &(struct adpt_type){
                .size= sizeof(void*),
                .align= sizeof(void*),
                .tyty= TYPE_PTR,
                .info.ptr= ptr,
            }, sizeof*r);

    case KIND_FUN:
        if (-1ul == ty->info.fun.count) return NULL;

        struct adpt_type cref ret = _decl_to_adpt_type(gs, &ty->info.fun.ret->type);
        if (!ret) return NULL;

        struct adpt_fun_param* const params = mallox(ty->info.fun.count*sizeof*params);

        size_t k = 0;
        for (struct decl_type_param const* curr = ty->info.fun.first; curr; curr = curr->next, k++) {
            struct adpt_type cref type = _decl_to_adpt_type(gs, &curr->decl->type);
            if (!type) {
                while (k--) free((void*)params[k].name);
                free(params);
                return NULL;
            }

            char* name = NULL;
            char cref decl_name = gstokn(curr->decl->name);
            if (*decl_name) strcpy(name = mallox(strlen(decl_name)+1), decl_name);

            memcpy(params+k, &(struct adpt_fun_param){.name= name, .type= type}, sizeof*params);
        }

        r = dyarr_push(&gs->ty_work);
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
        struct adpt_type cref item = _decl_to_adpt_type(gs, &ty->info.arr.item->type);
        if (!item) return NULL;

        size_t count = 0;
        if (ty->info.arr.count) {
            size_t const plen = gs->comp.res.len;
            size_t const psp = gs->runr.sp;

            expression cast = {
                .kind= UNOP_CAST,
                .info.cast= {
                    .opr= ty->info.arr.count,
                    .type= &(struct decl_type){.quals= {QUAL_LONG}},
                },
            };
            if (!check_expression(&gs->comp, &cast)) return NULL;

            struct slot slot = {.ty= &adptb_ulong_type};
            _alloc_slot(&gs->comp, &slot);
            _fit_expr_to_slot(&gs->comp, &cast, &slot);

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
        } else {
            notif("NIY: inferred length (or it's a function param, but those are not handled correctly at many more level anyways)");
            return NULL;
        }

        r = dyarr_push(&gs->ty_work);
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
void accept_decl(void ref usr, declaration cref decl, tokt ref tok)
{
    cintre_state ref gs = usr;
    (void)tok;

    char cref decl_name = gstokn(decl->name);

    if (!*decl_name) {
        notif("Declaration does not declare anything");
        return;
    }
    if (gs->comp.lookup(gs->comp.usr, decl_name)) {
        notif("Name already exits, no shadowing for now");
        return;
    }

    size_t const pwork = gs->ty_work.len;
    struct adpt_type cref ty = _decl_to_adpt_type(gs, &decl->type);
    struct adpt_type cref tty = _truetype(ty);
    if (!ty || !tty->size) {
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
        gs->runr.sp = ((gs->runr.sp-tty->size) / tty->align) * tty->align;
    }

    struct adpt_item ref it = dyarr_push(&gs->locs);
    memcpy(it, &(struct adpt_item){
            .name= strcpy(mallox(strlen(decl_name)+1), decl_name),
            .type= ty,
            .kind= kind,
            .as.variable= gs->runr.sp, // (yyy: `.as` not used when `ITEM_TYPEDEF`)
        }, sizeof *it);
}

void accept_expr(void ref usr, expression ref expr, tokt ref tok)
{
    cintre_state ref gs = usr;

    char const* xcmd = "";
    if (';' == *gstokn(*tok)) {
        tokt const xcmd_at = lext(&gs->lexr);
        xcmd = gstokn(xcmd_at);
        // yyy: hackish, this is for the gs->save (end of main repl)
        //      it assumes the ';' is part of the actual input
        //      (ie. not from macro, etc..)
        notif("FIXME: *(char*)tok->ptr = '\\0'");
    }
#   define xcmdis(s)  (!strcmp(s, xcmd)) //(xcmd.len && !memcmp(s, xcmd.ptr, strlen(s)))

    if (xcmdis("h")) {
        printf("List of commands:\n");
        printf("   h[elp]                  -  print this help and no more\n");
        printf("   loc[ales]               -  list local names\n");
        printf("   names[paces] or ns      -  list names in namespace\n");
        printf("   sta[cktop]              -  top of the stack, ie everything allocated onto it\n");
        printf("   cls[tack]               -  clear the stack (set sp back to top) and locals\n");
        printf("   save \"file\"           -  save each next lines to the file\n");
        printf("   load \"file\"           -  load the file, running each lines\n");
        printf("   qs[ave]                 -  save a snapshot of the state (list with qq)\n");
        printf("   ql[oad]                 -  load a snapshot of the state (list with qq)\n");
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
            print_type(stdout, it->type, false);
            printf("\n");
        }
        return;
    }

    if (xcmdis("names") || xcmdis("ns")) {
        tokt const name_at = lext(&gs->lexr);
        char cref name = gstokn(name_at);
        if (!*name) {
            printf("List of namespaces:\n");
            for (size_t ns = 0; ns < gs->nsps.count; ns++)
                printf("   %s (%zu names)\n", gs->nsps.spaces[ns].name, gs->nsps.spaces[ns].count);
            return;
        }
        for (size_t ns = 0; ns < gs->nsps.count; ns++) if (!strcmp(name, gs->nsps.spaces[ns].name)) {
            printf("List of names in %s:\n", gs->nsps.spaces[ns].name);
            for (size_t k = 0; k < gs->nsps.spaces[ns].count; k++) {
                struct adpt_item cref it = gs->nsps.spaces[ns].items+k;
                switch (it->kind) {
                case ITEM_VALUE:   printf("   [=%li] %-8s\t", it->as.value, it->name); break;
                case ITEM_OBJECT:  printf("   [%p] %-8s\t", it->as.object, it->name);  break;
                case ITEM_TYPEDEF: printf("   [typedef] %-8s\t", it->name);            break;
                    // unreachable case
                case ITEM_VARIABLE:;
                }
                print_type(stdout, it->type, false);
                printf("\n");
            }
            return;
        }
        printf("No such namespace %s\n", quoted(name));
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
        for (size_t k = 0; k < gs->locs.len; k++) free((void*)gs->locs.ptr[k].name); // yyy: cast const
        gs->locs.len = 0;
        // xxx: annoying, don't like this random thing here, same vibe as chk_work
        for (size_t k = 0; k < gs->comp.chk_interned.len; k++) free(gs->comp.chk_interned.ptr[k].ptr);
        gs->comp.chk_interned.len = 0;
        return;
    }

    if (xcmdis("qq")) {
        printf("Saved states:");
        for (size_t k = 0; k < gs->snaps.len; k++) printf(" %s", gs->snaps.ptr[k].name);
        printf("\n");
        return;
    }

    if (xcmdis("qs") || xcmdis("ql")) {
        bool const saving = 's' == xcmd[1];
        struct snap* found = NULL;

        tokt const name_at = lext(&gs->lexr);
        char name[sizeof(found->name)] = {0};
        {
            char cref x = gstokn(name_at);
            if (!*x) strcpy(name, "_");
            else memcpy(name, x, sizeof name-1);
        }

        for (size_t k = 0; k < gs->snaps.len; k++) if (!strcmp(name, gs->snaps.ptr[k].name)) {
            found = gs->snaps.ptr+k;
            break;
        }
        if (!found) {
            if (saving) {
                found = dyarr_push(&gs->snaps);
                strcpy(found->name, name);
                found->locs.ptr = NULL, found->locs.cap = 0;
                found->chk_interned.ptr = NULL, found->chk_interned.cap = 0;
                printf("New state: %s\n", quoted(found->name));
            } else {
                printf("No state named %s\n", quoted(name));
                return;
            }
        }

        // XXX: i oh so truly hate it

        if (saving) {
            found->stack = gs->runr;

            for (size_t k = 0; k < found->locs.len; k++) free((void*)found->locs.ptr[k].name); // yyy: cast const
            found->locs.len = 0;
            if (gs->locs.len) {
                dyarr_cpy(&found->locs, &gs->locs);
                for (size_t k = 0; k < gs->locs.len; k++)
                    *(char**)&found->locs.ptr[k].name = strcpy(mallox(strlen(gs->locs.ptr[k].name)+1), gs->locs.ptr[k].name); // yyy: cast const
            }

            for (size_t k = 0; k < found->chk_interned.len; k++) free(found->chk_interned.ptr[k].ptr);
            found->chk_interned.len = 0;
            if (gs->comp.chk_interned.len) {
                dyarr_resize(&found->chk_interned, found->chk_interned.len = gs->comp.chk_interned.len);
                for (size_t k = 0; k < gs->comp.chk_interned.len; k++) {
                    found->chk_interned.ptr[k] = (buf){0};
                    dyarr_cpy(&found->chk_interned.ptr[k], &gs->comp.chk_interned.ptr[k]);
                }
            }
        }

        else {
            gs->runr = found->stack;

            for (size_t k = 0; k < gs->locs.len; k++) free((void*)gs->locs.ptr[k].name); // yyy: cast const
            gs->locs.len = 0;
            if (found->locs.len) {
                dyarr_cpy(&gs->locs, &found->locs);
                for (size_t k = 0; k < found->locs.len; k++)
                    *(char**)&gs->locs.ptr[k].name = strcpy(mallox(strlen(found->locs.ptr[k].name)+1), found->locs.ptr[k].name); // yyy: cast const
            }

            for (size_t k = 0; k < gs->comp.chk_interned.len; k++) free(gs->comp.chk_interned.ptr[k].ptr);
            gs->comp.chk_interned.len = 0;
            if (found->chk_interned.len) {
                dyarr_resize(&gs->comp.chk_interned, gs->comp.chk_interned.len = found->chk_interned.len);
                for (size_t k = 0; k < found->chk_interned.len; k++) {
                    gs->comp.chk_interned.ptr[k] = (buf){0};
                    dyarr_cpy(&gs->comp.chk_interned.ptr[k], &found->chk_interned.ptr[k]);
                }
            }
        }
    }

    if (xcmdis("save") || xcmdis("load")) {
        bool const saving = 's' == xcmd[1];
        tokt const file_at = lext(&gs->lexr);
        char cref file = gstokn(file_at);
        if ('"' != *file) printf("Expected a name for the file to %s\n", saving ? "save" : "load");
        else {
            char filename[256];
            FILE ref f = fopen(filename, saving ? "w" : "r");
            if (!f) printf("Could not open file '%s'\n", filename);
            else {
                if (saving) {
                    if (gs->save) fclose(gs->save);
                    gs->save = f;
                } else notif("NIY: run loaded file");
            }
        }
        return;
    }

    if (xcmdis("ast")) {
        printf("AST of the expression:\n");
        print_expr(stdout, &gs->lexr, expr, 0);
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
        if (TYPE_NAMED == ty->tyty)
            printf("(\x1b[32m%s\x1b[m) ", ty->info.named.name);
        print_type(stdout, ty, true);
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

    gs->runr.sp+= res.type->size; // yyy: free only result (keeps lits, bit loose tho, some alignment padding sticks..)
} // accept_expr
// }}}

int main(int argc, char cref* argv)
{
    static cintre_state _gs = {
        .decl= {.ls= &_gs.lexr, .usr= &_gs, .on= accept_decl},
        .expr= {.ls= &_gs.lexr, .usr= &_gs, .on= accept_expr},
        .comp= {.ls= &_gs.lexr, .usr= &_gs, .lookup= cintre_lookup},
        .runr= {.sp= sizeof _gs.runr.stack}, // (xxx: sizeof stack)
    };
    {
        extern struct adpt_namespace cref namespaces_first;
        extern unsigned long const namespaces_count;
        _gs.nsps.spaces = namespaces_first;
        _gs.nsps.count = namespaces_count;
    }
    cintre_state ref gs = &_gs;

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

    // xxx: somewhat lexer internal? or just say it's valid to NULL-init that and use cstream...
    lex_entry(&gs->lexr, NULL, "<input>");

    printf("Type `;help` for a list of command\n");

    char* line = NULL;
    while (prompt("\1\x1b[35m\2(*^^),u~~\1\x1b[m\2 ", &line, gs)) {
        // xxx: somewhat lexer internal
        gs->lexr.cstream = line;
        ++gs->lexr.sources.ptr[gs->lexr.sources.len-1].line;

        tokt tok_at = lext(&gs->lexr);
        char const* tok = gstokn(tok_at);
        if (!*tok) continue;

        if (';' == *tok) accept_expr(gs->expr.usr, NULL, &tok_at);

        else if (_is_decl_keyword(gs, tok)) {
            gs->decl.base = (declaration){0};

            do {
                tok_at = parse_declaration(&gs->decl, tok_at);
                tok = gstokn(tok_at);

                if ('=' == *tok) {
                    lex_rewind(&gs->lexr, 1);

                    //char cref name = dyarr_top(&gs->locs)->name;

                    gs->expr.disallow_comma = true;
                    notif("FIXME: tok = parse_expression(&gs->expr, name)");
                    gs->expr.disallow_comma = false;
                }
            } while (',' == *tok ? tok_at = lext(&gs->lexr), true : false);
        }

        else parse_expression(&gs->expr, tok_at);

        if (gs->save && *line) fprintf(gs->save, "%s\n", line);
    }

    cintre_cleanup(gs);

    return EXIT_SUCCESS;
}

#undef gstokn
