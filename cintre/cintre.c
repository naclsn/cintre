/// `int main()` of the REPL; typically used as the "cintre.o" when linking with a "c-some.o"
///
/// it expects the two following symbols:
/// ```c
/// extern struct adpt_namespace cref namespaces_first;
/// extern unsigned long const namespaces_count;
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

    dyarr(struct adpt_item) locals;
    struct {
        size_t count;
        struct adpt_namespace const* spaces;
    } namespaces;

    FILE* save;
    dyarr(struct snap {
        char name[8];
        run_state stack;
        dyarr(struct adpt_item) locals;
    }) snaps;

    // needed for `check_expression`
    dyarr(struct comp_checked) chk_work;
    // needed for `decl_to_adpt_type`
    dyarr(struct adpt_type) ty_work;
} cintre_state;

#define gstokn(__at) (gs->lexr.tokens.ptr+(__at))

// readline {{{
#ifdef USE_READLINE
#include <readline/readline.h>
#include <readline/history.h>
#include <signal.h>

#define hist_file ".ignore/history"
static struct cintre_state const* _prompt_rl_gs;
static char cref _prompt_compl_cmds[] = {"help", "quit", "locals", "namespaces", "stacktop", "clstack", "save \"", "load \"", "qq", "qsave", "qload", "ast", "type", "bytecode", "silent"};
static char cref _prompt_compl_kws[] = {"auto ", /*"bool ",*/ "break ", "case ", "char ", "const ", "continue ", "default ", "do ", "double ", "else ", "enum ", "extern ", /*"false ",*/ "float ", "for ", "goto ", "if ", /*"inline ",*/ "int ", "long ", /*"register ",*/ /*"restrict ",*/ "return ", "short ", "signed ", "sizeof ", /*"static ",*/ "struct ", "switch ", /*"true ",*/ "typedef ", /*"typeof ",*/ "union ", "unsigned ", "void ", /*"volatile ",*/ "while "};

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
    int const cols_count = term_width/(max_length+2);
    int const rows_count = num_matches < cols_count ? 1 : num_matches/cols_count+1;

    for (int j = 0; j < rows_count; j++) {
        for (int i = 0; i < cols_count+1; i++) {
            if (i*rows_count+j >= num_matches) break;

            char cref it = matches[i*rows_count+j+1];
            unsigned const len = strlen(it);

            if ('(' == it[len-1]) printf("\x1b[33m%.*s\x1b[m(%*s", len-1, it, max_length+2-len, "");

            else if (' ' == it[len-1]) {
                bool is_kw = false;
                for (size_t k = 0; k < countof(_prompt_compl_kws); k++) if ((is_kw = !memcmp(_prompt_compl_kws[k], it, len))) break;
                if (is_kw) printf("\x1b[34m%-*s\x1b[m", max_length+2, it);
                else printf("\x1b[32m%-*s\x1b[m", max_length+2, it);
            }

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

    static size_t ns = 0, k = 0;
    static bool is_xcmd = false, did_kw = false, is_tagged = false;
    if (0 == state) {
        for (int p = rl_point; p && !(is_xcmd = ';' == rl_line_buffer[p-1]); p--);

        char const* beg = rl_line_buffer+rl_point;
        while (rl_line_buffer < beg && ' ' != beg[-1]) --beg;
        while (rl_line_buffer < beg && ' ' == beg[-1]) --beg;
        while (rl_line_buffer < beg && ' ' != beg[-1]) --beg;
        is_tagged = !memcmp("struct ", beg, strlen("struct ")) || !memcmp("union ", beg, strlen("union ")) || !memcmp("enum ", beg, strlen("enum "));

        ns = k = 0;
        did_kw = false;
    }

    size_t const len = strlen(text);
    cintre_state cref gs = _prompt_rl_gs;

    // todo(maybe): could have completion based on type
    //if (ADPT_TYPE_STRUCT || ADPT_TYPE_UNION) .. rl_point, rl_line_buffer ..;

    if (is_xcmd) {
        while (k < countof(_prompt_compl_cmds)) if (!memcmp(_prompt_compl_cmds[k++], text, len)) {
            size_t const len = strlen(_prompt_compl_cmds[k-1]);
            char ref r = malloc(len+1);
            if (!r) return NULL;
            strcpy(r, _prompt_compl_cmds[k-1]);
            return r;
        }
        return NULL;
    }

    if (is_tagged) {
        for (; ns < gs->namespaces.count+1; ns++, k = 0)
            for (; k < (!ns ? gs->locals.len : gs->namespaces.spaces[ns-1].count); k++) {
                struct adpt_item cref it = !ns ? gs->locals.ptr+k : gs->namespaces.spaces[ns-1].items+k;
                if ('@' == *it->name && !memcmp(it->name+1, text, len)) {
                    size_t const len = strlen(it->name+1);
                    char ref r = malloc(len+2);
                    if (!r) return NULL;
                    strcpy(r, it->name+1);
                    r[len] = ' ', r[len+1] = '\0';
                    k++;
                    return r;
                }
            }
        return NULL;
    }

    if (!did_kw) {
        while (k < countof(_prompt_compl_kws)) if (!memcmp(_prompt_compl_kws[k++], text, len)) {
            size_t const len = strlen(_prompt_compl_kws[k-1]);
            char ref r = malloc(len+1);
            if (!r) return NULL;
            strcpy(r, _prompt_compl_kws[k-1]);
            return r;
        }
        did_kw = true;
        k = 0;
    }

    for (; ns < gs->namespaces.count+1; ns++, k = 0)
        for (; k < (!ns ? gs->locals.len : gs->namespaces.spaces[ns-1].count); k++) {
            struct adpt_item cref it = !ns ? gs->locals.ptr+k : gs->namespaces.spaces[ns-1].items+k;
            if ('@' != *it->name && !memcmp(it->name, text, len)) {
                size_t const len = strlen(it->name);
                bool const tdf = ADPT_ITEM_TYPEDEF == it->kind;
                bool const fun = ADPT_TYPE_FUN == _tailtype(it->type)->tyty;
                bool const arr = ADPT_TYPE_ARR == _truetype(it->type)->tyty;
                char ref r = malloc(len+1 + (tdf|fun|arr));
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

    for (size_t k = 0; k < gs->locals.len; k++)
        free((void*)gs->locals.ptr[k].name);
    free(gs->locals.ptr);

    if (gs->save) fclose(gs->save);

    for (size_t k = 0; k < gs->snaps.len; k++) {
        for (size_t kk = 0; kk < gs->snaps.ptr[k].locals.len; kk++)
            free((void*)gs->snaps.ptr[k].locals.ptr[kk].name);
        free(gs->snaps.ptr[k].locals.ptr);
    }

    for (size_t k = 0; k < gs->ty_work.len; k++) switch (gs->ty_work.ptr[k].tyty) {
    case ADPT_TYPE_STRUCT:
    case ADPT_TYPE_UNION:;
        struct adpt_comp_desc cref comp = &gs->ty_work.ptr[k].info.comp;
        for (size_t kk = 0; kk < comp->count; kk++) free((void*)comp->fields[kk].name);
        free((void*)comp->fields);
        break;
    case ADPT_TYPE_FUN:;
        struct adpt_fun_desc cref fun = &gs->ty_work.ptr[k].info.fun;
        for (size_t kk = 0; kk < fun->count; kk++) free((void*)fun->params[kk].name);
        free((void*)fun->params);
        break;
        // noop cases
    case ADPT_TYPE_VOID: case ADPT_TYPE_CHAR: case ADPT_TYPE_UCHAR: case ADPT_TYPE_SCHAR: case ADPT_TYPE_SHORT: case ADPT_TYPE_INT: case ADPT_TYPE_LONG: case ADPT_TYPE_USHORT: case ADPT_TYPE_UINT: case ADPT_TYPE_ULONG: case ADPT_TYPE_FLOAT: case ADPT_TYPE_DOUBLE: case ADPT_TYPE_PTR: case ADPT_TYPE_ARR: case ADPT_TYPE_NAMED:;
    }
    free(gs->ty_work.ptr);
}

// compile time (lookup and random helpers) {{{
struct adpt_item const* cintre_lookup(void* usr, char cref name)
{
    cintre_state cref gs = usr;
    for (size_t k = 0; k < gs->locals.len; k++) {
        struct adpt_item cref it = gs->locals.ptr+k;
        if (!strcmp(name, it->name)) return it;
    }
    for (size_t ns = 0; ns < gs->namespaces.count; ns++)
        for (size_t k = 0; k < gs->namespaces.spaces[ns].count; k++) {
            struct adpt_item cref it = gs->namespaces.spaces[ns].items+k;
            if (!strcmp(name, it->name)) return it;
        }
    return NULL;
}

struct comp_checked const* cintre_alloc_checked(void* usr, struct comp_checked const niw)
{
    cintre_state cref gs = usr;
    (void)gs;
    return memcpy(mallox(sizeof niw), &niw, sizeof niw);
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
    return it && ADPT_ITEM_TYPEDEF == it->kind;
}

/// allocates using `gs->ty_work`; on failure, it is safe to set the length of
/// it back to what it was to free wip temporaries, it should not cause a leak
struct adpt_type const* _decl_to_adpt_type(cintre_state ref gs, struct decl_type cref ty)
{
    switch (ty->kind) {
        struct adpt_type* r;

    case DECL_KIND_NOTAG:;
        bool _signed = false, _unsigned = false, _short = false, _long = false;
        for (size_t k = 0; DECL_QUAL_END != ty->quals[k]; k++) switch (ty->quals[k]) {
            case DECL_QUAL_SIGNED:    _signed = true;          break;
            case DECL_QUAL_UNSIGNED:  _unsigned = true;        break;
            case DECL_QUAL_SHORT:     _short = true;           break;
            case DECL_QUAL_LONG:      _long = true;            break;
            case DECL_QUAL_COMPLEX:   notif("NIY: complex");   return NULL;
            case DECL_QUAL_IMAGINARY: notif("NIY: imaginary"); return NULL;

                // noop cases
            case DECL_QUAL_END: case DECL_QUAL_CONST: case DECL_QUAL_RESTRICT: case DECL_QUAL_VOLATILE:;
            }

#       define nameis(s)  (!strcmp(s, gstokn(ty->name)))
        if (nameis("char"))
            return _signed ? &adptb_schar_type
                 : _unsigned ? &adptb_uchar_type
                 : &adptb_char_type;
        if (nameis("int"))
            return _short ? (_unsigned ? &adptb_ushort_type : &adptb_short_type)
                 : _long ? (_unsigned ? &adptb_ulong_type : &adptb_long_type)
                 : _unsigned ? &adptb_uint_type : &adptb_int_type;
        if (nameis("float"))  return &adptb_float_type;
        if (nameis("double")) return &adptb_double_type;
        if (nameis("void"))   return &adptb_void_type;
#       undef nameis

        struct adpt_item cref it = gs->comp.lookup(gs->comp.usr, gstokn(ty->name));
        if (it && ADPT_ITEM_TYPEDEF == it->kind) return it->type;
        return NULL;

    case DECL_KIND_STRUCT:
    case DECL_KIND_UNION:
        if (-1ul == ty->info.comp.count) {
            char ref name = gstokn(ty->name);
            // xxx: lexer internal (the fact that we know we can)
            name[-1] = '@';
            struct adpt_item cref it = gs->comp.lookup(gs->comp.usr, name-1);
            name[-1] = '\0';
            if (it && ADPT_ITEM_TYPEDEF == it->kind) return it->type;
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
                .tyty= DECL_KIND_STRUCT == ty->kind ? ADPT_TYPE_STRUCT : ADPT_TYPE_UNION,
                .info.comp = {
                    .fields= NULL,
                    .count= 0,
                },
            }, sizeof*r);

    case DECL_KIND_ENUM: return &adptb_int_type;

    case DECL_KIND_PTR:;
        struct adpt_type cref ptr = _decl_to_adpt_type(gs, &ty->info.ptr->type);
        if (!ptr) return NULL;

        r = dyarr_push(&gs->ty_work);
        return memcpy(r, &(struct adpt_type){
                .size= sizeof(void*),
                .align= sizeof(void*),
                .tyty= ADPT_TYPE_PTR,
                .info.ptr= ptr,
            }, sizeof*r);

    case DECL_KIND_FUN:
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
                .tyty= ADPT_TYPE_FUN,
                .info.fun= {
                    .ret= ret,
                    .params= params,
                    .count= ty->info.fun.count,
                },
            }, sizeof*r);

    case DECL_KIND_ARR:;
        struct adpt_type cref item = _decl_to_adpt_type(gs, &ty->info.arr.item->type);
        if (!item) return NULL;

        size_t count = 0;
        if (ty->info.arr.count) {
            size_t const plen = gs->comp.res.len;
            size_t const psp = gs->runr.sp;

            expression cast = {
                .kind= EXPR_UNOP_CAST,
                .info.cast= {
                    .opr= ty->info.arr.count,
                    .type= &(struct decl_type){
                        .quals= {DECL_QUAL_LONG},
                        .name= (lex_inject(&gs->lexr, "int"), lext(&gs->lexr)), // the token stream will get all messed up from these :/ anyways
                    },
                },
            };
            gs->chk_work.len = 0;
            if (!check_expression(&gs->comp, &cast, cintre_alloc_checked)) return NULL;

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
                .tyty= ADPT_TYPE_ARR,
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
    if (!ty) {
        notif("Could not understand type");
        gs->ty_work.len = pwork;
    }
    struct adpt_type cref tty = _truetype(ty);
    if (!tty->size) {
        notif("Zero-sized variable type");
        gs->ty_work.len = pwork;
        return;
    }

    int kind = ADPT_ITEM_VARIABLE;
    switch (decl->spec) {
    case DECL_SPEC_TYPEDEF:
        kind = ADPT_ITEM_TYPEDEF;
        break;

    case DECL_SPEC_EXTERN:
        notif("`extern` in declaration, nothing declared");
        return;

    case DECL_SPEC_STATIC:
        notif("`static` ignored in declaration"); if (0)
    case DECL_SPEC_AUTO:
        notif("`auto` ignored in declaration"); if (0)
    case DECL_SPEC_REGISTER:
        notif("`register` ignored in declaration");
        // fall through
    case DECL_SPEC_NONE:
        //size_t end = gs->sp;
        gs->runr.sp = ((gs->runr.sp-tty->size) / tty->align) * tty->align;
    }

    struct adpt_item ref it = dyarr_push(&gs->locals);
    memcpy(it, &(struct adpt_item){
            .name= strcpy(mallox(strlen(decl_name)+1), decl_name),
            .type= ty,
            .kind= kind,
            .as.variable= gs->runr.sp, // (yyy: `.as` not used when `ADPT_ITEM_TYPEDEF`)
        }, sizeof *it);

    if ('=' == *gstokn(*tok)) {
        // FIXME: hack won't hold for 'type-less compound literal'
        //        (by that I mean compound initializer or whatsitsface-)
        lex_rewind(&gs->lexr, 1);
        lex_inject(&gs->lexr, dyarr_top(&gs->locals)->name);

        gs->expr.disallow_comma = true;
        gs->expr.allow_topcomplit = true;
        *tok = parse_expression(&gs->expr, lext(&gs->lexr));
        gs->expr.disallow_comma = false;
        gs->expr.allow_topcomplit = false;
    }

    if (',' == *gstokn(*tok)) *tok = parse_declaration(&gs->decl, lext(&gs->lexr));
}

void accept_expr(void ref usr, expression ref expr, tokt ref tok)
{
    cintre_state ref gs = usr;

    char const* xcmd = "";
    if (';' == *gstokn(*tok)) {
        *tok = lext(&gs->lexr);
        xcmd = gstokn(*tok);
    }
#   define xcmdis(s)  (!memcmp(s, xcmd, strlen(s)))

    if (xcmdis("h")) {
        printf("List of commands:\n");
        printf("   h[elp]                  -  print this help\n");
        printf("   q[uit]                  -  quits\n");
        printf("   loc[als]                -  list local names\n");
        printf("   names[paces] or ns      -  list names in namespace\n");
        printf("   sta[cktop]              -  top of the stack, ie everything allocated onto it\n");
        printf("   cls[tack]               -  clear the stack (set sp back to top) and locals\n");
        printf("   save \"file\"             -  save following lines to the file\n");
        printf("   load \"file\"             -  load the file, executing content\n");
        printf("   qs[ave]                 -  save a snapshot of the state (list with qq)\n");
        printf("   ql[oad]                 -  load a snapshot of the state (list with qq)\n");
        printf("   ast                     -  ast of the expression\n");
        printf("   ty[pe]                  -  type of the expression, eg. `strlen; ty`\n");
        printf("   bytec[ode] or bc        -  internal bytecode from compilation\n");
        printf("   sil[ent]                -  do not print the result of the expression\n");
        printf("no command after the ; (or no ;) will simply execute the expression\n");
        return;
    }

    if (xcmdis("q")) {
        // XXX: not everything is freed, but whatever
        cintre_cleanup(gs);
        exit(EXIT_SUCCESS);
        return;
    }

    if (xcmdis("loc")) {
        printf("List of locals:\n");
        size_t const sz = sizeof gs->runr.stack; // (xxx: sizeof stack)
        for (size_t k = 0; k < gs->locals.len; k++) {
            struct adpt_item cref it = gs->locals.ptr+k;
            if (ADPT_ITEM_TYPEDEF == it->kind) printf("   [typedef] %-8s\t", it->name);
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
            for (size_t ns = 0; ns < gs->namespaces.count; ns++)
                printf("   %s (%zu names)\n", gs->namespaces.spaces[ns].name, gs->namespaces.spaces[ns].count);
            return;
        }
        for (size_t ns = 0; ns < gs->namespaces.count; ns++) if (!strcmp(name, gs->namespaces.spaces[ns].name)) {
            printf("List of names in %s:\n", gs->namespaces.spaces[ns].name);
            for (size_t k = 0; k < gs->namespaces.spaces[ns].count; k++) {
                struct adpt_item cref it = gs->namespaces.spaces[ns].items+k;
                switch (it->kind) {
                case ADPT_ITEM_VALUE:   printf("   [=%li] %-8s\t", it->as.value, it->name); break;
                case ADPT_ITEM_OBJECT:  printf("   [%p] %-8s\t", it->as.object, it->name);  break;
                case ADPT_ITEM_TYPEDEF: printf("   [typedef] %-8s\t", it->name);            break;
                    // unreachable case
                case ADPT_ITEM_VARIABLE:;
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
        print_tops(stdout, &gs->runr, gs->locals.ptr, gs->locals.len);
        return;
    }

    if (xcmdis("cls")) {
        size_t const sz = sizeof gs->runr.stack; // (xxx: sizeof stack)
        gs->runr.sp = sz;
        for (size_t k = 0; k < gs->locals.len; k++) free((void*)gs->locals.ptr[k].name);
        gs->locals.len = 0;
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

        struct snap* found = NULL; {
            char name[sizeof found->name] = {0}; {
                tokt const name_at = lext(&gs->lexr);
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
                    found->locals.ptr = NULL, found->locals.cap = 0;
                    printf("New state: %s\n", quoted(found->name));
                } else {
                    printf("No state named %s\n", quoted(name));
                    return;
                }
            } else if (saving) printf("Old state: %s\n", quoted(found->name));
        }

        if (saving) {
            found->stack = gs->runr;

            for (size_t k = 0; k < found->locals.len; k++) free((void*)found->locals.ptr[k].name);
            found->locals.len = 0;
            if (gs->locals.len) {
                dyarr_cpy(&found->locals, &gs->locals);
                for (size_t k = 0; k < gs->locals.len; k++)
                    *(char**)&found->locals.ptr[k].name = strcpy(mallox(strlen(gs->locals.ptr[k].name)+1), gs->locals.ptr[k].name);
            }
        }

        else {
            gs->runr = found->stack;

            for (size_t k = 0; k < gs->locals.len; k++) free((void*)gs->locals.ptr[k].name);
            gs->locals.len = 0;
            if (found->locals.len) {
                dyarr_cpy(&gs->locals, &found->locals);
                for (size_t k = 0; k < found->locals.len; k++)
                    *(char**)&gs->locals.ptr[k].name = strcpy(mallox(strlen(found->locals.ptr[k].name)+1), found->locals.ptr[k].name);
            }
        }
    }

    if (xcmdis("save") || xcmdis("load")) {
        bool const saving = 's' == xcmd[0];
        tokt const file_at = lext(&gs->lexr);
        char cref file = gstokn(file_at);
        if ('"' != *file) printf("Expected a name for the file to %s\n", saving ? "save" : "load");
        else {
            char filename[256];
            lex_struqo(filename, sizeof filename, file);
            FILE ref f = fopen(filename, saving ? "w" : "r");
            if (!f) printf("Could not open file '%s'\n", filename);
            else {
                if (saving) {
                    if (gs->save) fclose(gs->save);
                    gs->save = f;
                } else {
                    // xxx: lexer internal; now we would like to read from
                    //      a stream normally so reset cstream to NULL; the
                    //      last token we pulled was a string literal which
                    //      means it looked ahead to merge with a following
                    //      one, so there is an unavoidable ahead of at least
                    //      1 (the empty EOF token), force it to 0
                    gs->lexr.cstream = NULL;
                    gs->lexr.ahead = 0;
                    lex_entry(&gs->lexr, f, filename);
                    for (tokt tok_at = lext(&gs->lexr); *gstokn(tok_at); ) {
                        tok_at = _is_decl_keyword(gs, gstokn(tok_at)) ? parse_declaration(&gs->decl, tok_at) : parse_expression(&gs->expr, tok_at);
                        if (';' == *gstokn(tok_at)) tok_at = lext(&gs->lexr);
                    }
                }
            } // did open file
        } // did provide file name
        return;
    }

    if (xcmdis("ast")) {
        printf("AST of the expression:\n");
        print_tree(stdout, &gs->lexr, expr, 0);
        return;
    }

    if (!expr) return;
    gs->comp.vsp = gs->runr.sp;
    gs->comp.res.len = 0;
    gs->chk_work.len = 0;

    struct adpt_type cref ty = check_expression(&gs->comp, expr, cintre_alloc_checked);
    if (!ty) return;

    if (xcmdis("ty")) {
        if (!ty) return;
        printf("Expression is of type: ");
        if (ADPT_TYPE_NAMED == ty->tyty)
            printf("(\x1b[32m%s\x1b[m) ", ty->info.named.name);
        print_type(stdout, ty, true);
        printf("\n");
        return;
    }

    struct slot slot = {.ty= _truetype(ty)};
    _alloc_slot(&gs->comp, &slot);

    compile_expression(&gs->comp, expr, &slot);
    // XXX: mouai :/
    if (_slot_value == slot.usage) {
        _emit_data(&gs->comp, slot.loc - gs->comp.vsp, slot.ty->size, slot.as.value.bytes);
        slot.usage = _slot_used;
    }

///    switch (slot.usage) {
///    case _slot_value:
///        _emit_data(&gs->comp, slot.loc - gs->comp.vsp, slot.ty->size, slot.as.value.bytes);
///        break;
///
///    case _slot_used:
///        break;
///
///    case _slot_variable:
///        _emit_move(&gs->comp, slot.loc - gs->comp.vsp, slot.ty->size, slot.as.variable - gs->comp.vsp);
///        // yyy: result is a variable, show it and not "_"?
///        break;
///    }
///    slot.usage = _slot_used;

    if (xcmdis("bytec") || xcmdis("bc")) {
        printf("Resulting bytecode (%zuB):\n", gs->comp.res.len);
        print_code(stdout, gs->comp.res);
        return;
    }
        printf("Resulting bytecode (%zuB):\n", gs->comp.res.len);
        print_code(stdout, gs->comp.res);

    run(&gs->runr, gs->comp.res);
    struct adpt_item const res = {
        .name= "",
        .type= slot.ty,
        .kind= ADPT_ITEM_VARIABLE,
        // (YYY: atv/at)
        .as.variable= gs->runr.sp + (_slot_variable == slot.usage ? slot.as.variable : slot.loc) - gs->comp.vsp,
    };

    if (!xcmdis("sil")) {// && res.type->size) { // FIXME: something is broken I have types of size 0 somehow
        printf("Result");
        print_item(stdout, &res, gs->runr.stack, 0);
    }

    // XXX: not great, not enough
    gs->runr.sp+= res.type->size; // yyy: free only result (keeps lits, bit loose tho, some alignment padding sticks around..)

    // XXX: all this jazz ^^^ is somewhat temporary anyways until we have
    //      a proper statement parser and compiler
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
        _gs.namespaces.spaces = namespaces_first;
        _gs.namespaces.count = namespaces_count;
    }
    cintre_state ref gs = &_gs;

    char cref prog = (argc--, *argv++);
    while (0 < argc) if ('-' == **argv) switch ((argc--, *argv++)[1]) {

    case 'h':
        printf("Usage: %s [-f file]\n", prog);
        return 1;

    case 'f':;
        FILE* const f = fopen((argc--, *argv++), "r");
        if (!f) printf("Could not opent file '%s'\n", argv[-1]);
        else {
            lex_entry(&gs->lexr, f, argv[-1]);
            for (tokt tok_at = lext(&gs->lexr); *gstokn(tok_at); ) {
                tok_at = _is_decl_keyword(gs, gstokn(tok_at)) ? parse_declaration(&gs->decl, tok_at) : parse_expression(&gs->expr, tok_at);
                if (';' == *gstokn(tok_at)) tok_at = lext(&gs->lexr);
            }
        }
        break;

    } else {
        printf("Unexpected argument '%s', see -h, continuing\n", *argv);
        break;
    }

    // xxx: somewhat lexer internal? or just say it's valid to NULL-init that and use cstream...
    lex_entry(&gs->lexr, NULL, "<input>");

    printf("Type `;help` for a list of command\n");

    char* line = NULL;
    while (gs->lexr.cstream = NULL, gs->lexr.ahead = 0, // xxx: lexer internal, see note in "load" xcmd
            prompt("\1\x1b[35m\2(*^^),u~~\1\x1b[m\2 ", &line, gs)) {
        // xxx: somewhat lexer internal
        gs->lexr.cstream = line;
        ++gs->lexr.sources.ptr[gs->lexr.sources.len-1].line;

        tokt tok_at = lext(&gs->lexr);
        char const* tok = gstokn(tok_at);
        if (!*tok) continue;

        if (gs->save) fprintf(gs->save, "%s\n", line);

        if (';' == *tok) accept_expr(gs->expr.usr, NULL, &tok_at);
        else if (_is_decl_keyword(gs, tok)) parse_declaration(&gs->decl, tok_at);
        else parse_expression(&gs->expr, tok_at);
    }

    cintre_cleanup(gs);

    return EXIT_SUCCESS;
}

#undef gstokn
