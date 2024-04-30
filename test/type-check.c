#include "run"

struct adpt_item const* test_lookup(void* _, bufsl const name) {
    static int si;
    static unsigned ui;
    static bufsl ab = {.ptr= "coucou", .len= 6};

    static struct adpt_item const si_it = {.name= "si", .type= &adptb_int_type, .kind= ITEM_VALUE, .as.object= &si};
    static struct adpt_item const ui_it = {.name= "ui", .type= &adptb_uint_type, .kind= ITEM_VALUE, .as.object= &ui};

    static struct adpt_type const cp_ty = {
        .size= sizeof(char*), .align= alignof(char*),
        .tyty= TYPE_PTR,
        .info.ptr= &adptb_char_type,
    };
    static struct adpt_comp_field ab_fs[2] = {
        [0]= {.name= "ptr", .type= &cp_ty, .offset= offsetof(bufsl, ptr)},
        [1]= {.name= "len", .type= &adptb_ulong_type, .offset= offsetof(bufsl, len)},
    };
    static struct adpt_type const ab_ty = {
        .size= sizeof(bufsl), .align= alignof(bufsl),
        .tyty= TYPE_STRUCT,
        .info.comp= {
            .fields= ab_fs,
            .count= 2,
        },
    };
    static struct adpt_item const ab_it = {
        .name= "ab",
        .type= &ab_ty,
        .kind= ITEM_VALUE,
        .as.object= &ab,
    };

    switch (name.ptr[0] <<8| name.ptr[1]) {
    case 's' <<8| 'i': return &si_it;
    case 'u' <<8| 'i': return &ui_it;
    case 'a' <<8| 'b': return &ab_it;
    }

    return NULL;
}

void check(void ref _, expression ref expr, bufsl ref tok) {
    (void)_;
    struct adpt_type cref ty = check_expression(&(compile_state){.lookup= test_lookup}, expr);

    print_type(stdout, ty);
    printf(" -- tok: %.*s\n", (unsigned)tok->len, tok->ptr);
}

void run_test(char* file) {
    lex_state ls = {0};
    lini(&ls, file);

    bufsl tok = lext(&ls);
    parse_expr_state ps = {.ls= &ls, .on= check};
    while (tok.len) if ((tok = parse_expression(&ps, tok)).len)
        switch (*tok.ptr) {
        case ';':
            tok = lext(&ls);
            continue;

        default:
            exitf("other: %.*s", (unsigned)tok.len, tok.ptr);
        }

    ldel(&ls);
}
