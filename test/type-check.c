#include "run"

// fake lookup {{{
struct adpt_item const* test_lookup(void* _, bufsl const name) {
    static int si = -1;
    static unsigned ui = 1;
    static bufsl ab = {.ptr= "coucou", .len= 6};
    int pu(char* s, unsigned long z);
    typedef int (*pu_fpty)(char*, unsigned long);
    static bufsl ar[3] = {0};

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
            .count= countof(ab_fs),
        },
    };
    static struct adpt_item const ab_it = {
        .name= "abuf",
        .type= &ab_ty,
        .kind= ITEM_VALUE,
        .as.object= &ab,
    };

    static struct adpt_fun_param pu_ps[2] = {
        [0]= {.name= "s", .type= &cp_ty},
        [1]= {.name= "z", .type= &adptb_ulong_type},
    };
    static struct adpt_type const pu_ty = {
        .size= sizeof(pu_fpty), .align= alignof(pu_fpty),
        .tyty= TYPE_FUN,
        .info.fun= {
            .ret= &adptb_int_type,
            .params= pu_ps,
            .count= countof(pu_ps),
        },
    };
    static struct adpt_item const pu_it = {
        .name= "put",
        .type= &pu_ty,
        .kind= ITEM_VALUE,
        .as.function= NULL,
    };

    static struct adpt_type const ar_ty = {
        .size= sizeof(bufsl)*3, .align= alignof(bufsl),
        .tyty= TYPE_ARR,
        .info.arr= {
            .item= &ab_ty,
            .count= 3,
        },
    };
    static struct adpt_item const ar_it = {
        .name= "arry",
        .type= &ar_ty,
        .kind= ITEM_VALUE,
        .as.object= &ar,
    };

    switch (name.ptr[0] <<8| name.ptr[1]) {
    case 's' <<8| 'i': return &si_it;
    case 'u' <<8| 'i': return &ui_it;
    case 'a' <<8| 'b': return &ab_it;
    case 'p' <<8| 'u': return &pu_it;
    case 'a' <<8| 'r': return &ar_it;
    }
    return NULL;
}
// }}}

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
