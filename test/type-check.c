#include "run"

// fake lookup {{{
struct ct_adpt_item const* test_lookup(void* _, ct_bufsl const name)
{
    static int si = -1;
    static unsigned ui = 1;
    static ct_bufsl ab = {.ptr= "coucou", .len= 6};
    int pu(char* s, unsigned long z);
    typedef int (*pu_fpty)(char*, unsigned long);
    static ct_bufsl ar[3] = {0};

    static struct ct_adpt_item const si_it = {.name= "si", .type= &ct_adptb_int_type, .kind= CT_ITEM_OBJECT, .as.object= &si};
    static struct ct_adpt_item const ui_it = {.name= "ui", .type= &ct_adptb_uint_type, .kind= CT_ITEM_OBJECT, .as.object= &ui};

    static struct ct_adpt_type const cp_ty = {
        .size= sizeof(char*), .align= alignof(char*),
        .tyty= CT_TYPE_PTR,
        .info.ptr= &ct_adptb_char_type,
    };

    static struct ct_adpt_comp_field ab_fs[2] = {
        [0]= {.name= "ptr", .type= &cp_ty, .offset= offsetof(ct_bufsl, ptr)},
        [1]= {.name= "len", .type= &ct_adptb_ulong_type, .offset= offsetof(ct_bufsl, len)},
    };
    static struct ct_adpt_type const ab_ty = {
        .size= sizeof(ct_bufsl), .align= alignof(ct_bufsl),
        .tyty= CT_TYPE_STRUCT,
        .info.comp= {
            .fields= ab_fs,
            .count= countof(ab_fs),
        },
    };
    static struct ct_adpt_item const ab_it = {
        .name= "abuf",
        .type= &ab_ty,
        .kind= CT_ITEM_OBJECT,
        .as.object= &ab,
    };

    static struct ct_adpt_fun_param pu_ps[2] = {
        [0]= {.name= "s", .type= &cp_ty},
        [1]= {.name= "z", .type= &ct_adptb_ulong_type},
    };
    static struct ct_adpt_type const pu_ty = {
        .size= sizeof(pu_fpty), .align= alignof(pu_fpty),
        .tyty= CT_TYPE_FUN,
        .info.fun= {
            .ret= &ct_adptb_int_type,
            .params= pu_ps,
            .count= countof(pu_ps),
        },
    };
    static struct ct_adpt_item const pu_it = {
        .name= "put",
        .type= &pu_ty,
        .kind= CT_ITEM_OBJECT,
        .as.function= NULL,
    };

    static struct ct_adpt_type const ar_ty = {
        .size= sizeof(ct_bufsl)*3, .align= alignof(ct_bufsl),
        .tyty= CT_TYPE_ARR,
        .info.arr= {
            .item= &ab_ty,
            .count= 3,
        },
    };
    static struct ct_adpt_item const ar_it = {
        .name= "arry",
        .type= &ar_ty,
        .kind= CT_ITEM_OBJECT,
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

void check(void ref usr, ct_expression ref expr, ct_bufsl ref tok)
{
    struct ct_adpt_type cref ty = ct_check_expression(&(ct_compile_state){.lookup= test_lookup}, expr);
    ct_print_type(stdout, ty, true);
    printf("\n");
    ct_lex_state cref ls = usr;
    report_lex_locate(ls, " -- tok: %.*s", bufmt(*tok));
}

void run_test(char* file)
{
    ct_lex_state ls = {0};
    ct_lini(&ls, file);

    ct_bufsl tok = ct_lext(&ls);
    ct_parse_expr_state ps = {.ls= &ls, .usr= &ls, .on= check};
    while (tok.len) if ((tok = ct_parse_expression(&ps, tok)).len)
        switch (*tok.ptr) {
        case ';':
            tok = ct_lext(&ls);
            continue;

        default:
            exitf("other: %.*s", (unsigned)tok.len, tok.ptr);
        }

    ct_ldel(&ls);
}
