#include "run"
#include "../cintre/compiler.h"

void comp(void ref usr, ct_expression ref expr, ct_bufsl ref tok)
{
    static char stack[1042];
    ct_compile_state cs = {.vsp= sizeof stack};

    struct ct_slot slot = {.ty= ct_check_expression(&cs, expr)};
    if (!slot.ty) printf("checking error");
    else switch (ct_compile_expression(&cs, expr, &slot), slot.usage) {
    case _slot_value:
        switch (slot.ty->tyty) {
        case CT_TYPE_CHAR: switch (slot.as.value.c) {
            case '\0': printf("'\\0'"); break;
            case '\'': printf("'\\''"); break;
            case '\"':printf("'\\\"'"); break;
            case '\?': printf("'\\?'"); break;
            case '\\':printf("'\\\\'"); break;
            case '\a': printf("'\\a'"); break;
            case '\b': printf("'\\b'"); break;
            case '\f': printf("'\\f'"); break;
            case '\n': printf("'\\n'"); break;
            case '\r': printf("'\\r'"); break;
            case '\t': printf("'\\t'"); break;
            case '\v': printf("'\\v'"); break;
            default: printf(' ' <= slot.as.value.c && slot.as.value.c <= '~' ? "'%c'" : "'\\x%02hhx'", slot.as.value.c);
        } break;
        case CT_TYPE_UCHAR:  printf("0x%02hhx", slot.as.value.uc); break;
        case CT_TYPE_SCHAR:  printf("%hhi",     slot.as.value.sc); break;
        case CT_TYPE_SHORT:  printf("%hi",      slot.as.value.ss); break;
        case CT_TYPE_INT:    printf("%i",       slot.as.value.si); break;
        case CT_TYPE_LONG:   printf("%li",      slot.as.value.sl); break;
        case CT_TYPE_USHORT: printf("%hu",      slot.as.value.us); break;
        case CT_TYPE_UINT:   printf("%u",       slot.as.value.ui); break;
        case CT_TYPE_ULONG:  printf("%lu",      slot.as.value.ul); break;
        case CT_TYPE_FLOAT:  printf("%f",       slot.as.value.f);  break;
        case CT_TYPE_DOUBLE: printf("%lf",      slot.as.value.d);  break;
        }
        break;

    case _slot_used:
        printf("TODO: slot used");
        //ct_run();
        //ct_print_item();
        break;
    case _slot_variable:
        printf("TODO: slot variable");
        //ct_print_item();
        break;
    }
    printf("\n");

    ct_lex_state cref ls = usr;
    report_lex_locate(ls, " -- tok: %.*s", bufmt(*tok));
}

void run_test(char* file)
{
    ct_lex_state ls = {0};
    ct_lini(&ls, file);

    ct_bufsl tok = ct_lext(&ls);
    ct_parse_expr_state ps = {.ls= &ls, .usr= &ls, .on= comp};
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
