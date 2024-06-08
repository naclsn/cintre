#include "run"
#include "../cintre/compiler.h"

void comp(void ref usr, expression ref expr, tokt ref tok)
{
    lex_state cref ls = usr;
    static char stack[1042];
    compile_state cs = {.ls= ls, .vsp= sizeof stack};

    struct slot slot = {.ty= check_expression(&cs, expr)};
    if (!slot.ty) printf("checking error");
    else switch (compile_expression(&cs, expr, &slot), slot.usage) {
    case _slot_value:
        switch (slot.ty->tyty) {
        case TYPE_CHAR: switch (slot.as.value.c) {
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
        case TYPE_UCHAR:  printf("0x%02hhx", slot.as.value.uc); break;
        case TYPE_SCHAR:  printf("%hhi",     slot.as.value.sc); break;
        case TYPE_SHORT:  printf("%hi",      slot.as.value.ss); break;
        case TYPE_INT:    printf("%i",       slot.as.value.si); break;
        case TYPE_LONG:   printf("%li",      slot.as.value.sl); break;
        case TYPE_USHORT: printf("%hu",      slot.as.value.us); break;
        case TYPE_UINT:   printf("%u",       slot.as.value.ui); break;
        case TYPE_ULONG:  printf("%lu",      slot.as.value.ul); break;
        case TYPE_FLOAT:  printf("%f",       slot.as.value.f);  break;
        case TYPE_DOUBLE: printf("%lf",      slot.as.value.d);  break;
        }
        break;

    case _slot_used:
        printf("TODO: slot used");
        //run();
        //print_item();
        break;
    case _slot_variable:
        printf("TODO: slot variable");
        //print_item();
        break;
    }
    printf("\n");

    report_lex_locate(ls, " -- tok: %s", tokn(*tok));
}

void run_test(FILE* stream, char* file)
{
    lex_state* const ls = &(lex_state){0};
    lex_entry(ls, stream, file);

    tokt tok = lext(ls);
    parse_expr_state ps = {.ls= ls, .usr= ls, .on= comp};
    while (*tokn(tok)) switch (tok = parse_expression(&ps, tok), *tokn(tok)) {
    case ';':
        tok = lext(ls);
        continue;

    default:
        exitf("other: %s", tokn(tok));
    }

    lex_free(ls);
}
