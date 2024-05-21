#include "run"

void show(void ref usr, ct_expression ref expr, ct_bufsl ref tok)
{
    ct_lex_state cref ls = usr;
    ct_print_expr(stdout, expr, 0);
    report_lex_locate(ls, " -- tok: %.*s", bufmt(*tok));
}

void run_test(char* file)
{
    ct_lex_state ls = {0};
    ct_lini(&ls, file);

    ct_bufsl tok = ct_lext(&ls);
    ct_parse_expr_state ps = {.ls= &ls, .usr= &ls, .on= show};
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
