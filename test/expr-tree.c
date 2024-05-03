#include "run"

void show(void ref usr, expression ref expr, bufsl ref tok) {
    lex_state cref ls = usr;
    print_expr(stdout, expr, 0);
    report_lex_locate(ls, " -- tok: %.*s", bufmt(*tok));
}

void run_test(char* file) {
    lex_state ls = {0};
    lini(&ls, file);

    bufsl tok = lext(&ls);
    parse_expr_state ps = {.ls= &ls, .usr= &ls, .on= show};
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
