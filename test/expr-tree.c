#include "run"

void show(void ref usr, expression ref expr, tokt ref tok)
{
    lex_state cref ls = usr;
    print_expr(stdout, ls, expr, 0);
    report_lex_locate(ls, " -- tok: %s", tokn(*tok));
}

void run_test(FILE* stream, char* file)
{
    lex_state* const ls = &(lex_state){0};
    lex_entry(ls, stream, file);

    tokt tok = lext(ls);
    parse_expr_state ps = {.ls= ls, .usr= ls, .on= show};
    while (*tokn(tok)) switch (tok = parse_expression(&ps, tok), *tokn(tok)) {
    case ';':
        tok = lext(ls);
        continue;

    default:
        exitf("other: %s", tokn(tok));
    }

    lex_free(ls);
}
