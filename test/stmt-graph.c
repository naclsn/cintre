#include "run"

bool valid;
void show(void ref usr, statement ref stmt, tokt ref tok)
{
    lex_state cref ls = usr;
    print_stmt(stdout, ls, stmt, 0);
    report_lex_locate(ls, " -- tok: %s", tokn(*tok));
    valid = true;
}

void run_test(FILE* stream, char* file)
{
    lex_state* const ls = &(lex_state){0};
    lex_entry(ls, stream, file);

    parse_stmt_state ps = {.ls= ls, .usr= ls, .on= show};
    for (tokt tok = lext(ls); *tokn(tok); valid = false) {
        tok = parse_statement(&ps, tok);
        if (!valid) exitf("invalid: %s", tokn(tok));
    }

    lex_free(ls);
}
