#include "run"
#include "../cintre/parser.h"

void show(void ref usr, declaration cref decl, tokt ref tok)
{
    parse_decl_state ref ps = usr;
    lex_state ref ls = ps->ls;
    print_decl(stdout, ls, decl);
    printf("\n");
    report_lex_locate(ls, " -- tok: %s", tokn(*tok));

    switch (*tokn(*tok)) {
    case '=':
        *tok = lext(ls);
        if ('{' == *tokn(*tok)) {

    case '{':
            for (unsigned depth = 0; (*tok = lext(ls)), *tokn(*tok); ) {
                bool c = '}' == *tokn(*tok);
                if (!*tokn(*tok) || (!depth && c)) break;
                depth+= ('{' == *tokn(*tok))-c;
            }
            *tok = lext(ls);

            if (!*tokn(*tok) || !strchr(",;", *tokn(*tok))) {
                ps->base = NULL; // reset
                return;
            }

        } else *tok = parse_expression(&(parse_expr_state){.ls= ls, .disallow_comma= true}, *tok);

        if (';' == *tokn(*tok)) {
    case ';':
            *tok = lext(ls);
            ps->base = NULL; // reset
            return;
        }
        // fall through
    case ',':
        *tok = parse_declaration(ps, lext(ls));
        return;

    case '\0':
        return;

    default:
        exitf("other: %s", tokn(*tok));
    }
}

void run_test(FILE* stream, char* file)
{
    lex_state* const ls = &(lex_state){0};
    lex_entry(ls, stream, file);

    parse_decl_state ps = {.ls= ls, .usr= &ps, .on= show};
    for (tokt tok = lext(ls); *tokn(tok); tok = parse_declaration(&ps, tok));

    lex_free(ls);
}
