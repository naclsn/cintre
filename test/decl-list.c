#include "run"
#include "../cintre/parser.h"

void show(void ref usr, declaration cref decl, tokt ref tok)
{
    lex_state cref ls = usr;
    print_decl(stdout, ls, decl);
    printf("\n");
    report_lex_locate(ls, " -- tok: %s", tokn(*tok));
}

void run_test(FILE* stream, char* file)
{
    lex_state* const ls = &(lex_state){0};
    lex_entry(ls, stream, file);

    tokt tok = lext(ls);
    parse_decl_state ps = {.ls= ls, .usr= ls, .on= show};
    while (*tokn(tok)) switch (tok = parse_declaration(&ps, tok), *tokn(tok)) {
    case '=':
        tok = lext(ls);
        if ('{' == *tokn(tok)) {

    case '{':
            for (unsigned depth = 0; (tok = lext(ls)), *tokn(tok); ) {
                bool c = '}' == *tokn(tok);
                if (!*tokn(tok) || (!depth && c)) break;
                depth+= ('{' == *tokn(tok))-c;
            }
            tok = lext(ls);

            if (!*tokn(tok) || !strchr(",;", *tokn(tok))) {
                ps.base = (declaration){0}; // reset
                continue;
            }

        } else tok = parse_expression(&(parse_expr_state){.ls= ls, .disallow_comma= true}, tok);

        if (';' == *tokn(tok))
    case ';':
            ps.base = (declaration){0}; // reset
        // fall through
    case ',':
        tok = lext(ls);
        continue;

    default:
        exitf("other: %s", tokn(tok));
    }

    lex_free(ls);
}
