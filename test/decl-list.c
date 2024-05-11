#include "run"
#include "../cintre/parser.h"

void show(void ref usr, declaration cref decl, bufsl ref tok)
{
    print_decl(stdout, decl);
    printf("\n");
    lex_state cref ls = usr;
    report_lex_locate(ls, " -- tok: %.*s", bufmt(*tok));
}

void run_test(char* file)
{
    lex_state ls = {0};
    lini(&ls, file);

    bufsl tok = lext(&ls);
    parse_decl_state ps = {.ls= &ls, .usr= &ls, .on= show};
    while (tok.len) if ((tok = parse_declaration(&ps, tok)).len)
        switch (*tok.ptr) {
        case '=':
            tok = lext(&ls);
            if (tok.len && '{' == *tok.ptr) {

        case '{':
                for (unsigned depth = 0; (tok = lext(&ls)).len; ) {
                    bool c = '}' == *tok.ptr;
                    if (!tok.len || (!depth && c)) break;
                    depth+= ('{' == *tok.ptr)-c;
                }
                tok = lext(&ls);

                if (!tok.len || !strchr(",;", *tok.ptr)) {
                    ps.base = (declaration){0}; // reset
                    continue;
                }

            } else tok = parse_expression(&(parse_expr_state){.ls= &ls, .disallow_comma= true}, tok);

            if (tok.len && ';' == *tok.ptr)
        case ';':
                ps.base = (declaration){0}; // reset
            // fall through
        case ',':
            tok = lext(&ls);
            continue;

        default:
            exitf("other: %.*s", (unsigned)tok.len, tok.ptr);
        }

    ldel(&ls);
}
