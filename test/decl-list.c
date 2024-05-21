#include "run"
#include "../cintre/parser.h"

void show(void ref usr, ct_declaration cref decl, ct_bufsl ref tok)
{
    ct_print_decl(stdout, decl);
    printf("\n");
    ct_lex_state cref ls = usr;
    report_lex_locate(ls, " -- tok: %.*s", bufmt(*tok));
}

void run_test(char* file)
{
    ct_lex_state ls = {0};
    ct_lini(&ls, file);

    ct_bufsl tok = ct_lext(&ls);
    ct_parse_decl_state ps = {.ls= &ls, .usr= &ls, .on= show};
    while (tok.len) if ((tok = ct_parse_declaration(&ps, tok)).len)
        switch (*tok.ptr) {
        case '=':
            tok = ct_lext(&ls);
            if (tok.len && '{' == *tok.ptr) {

        case '{':
                for (unsigned depth = 0; (tok = ct_lext(&ls)).len; ) {
                    bool c = '}' == *tok.ptr;
                    if (!tok.len || (!depth && c)) break;
                    depth+= ('{' == *tok.ptr)-c;
                }
                tok = ct_lext(&ls);

                if (!tok.len || !strchr(",;", *tok.ptr)) {
                    ps.base = (ct_declaration){0}; // reset
                    continue;
                }

            } else tok = ct_parse_expression(&(ct_parse_expr_state){.ls= &ls, .disallow_comma= true}, tok);

            if (tok.len && ';' == *tok.ptr)
        case ';':
                ps.base = (ct_declaration){0}; // reset
            // fall through
        case ',':
            tok = ct_lext(&ls);
            continue;

        default:
            exitf("other: %.*s", (unsigned)tok.len, tok.ptr);
        }

    ct_ldel(&ls);
}
