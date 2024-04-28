#include "run"
#include "../cintre/parser.h"

void show(void ref _, declaration cref decl, bufsl ref tok) {
    (void)_;
    print_decl(stdout, decl);
    printf(" -- tok: %.*s\n", (unsigned)tok->len, tok->ptr);
}

void run_test(char* file) {
    lex_state ls = {0};
    lini(&ls, file);

    bufsl tok = lext(&ls);
    parse_decl_state ps = {.ls= &ls, .on= show};
    while (tok.len) if ((tok = parse_declaration(&ps, tok)).len)
        switch (*tok.ptr) {
        case '{':
            for (unsigned depth = 0; (tok = lext(&ls)).len; ) {
                bool c = '}' == *tok.ptr;
                if (!tok.len || (!depth && c)) break;
                depth+= ('{' == *tok.ptr)-c;
            }
            tok = lext(&ls);
            ps.base = (declaration){0}; // reset
            continue;

        case '=':
            tok = parse_expression(&(parse_expr_state){.ls= &ls}, lext(&ls));
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
