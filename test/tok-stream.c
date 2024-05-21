#include "run"

void run_test(char* file)
{
    ct_lex_state ls = {0};
    ct_lini(&ls, file);

    ct_ldef(&ls, "ONE", "1");
    ct_ldef(&ls, "TWO", "2");

    ct_linc(&ls, "./include"); // this one won't find "coucou.h"
    ct_linc(&ls, "test/in/tok-stream/include/");

    try while (!lend(&ls)) {
        ct_bufsl const token = ct_lext(&ls);
        printf("[%s:%3zu]\t%.*s\n", ls.file, ls.line, (int)token.len, token.ptr);
    }

    ct_ldel(&ls);
}
