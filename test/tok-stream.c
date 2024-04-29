#include "run"

void run_test(char* file) {
    lex_state ls = {0};
    lini(&ls, file);

    ldef(&ls, "ONE", "1");
    ldef(&ls, "TWO", "2");
    //ldef(&ls, "COMP", "");

    try while (!lend(&ls)) {
        bufsl const token = lext(&ls);
        printf("[%s:%3zu]\t%.*s\n", ls.file, ls.line, (int)token.len, token.ptr);
    }

    ldel(&ls);
}
