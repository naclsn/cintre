#if 0
set -ex
cd ${0%/*} #*/
cc tok-stream.c -o ../build/tok-stream && \
       ../build/tok-stream tok-stream.c |:
exit
#endif

#include "../preparer/lexer.h"

int main(int argc, char** argv) {
    if (1 == argc) return 1;

    lex_state ls = {0};
    // TODO: remove both once fixed in `lexer.h`
    linc(&ls, "./");
    linc(&ls, "./preparer/");

    lini(&ls, argv[1]);
    while (!lend(&ls)) {
        bufsl const token = lext(&ls);
        printf("[%.*s:%zu] %.*s\n", (int)ls.file.len, ls.file.ptr, ls.line, (int)token.len, token.ptr);
    }
    ldel(&ls);

    return 0;
}
