#if 0
p=${0%/*}/../build/${0##*/}.exe
cc -ggdb $0 -o $p || exit 1
exec $p "$@"
#endif

#include "../preparer/lexer.h"

int main(int argc, char** argv) {
    if (1 == argc) return puts("Usage: <prog> <filename>");

    lex_state ls = {0};
    lini(&ls, argv[1]);

    while (!lend(&ls)) {
        bufsl const token = lext(&ls);
        printf("[%s:%3zu]\t%.*s\n", ls.file, ls.line, (int)token.len, token.ptr);
    }

    ldel(&ls);
}
