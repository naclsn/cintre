#include "common.h"
#include "lexer.h"

lex_state ls;

void cleanup(void) {
    ldel(&ls);
}

int main(int argc, char** argv) {
    atexit(cleanup);
    char const* prog = (argc--, *argv++);
    if (!argc || !strcmp("-h", *argv) || !strcmp("--help", *argv)) exitf("Usage: %s <entry.h> [-D...,-I...]", prog);
    char const* file = (argc--, *argv++);

    linc(&ls, "./");

    while (argc) {
        char const* arg = (argc--, *argv++);

        if (!memcmp("-D", arg, 2)) {
            char* val = strchr(arg, '=');
            if (val) *(val++) = '\0';
            else val = "1";
            ldef(&ls, arg+2, val);
        }

        else if (!memcmp("-I", arg, 2)) {
            linc(&ls, arg+2);
        }

        else notif("unused or unimplemented argument: %s", arg);
    } // while args

    lini(&ls, file);

    while (!lend(&ls)) {
        bufsl const tok = lext(&ls);
        printf("/*%.*s:%zu*/ %.*s\n", (unsigned)ls.file.len, ls.file.ptr, ls.line, (unsigned)tok.len, tok.ptr);
    }

    return EXIT_SUCCESS;
}
