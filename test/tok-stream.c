#include "run"

void run_test(FILE* stream, char* file)
{
    lex_state* const ls = &(lex_state){0};
    lex_entry(ls, stream, file);

    lex_define(ls, "ONE", "1");
    lex_define(ls, "TWO", "2");

    lex_incdir(ls, "./include"); // this one won't find "coucou.h"
    lex_incdir(ls, "test/in/tok-stream/include/");

    try for (tokt tok; tok = lext(ls), *tokn(tok);)
        printf("[%s:%3zu]\t%s\n",
                ls->work.ptr+ls->sources.ptr[ls->sources.len-1].file,
                ls->sources.ptr[ls->sources.len-1].line,
                ls->tokens.ptr+tok);

    lex_free(ls);
}
