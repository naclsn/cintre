#ifndef _PREP_ADAPT_DOTHETHING
#include "run"

#define main(c, v) _preparer_main(c, v)
#include "../cintre/preparer.c"

#define stacat(__name, ...)                                                   \
    size_t __name##_len = 0;                                                  \
    for (char const** _it = (char const*[]){__VA_ARGS__, NULL}; *_it; ++_it)  \
        __name##_len+= strlen(*_it);                                          \
    char __name[__name##_len+1]; __name[0] = '\0';                            \
    for (char const** _it = (char const*[]){__VA_ARGS__, NULL}; *_it; ++_it)  \
        strcat(__name, *_it);

void run_test(char* file)
{
    char cref name = strrchr(file, '/') ? strrchr(file, '/')+1 : file;

    stacat(src, "/tmp/cintre-test-", name, ".c");
    stacat(bin, "/tmp/cintre-test-", name, ".exe");

    char ab[strlen(src)-strlen("/tmp/")+1];
    strcpy(ab, src+strlen("/tmp/"));
    bufsl const _ns = name_space(ab);
    char ns[_ns.len+1];
    memcpy(ns, _ns.ptr, _ns.len);
    ns[_ns.len] = '\0';

    stacat(com, "${CC:-cc} " __FILE__ " -Icintre -include ", src, " -D_PREP_ADAPT_DOTHETHING=adptns_", ns, " -o ", bin, " -Wl,--unresolved-symbols=ignore-all");

    do_prepare(3, (char*[]){file, "-o", src, NULL});
    extern int execv(char const* path, char* const argv[]);
    printf("+++ %s\n", com);
    if (!system(com)) execv(bin, (char*[]){NULL});
}

#else // _PREP_ADAPT_DOTHETHING

#include "prints.h"

int main(void)
{
    for (size_t k = 0; k < countof(_PREP_ADAPT_DOTHETHING); k++) {
        struct adpt_item cref it = _PREP_ADAPT_DOTHETHING+k;
        //printf("%s: ", );
        print_item(stdout, it);
    }
}

#endif // _PREP_ADAPT_DOTHETHING
