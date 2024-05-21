#ifndef _PREP_ADAPT_DUMP_NS
#undef LOC_NOTIF
#define main(c, v) _preparer_main(c, v)
#include "../cintre/preparer.c"
#undef main

#include "run"

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
    ct_bufsl const _ns = name_space(ab);
    char ns[_ns.len+1];
    memcpy(ns, _ns.ptr, _ns.len);
    ns[_ns.len] = '\0';

    stacat(com, "${CC:-cc} " __FILE__ " -Icintre -include ", src, " -D_PREP_ADAPT_DUMP_NS=adptns_", ns, " -o ", bin, " -Wl,--unresolved-symbols=ignore-all");

    if (!strcmp("cintre_test_std", ns))
        do_prepare(5, (char*[]){file, "-Pno-emit-decl", "-Pno-emit-incl", "-o", src, NULL});
    else
        do_prepare(3, (char*[]){file, "-o", src, NULL});
    fflush(result);
    extern int execv(char const* path, char* const argv[]);
    if (!system(com)) execv(bin, (char*[]){NULL});
}

#else // _PREP_ADAPT_DUMP_NS

#include "prints.h"

int main(void)
{
    for (size_t k = 0; k < countof(_PREP_ADAPT_DUMP_NS); k++) {
        struct ct_adpt_item cref it = _PREP_ADAPT_DUMP_NS+k;
        printf("%s: ", it->name);
        ct_print_type(stdout, it->type);
        printf(";\n");
    }
}

#endif // _PREP_ADAPT_DUMP_NS
