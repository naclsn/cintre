#include <stdio.h>

#include "adapt.h"
#include "mylib_adapt.h"

char stack[2048];
size_t sp = 2048;
#define top() (stack+sp)
#define topas(ty) ((ty*)top())
#define stackalloc(sz, aln) (sp = (sp-sz)/aln*aln)
#define stackalloct(t) stackalloc((t).size, (t).align)
#define stackalloc2(w) (sp = (sp-(1<<w)) & ~(size_t)0<<2)
char static_mem[2048];
char* static_head = static_mem;

int function_test(void) {
    char func_name[30];
    scanf("%s", func_name);

    search_namespace(func_name, mylib_namespace) {
        printf("found %p\n", mylib_namespace[k].as.object);

        //size_t size = mylib_namespace[k].info.size, align = mylib_namespace[k].info.align;
        // YYY: aligning could be a simple (sp-size)&ffs(align), this will do for now
        //sp = (sp-size)/align*align;
        //stackalloc(mylib_namespace[k].info.size, mylib_namespace[k].info.align);
        stackalloct(mylib_namespace[k].info);
        char* ret = top();

        size_t argc;
        while (!feof(stdin)) {
            //sp = (sp-1) & ~(size_t)0 << 2; // char: 0, short: 1, int/float: 2, long/double: 3
            stackalloc2(2);
            scanf("%d", topas(int));
            argc++;
        }
        argc--, sp+= 4; // compensate for my suckiness

        printf("32b stack: %zu/%zu\n", sp, sizeof stack);
        for (size_t k = 0; k < 5; k++)
            printf("\t%zu-%zu: %i\n", sizeof stack, (sizeof stack-sp)-4*k, topas(int)[k]);

        char* argv[argc];
        size_t h = sp;
        for (size_t k = 0; k < argc; k++) {
            size_t const w = 2; // char: 0, ...
            h = (h+(1<<w)-1) & ~(size_t)0<<w;
            argv[argc-1-k] = &stack[h];
            h+= (1<<w);
        }

        for (size_t k = 0; k < argc; k++)
            printf("\t[%zu]: %d\n", k, *(int*)argv[k]);

        mylib_namespace[k].as.function(ret, argv);

        sp = ret-stack;
        printf("int res: %d\n", *topas(int));

        return 0;
    }

    printf("name not found: %s\n", func_name);
    return 1;
}

int object_test(void) {
    stackalloct(mystruct_adapt.info);
    char* res = top(); // a struct mystruct*

    //for in .fields ...

    // uuh :s special case because static literal string
    scanf("%s", static_head);
    *(char**)(res+mystruct_adapt.fields[0].offset) = static_head;
    static_head+= strlen(static_head);

    scanf("%llu", (unsigned long long*)(res+mystruct_adapt.fields[1].offset));

    char* args[1] = {res};
    myprint_adapt(NULL, args);

    return 1;
}

int main(void) { function_test(); }
