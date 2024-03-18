#ifndef __ADAPT__
#define __ADAPT__

#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#define countof(__a) (sizeof(__a)/sizeof*(__a))
#define alignof(...) (offsetof(struct { char _; __VA_ARGS__ it; }, it))
// note to later self: if that is indeed incorrect, an other possible one is
// that the alignment of a structure it the alignment of its first member and
// recursively (maybe, idk)

#define search_namespace(__n, __ns)  \
    for (size_t k = 0; (__ns)[k].name; k++)  \
        if (!strcmp((__ns)[k].name, (__n)))

struct _type { size_t size, align; };
#define infoof(...) {.size= sizeof(__VA_ARGS__), .align= alignof(__VA_ARGS__)}

struct _item {
    char const* const name;
    union {
        void* object;
        void (*function)(char*, char**);
    } as;
    struct _type const info; // for a function this is the return's type
};

struct _struct {
    struct _type const info;
    struct _field_desc {
        char const* const name;
        size_t const offset;
        struct _type const info;
    }* fields;
};

struct _func {
    size_t idk;
};

struct _param {
    char const* const name;
};

#endif // __ADAPT__
