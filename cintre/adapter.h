#ifndef CINTRE_ADAPT_H
#define CINTRE_ADAPT_H

#include <stddef.h>
#include <stdlib.h>

//#define countof(__a) (sizeof(__a)/sizeof*(__a))
#define alignof(...) (offsetof(struct { char _; __VA_ARGS__ it; }, it))

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

#endif // CINTRE_ADAPT_H
