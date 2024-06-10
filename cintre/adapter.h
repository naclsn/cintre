/// Data types and util used to describe functions and objects in a parsed
/// header. Typically included by an "adapter" <a-file.h> (see preparer).
///
/// A namespace has a name and a number of items, an item is (statically) an
/// object or a function with its type. Everything is considered static and
/// immutable, because that's how it would be in an adapter file.
///
/// Once again LP64 is assumed, this matters for `sizeof(size_t) == sizeof(long) == 8`.

#ifndef CINTRE_ADAPT_H
#define CINTRE_ADAPT_H

#ifndef alignof
# ifndef offsetof
#  include <stddef.h>
# endif
# define alignof(...) (offsetof(struct { char _; __VA_ARGS__ it; }, it))
#endif

static struct adpt_type {
    unsigned long const size, align;

    enum adpt_type_tag {
        ADPT_TYPE_VOID,
        ADPT_TYPE_CHAR, ADPT_TYPE_UCHAR, ADPT_TYPE_SCHAR,
        ADPT_TYPE_SHORT, ADPT_TYPE_INT, ADPT_TYPE_LONG,
        ADPT_TYPE_USHORT, ADPT_TYPE_UINT, ADPT_TYPE_ULONG,
        ADPT_TYPE_FLOAT, ADPT_TYPE_DOUBLE,
        ADPT_TYPE_STRUCT, ADPT_TYPE_UNION,
        ADPT_TYPE_FUN,
        ADPT_TYPE_PTR,
        ADPT_TYPE_ARR,
        ADPT_TYPE_NAMED,
    } const tyty;

    union adpt_type_info {
        struct adpt_comp_desc {
            char const* const named;
            struct adpt_comp_field {
                char const* const name;
                struct adpt_type const* const type;
                unsigned long const offset;
                // TODO: does not handle bitfields
            } const* const fields;
            unsigned long const count;
        } const comp; // struct and union

        struct adpt_fun_desc {
            struct adpt_type const* const ret;
            struct adpt_fun_param {
                char const* const name;
                struct adpt_type const* const type;
            } const* const params;
            unsigned long const count;
        } const fun; // fun

        struct adpt_type const* const ptr; // ptr

        struct adpt_arr_desc {
            struct adpt_type const* const item;
            unsigned long const count;
        } const arr; // arr

        struct adpt_named_desc {
            struct adpt_type const* const def;
            char const* const name;
        } const named; // typedefs
    } const info;
}
const adptb_void_type = {0}
, adptb_char_type     = {.size= sizeof(char),           .align= sizeof(char),           .tyty= ADPT_TYPE_CHAR  }
, adptb_uchar_type    = {.size= sizeof(unsigned char),  .align= sizeof(unsigned char),  .tyty= ADPT_TYPE_UCHAR }
, adptb_schar_type    = {.size= sizeof(signed char),    .align= sizeof(signed char),    .tyty= ADPT_TYPE_SCHAR }
, adptb_short_type    = {.size= sizeof(short),          .align= sizeof(short),          .tyty= ADPT_TYPE_SHORT }
, adptb_int_type      = {.size= sizeof(int),            .align= sizeof(int),            .tyty= ADPT_TYPE_INT   }
, adptb_long_type     = {.size= sizeof(long),           .align= sizeof(long),           .tyty= ADPT_TYPE_LONG  }
, adptb_ushort_type   = {.size= sizeof(unsigned short), .align= sizeof(unsigned short), .tyty= ADPT_TYPE_USHORT}
, adptb_uint_type     = {.size= sizeof(unsigned int),   .align= sizeof(unsigned int),   .tyty= ADPT_TYPE_UINT  }
, adptb_ulong_type    = {.size= sizeof(unsigned long),  .align= sizeof(unsigned long),  .tyty= ADPT_TYPE_ULONG }
, adptb_float_type    = {.size= sizeof(float),          .align= sizeof(float),          .tyty= ADPT_TYPE_FLOAT }
, adptb_double_type   = {.size= sizeof(double),         .align= sizeof(double),         .tyty= ADPT_TYPE_DOUBLE}
;

static inline struct adpt_type const* _truetype(struct adpt_type const* ty)
{
    while (ADPT_TYPE_NAMED == ty->tyty) ty = ty->info.named.def;
    return ty;
}
static inline struct adpt_type const* _tailtype(struct adpt_type const* ty)
{
    for (ty = _truetype(ty); ADPT_TYPE_PTR == ty->tyty; ty = _truetype(ty->info.ptr));
    return ty;
}

struct adpt_item {
    char const* const name;
    struct adpt_type const* const type;
    enum {
        ADPT_ITEM_VALUE, // enumerator values
        ADPT_ITEM_OBJECT, // object, function
        ADPT_ITEM_TYPEDEF, // typedef
        ADPT_ITEM_VARIABLE, // runtime stack variable
    } kind;
    union {
        long const value; // int is enough, but to be safer
        void* const object;
        void (* const function)(char*, char**);
        unsigned long const variable;
    } as;
};

struct adpt_namespace {
    char const* const name;
    unsigned long const count;
    struct adpt_item const* const items;
};

#endif // CINTRE_ADAPT_H
