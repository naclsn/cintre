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

static struct ct_adpt_type {
    unsigned long const size, align;

    enum ct_adpt_type_tag {
        CT_TYPE_VOID,
        CT_TYPE_CHAR, CT_TYPE_UCHAR, CT_TYPE_SCHAR,
        CT_TYPE_SHORT, CT_TYPE_INT, CT_TYPE_LONG,
        CT_TYPE_USHORT, CT_TYPE_UINT, CT_TYPE_ULONG,
        CT_TYPE_FLOAT, CT_TYPE_DOUBLE,
        CT_TYPE_STRUCT, CT_TYPE_UNION,
        CT_TYPE_FUN,
        CT_TYPE_PTR,
        CT_TYPE_ARR,
        CT_TYPE_NAMED,
    } const tyty;

    union ct_adpt_type_info {
        struct ct_adpt_comp_desc {
            char const* const named;
            struct ct_adpt_comp_field {
                char const* const name;
                struct ct_adpt_type const* const type;
                unsigned long const offset;
                // TODO: does not handle bitfields
            } const* const fields;
            unsigned long const count;
        } const comp; // struct and union

        struct ct_adpt_fun_desc {
            struct ct_adpt_type const* const ret;
            struct ct_adpt_fun_param {
                char const* const name;
                struct ct_adpt_type const* const type;
            } const* const params;
            unsigned long const count;
        } const fun; // fun

        struct ct_adpt_type const* const ptr; // ptr

        struct ct_adpt_arr_desc {
            struct ct_adpt_type const* const item;
            unsigned long const count;
        } const arr; // arr

        struct ct_adpt_named_desc {
            struct ct_adpt_type const* const def;
            char const* const name;
        } const named; // typedefs
    } const info;
}
const ct_adptb_void_type = {0}
, ct_adptb_char_type     = {.size= sizeof(char),           .align= sizeof(char),           .tyty= CT_TYPE_CHAR  }
, ct_adptb_uchar_type    = {.size= sizeof(unsigned char),  .align= sizeof(unsigned char),  .tyty= CT_TYPE_UCHAR }
, ct_adptb_schar_type    = {.size= sizeof(signed char),    .align= sizeof(signed char),    .tyty= CT_TYPE_SCHAR }
, ct_adptb_short_type    = {.size= sizeof(short),          .align= sizeof(short),          .tyty= CT_TYPE_SHORT }
, ct_adptb_int_type      = {.size= sizeof(int),            .align= sizeof(int),            .tyty= CT_TYPE_INT   }
, ct_adptb_long_type     = {.size= sizeof(long),           .align= sizeof(long),           .tyty= CT_TYPE_LONG  }
, ct_adptb_ushort_type   = {.size= sizeof(unsigned short), .align= sizeof(unsigned short), .tyty= CT_TYPE_USHORT}
, ct_adptb_uint_type     = {.size= sizeof(unsigned int),   .align= sizeof(unsigned int),   .tyty= CT_TYPE_UINT  }
, ct_adptb_ulong_type    = {.size= sizeof(unsigned long),  .align= sizeof(unsigned long),  .tyty= CT_TYPE_ULONG }
, ct_adptb_float_type    = {.size= sizeof(float),          .align= sizeof(float),          .tyty= CT_TYPE_FLOAT }
, ct_adptb_double_type   = {.size= sizeof(double),         .align= sizeof(double),         .tyty= CT_TYPE_DOUBLE}
;

static inline struct ct_adpt_type const* _ct_truetype(struct ct_adpt_type const* ty)
{
    while (CT_TYPE_NAMED == ty->tyty) ty = ty->info.named.def;
    return ty;
}
static inline struct ct_adpt_type const* _ct_tailtype(struct ct_adpt_type const* ty)
{
    for (ty = _ct_truetype(ty); CT_TYPE_PTR == ty->tyty; ty = _ct_truetype(ty->info.ptr));
    return ty;
}

struct ct_adpt_item {
    char const* const name;
    struct ct_adpt_type const* const type;
    enum {
        CT_ITEM_VALUE, // enumerator values
        CT_ITEM_OBJECT, // object, function
        CT_ITEM_TYPEDEF, // typedef
        CT_ITEM_VARIABLE, // runtime stack variable
    } kind;
    union {
        long const value; // int is enough, but to be safer
        void* const object;
        void (* const function)(char*, char**);
        unsigned long const variable;
    } as;
};

struct ct_adpt_namespace {
    char const* const name;
    unsigned long const count;
    struct ct_adpt_item const* const items;
};

#endif // CINTRE_ADAPT_H
