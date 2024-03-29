#ifndef CINTRE_ADAPT_H
#define CINTRE_ADAPT_H

#include <stddef.h>
#include <stdlib.h>

#define countof(__a) (sizeof(__a)/sizeof*(__a))
#define alignof(...) (offsetof(struct { char _; __VA_ARGS__ it; }, it))

static struct adpt_type {
    size_t const size, align;

    enum adpt_type_kind {
        ADPT_KIND_VOID,
        ADPT_KIND_CHAR, ADPT_KIND_UCHAR, ADPT_KIND_SCHAR,
        ADPT_KIND_SHORT, ADPT_KIND_INT, ADPT_KIND_LONG,
        ADPT_KIND_USHORT, ADPT_KIND_UINT, ADPT_KIND_ULONG,
        ADPT_KIND_ENUM,
        ADPT_KIND_FLOAT, ADPT_KIND_DOUBLE,
        ADPT_KIND_STRUCT, ADPT_KIND_UNION,
        ADPT_KIND_FUN,
        ADPT_KIND_PTR,
    } const kind;

    union adpt_type_info {
        struct adpt_comp_desc {
            struct adpt_comp_field {
                char const* const name;
                struct adpt_type const* const type;
                size_t const offset;
            } const* const fields;
            size_t const count;
        } const comp; // struct and union

        struct adpt_fun_desc {
            struct adpt_type const* const ret;
            struct adpt_fun_param {
                char const* const name;
                struct adpt_type const* const type;
            } const* const params;
            size_t const count;
        } const fun; // fun

        struct adpt_type const* const to; // ptr
    } const info;

} const adptb_void_type = {0}
, adptb_char_type       = {.size= sizeof(char),               .align= sizeof(char),               .kind= ADPT_KIND_CHAR  }
, adptb_uchar_type      = {.size= sizeof(unsigned char),      .align= sizeof(unsigned char),      .kind= ADPT_KIND_UCHAR }
, adptb_schar_type      = {.size= sizeof(signed char),        .align= sizeof(signed char),        .kind= ADPT_KIND_SCHAR }
, adptb_short_type      = {.size= sizeof(short),              .align= sizeof(short),              .kind= ADPT_KIND_SHORT }
, adptb_int_type        = {.size= sizeof(int),                .align= sizeof(int),                .kind= ADPT_KIND_INT   }
, adptb_long_type       = {.size= sizeof(long),               .align= sizeof(long),               .kind= ADPT_KIND_LONG  }
, adptb_ushort_type     = {.size= sizeof(unsigned short),     .align= sizeof(unsigned short),     .kind= ADPT_KIND_USHORT}
, adptb_uint_type       = {.size= sizeof(unsigned int),       .align= sizeof(unsigned int),       .kind= ADPT_KIND_UINT  }
, adptb_ulong_type      = {.size= sizeof(unsigned long),      .align= sizeof(unsigned long),      .kind= ADPT_KIND_ULONG }
, adptb_float_type      = {.size= sizeof(float),              .align= sizeof(float),              .kind= ADPT_KIND_FLOAT }
, adptb_double_type     = {.size= sizeof(double),             .align= sizeof(double),             .kind= ADPT_KIND_DOUBLE}
;

/*
#define adptb_ptr_type(__to) (struct adpt_type){  \
        .size= sizeof(void*),                     \
        .align= sizeof(void*),                    \
        .kind= ADPT_KIND_PTR,                     \
        .info.to= &__to,                          \
    }
*/

struct adpt_item {
    char const* const name;
    struct adpt_type const* type;
    union {
        void* const object;
        void (* const function)(char*, char**);
    } as;
};

#endif // CINTRE_ADAPT_H
