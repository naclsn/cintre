struct arr {
    struct itm {
        int x, y, z;
        unsigned c;
    }* ptr;
    long unsigned len;
} makestuffup(void) {
    static struct itm rr[] = {
        {2, 0, 0, 0xff0000},
        {0, +1, 0, 0x00ff00},
        {0, -1, 0, 0x0000ff},
    };
    static struct arr r = {
        .ptr= rr,
        .len= sizeof rr / sizeof*rr,
    };
    return r;
}

/*
int bidoof = 42;

int hi(int c) {
    int puts(char const*);
    int r = 0;
    while (c--) r+= puts("hi");
    return r;
}

#define _JOIN(__l, __r) __l##__r
#define _XJOIN(__l, __r) _JOIN(__l, __r)
#define _CALL(__macro, ...) __macro(__VA_ARGS__)

#define  _FOR_TYNM_1(__n, __macro, __inv, __ty, __nm)       __macro((__n- 1), __n, __inv, __ty, __nm)
#define  _FOR_TYNM_2(__n, __macro, __inv, __ty, __nm, ...)  __macro((__n- 2), __n, __inv, __ty, __nm)  _FOR_TYNM_1(__n, __macro, __inv, __VA_ARGS__)
#define  _FOR_TYNM_3(__n, __macro, __inv, __ty, __nm, ...)  __macro((__n- 3), __n, __inv, __ty, __nm)  _FOR_TYNM_2(__n, __macro, __inv, __VA_ARGS__)
#define  _FOR_TYNM_4(__n, __macro, __inv, __ty, __nm, ...)  __macro((__n- 4), __n, __inv, __ty, __nm)  _FOR_TYNM_3(__n, __macro, __inv, __VA_ARGS__)
#define  _FOR_TYNM_5(__n, __macro, __inv, __ty, __nm, ...)  __macro((__n- 5), __n, __inv, __ty, __nm)  _FOR_TYNM_4(__n, __macro, __inv, __VA_ARGS__)
#define _FOR_TYNM(__n, __macro, __inv, ...)  _FOR_TYNM_##__n(__n, __macro, __inv, __VA_ARGS__)

#define _typename(__ty, ...) _CALL(_typename_##__ty, __VA_ARGS__)


#define _typename_void() char


#define _union_getvar_name(__tty, __tva, __nm)     __nm
#define _union_getvar_tagtype(__tty, __tva, __nm)  __tty
#define _union_getvar_tagvalue(__tty, __tva, __nm) __tva

#define _union_kinds_typename_one(__k, __n, __inv, __ty, __nm) _typename __ty _union_getvar_name __nm;
#define _union_kinds_tagname_one(__k, __n, __tname, __ty, __nm) _XJOIN(__tname##_tag_, _union_getvar_name __nm) = __k,

#define bipa_union(__tname, __n_kinds, ...)                                                                 \
    struct __tname {                                                                                        \
        union {                                                                                             \
            _FOR_TYNM(__n_kinds, _union_kinds_typename_one, 0, __VA_ARGS__)                                 \
        } val;                                                                                              \
        enum __tname##_tag_enum {                                                                           \
            _FOR_TYNM(__n_kinds, _union_kinds_tagname_one, __tname, __VA_ARGS__)                            \
        } tag;                                                                                              \
    };                                                                                                      \

bipa_union(color_type, 5
        , (void,), (u8, 0, grayscale)
        , (void,), (u8, 2, truecolor)
        , (void,), (u8, 3, indexed_color)
        , (void,), (u8, 4, grayscale_with_alpha)
        , (void,), (u8, 6, truecolor_with_alpha)
        )
        */
