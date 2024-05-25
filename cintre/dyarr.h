#include <stdbool.h>
#include <stdlib.h>

#ifndef _dyarr_emptyresize
#define _dyarr_emptyresize 8
#endif

#ifndef _dyarr_allocfail
#define _dyarr_allocfail NULL
#endif

#if __STDC_VERSION__ < 199901L
#define inline
#endif

static inline bool  _ct_dyarr_resize(void** ptr, size_t isz, size_t* cap, size_t rsz);
static inline void* _ct_dyarr_insert(void** ptr, size_t isz, size_t* cap, size_t* len, size_t k, size_t n);
static inline void  _ct_dyarr_remove(void** ptr, size_t isz, size_t* len, size_t k, size_t n);
static inline void* _ct_dyarr_replace(void** ptr, size_t isz, size_t* cap, size_t* len, size_t k, size_t n, void* spt, size_t sln);
static inline int   _ct_dyarr_cmp(void** s1, size_t z1, void** s2, size_t z2);

#ifdef inline
#undef inline
#endif

#ifndef ct_dyarr
#include <string.h>

#define ct_dyarr(...) struct { __VA_ARGS__* ptr; size_t len, cap; }

/* clears it empty and free used memory */
#define dyarr_clear(__da)  ((__da)->len = (__da)->cap = (__da)->cap ? free((__da)->ptr), 0 : 0, (__da)->ptr = NULL)
/* resizes exactly to given, new size should not be 0 */
#define dyarr_resize(__da, __rsz)  _ct_dyarr_resize((void**)&(__da)->ptr, sizeof*(__da)->ptr, &(__da)->cap, (__rsz))

/* doubles the capacity if more memory is needed */
#define dyarr_push(__da)  ((__da)->len < (__da)->cap || dyarr_resize((__da), (__da)->cap ? (__da)->cap*2 : (_dyarr_emptyresize)) ? &(__da)->ptr[(__da)->len++] : (_dyarr_allocfail))
/* NULL if empty */
#define dyarr_pop(__da)  ((__da)->len ? &(__da)->ptr[--(__da)->len] : NULL)
/* NULL if empty */
#define dyarr_top(__da)  ((__da)->len ? &(__da)->ptr[(__da)->len-1] : NULL)
/* NULL if empty */
#define dyarr_bot(__da)  ((__da)->len ? &(__da)->ptr[0] : NULL)

/* insert spaces at [k : k+n], NULL if OOM else (void*)(da->ptr+k) */
#define dyarr_insert(__da, __k, __n)  _ct_dyarr_insert((void**)&(__da)->ptr, sizeof*(__da)->ptr, &(__da)->cap, &(__da)->len, (__k), (__n))
/* removes [k : k+n], doesn't check bounds */
#define dyarr_remove(__da, __k, __n)  _ct_dyarr_remove((void**)&(__da)->ptr, sizeof*(__da)->ptr, &(__da)->len, (__k), (__n))
/* replace [k : k+n] with a copy of src (src->len can be different from n), NULL if OOM else (void*)(da->ptr+k) */
#define dyarr_replace(__da, __k, __n, __src)  _ct_dyarr_replace((void**)&(__da)->ptr, sizeof*(__da)->ptr, &(__da)->cap, &(__da)->len, (__k), (__n), (void*)(__src)->ptr, (__src)->len)

/* same as memcmp as for value sign, also compares lengths first in same way */
#define dyarr_cmp(__s1, __s2)  _ct_dyarr_cmp((void**)(__s1)->ptr, (__s1)->len*sizeof*(__s1)->ptr, (void**)(__s2)->ptr, (__s2)->len*sizeof*(__s2)->ptr)
/* returns dest->ptr, assumes dest is in a correct state (eg. zeroed out) */
#define dyarr_cpy(__dest, __src)  (((__dest)->len = (__src)->len) < (__dest)->cap || dyarr_resize((__dest), (__src)->len) ? memcpy((__dest)->ptr, (__src)->ptr, (__src)->len*sizeof*(__src)->ptr), (__dest)->ptr : (_dyarr_allocfail))

bool _ct_dyarr_resize(void** ptr, size_t isz, size_t* cap, size_t rsz)
{
    void* niw = realloc(*ptr, rsz * isz);
    if (!niw) return !!(_dyarr_allocfail);
    return *ptr = niw, *cap = rsz, true;
}

void* _ct_dyarr_insert(void** ptr, size_t isz, size_t* cap, size_t* len, size_t k, size_t n)
{
    size_t nln = *len+n;
    if (*cap < nln && !_ct_dyarr_resize(ptr, isz, cap, nln)) return (_dyarr_allocfail);
    memmove(*(char**)ptr+(k+n)*isz, *(char**)ptr+k*isz, (*len-k)*isz);
    *len = nln;
    return *(char**)ptr+k*isz;
}

void _ct_dyarr_remove(void** ptr, size_t isz, size_t* len, size_t k, size_t n)
{
    memmove(*(char**)ptr+k*isz, *(char**)ptr+(k+n)*isz, ((*len-= n)-k)*isz);
}

void* _ct_dyarr_replace(void** ptr, size_t isz, size_t* cap, size_t* len, size_t k, size_t n, void* spt, size_t sln)
{
    size_t nln = *len+sln-n;
    if (n < sln && *cap < nln && !_ct_dyarr_resize(ptr, isz, cap, nln)) return (_dyarr_allocfail);
    memmove(*(char**)ptr+(k+sln)*isz, *(char**)ptr+(k+n)*isz, (*len-n-k)*isz);
    memcpy(*(char**)ptr+k*isz, spt, sln*isz);
    *len = nln;
    return *(char**)ptr+k*isz;
}

int _ct_dyarr_cmp(void** s1, size_t z1, void** s2, size_t z2)
{
    int d = z1-z2;
    return d ? d : memcmp(s1, s2, z1);
}

#endif /* ct_dyarr */
