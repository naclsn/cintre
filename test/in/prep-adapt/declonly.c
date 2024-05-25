#undef _declonly
#ifdef _DECLONLY
#define _declonly(...) ;
#else
#define _declonly(...) __VA_ARGS__
#endif

unsigned some_function(void* self, unsigned hey) _declonly({
    return self?* self*hey :3;
})
