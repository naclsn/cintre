#define _mkbin(__t, __type)                                                \
    char const* bin##__t(__type const val) {                               \
        static char bufs[2][2+8*sizeof val + 1] = {"0b", "0b"};            \
        static int last = 0;                                               \
                                                                           \
        char* const r = bufs[last = !last] + 2;                            \
        char const* const bytes = (void*)&val;                             \
                                                                           \
        for (long unsigned k = 0; k < sizeof val; ++k)                     \
            for (int b = 0; b < 8; ++b) /* (yyy: endianness) */            \
                r[8*k + b] = '1' - !(bytes[sizeof val-1 - k] & 1<<(7-b));  \
                                                                           \
        return r;                                                          \
    }

#define _mkhex(__t, __type)                                      \
    char const* hex##__t(__type const val) {                     \
        static char bufs[2][2+2*sizeof val + 1] = {"0x", "0x"};  \
        static int last = 0;                                     \
                                                                 \
        char* const r = bufs[last = !last] + 2;                  \
        char const* bytes = (void*)&val;                         \
                                                                 \
        for (long unsigned k = 0; k < sizeof val; ++k) {         \
                                  /* (yyy: endianness) */        \
            int const b = (int)bytes[sizeof val-1 - k];          \
            r[2*k] = "0123456789abcdef"[b>>4 & 15];              \
            r[2*k+1] = "0123456789abcdef"[b & 15];               \
        }                                                        \
                                                                 \
        return r;                                                \
    }

_mkbin(, int)
_mkbin(l, long)
_mkbin(ul, unsigned long)
_mkbin(f, float)
_mkbin(d, double)

_mkhex(, int)
_mkhex(l, long)
_mkhex(ul, unsigned long)
_mkhex(f, float)
_mkhex(d, double)

#undef _mkbin
#undef _mkhex
