/// Re-export a set of standard functions and types from includes picked from
/// ISO/IEC 9899:1999 ("C99") 7.1.2 "Standard headers"; this is meant to be use
/// as an entry for the preparer, to generate the "standard" namespace.

// TODO: fix these ptr[void] comming from `typedef`ing all opaque types as void

//#include <assert.h> // nothing interesting
//#include <complex.h>
#include <ctype.h>
//#include <errno.h> // not much interesting from a platform-independent perspective
#include <fenv.h>
//#include <float.h> // macro-only
//#include <inttypes.h> // macro-only
//#include <iso646.h> // macro-only (and, just, why anyways!?)
//#include <limits.h> // macro-only
//#include <locale.h>
#include <math.h>
#include <setjmp.h>
//#include <signal.h> // maybe at some point, but again platform-independant not
//#include <stdarg.h> // not exposed either
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
//#include <tgmath.h> // no
//#include <time.h>
//#include <wchar.h> // no
//#include <wctype.h> // no

//#include <unistd.h> // maybe at some point?, but maybe not in standard?

// complex.h (xxx: not yet if ever) {{{
// xxx(complex): extern double complex (cacos)(double complex z);
// xxx(complex): extern double complex (casin)(double complex z);
// xxx(complex): extern double complex (catan)(double complex z);
// xxx(complex): extern double complex (ccos)(double complex z);
// xxx(complex): extern double complex (csin)(double complex z);
// xxx(complex): extern double complex (ctan)(double complex z);
// xxx(complex): extern double complex (cacosh)(double complex z);
// xxx(complex): extern double complex (casinh)(double complex z);
// xxx(complex): extern double complex (catanh)(double complex z);
// xxx(complex): extern double complex (ccosh)(double complex z);
// xxx(complex): extern double complex (csinh)(double complex z);
// xxx(complex): extern double complex (ctanh)(double complex z);
// xxx(complex): extern double complex (cexp)(double complex z);
// xxx(complex): extern double complex (clog)(double complex z);
// xxx(complex): extern double complex (cpow)(double complex x, double complex y);
// xxx(complex): extern double complex (csqrt)(double complex z);
// xxx(complex): extern double (cabs)(double complex z);
// xxx(complex): extern double (carg)(double complex z);
// xxx(complex): extern double complex (conj)(double complex z);
// xxx(complex): extern double complex (cproj)(double complex z);
// xxx(complex): extern double (cimag)(double complex z);
// xxx(complex): extern double (creal)(double complex z);
// xxx(complex): extern float complex (cacosf)(float complex z);
// xxx(complex): extern float complex (casinf)(float complex z);
// xxx(complex): extern float complex (catanf)(float complex z);
// xxx(complex): extern float complex (ccosf)(float complex z);
// xxx(complex): extern float complex (csinf)(float complex z);
// xxx(complex): extern float complex (ctanf)(float complex z);
// xxx(complex): extern float complex (cacoshf)(float complex z);
// xxx(complex): extern float complex (casinhf)(float complex z);
// xxx(complex): extern float complex (catanhf)(float complex z);
// xxx(complex): extern float complex (ccoshf)(float complex z);
// xxx(complex): extern float complex (csinhf)(float complex z);
// xxx(complex): extern float complex (ctanhf)(float complex z);
// xxx(complex): extern float complex (cexpf)(float complex z);
// xxx(complex): extern float complex (clogf)(float complex z);
// xxx(complex): extern float complex (cpowf)(float complex x, float complex y);
// xxx(complex): extern float complex (csqrtf)(float complex z);
// xxx(complex): extern float (cabsf)(float complex z);
// xxx(complex): extern float (cargf)(float complex z);
// xxx(complex): extern float complex (conjf)(float complex z);
// xxx(complex): extern float complex (cprojf)(float complex z);
// xxx(complex): extern float (cimagf)(float complex z);
// xxx(complex): extern float (crealf)(float complex z);
// xxx(complex, long double): extern long double complex (cacosl)(long double complex z);
// xxx(complex, long double): extern long double complex (casinl)(long double complex z);
// xxx(complex, long double): extern long double complex (catanl)(long double complex z);
// xxx(complex, long double): extern long double complex (ccosl)(long double complex z);
// xxx(complex, long double): extern long double complex (csinl)(long double complex z);
// xxx(complex, long double): extern long double complex (ctanl)(long double complex z);
// xxx(complex, long double): extern long double complex (cacoshl)(long double complex z);
// xxx(complex, long double): extern long double complex (casinhl)(long double complex z);
// xxx(complex, long double): extern long double complex (catanhl)(long double complex z);
// xxx(complex, long double): extern long double complex (ccoshl)(long double complex z);
// xxx(complex, long double): extern long double complex (csinhl)(long double complex z);
// xxx(complex, long double): extern long double complex (ctanhl)(long double complex z);
// xxx(complex, long double): extern long double complex (cexpl)(long double complex z);
// xxx(complex, long double): extern long double complex (clogl)(long double complex z);
// xxx(complex, long double): extern long double complex (cpowl)(long double complex x, long double complex y);
// xxx(complex, long double): extern long double complex (csqrtl)(long double complex z);
// xxx(complex, long double): extern long double (cabsl)(long double complex z);
// xxx(complex, long double): extern long double (cargl)(long double complex z);
// xxx(complex, long double): extern long double complex (conjl)(long double complex z);
// xxx(complex, long double): extern long double complex (cprojl)(long double complex z);
// xxx(complex, long double): extern long double (cimagl)(long double complex z);
// xxx(complex, long double): extern long double (creall)(long double complex z);
// }}}

// ctype.h {{{
extern int (isalnum)(int c);
extern int (isalpha)(int c);
extern int (iscntrl)(int c);
extern int (isdigit)(int c);
extern int (islower)(int c);
extern int (isgraph)(int c);
extern int (isprint)(int c);
extern int (ispunct)(int c);
extern int (isspace)(int c);
extern int (isupper)(int c);
extern int (isxdigit)(int c);
extern int (tolower)(int c);
extern int (toupper)(int c);
extern int (isblank)(int c);
// }}}

// errno.h (xxx: not handled either) {{{
// todo: handle, there might not be any easy way because of the definition of
// `errno`, but it could be interesting to have it and the various E<..> on the
// platform
// }}}

// fenv.h {{{
typedef void fexcept_t;
typedef void fenv_t;

/* same deal as errno, idk how to do with these yet
static const fenv_t *_FE_DFL_ENV = FE_DFL_ENV;
enum {
    _FE_DIVBYZERO= FE_DIVBYZERO,
    _FE_INEXACT= FE_INEXACT,
    _FE_INVALID= FE_INVALID,
    _FE_OVERFLOW= FE_OVERFLOW,
    _FE_UNDERFLOW= FE_UNDERFLOW,
    _FE_ALL_EXCEPT= _FE_INEXACT|_FE_DIVBYZERO|_FE_UNDERFLOW|_FE_OVERFLOW|_FE_INVALID,
};
enum {
    _FE_DOWNWARD= FE_DOWNWARD,
    _FE_TONEAREST= FE_TONEAREST,
    _FE_TOWARDZERO= FE_TOWARDZERO,
    _FE_UPWARD= FE_UPWARD,
};
*/

extern int (feclearexcept)(int excepts);
extern int (fegetexceptflag)(fexcept_t *flagp, int excepts);
extern int (feraiseexcept)(int excepts);
extern int (fesetexceptflag)(const fexcept_t *flagp, int excepts);
extern int (fetestexcept)(int excepts);

extern int (fegetround)(void);
extern int (fesetround)(int rounding_mode);

extern int (fegetenv)(fenv_t *envp);
extern int (feholdexcept)(fenv_t *envp);
extern int (fesetenv)(const fenv_t *envp);
extern int (feupdateenv)(const fenv_t *envp);
// }}}

// locale.h (xxx: maybe at some point) {{{
// }}}

// math.h {{{
extern double (acos)(double x);
extern double (asin)(double x);
extern double (atan)(double x);
extern double (atan2)(double y, double x);
extern double (cos)(double x);
extern double (sin)(double x);
extern double (tan)(double x);
extern double (cosh)(double x);
extern double (sinh)(double x);
extern double (tanh)(double x);
extern double (acosh)(double x);
extern double (asinh)(double x);
extern double (atanh)(double x);
extern double (exp)(double x);
extern double (frexp)(double x, int *exponent);
extern double (ldexp)(double x, int exponent);
extern double (log)(double x);
extern double (log10)(double x);
extern double (modf)(double x, double *iptr);
extern double (expm1)(double x);
extern double (log1p)(double x);
extern double (logb)(double x);
extern double (exp2)(double x);
extern double (log2)(double x);
extern double (pow)(double x, double y);
extern double (sqrt)(double x);
extern double (hypot)(double x, double y);
extern double (cbrt)(double x);
extern double (ceil)(double x);
extern double (fabs)(double x);
extern double (floor)(double x);
extern double (fmod)(double x, double y);
extern int (isinf)(double value);
extern double (copysign)(double x, double y);
extern double (nan)(const char *tagb);
extern int (isnan)(double value);
extern double (erf)(double);
extern double (erfc)(double);
extern double (lgamma)(double);
extern double (tgamma)(double);
extern double (rint)(double x);
extern double (nextafter)(double x, double y);
// xxx(long double): extern double (nexttoward)(double x, long double y);
extern double (remainder)(double x, double y);
extern double (scalbn)(double x, int n);
extern int (ilogb)(double x);
extern double (scalbln)(double x, long int n);
extern double (nearbyint)(double x);
extern double (round)(double x);
extern double (trunc)(double x);
extern double (remquo)(double x, double y, int *quo);
extern long int (lrint)(double x);
extern long long int (llrint)(double x);
extern long int (lround)(double x);
extern long long int (llround)(double x);
extern double (fdim)(double x, double y);
extern double (fmax)(double x, double y);
extern double (fmin)(double x, double y);
extern double (fma)(double x, double y, double z);
extern float (acosf)(float x);
extern float (asinf)(float x);
extern float (atanf)(float x);
extern float (atan2f)(float y, float x);
extern float (cosf)(float x);
extern float (sinf)(float x);
extern float (tanf)(float x);
extern float (coshf)(float x);
extern float (sinhf)(float x);
extern float (tanhf)(float x);
extern float (acoshf)(float x);
extern float (asinhf)(float x);
extern float (atanhf)(float x);
extern float (expf)(float x);
extern float (frexpf)(float x, int *exponent);
extern float (ldexpf)(float x, int exponent);
extern float (logf)(float x);
extern float (log10f)(float x);
extern float (modff)(float x, float *iptr);
extern float (expm1f)(float x);
extern float (log1pf)(float x);
extern float (logbf)(float x);
extern float (exp2f)(float x);
extern float (log2f)(float x);
extern float (powf)(float x, float y);
extern float (sqrtf)(float x);
extern float (hypotf)(float x, float y);
extern float (cbrtf)(float x);
extern float (ceilf)(float x);
extern float (fabsf)(float x);
extern float (floorf)(float x);
extern float (fmodf)(float x, float y);
extern float (copysignf)(float x, float y);
extern float (nanf)(const char *tagb);
extern float (erff)(float);
extern float (erfcf)(float);
extern float (lgammaf)(float);
extern float (tgammaf)(float);
extern float (rintf)(float x);
extern float (nextafterf)(float x, float y);
// xxx(long double): extern float (nexttowardf)(float x, long double y);
extern float (remainderf)(float x, float y);
extern float (scalbnf)(float x, int n);
extern int (ilogbf)(float x);
extern float (scalblnf)(float x, long int n);
extern float (nearbyintf)(float x);
extern float (roundf)(float x);
extern float (truncf)(float x);
extern float (remquof)(float x, float y, int *quo);
extern long int (lrintf)(float x);
extern long long int (llrintf)(float x);
extern long int (lroundf)(float x);
extern long long int (llroundf)(float x);
extern float (fdimf)(float x, float y);
extern float (fmaxf)(float x, float y);
extern float (fminf)(float x, float y);
extern float (fmaf)(float x, float y, float z);
// xxx(long double): extern long double (acosl)(long double x);
// xxx(long double): extern long double (asinl)(long double x);
// xxx(long double): extern long double (atanl)(long double x);
// xxx(long double): extern long double (atan2l)(long double y, long double x);
// xxx(long double): extern long double (cosl)(long double x);
// xxx(long double): extern long double (sinl)(long double x);
// xxx(long double): extern long double (tanl)(long double x);
// xxx(long double): extern long double (coshl)(long double x);
// xxx(long double): extern long double (sinhl)(long double x);
// xxx(long double): extern long double (tanhl)(long double x);
// xxx(long double): extern long double (acoshl)(long double x);
// xxx(long double): extern long double (asinhl)(long double x);
// xxx(long double): extern long double (atanhl)(long double x);
// xxx(long double): extern long double (expl)(long double x);
// xxx(long double): extern long double (frexpl)(long double x, int *exponent);
// xxx(long double): extern long double (ldexpl)(long double x, int exponent);
// xxx(long double): extern long double (logl)(long double x);
// xxx(long double): extern long double (log10l)(long double x);
// xxx(long double): extern long double (modfl)(long double x, long double *iptr);
// xxx(long double): extern long double (expm1l)(long double x);
// xxx(long double): extern long double (log1pl)(long double x);
// xxx(long double): extern long double (logbl)(long double x);
// xxx(long double): extern long double (exp2l)(long double x);
// xxx(long double): extern long double (log2l)(long double x);
// xxx(long double): extern long double (powl)(long double x, long double y);
// xxx(long double): extern long double (sqrtl)(long double x);
// xxx(long double): extern long double (hypotl)(long double x, long double y);
// xxx(long double): extern long double (cbrtl)(long double x);
// xxx(long double): extern long double (ceill)(long double x);
// xxx(long double): extern long double (fabsl)(long double x);
// xxx(long double): extern long double (floorl)(long double x);
// xxx(long double): extern long double (fmodl)(long double x, long double y);
// xxx(long double): extern int (isinfl)(long double value);
// xxx(long double): extern int (finitel)(long double value);
// xxx(long double): extern long double (dreml)(long double x, long double y);
// xxx(long double): extern long double (significandl)(long double x);
// xxx(long double): extern long double (copysignl)(long double x, long double y);
// xxx(long double): extern long double (nanl)(const char *tagb);
// xxx(long double): extern int (isnanl)(long double value);
// xxx(long double): extern long double (j0l)(long double);
// xxx(long double): extern long double (j1l)(long double);
// xxx(long double): extern long double (jnl)(int, long double);
// xxx(long double): extern long double (y0l)(long double);
// xxx(long double): extern long double (y1l)(long double);
// xxx(long double): extern long double (ynl)(int, long double);
// xxx(long double): extern long double (erfl)(long double);
// xxx(long double): extern long double (erfcl)(long double);
// xxx(long double): extern long double (lgammal)(long double);
// xxx(long double): extern long double (tgammal)(long double);
// xxx(long double): extern long double (gammal)(long double);
// xxx(long double): extern long double (lgammal_r)(long double, int *signgamp);
// xxx(long double): extern long double (rintl)(long double x);
// xxx(long double): extern long double (nextafterl)(long double x, long double y);
// xxx(long double): extern long double (nexttowardl)(long double x, long double y);
// xxx(long double): extern long double (remainderl)(long double x, long double y);
// xxx(long double): extern long double (scalbnl)(long double x, int n);
// xxx(long double): extern int (ilogbl)(long double x);
// xxx(long double): extern long double (scalblnl)(long double x, long int n);
// xxx(long double): extern long double (nearbyintl)(long double x);
// xxx(long double): extern long double (roundl)(long double x);
// xxx(long double): extern long double (truncl)(long double x);
// xxx(long double): extern long double (remquol)(long double x, long double y, int *quo);
// xxx(long double): extern long int (lrintl)(long double x);
// xxx(long double): extern long long int (llrintl)(long double x);
// xxx(long double): extern long int (lroundl)(long double x);
// xxx(long double): extern long long int (llroundl)(long double x);
// xxx(long double): extern long double (fdiml)(long double x, long double y);
// xxx(long double): extern long double (fmaxl)(long double x, long double y);
// xxx(long double): extern long double (fminl)(long double x, long double y);
// xxx(long double): extern long double (fmal)(long double x, long double y, long double z);
// xxx(long double): extern long double (scalbl)(long double x, long double n);
// }}}

// setjmp.h {{{
static struct ct_adpt_type const jmp_buf_adapt_type = {
    .size= sizeof(jmp_buf), .align= alignof(jmp_buf),
    .tyty= CT_TYPE_ARR,
    .info.arr= {
        .item= &ct_adptb_void_type,
        .count= 1,
    },
};

// expectedly not exposed in a repl
//int (setjmp)(jmp_buf env);
//void (longjmp)(jmp_buf env, int val);
// }}}

// stdbool.h {{{
//static struct ct_adpt_type const bool_adapt_type = {
//    .size= sizeof(bool), .align= 1,
//    .tyty= CT_TYPE_CHAR,
//};
typedef char bool;
// }}}

// stddef.h {{{
typedef long int ptrdiff_t;
typedef long unsigned int size_t;
// }}}

// stdint.h {{{
typedef signed char        int8_t;
typedef signed short int   int16_t;
typedef signed int         int32_t;
typedef signed long int    int64_t;
typedef unsigned char      uint8_t;
typedef unsigned short int uint16_t;
typedef unsigned int       uint32_t;
typedef unsigned long int  uint64_t;
typedef long int           intptr_t;
typedef unsigned long int  uintptr_t;
typedef long int           intmax_t;
typedef unsigned long int  uintmax_t;
// }}}

// stdio.h {{{
typedef void FILE;
typedef void fpos_t;

// XXX: C89/C99 say they're macros.  im not happy.
extern FILE *stdin;
extern FILE *stdout;
extern FILE *stderr;

extern int (remove)(const char *filename);
extern int (rename)(const char *old, const char *new);
extern int (fclose)(FILE *stream);
extern FILE *(tmpfile)(void);
extern int (fflush)(FILE *stream);
extern FILE *(fopen)(const char *restrict filename, const char *restrict modes);
extern FILE *(freopen)(const char *restrict filename, const char *restrict modes, FILE *restrict stream);
extern void (setbuf)(FILE *restrict stream, char *restrict buf);
extern int (setvbuf)(FILE *restrict stream, char *restrict buf, int modes, size_t n);
extern int (fgetc)(FILE *stream);
extern int (getc)(FILE *stream);
extern int (getchar)(void);
extern int (fputc)(int c, FILE *stream);
extern int (putc)(int c, FILE *stream);
extern int (putchar)(int c);
extern char *(fgets)(char *restrict s, int n, FILE *restrict stream);
extern int (fputs)(const char *restrict s, FILE *restrict stream);
extern int (puts)(const char *s);
extern int (ungetc)(int c, FILE *stream);
extern size_t (fread)(void *restrict ptr, size_t size, size_t n, FILE *restrict stream);
extern size_t (fwrite)(const void *restrict ptr, size_t size, size_t n, FILE *restrict s);
extern int (fseek)(FILE *stream, long int off, int whence);
extern long int (ftell)(FILE *stream);
extern void (rewind)(FILE *stream);
extern int (fgetpos)(FILE *restrict stream, fpos_t *restrict pos);
extern int (fsetpos)(FILE *stream, const fpos_t *pos);
extern void (clearerr)(FILE *stream);
extern int (feof)(FILE *stream);
extern int (ferror)(FILE *stream);
extern void (perror)(const char *s);
// }}}

// stdlib.h {{{
typedef struct { int quot; int rem; } div_t;
typedef struct { long int quot; long int rem; } ldiv_t;
typedef struct { long long int quot; long long int rem; } lldiv_t;

extern double (atof)(const char *nptr);
extern int (atoi)(const char *nptr);
extern long int (atol)(const char *nptr);
extern long long int (atoll)(const char *nptr);
extern double (strtod)(const char *restrict nptr, char **restrict endptr);
extern float (strtof)(const char *restrict nptr, char **restrict endptr);
// xxx(long double): extern long double (strtold)(const char *restrict nptr, char **restrict endptr);
extern long int (strtol)(const char *restrict nptr, char **restrict endptr, int base);
extern unsigned long int (strtoul)(const char *restrict nptr, char **restrict endptr, int base);
extern long long int (strtoll)(const char *restrict nptr, char **restrict endptr, int base);
extern unsigned long long int (strtoull)(const char *restrict nptr, char **restrict endptr, int base);
extern int (rand)(void);
extern void (srand)(unsigned int seed);
extern void *(malloc)(size_t size);
extern void *(calloc)(size_t nmemb, size_t size);
extern void *(realloc)(void *ptr, size_t size);
extern void (free)(void *ptr);
extern void (abort)(void);
extern int (atexit)(void (*func)(void));
extern void (exit)(int status);
extern void (_Exit)(int status);
extern char *(getenv)(const char *name);
extern int (system)(const char *command);
extern void *(bsearch)(const void *key, const void *base, size_t nmemb, size_t size, int (*compar)(const void *, const void *));
extern void (qsort)(void *base, size_t nmemb, size_t size, int (*compar)(const void *, const void *));
extern int (abs)(int x);
extern long int (labs)(long int x);
extern long long int (llabs)(long long int x);
extern div_t (div)(int numer, int denom);
extern ldiv_t (ldiv)(long int numer, long int denom);
extern lldiv_t (lldiv)(long long int numer, long long int denom);
extern int (mblen)(const char *s, size_t n);
// }}}

// string.h {{{
extern void *(memcpy)(void *restrict dest, const void *restrict src, size_t n);
extern void *(memmove)(void *dest, const void *src, size_t n);
extern void *(memset)(void *s, int c, size_t n);
extern int (memcmp)(const void *s1, const void *s2, size_t n);
extern void *(memchr)(const void *s, int c, size_t n);
extern char *(strcpy)(char *restrict dest, const char *restrict src);
extern char *(strncpy)(char *restrict dest, const char *restrict src, size_t n);
extern char *(strcat)(char *restrict dest, const char *restrict src);
extern char *(strncat)(char *restrict dest, const char *restrict src, size_t n);
extern int (strcmp)(const char *s1, const char *s2);
extern int (strncmp)(const char *s1, const char *s2, size_t n);
extern int (strcoll)(const char *s1, const char *s2);
extern size_t (strxfrm)(char *restrict dest, const char *restrict src, size_t n);
extern char *(strchr)(const char *s, int c);
extern char *(strrchr)(const char *s, int c);
extern size_t (strcspn)(const char *s, const char *reject);
extern size_t (strspn)(const char *s, const char *accept);
extern char *(strpbrk)(const char *s, const char *accept);
extern char *(strstr)(const char *haystack, const char *needle);
extern char *(strtok)(char *restrict s, const char *restrict delim);
extern size_t (strlen)(const char *s);
extern char *(strerror)(int errnum);
// }}}

/*
// time.h (xxx: too lazy to figure this one out yet) {{{
typedef long int clock_t;
typedef long int time_t;
typedef int clockid_t;
typedef void* timer_t;
typedef long int suseconds_t;

struct tm {
    int tm_sec;
    int tm_min;
    int tm_hour;
    int tm_mday;
    int tm_mon;
    int tm_year;
    int tm_wday;
    int tm_yday;
    int tm_isdst;
    long int tm_gmtoff;
    const char *tm_zone;
};

struct timeval { long int tv_sec; long int tv_usec; };
struct timespec { long int tv_sec; long int tv_nsec; };
struct itimerspec { struct timespec it_interval; struct timespec it_value; };

//struct sigevent { int _; };
typedef int pid_t;

extern clock_t (clock)(void); // std?
extern time_t (time)(time_t *timer); // std?
extern double (difftime)(time_t time1, time_t time0); // std?
extern time_t (mktime)(struct tm *tp);
extern size_t (strftime)(char *restrict s, size_t maxsize, const char *restrict format, const struct tm *restrict tp);
extern struct tm *(gmtime)(const time_t *timer);
extern struct tm *(localtime)(const time_t *timer);
extern char *(asctime)(const struct tm *tp);
extern char *(ctime)(const time_t *timer);
//xxx(object): extern char *tzname[2];
//xxx(object): extern int daylight;
//xxx(object): extern long int timezone;
// }}}
*/
