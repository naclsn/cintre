/// Bytecode interpreter on top of the compiler; example:
/// ```c
/// struct slot slot = ...;
/// bytecode bc = ...;
///
/// run_state rs = {.sp= sizeof rs.stack};
///
/// run(&rs, bc);
/// // result are the `slot.ty->size` bytes at `rs.stack + rs.sp`
/// ```

#ifndef CINTRE_RUNNER_H
#define CINTRE_RUNNER_H

// emit a bunch of verbose runtime
#define _RUNR_VERBOSE_INSTRUCT 0

#include "common.h"
#include "adapter.h"
#include "compiler.h"

#ifndef STACK_SIZE
#define STACK_SIZE 1024*1024
#endif

typedef struct run_state {
    char stack[STACK_SIZE];
    size_t sp; // xxx: don't like that 0 is not sane default, it has
               //      to be explicitly initialized to `sizeof ::stack`
} run_state;

void run(run_state ref rs, bytecode const code);

// ---

void run(run_state ref rs, bytecode const code)
{
    size_t a, b, c;
#   define imm(nm) for (                        \
        unsigned xx = (nm = code.ptr[++k], 0);  \
        code.ptr[k]&0x80;                       \
        nm = nm | (code.ptr[++k]&0x7f)<<(xx+= 7))
#   define at(off, ty) ((ty*)(rs->stack+rs->sp+off))

#   if _RUNR_VERBOSE_INSTRUCT
#    define verbose(does, ...) (notif(#does " // " __VA_ARGS__), does)
#   else
#    define verbose(does, ...) (does)
#   endif

    for (size_t k = 0; k < code.len; k++) switch (code.ptr[k]) {
    case 0x2a:
        imm(b);
        notif("yyy: hit debug \"%.*s\"", (unsigned)b, code.ptr+k+1);
        k+= b;
        // fall through
    case 0x00:
        continue;

    case 0x0d: imm(a); verbose(rs->sp+= a, "%zu, %zu", rs->sp, a); continue; // pop
    case 0x0f: imm(a); verbose(rs->sp-= a, "%zu, %zu", rs->sp, a); continue; // push

    case 0x1d: // data
        imm(a);
        imm(b);
        verbose(memcpy(at(a, char), code.ptr+k+1, b), "%zu, %zu, %zu", rs->sp+a, k+1, b);
        k+= b;
        continue;
    case 0x1f: // move
        imm(a);
        imm(b);
        imm(c);
        verbose(memmove(at(a, char), at(c, char), b), "%zu, %zu, %zu", rs->sp+a, rs->sp+c, b);
        continue;

    case 0x2d: // write
        imm(a);
        imm(b);
        imm(c);
        verbose(memmove(*at(a, char*), at(c, char), b), "%zu(%p), %zu, %zu", rs->sp+a, *at(a, void*), rs->sp+c, b);
        continue;
    case 0x2f: // read
        imm(a);
        imm(b);
        imm(c);
        verbose(memmove(at(c, char), *at(a, char*), b), "%zu, %zu(%p), %zu", rs->sp+c, rs->sp+a, *at(a, void*), b);
        continue;

    case 0x20: // lea
        imm(a);
        imm(b);
        verbose(*at(a, char*) = at(b, char), "%zu(%p), %zu", rs->sp+a, *at(a, void*), rs->sp+b);
        continue;

#       define cvt(code, from, to)  \
    case code:                      \
        imm(a);                     \
        imm(b);                     \
        verbose(*at(a, to) = *at(b, from), "(%s -> %s) %zu, %zu", #from, #to, rs->sp+a, rs->sp+b);  \
        continue;
#       define extend_cvt(from_w, from, to_w, to)      \
        cvt(from_w<<4 | to_w, signed from, signed to)  \
        cvt(from_w<<4 | to_w | 0x4, unsigned from, unsigned to)
        extend_cvt(0, char, 1, short)
        extend_cvt(0, char, 2, int)
        extend_cvt(0, char, 3, long long)
        extend_cvt(1, short, 2, int)
        extend_cvt(1, short, 3, long long)
        extend_cvt(2, int, 3, long long)
#       undef extend_cvt
        cvt(0x11, float, int)
        cvt(0x22, double, long long)
        cvt(0x15, int, float)
        cvt(0x26, long long, double)
        cvt(0x21, double, float)
        cvt(0x25, float, double)
#       undef cvt

    case 0x04: notif("NIY: (run) not");  return;
    case 0x14: notif("NIY: (run) cmp1"); return;
    case 0x24: notif("NIY: (run) cmp2"); return;
    case 0x0a: notif("NIY: (run) jmp");  return;
    case 0x1a: notif("NIY: (run) jmb");  return;
    case 0x0b: notif("NIY: (run) breq"); return;
    case 0x1b: notif("NIY: (run) brlt"); return;
    case 0x2b: notif("NIY: (run) brle"); return;
        continue;

    default:;
        unsigned const hi = code.ptr[k]>>4 & 0xf, lo = code.ptr[k] & 0xf;

        if (0xc == lo) {
            imm(a);
            char* ret = at(a, char);
            imm(b);
            typedef void (*fun_t)(char* ret, char** args);
            fun_t fun = *at(b, fun_t);
            char* args[15];
            for (unsigned l = 0; l < hi; l++) {
                imm(c);
                verbose(args[l] = at(c, char), "[%u], %zu", l, rs->sp+c);
            }
            verbose(fun(ret, args), "(%zu)%p, (%zu), ...", rs->sp+b, *(void**)&fun, rs->sp+a);
            continue;
        }

        imm(a);
        imm(b);
        imm(c);
        verbose((void)(a == b * c), "%zu, %zu~%zu, %zu~%zu", rs->sp+a, rs->sp+b, b, rs->sp+c, c);
        switch (hi) {
#           define  _x( op, ty) *at(a, ty) = *at(b, ty) op *at(c, ty); continue;
#           define  _xi(op, ty) *at(a, ty) = (ty)b      op *at(c, ty); continue;
#           define _rxi(op, ty) *at(a, ty) = *at(c, ty) op (ty)b     ; continue;
#           define dox(n, ...) n(__VA_ARGS__)
#           define bop_u(x, op)                       \
                case 0x0: dox(x, op, unsigned char)   \
                case 0x1: dox(x, op, unsigned short)  \
                case 0x2: dox(x, op, unsigned int)    \
                case 0x3: dox(x, op, unsigned long long)
#           define bop_b(x, op)                       \
                case 0x4: dox(x, op, unsigned char)   \
                case 0x5: dox(x, op, unsigned short)  \
                case 0x6: dox(x, op, unsigned int)    \
                case 0x7: dox(x, op, unsigned long long)
#           define bop_f(x, op)                       \
                case 0xd: dox(x, op, double)          \
                case 0xf: dox(x, op, float)
        case 0x3: switch (lo) { bop_u( _x , +) bop_b( _x , |)  bop_f( _x , +) } break;
        case 0x4: switch (lo) { bop_u( _x , -) bop_b( _x , ^)  bop_f( _x , -) } break;
        case 0x5: switch (lo) { bop_u( _x , *) bop_b( _x , <<) bop_f( _x , *) } break;
        case 0x6: switch (lo) { bop_u( _x , /) bop_b( _x , >>) bop_f( _x , /) } break;
        case 0x7: switch (lo) { bop_u( _x , %) bop_b( _x , &)                 } break;
        case 0x8: switch (lo) { bop_u( _xi, +) bop_b( _xi, |)  bop_f( _xi, +) } break;
        case 0x9: switch (lo) { bop_u( _xi, -) bop_b( _xi, ^)  bop_f( _xi, -) } break;
        case 0xa: switch (lo) { bop_u( _xi, *) bop_b( _xi, <<) bop_f( _xi, *) } break;
        case 0xb: switch (lo) { bop_u( _xi, /) bop_b( _xi, >>) bop_f( _xi, /) } break;
        case 0xc: switch (lo) { bop_u( _xi, %) bop_b( _xi, &)                 } break;
        case 0xd: switch (lo) { bop_u(_rxi, -) bop_b(_rxi, <<) bop_f(_rxi, -) } break;
        case 0xe: switch (lo) { bop_u(_rxi, /) bop_b(_rxi, >>) bop_f(_rxi, /) } break;
        case 0xf: switch (lo) { bop_u(_rxi, %)                                } break;
#           undef bop_f
#           undef bop_b
#           undef bop_u
#           undef dox
#           undef _rxi
#           undef _xi
#           undef _x
        }

        notif("Unknown op code 0x%02x", code.ptr[k]);
        return;
    }

#   undef verbose

#   undef at
#   undef imm
}

#endif // CINTRE_RUNNER_H
