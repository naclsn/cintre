## Running a REPL against a C lib

Situation: you have a C lib (header/s + obj/ar/shared)

Idea: automatically make a program that is a REPL linked against this lib, you
enter C code, can interact with everything exposed in the header (declare
variables with lib types, call lib functions, ...)

---

## `example/`

Example/PoC with `example/mylib`:
```console
$ cd example
$ make -f mylib.makefile && ./mylib
Type `;help` for a list of command
(*^^),u~~ sayhi; ty
Expression is of type: fun(times: int) -> void
(*^^),u~~ sayhi(2);
hi
hi
Result:
_: void
   = ()
(*^^),u~~
```

If you really don't want to use make (why?):
```console
$ make -Bnf mylib.makefile
cc -O2   -c -o mylib.o mylib.c
cc ../cintre/preparer.c -o preparer.exe -O2
./preparer.exe ../cintre/standard.h -Pno-emit-decl -Pno-emit-incl -o a-standard.h
./preparer.exe mylib.h -o a-mylib.h -O2
./preparer.exe -m ./a-standard.h ./a-mylib.h -o c-mylib.c
cc -c c-mylib.c -o c-mylib.o -O2 -I. -I../cintre
cc -c ../cintre/cintre.c -o cintre.o -O2 -DUSE_READLINE -D_DEFAULT_SOURCE -D_XOPEN_SOURCE=600
cc mylib.o c-mylib.o cintre.o -o mylib -O2  -lc -lm -lreadline
```

Bluntly the steps are:
- build `mylib.o`
- build `pr` _(one time)_
- `pr standard.h` make an interface (or "adapter") to stdlib/stdio/string... _(one time)_
- `pr mylib.h` make an adapter with `mylib.o`
- `pr -m` merge and list namespaces in a way `cintre.o` can use
- build `c-mylib.c`
- build `cintre.o`, the `main()` of the REPL (here with readline) _(one time)_
- link `mylib.o` `c-mylib.o` and `cintre.o`

The `driver.Makefile` does exactly that, see `example/Makefile`.

---

## Bugs

Assumes platform:
- has 8 bits bytes (ie. `CHAR_BIT == 8`);
- is little endian (ie. `(char[4]){0xd6, 0xff, 0x00, 0x00} == (int)0xff2d`);
- respects `sizeof(size_t) == sizeof(void*)` (which is "not guarenteed", but like..).

It also collapses `long` and `long long` into the largest (ie. at runtime
`sizeof(long) == sizeof(long long)` and usually `== 2*sizeof(int)`).

## (wip and such)

### breaking
- fix precedence in `a = 1 ? 2 : 3`
- pr: bitfields here and there
- pr: array size/align

### progress
- pr: macros, like Err codes
- more conversions in `_fit_expr_to_slot`
- handle bitfields in runtime types

### opti/cleanup
- maybe reverse the sp and vsp to have sane zero init (// xxx: sizeof stack)
- limit avoidable capture-copying
- go through tokn(...) to reduce expansion in favor of locals
- something in using cstream to lex and rewind makes `lskdjlksj` go to the stmt decl route as if 2 idents in a row
- more coverage
- pr: "ptr tail" in `emit_forward` => is a hack on fun/arr declarator, so do it proper
- pr: is getting weird again (eg. `emit_forward` for ptr to x, `emit_adpt_type_val` for param[n], size/align situation for arrays, ..)
- n -> logn ns search
- (i386, core linux, qemu, tcc.tcz) `tcc: error: undefined symbol '_dso_handle'`

---

> the GitHub "Need inspiration?" bit was "animated-spoon"
