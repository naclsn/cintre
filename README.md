## Running a REPL against a C lib

Situation: you have a C lib (header/s + obj/ar/shared)

Idea: automatically make a program that is a REPL linked against this lib, you
enter C expressions, can interact with everything exposed in the header
(declare variables with lib types, call lib functions, ...) as if you were
writing a `main()` and compiling it over and over and editing it and mixing the order of the arguments and recompiling it and not thinking about having a `printf` right here and it should be printing something else and that was just an off by one and-

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

The `driver.Makefile` does exactly that, see `example/Makefile`.

---

## Bugs

Assumes platform is:
- 8 bits bytes (ie. `CHAR_BIT == 8`);
- little endian (ie. `(char[4]){0xd6, 0xff, 0x00, 0x00} == (int)0xff2d`);
- LP64 (ie. `sizeof(size_t) == sizeof(void*) == sizeof(long int) == 8`).

## (wip and such)

### breaking
- compiling with `some.c` crashes; find why
- running the `sayhi(2)` example crashes; find why function calls fail
- pr: `.._call` param[n] in cast
- pr: bitfields here and there
- pr: array size/align
- `(int-1)` or `(int++)` or ... crashes

### progress
- parser: comp lits, (idk because it's at boundary between decl and expr parsing)
- pr: macros, like Err codes
- pr: emit compound literals (somewhat breaking technically)
- more conversions in `_fit_expr_to_slot`
- handle bitfields in runtime types

### opti/cleanup
- `chk_work` and `chk_interned` roamming around in cintre.c (// xxx: annoying) (aka. rework memory/structures in places (think the mess around qs/ql))
- maybe reverse the sp and vsp to have sane zero init (// xxx: sizeof stack)
- (limit avoidable capture-copying)
- go through tokn(...) to reduce expansion in favor of locals
- more coverage
- pr: "ptr tail" in `emit_forward` => is a hack on fun/arr declarator, so do it proper
- n -> logn ns search

---

> the GitHub "Need inspiration?" bit was "animated-spoon"
