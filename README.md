### running a repl against a C lib

Situation: you have a simplistic C lib (header/s + obj/ar/shared)

Idea: automatically make a program that is a REPL linked against this lib, you
enter C expressions, can interact with everything exposed in the header
(declare variables with lib types, call lib functions, ...) as if you were
writing a `main()` and compiling it over and over and editing it and mixing the order of the arguments and recompiling it and not thinking about having a `printf` right here and it should be printing something else and that was just an off by one and-

Could you do that with just `gdb`? Yeah! (Well mostly I guess...)

---

### `example/`

Example/PoC with `example/mylib`:
```console
$ cd example
$ make && ./example
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
$ make -n
cc -O2   -c -o mylib.o mylib.c
cc ../cintre/preparer.c -o preparer.exe -O2
./preparer.exe ../cintre/standard.h -Pno-emit-decl -Pno-emit-incl -o a-standard.h
./preparer.exe mylib.h -o a-mylib.h -O2
./preparer.exe -m ./a-standard.h ./a-mylib.h -o c-example.c
cc ./mylib.o ./c-example.c -o example -O2 -I. -I../cintre -lc -lm -DUSE_READLINE -D_DEFAULT_SOURCE -D_XOPEN_SOURCE=600 -lreadline
```

Bluntly the steps are:
- build mylib.o
- build `pr`
- `pr standard.h` make an interface ("adapter") to stdlib/stdio/string...
- `pr mylib.h` make an interface ("adapter") with mylib.o
- `pr -m` merge into a compilable `main()`
- compile the REPL (here with readline)
The `driver.Makefile` does exactly that, see `example/Makefile`.

---

### not handled / not planned / known limitations / notable differences with std C

For now assumes platform is:
- 8 bits bytes (ie. `CHAR_BIT == 8`);
- little endian (ie. `(char[4]){0xd6, 0xff, 0x00, 0x00} == (int)0xff2d`);
- LP64 (ie. `sizeof(size_t) == sizeof(void*) == sizeof(long int) == 8`).
ie "my machine"

<details>
  <summary>Other jankiness and arbitrary changes:</summary>
  <ul>
    <li>lexer doesn't handle insanely placed line continuation (eg. `#def\<nl>ine some` where `<nl>` is a literal new line)</li>
    <li>declarators without a type (old C assumes these to be of type `int`)</li>
    <li>unnamed function parameters (eg. `void main(int, char**)`)</li>
    <li>expressions in array size (only plain int literals for now..)</li>
    <li>pointer-to-pointer-to-function and more for now until less lazy about it</li>
    <li>[forward] declaration of a function with `ret name()` (ie. "any params" syntax) or will be interpreted as `(void)`</li>
    <li>va-args function</li>
    <li>for now at least, there is no way to distinguish between `fn(1, 2, 3)` and `fn((1, 2), 3)` (later has only 2 args)</li>
    <li>base of a subscript expression must be the pointer, and the offset must be integral</li>
    <li>function call has at most 15 arguments</li>
    <li>octal constants are written with the `0o` prefix, and so `042 == 42 == '*'`</li>
  </ul>
</details>

---

> the GitHub "Need inspiration?" bit was "animated-spoon"
