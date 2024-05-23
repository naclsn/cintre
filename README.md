### Running a REPL against a C lib

Situation: you have a C lib (header/s + obj/ar/shared)

Idea: automatically make a program that is a REPL linked against this lib, you
enter C expressions, can interact with everything exposed in the header
(declare variables with lib types, call lib functions, ...) as if you were
writing a `main()` and compiling it over and over and editing it and mixing the order of the arguments and recompiling it and not thinking about having a `printf` right here and it should be printing something else and that was just an off by one and-

---

### `example/`

Example/PoC with `example/mylib`:
```console
$ cd example
$ make -f mylib.makefile && ./example
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

<details>
  <summary>Other jankiness and arbitrary changes:</summary>
  <ul>
    <li>current lexer doesn't handle insanely placed line continuation (eg. <code>#def\&lt;nl&gt;ine some</code> where <code>&lt;nl&gt;</code> is a literal new line)</li>
    <li>some exoteric declarations like <code>const a = 1</code> (C would assume it to be <code>int</code>)</li>
    <li>[forward] declaration of a function with <code>ret name()</code> (ie. "any params" syntax) or will be interpreted as <code>(void)</code></li>
    <li>va-args function</li>
    <li>base of a subscript expression must be the pointer, and the offset must be integral (ie. no <code>2["abc"]</code>)</li>
    <li>function call has at most 15 arguments</li>
    <li>octal constants are written with the <code>0o</code> prefix, and so <code>042 == 42 == '&ast;'</code></li>
    <li>character literals are of type <code>char</code> (instead of <code>int</code>)</li>
  </ul>
</details>

---

> the GitHub "Need inspiration?" bit was "animated-spoon"
