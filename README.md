### running a repl against a C lib

Example/PoC with `example/mylib`:
```console
$ cp example/mylib.[ch] .
$ cc cintre/preparer.c -o pr

$ cc -c mylib.c -o mylib.o
$ ./pr mylib.h -o a-mylib.h
$ ./pr cintre/standard.h -o a-standard.h
$ ./pr -m a-mylib.h a-standard.h -o c-main.c

$ cc mylib.o c-main.c -o c-main -Icintre -DUSE_READLINE -lreadline

$ ./c-main
Type `;help` for a list of command
(*^^),u~~ sayhi; ty
Expression is of type: fun(times: int) -> void
(*^^),u~~ sayhi(42);
Segmentation fault (core dump)

$ cd ..
$ rm -rf $OLDPWD
```

yeah that's a work in progress

### not handled / not planned / known limitations / notable differences

For now assumes platform is:
- 8 bits bytes (ie. `CHAR_BIT == 8`);
- little endian (ie. `(char[4]){0xd6, 0xff} == 0xff2d`);
- LP64 (ie. `sizeof(size_t) == sizeof(void*) == sizeof(long int) == 8`).

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
