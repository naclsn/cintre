### running a repl against a C lib

hello

### not handled / not planned / known limitations / notable differences

For now assumes platform is:
- 8 bits bytes (ie. `CHAR_BIT == 8`);
- little endian (ie. `(char[4]){0xd6, 0xff} == 0xff2d`);
- LP64 (ie. `sizeof(size_t) == sizeof(void*) == sizeof(long int) == 8`).

<details>
<summary>Other jankiness and arbitrary changes:</summary>
- lexer doesn't handle insanely placed line continuation (eg. `#def\<nl>ine some` where `<nl>` is a literal new line)
- declarators without a type (old C assumes these to be of type `int`)
- unnamed function parameters (eg. `void main(int, char**)`)
- expressions in array size (only plain int literals for now..)
- pointer-to-pointer-to-function and more for now until less lazy about it
- [forward] declaration of a function with `ret name()` (ie. "any params" syntax) or will be interpreted as `(void)`
- va-args function
- for now at least, there is no way to distinguish between `fn(1, 2, 3)` and `fn((1, 2), 3)` (later has only 2 args)
- base of a subscript expression must be the pointer, and the offset must be integral
- function call has at most 15 arguments
- octal constants are written with the `0o` prefix, and so `042 == 42 == '*'`
</details>

---

> the GitHub "Need inspiration?" bit was "animated-spoon"
