### running a repl against a C lib

hello

### not handled / not planned / known limitations / notable differences

For now assumes platform is little endian LP64 (ie. `sizeof(size_t) == sizeof(void*) == sizeof(long) == 8`).

- lexer doesn't handle insanely placed line continuation (eg. `#def\<nl>ine some` where `<nl>` is a literal new line)
- unnamed function parameters (eg. `void main(int, char**)`)
- expressions in array size (only plain int literals for now..)
- pointer-to-pointer-to-function and more for now until less lazy about it
- [forward] declaration of a function with `ret name()` (ie. "any params" syntax) or will be interpreted as `(void)`
- va-args function
- for now at least, there is no way to distinguish between `fn(1, 2, 3)` and `fn((1, 2), 3)` (later has only 2 args)
- base of a subscript expression must be the pointer, and the offset must be integral
- function call has at most 15 arguments
- octal constants are written with the `0o` prefix, and so `042 == 42 == '*'`

---

> the GitHub "Need inspiration?" bit was "animated-spoon"
