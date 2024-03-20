### running a repl against a C lib

hello

### not handled / not planned / known limitations:
- lexer doesn't handle insanely placed line continuation (eg. `#def\<nl>ine some` where `<nl>` is a literal new line)
- unnamed function parameters (eg. `void main(int, char**)`)
- expressions in array size (only plain int literals for now..)
- pointer-to-pointer-to-function and more for now until less lazy about it
- [forward] declaration of a function with `ret name()` (ie. "any params" syntax) or will be interpreted as `(void)`
- va-args function

---

> the GitHub "Need inspiration?" bit was "animated-spoon"
