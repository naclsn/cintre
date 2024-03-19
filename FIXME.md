no token when parsing:
```c

//int const a;
/*typedef float b;
long long c, * const d, e;
void f();
void g(void);
/**/

int * a ( ), b;
```

unfinished comment segfaults
nvm maybe it wasn't the comment

sees the `void` and thinks it's `void show(void)`
```c
void show(void ref _, declaration cref decl, bufsl ref tok) {..}
```

does not follow the includes?
```console
build/cintre cintre.c
```
-> see FIXME in `lexer.h`

not handled / limitations:
- unnamed function parameters
- expressions in array size (only plain int literals for now..)
- pointer-to-pointer-to-function and more for now until less lazy about it
previsional limitations:
- [forward] declaration of a function with `ret name()` (ie. "any params" syntax) or will be interpreted as `(void)`
