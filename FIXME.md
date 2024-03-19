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

not handled:
- unnamed function parameters
- expressions in array size (only plain int literals for now..)
- pointer-to-pointer-to-function and more for now until less lazy about it
