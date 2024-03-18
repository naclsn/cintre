#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int myfunction(int a, int b, int c) { return a + b * c; }

struct mystruct {
    char const* name;
    unsigned long long value;
};

void myprint(struct mystruct s) { printf("%s: %llu\n", s.name, s.value); }
