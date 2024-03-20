#include <stdio.h>

int sum3(int a, int b, int c);
void sayhi(char const* to);

int sum3(int a, int b, int c) {
    return a+b+c;
}

void sayhi(char const* to) {
    if (to) printf("hi %s\n", to);
    else puts("oh :<");
}
