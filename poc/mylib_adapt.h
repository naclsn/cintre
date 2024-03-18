#include "adapt.h"

int myfunction(int a, int b, int c);
void myfunction_adapt(char* ret, char** args) {
    *(int*)ret = myfunction(*(int*)args[0], *(int*)args[1], *(int*)args[2]);
}

struct mystruct {
    char const* name;
    unsigned long long value;
};
static struct _struct const mystruct_adapt = {
    .info= infoof(struct mystruct),
    .fields = (struct _field_desc[]){
        {.name= "name", .offset= offsetof(struct mystruct, name), .info= infoof(char const*)},
        {.name= "value", .offset= offsetof(struct mystruct, value), .info= infoof(unsigned long long)},
        {0},
    },
};

void myprint(struct mystruct s);
void myprint_adapt(char* ret, char** args) {
    (void)ret;
    myprint(*(struct mystruct*)args[0]);
}

static struct _item const mylib_namespace[] = {
    {.name= "myfunction", .as.function= myfunction_adapt, .info= infoof(int)},
    {.name= "myprint", .as.function= myprint_adapt, .info= {0}}, // because void
    {0},
};
