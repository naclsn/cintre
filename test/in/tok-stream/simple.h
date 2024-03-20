#include <stdio.h>
#include <stdlib.h>
#include "exitf.h"

#define SIMPLE_IMPLEMENTATION

void sayhi(void);

#ifdef SIMPLE_IMPLEMENTATION

void sayhi(void) {
    exitf("hi :3");
}

#endif // SIMPLE_IMPLEMENTATION
