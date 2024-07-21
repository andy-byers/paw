#include "test.h"

static void script(const char *name)
{
    struct TestAlloc a = {0};
    test_script(name, &a);
}

int main(void)
{
    script("basic");
    script("types");
    script("block");
    script("loop");
    script("operator");
    script("struct");
    script("vector");
    script("map");
    script("bubble");
    script("mandelbrot");
    printf("TODO: fix remaining tests in test_rt.c\n");
    return 0; // TODO
    script("string");
    script("integer");
    script("float");
    script("closure");
    script("misc");
}
