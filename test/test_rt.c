#include "test.h"

static void script(const char *name)
{
    struct TestAlloc a = {0};
    test_script(name, &a);
}

int main(void)
{
    script("basic");
    return 0;
    script("types");
    script("block");
    script("loop");
    script("operator");
    script("vector");
    script("mandelbrot");
    return 0; // TODO
    script("string");
    script("integer");
    script("float");
    script("closure");
    script("map");
    script("class");
    script("error");
    script("misc");
}
