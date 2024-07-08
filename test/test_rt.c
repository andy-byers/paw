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
    script("vector");
    script("map");
    return 0; // TODO
    script("mandelbrot");
    script("string");
    script("integer");
    script("float");
    script("closure");
    script("class");
    script("error");
    script("misc");
}
