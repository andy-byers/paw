#include "test.h"

static void script(const char *name)
{
    struct TestAlloc a = {0};
    test_script(name, &a);
}

int main(void)
{
    script("types");
    script("basic");
    return 0; // TODO
    script("operator");
    script("integer");
    script("float");
    script("block");
    script("loop");
    script("closure");
    script("array");
    script("map");
    script("string");
    script("class");
    script("error");
    script("misc");
}
