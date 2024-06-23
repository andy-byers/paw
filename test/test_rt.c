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
    script("block");
    script("loop");
    script("operator");
    return 0; // TODO
    script("string");
    script("integer");
    script("float");
    script("closure");
    script("array");
    script("map");
    script("class");
    script("error");
    script("misc");
}
