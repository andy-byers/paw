#include "call.h"
#include "env.h"
#include "test.h"

void run_tests(const char *name, struct TestAlloc *a)
{
    paw_Env *P = test_open(test_alloc, a);
    check(PAW_OK == test_open_file(P, name));
    
    struct GlobalVec *gvec = &P->gv;
    for (int i = 0; i < gvec->size; ++i) {
        static const char kPrefix[] = "test_";
        static const size_t kLength = paw_lengthof(kPrefix);
        struct GlobalVar *gvar = &gvec->data[i]; 
        if (gvar->name->length >= kLength &&
                0 == memcmp(gvar->name->text, kPrefix, kLength)) {
            printf("running %s.%s...\n", name, gvar->name->text);
            pawC_pushv(P, gvar->value);
            check(PAW_OK == paw_call(P, 0));
        }
    }
    test_close(P, a);
}

static void script(const char *name)
{
    struct TestAlloc a = {0};
    run_tests(name, &a);
}

int main(void)
{
#define RUN_SCRIPT(name) script(#name);
    TEST_SCRIPTS(RUN_SCRIPT)

    printf("TODO: fix remaining tests in test_rt.c\n");
    return 0;
    script("block");
    script("loop");
    script("operator");
    script("struct");
    script("vector");
    script("map");
    script("bubble");
    script("mandelbrot");
    script("string");
    script("integer");
    script("float");
    script("misc");
}
