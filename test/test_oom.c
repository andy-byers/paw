#include "test.h"

void *oom_alloc(void *ud, void *ptr, size_t size0, size_t size)
{
    struct TestAlloc *a = ud;
    if (a->extra + size0 < size) {
        return NULL;
    }
    a->extra += size0;
    a->extra -= size;
    return test_alloc(ud, ptr, size0, size);
}

static int script_aux(const char *name, struct TestAlloc *a)
{
    paw_Env *P = paw_open(oom_alloc, a);
    if (!P) {
        return PAW_EMEMORY;
    }
    int status = test_open_file(P, name);
    if (status == PAW_OK) {
        // run the module
        status = paw_call(P, 0);
    }
    test_close(P, a);
    return status;
}

static void script(const char *name)
{
    struct TestAlloc a = {0};
    size_t nextra = 10;
    int rc;
    do {
        // Run the script, allowing twice the number of bytes to be allocated
        // each time. Eventually, it should be able to allocate enough memory
        // to complete.
        rc = script_aux(name, &a);
        a.extra = nextra;
        nextra *= 2;
    } while (rc == PAW_EMEMORY);
    CHECK(rc == PAW_OK);
}

int main(void)
{
    script("basic");
    script("operator");
    script("integer");
    script("block");
    script("loop");
    script("closure");
    script("array");
    script("map");
    script("class");
}
