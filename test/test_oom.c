#include "test.h"
#include "call.h"
#include "env.h"

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

void run_tests(paw_Env *P)
{
    struct GlobalVec *gvec = &P->gv;
    for (int i = 0; i < gvec->size; ++i) {
        static const char kPrefix[] = "test_";
        static const size_t kLength = paw_lengthof(kPrefix);
        struct GlobalVar *gvar = &gvec->data[i]; 
        if (gvar->name->length >= kLength &&
                0 == memcmp(gvar->name->text, kPrefix, kLength)) {
            printf("running oom.%s...\n", gvar->name->text);
            pawC_pushv(P, gvar->value);
            check(PAW_OK == paw_call(P, 0));
        }
    }
}

static int script_aux(const char *name, struct TestAlloc *a)
{
    paw_Env *P = paw_open(oom_alloc, a);
    if (P == NULL) return PAW_EMEMORY;

    int status = test_open_file(P, name);
    if (status == PAW_OK) run_tests(P);
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
    check(rc == PAW_OK);
}

int main(void)
{
#define RUN_SCRIPT(name) script(#name);
    TEST_SCRIPTS(RUN_SCRIPT)
}
