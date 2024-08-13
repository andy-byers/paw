#include "test.h"
#include "call.h"
#include "env.h"

static void *oom_alloc(void *ud, void *ptr, size_t size0, size_t size)
{
    struct TestAlloc *a = ud;
    if (a->extra + size0 < size) {
        return NULL;
    }
    a->extra += size0;
    a->extra -= size;
    return test_alloc(ud, ptr, size0, size);
}

static int run_tests(paw_Env *P)
{
    struct GlobalVec *gvec = &P->gv;
    for (int i = 0; i < gvec->size; ++i) {
        static const char kPrefix[] = "test_";
        static const size_t kLength = paw_lengthof(kPrefix);
        struct GlobalVar *gvar = &gvec->data[i]; 
        if (gvar->name->length >= kLength &&
                0 == memcmp(gvar->name->text, kPrefix, kLength)) {
            pawC_pushv(P, gvar->value);
            const int status = paw_call(P, 0);
            if (status != PAW_OK) return status;
        }
    }
    return PAW_OK;
}

static int script_aux(const char *name, paw_Alloc alloc, struct TestAlloc *a, size_t heap_size)
{
    paw_Env *P = paw_open(&(struct paw_Options){
                .heap_size = heap_size,
                .alloc = alloc,
                .ud = a,
            });
    if (P == NULL) return PAW_EMEMORY;

    int status = test_open_file(P, name);
    if (status == PAW_OK) run_tests(P);
    if (a == NULL) {
        paw_close(P);
    } else {
        test_close(P, a);
    }
    return status;
}

static void script(const char *name)
{
    struct TestAlloc a = {0};
    size_t nextra = 10;
    int count = 0;
    int rc;
    do {
        // Run the script, allowing twice the number of bytes to be allocated
        // each time. Eventually, it should be able to allocate enough memory
        // to complete.
        rc = script_aux(name, oom_alloc, &a, 0);
        a.extra = nextra;
        nextra *= 2;
        ++count;
    } while (rc == PAW_EMEMORY);
    check(rc == PAW_OK);
    check(count > 0);
    printf("simulated OOM count: %d\n", count);
}

static void real_oom(const char *name)
{
    size_t heap_size = 1;
    int count = 0;
    int rc;
    do {
        rc = script_aux(name, NULL, NULL, heap_size);
        heap_size *= 2;
        ++count;
    } while (rc == PAW_EMEMORY);
    check(rc == PAW_OK);
    check(count > 0);
    printf("actual OOM count: %d\n", count);
}

int main(void)
{
#define RUN_SCRIPT(name) real_oom(#name); script(#name);
    TEST_SCRIPTS(RUN_SCRIPT)
}
