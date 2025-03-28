#include "paw.h"
#include <stdlib.h>
#include <string.h>

extern int LLVMFuzzerTestOneInput(uint8_t const *, size_t);

__AFL_FUZZ_INIT();

int main(void)
{
    __AFL_INIT();

    char *src;
    unsigned char *buf = __AFL_FUZZ_TESTCASE_BUF;
    while (__AFL_LOOP(10000)) {
        size_t const len = __AFL_FUZZ_TESTCASE_LEN;
        src = realloc(src, len);
        memcpy(src, buf, len);

        LLVMFuzzerTestOneInput(src, len);
    }
}
