// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// Based off of StandaloneFuzzTargetMain.c in libFuzzer.

#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

extern int LLVMFuzzerTestOneInput(uint8_t const *data, size_t size);

static void run_input(char const *pathname)
{
    printf("Running: %s\n", pathname);

    FILE *file = fopen(pathname, "r");
    assert(file);

    fseek(file, 0, SEEK_END);
    long const rc = ftell(file);
    assert(0 <= rc);
    size_t const nbytes = rc;
    fseek(file, 0, SEEK_SET);

    char *buf = malloc(nbytes);
    size_t const read_size = fread(buf, 1, nbytes, file);
    assert(read_size == nbytes);

    fclose(file);

    LLVMFuzzerTestOneInput((uint8_t const *)buf, nbytes);
    printf("Done:    %s: (%zu bytes)\n", pathname, nbytes);
    free(buf);
};

int main(int argc, char const *argv[])
{
    printf("main: running %d inputs\n", argc - 1);
    for (int i = 1; i < argc; ++i) {
        run_input(argv[i]);
    }
    return 0;
}
