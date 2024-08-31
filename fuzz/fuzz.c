// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "fuzz.h"
#include "lib.h"

extern int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size)
{
    struct FuzzState fs = fuzz_open(PAW_HEAP_DEFAULT);
    pawL_load_nchunk(fs.P, "fuzz", (const char *)data, size);
    fuzz_close(fs);
    return 0;
}
