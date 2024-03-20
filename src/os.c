// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#include "prefix.h"

#include "call.h"
#include "os.h"
#include "util.h"
#include <errno.h>

#define INTR_TIMEOUT 100

void pawO_system_error(paw_Env *P, int error)
{
    paw_push_string(P, strerror(error));
    pawC_throw(P, PAW_ESYSTEM);
}

FILE *pawO_open(const char *pathname, const char *mode)
{
    for (int i = 0; i < INTR_TIMEOUT; ++i) {
        FILE *file = fopen(pathname, mode);
        if (file) {
            return file;
        } else if (errno != EINTR) {
            break;
        }
    }
    return NULL;
}

void pawO_close(FILE *file)
{
    for (int i = 0; i < INTR_TIMEOUT; ++i) {
        const int rc = fclose(file);
        if (rc == 0 || errno != EINTR) {
            break; // Success or non-interrupt failure
        }
    }
}

size_t pawO_read(FILE *file, void *data, size_t size)
{
    const size_t original = size;
    for (size_t i = 0; size > 0 && i < INTR_TIMEOUT; ++i) {
        const size_t n = fread(data, 1, size, file);
        if (n > 0) {
            data = (char *)data + n;
            size -= n;
        } else if (feof(file) || errno != EINTR) {
            break;
        }
    }
    return original - size;
}

size_t pawO_write(FILE *file, const void *data, size_t size)
{
    const size_t original = size;
    for (size_t i = 0; size > 0 && i < INTR_TIMEOUT; ++i) {
        const size_t n = fwrite(data, 1, size, file);
        if (n > 0) {
            data = (char *)data + n;
            size -= n;
        } else if (feof(file) || errno != EINTR) {
            break;
        }
    }
    return original - size;
}

void pawO_read_exact(paw_Env *P, FILE *file, void *data, size_t size)
{
    if (pawO_read(file, data, size) != size) {
        pawO_system_error(P, errno);
    }
}

void pawO_write_all(paw_Env *P, FILE *file, const void *data, size_t size)
{
    if (pawO_write(file, data, size) != size) {
        pawO_system_error(P, errno);
    }
}
