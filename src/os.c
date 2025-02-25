// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#include "prefix.h"

#include "api.h"
#include "call.h"
#include "mem.h"
#include "os.h"
#include "util.h"
#include <errno.h>
#include <stdio.h>

#define INTR_TIMEOUT 100

struct File {
    FILE *file;
};

File *pawO_stdout(void)
{
    static File file;
    file.file = stdout;
    return &file;
}

paw_Bool pawO_is_open(File const *file)
{
    return file->file != NULL;
}

void pawO_error(paw_Env *P)
{
    paw_push_string(P, strerror(errno));
    pawC_throw(P, PAW_ESYSTEM);
}

File *pawO_new_file(paw_Env *P)
{
    Value *pv = pawC_push0(P);
    Foreign *f = pawV_new_foreign(P, sizeof(File), 0, VBOX_FILE, pv);

    File *file = f->data;
    *file = (File){0};
    return file;
}

int pawO_open(File *file, char const *pathname, char const *mode)
{
    for (int i = 0; i < INTR_TIMEOUT; ++i) {
        FILE *f = fopen(pathname, mode);
        if (f != NULL) {
            file->file = f;
            return 0;
        } else if (errno != EINTR) {
            break;
        }
    }
    return -errno;
}

void pawO_close(File *file)
{
    if (file->file == NULL)
        return;
    for (int i = 0; i < INTR_TIMEOUT; ++i) {
        int const rc = fclose(file->file);
        if (rc == 0 || errno != EINTR) {
            file->file = NULL;
            break;
        }
    }
}

int pawO_seek(File *file, paw_Int offset, int whence)
{
    return fseek(file->file, CAST(long, offset), whence);
}

paw_Int pawO_tell(File *file)
{
    return ftell(file->file);
}

int pawO_flush(File *file)
{
    return fflush(file->file);
}

static void check_for_errors(paw_Env *P, File *file)
{
    if (errno != EINTR) {
        paw_assert(ferror(file->file));
        pawO_error(P);
    }
}

size_t pawO_read(paw_Env *P, File *file, void *data, size_t size)
{
    size_t const original = size;
    for (size_t i = 0; i < INTR_TIMEOUT; ++i) {
        size_t const n = fread(data, 1, size, file->file);
        data = CAST(char *, data) + n;
        size -= n;

        if (size == 0) {
            break;
        } else if (feof(file->file)) {
            break;
        } else {
            check_for_errors(P, file);
        }
    }
    return original - size;
}

size_t pawO_write(paw_Env *P, File *file, void const *data, size_t size)
{
    size_t const original = size;
    for (size_t i = 0; i < INTR_TIMEOUT; ++i) {
        size_t const n = fwrite(data, 1, size, file->file);
        data = CAST(char *, data) + n;
        size -= n;

        if (size == 0) {
            break;
        } else {
            check_for_errors(P, file);
        }
    }
    return original - size;
}

void pawO_read_exact(paw_Env *P, File *file, void *data, size_t size)
{
    if (pawO_read(P, file, data, size) != size) {
        pawO_error(P);
    }
}

void pawO_write_all(paw_Env *P, File *file, void const *data, size_t size)
{
    if (pawO_write(P, file, data, size) != size) {
        pawO_error(P);
    }
}

File *pawO_detach_file(paw_Env *P, File *src)
{
    File *dst = pawM_new(P, File);
    *dst = *src;
    *src = (File){0};
    return dst;
}

void pawO_free_file(paw_Env *P, File *file)
{
    pawM_free(P, file);
}
