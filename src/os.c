// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "call.h"
#include "env.h"
#include "mem.h"
#include "os.h"
#include "util.h"
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>

#if defined(PAW_OS_POSIX)
# include <sys/stat.h>
#else
# error "Windows is not yet supported"
#endif

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
    P->current_errmsg = pawS_new_str(P, strerror(errno));
    pawC_throw(P, PAW_ESYSTEM);
}

File *pawO_new_file(paw_Env *P)
{
    File *file = P->alloc(P, NULL, 0, sizeof *file);
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

enum DirNextStatus {
    DN_FOUND_ENTRY,
    DN_END_OF_DIR,
    DN_ERROR,
};

#if defined(PAW_OS_POSIX)
# include <dirent.h>

struct Directory {
    DIR *inner;
};

static int os_open_dir(paw_Env *P, char const *pathname, Directory *out)
{
    DIR *dir = opendir(pathname);
    if (dir == NULL) return -1;
    out->inner = dir;
    return 0;
}

static enum DirNextStatus os_dir_next(paw_Env *P, Directory dir, char const **name_out, size_t *length_out)
{
    PAW_UNUSED(P);
    struct dirent *entry = readdir(dir.inner);
    if (entry != NULL) {
        *name_out = entry->d_name;
        *length_out = entry->d_namlen;
        return DN_FOUND_ENTRY;
    } else if (errno == 0) {
        return DN_END_OF_DIR;
    } else {
        return DN_ERROR;
    }
}

static void os_close_dir(paw_Env *P, Directory dir)
{
    PAW_UNUSED(P);
    closedir(dir.inner);
}

#else
# error "Windows not yet supported"
#endif

void pawOs_traverse_dir(paw_Env *P, char const *dirname, AcceptEntry accept, void *ctx)
{
    enum DirNextStatus s = DN_ERROR;

    struct Directory dir;
    if (os_open_dir(P, dirname, &dir) == 0) {
        char const *p;
        size_t n;

        while ((s = os_dir_next(P, dir, &p, &n)) == DN_FOUND_ENTRY
                && accept(P, p, n, ctx)) {}

        os_close_dir(P, dir);
    }

    if (s == DN_ERROR)
        pawO_error(P);
}

paw_Bool pawOs_is_dir(paw_Env *P, char const *name)
{
    PAW_UNUSED(P);
#if defined(PAW_OS_POSIX)
    struct stat st;
    return lstat(name, &st) == 0 && S_ISDIR(st.st_mode);
#else
    return PAW_FALSE;
#endif
}

char const *pawOs_find_last_sep(char const *s, size_t n, size_t *pn)
{
    paw_assert(n > 0);
    char const *s0 = s;
    s += n;
    *pn = 0;

    do {
        --s;
        ++*pn;
        char const *q = PAW_FOLDER_SEPS;
        while (*q != '\0') {
            if (*q++ == *s)
                return s;
        }
    } while (s != s0);
    return NULL;
}
