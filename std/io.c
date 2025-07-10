// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "prefix.h"

#include "api.h"
#include "auxlib.h"
#include "call.h"
#include "env.h"
#include "lib.h"
#include "os.h"
#include <stdio.h>
#include <errno.h>

#define PUSH_RESULT(P_, Payload_) { \
        paw_push_int(P_, PAW_RESULT_OK); \
        *(P_)->top.p++ = (Payload_); \
    }

#define PUSH_ERROR(P_) { \
        paw_push_int(P_, PAW_RESULT_ERR); \
        paw_push_int(P_, error_kind()); \
    }

#define RESULT_SIZE(Payload_) (1 + PAW_MAX(Payload_, 1))

// ORDER ErrorKind
enum ErrorKind {
    ERROR_NOT_FOUND,
    ERROR_PERMISSION_DENIED,
    ERROR_FILE_TOO_LARGE,
    ERROR_ALREADY_EXISTS,
    ERROR_NOT_OPENED,
    ERROR_NOT_SUPPORTED,
    ERROR_INVALID_ARGUMENT,
    ERROR_OTHER,
};

static int error_kind(void)
{
    switch (errno) {
        case ENOENT:
            return ERROR_NOT_FOUND;
        case EPERM:
        case EACCES:
            return ERROR_PERMISSION_DENIED;
        case ENOSPC:
        case EFBIG:
            return ERROR_FILE_TOO_LARGE;
        case EEXIST:
            return ERROR_ALREADY_EXISTS;
        case EBADF:
            return ERROR_NOT_OPENED;
        case ENOSYS:
        case EOPNOTSUPP:
            return ERROR_NOT_SUPPORTED;
        case EINVAL:
            return ERROR_INVALID_ARGUMENT;
        default:
            return ERROR_OTHER;
    }
}

static int file_open(paw_Env *P)
{
    File *file = pawO_new_file(P);
    char const *path = paw_str(P, 1);
    char const *mode = paw_str(P, 2);
    if (pawO_open(file, path, mode) == 0) {
        // A foreign object containing the File is already on top of the stack. Transform it
        // into a io::Result<File>, i.e. an integer discriminant followed by the payload.
        paw_push_int(P, PAW_RESULT_OK);
        paw_rotate(P, -2, 1);
    } else {
        PUSH_ERROR(P);
    }
    return RESULT_SIZE(1);
}

static int file_flush(paw_Env *P)
{
    File *file = paw_pointer(P, 1);
    if (pawO_flush(file) == 0) {
        PUSH_RESULT(P, I2V(0));
    } else {
        PUSH_ERROR(P);
    }
    return RESULT_SIZE(1);
}

static int seek_kind(paw_Int kind)
{
    switch (kind) {
        case 0:
            return SEEK_SET;
        case 1:
            return SEEK_CUR;
        default:
            paw_assert(kind == 2);
            return SEEK_END;
    }
}

static int file_seek(paw_Env *P)
{
    File *file = paw_pointer(P, 1);
    paw_Int const offset = paw_int(P, 2);
    int const seek = seek_kind(paw_int(P, 3));
    if (pawO_seek(file, offset, seek) == 0) {
        PUSH_RESULT(P, I2V(0));
    } else {
        PUSH_ERROR(P);
    }
    return RESULT_SIZE(1);
}

static int file_tell(paw_Env *P)
{
    File *file = paw_pointer(P, 1);
    long const offset = pawO_tell(file);
    if (offset >= 0) {
        PUSH_RESULT(P, I2V(offset));
    } else {
        PUSH_ERROR(P);
    }
    return RESULT_SIZE(1);
}

static int file_read(paw_Env *P)
{
    Buffer buf;
    pawL_init_buffer(P, &buf);

    File *file = paw_pointer(P, 1);
    paw_Int const size = paw_int(P, 2);

    pawL_buffer_resize(P, &buf, size);
    paw_Int const count = pawO_read(P, file, buf.data, size);
    if (count >= 0) {
        pawL_buffer_resize(P, &buf, count);
        pawL_push_result(P, &buf);
        paw_push_int(P, PAW_RESULT_OK);
        paw_rotate(P, -2, 1);
    } else {
        PUSH_ERROR(P);
    }

    return RESULT_SIZE(1);
}

static int file_write(paw_Env *P)
{
    File *file = paw_pointer(P, 1);
    char const *data = paw_str(P, 2);
    paw_str_length(P, -1);
    paw_Int const size = paw_int(P, -1);

    paw_Int const count = pawO_write(P, file, data, size);
    if (count >= 0) {
        PUSH_RESULT(P, I2V(count));
    } else {
        PUSH_ERROR(P);
    }

    return RESULT_SIZE(1);
}

void l_import_io(paw_Env *P)
{
    pawE_push_cstr(P, CSTR_KSYMBOLS);
    paw_map_get(P, PAW_REGISTRY_INDEX);

    pawL_add_extern_method(P, "io", "File", "open", file_open);
    pawL_add_extern_method(P, "io", "File", "seek", file_seek);
    pawL_add_extern_method(P, "io", "File", "tell", file_tell);
    pawL_add_extern_method(P, "io", "File", "read", file_read);
    pawL_add_extern_method(P, "io", "File", "write", file_write);
    pawL_add_extern_method(P, "io", "File", "flush", file_flush);
    paw_pop(P, 1); // paw.symbols

    pawL_file_reader(P, PAWL_STDLIB_PATH(PAWL_IO_NAME));
}

