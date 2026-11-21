// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "io.h"
#include <errno.h>
#include <fcntl.h>
#include <unistd.h>

enum ErrorKind {
    EK_NOTFOUND,
    EK_TOOLARGE,
    EK_PERMISSIONDENIED,
    EK_ALREADYEXISTS,
    EK_NOTOPENED,
    EK_NOTSUPPORTED,
    EK_INVALIDARGUMENT,
    EK_OTHER,
};

int32_t paw_io_current_error(void)
{
    switch (errno) {
        case ENOENT:
            return EK_NOTFOUND;
        case EPERM:
        case EACCES:
            return EK_PERMISSIONDENIED;
        case ENOSPC:
        case EFBIG:
            return EK_TOOLARGE;
        case EEXIST:
            return EK_ALREADYEXISTS;
        case EBADF:
            return EK_NOTOPENED;
        case ENOSYS:
        case EOPNOTSUPP:
            return EK_NOTSUPPORTED;
        case EINVAL:
            return EK_INVALIDARGUMENT;
        default:
            return EK_OTHER;
    }
}

enum OpenFlag {
    OF_READ,
    OF_WRITE,
    OF_CREATE,
    OF_TRUNCATE,
    OF_APPEND,
};

_Bool paw_io_translate_open_flags(int32_t flags, int32_t *out)
{
#define F(Target_) !!((flags) & (1 << (Target_)))
    _Bool const read = F(OF_READ);
    _Bool const write = F(OF_WRITE);
    _Bool const create = F(OF_CREATE);
    _Bool const append = F(OF_APPEND);
    _Bool const truncate = F(OF_TRUNCATE);
#undef F

    *out = 0;
    if (read && write) {
        *out |= O_RDWR;
    } else if (read) {
        *out |= O_RDONLY;
    } else if (write) {
        *out |= O_WRONLY;
    }

    if (!write && (create || append || truncate))
        return 0;

    if (create) *out |= O_CREAT;
    if (append) *out |= O_APPEND;
    if (truncate) *out |= O_TRUNC;

    return 1;
}

struct File {
    int32_t inner;
};

#define DEFINE_STREAM_GETTER(Name_, Fd_) \
    struct File *paw_io_##Name_(void) \
    { \
        static struct File s_file; \
        s_file.inner = Fd_; \
        return &s_file; \
    }
DEFINE_STREAM_GETTER(stdin, 0)
DEFINE_STREAM_GETTER(stdout, 1)
DEFINE_STREAM_GETTER(stderr, 2)
#undef DEFINE_STREAM_GETTER

#define INTR_TIMEOUT 100

int32_t paw_syscall_open(char const *path, int32_t flags)
{
    for (int i = 0; i < INTR_TIMEOUT; ++i) {
        int32_t const fd = open(path, flags);
        if (fd >= 0)
            return fd;
        if (errno != EINTR)
            break;
    }
    return -1;
}

int32_t paw_syscall_close(int32_t fd)
{
    return close(fd);
}

ssize_t paw_syscall_seek(int32_t fd, int64_t offset, int32_t whence)
{
    return lseek(fd, (long)offset, whence);
}

int32_t paw_syscall_sync(int32_t fd)
{
    return fsync(fd);
}

ssize_t paw_syscall_read(int32_t fd, char *data, size_t size)
{
    size_t remaining = size;
    for (int i = 0; remaining > 0 && i < INTR_TIMEOUT; ++i) {
        ssize_t const n = read(fd, data, remaining);
        if (n < 0) return -1;
        if (n == 0) break;

        remaining -= (size_t)n;
        data += n;
    }
    return (ssize_t)(size - remaining);
}

ssize_t paw_syscall_write(int32_t fd, char const *data, size_t size)
{
    size_t remaining = size;
    for (int i = 0; remaining > 0 && i < INTR_TIMEOUT; ++i) {
        ssize_t const n = write(fd, data, remaining);
        if (n < 0) return -1;
        if (n == 0) break;

        remaining -= (size_t)n;
        data += n;
    }
    return (ssize_t)(size - remaining);
}
