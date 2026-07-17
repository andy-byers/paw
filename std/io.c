// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "io.h"
#include <errno.h>
#include <stdio.h>

#define IO_ERROR(E_) (paw_io_Error){E_}

#define IO_RESULT_OK(T_, Value_) paw_Result_##T_##_io_Error_ok(Value_)
#define IO_RESULT_ERR(T_, Error_) paw_Result_##T_##_io_Error_err(IO_ERROR(Error_))


//
// OS interface
//

#define INTR_TIMEOUT 100

#define DEFINE_STREAM_GETTER(Stream_) \
    paw_io_File *paw_io_##Stream_(void) \
    { \
        static struct paw_io_File s_file; \
        s_file.inner = Stream_; \
        return &s_file; \
    }
DEFINE_STREAM_GETTER(stdin)
DEFINE_STREAM_GETTER(stdout)
DEFINE_STREAM_GETTER(stderr)
#undef DEFINE_STREAM_GETTER

paw_Bool file_is_open(paw_io_File *file)
{
    return file->inner != NULL;
}

static int os_open(paw_Char const *pathname, paw_Char const *mode, paw_io_File *file_out)
{
    for (int i = 0; i < INTR_TIMEOUT; ++i) {
        FILE *file = fopen(pathname, mode);
        if (file != NULL) {
            file_out->inner = file;
            return 0;
        } else if (errno != EINTR) {
            break;
        }
    }
    return -1;
}

static int os_close(paw_io_File *file)
{
    // Check for multiple calls to this function on a particular File instance. This should
    // never happen becuase (a) `Drop::drop()` is never called more than once on any value
    // and (b) `File::close()`, which takes ownership of the File handle, is a C function,
    // so there is no drop call generated when the File goes out of scope.
    paw_assert(file->inner != NULL);
    int const rc = fclose(file->inner);
    file->inner = NULL;
    return rc;
}

int os_seek(paw_io_File const *file, paw_Int64 offset, int whence)
{
    return fseek(file->inner, (long)offset, whence);
}

static paw_Int64 os_tell(paw_io_File const *file)
{
    return ftell(file->inner);
}

static int os_flush(paw_io_File const *file)
{
    return fflush(file->inner);
}

#define IO_FERROR(File_) (ferror((File_)->inner) && errno != EINTR)

static paw_Int64 os_read(paw_io_File const *file, void *data, paw_Usize size)
{
    paw_Usize remaining = (paw_Usize)size;
    for (int i = 0; i < INTR_TIMEOUT; ++i) {
        paw_Usize const n = fread(data, 1, remaining, file->inner);
        if (n == 0) break;

        data = (paw_Char *)data + n;
        remaining -= n;

        if (remaining == 0) {
            break;
        } else if (feof(file->inner)) {
            break;
        } else if (ferror(file->inner) && errno != EINTR) {
            return -1;
        }
    }
    return (paw_Int64)(size - remaining);
}

static paw_Int64 os_write(paw_io_File const *file, void const *data, paw_Usize size)
{
    paw_Usize remaining = (paw_Usize)size;
    for (int i = 0; i < INTR_TIMEOUT; ++i) {
        paw_Usize const n = fwrite(data, 1, remaining, file->inner);
        if (n == 0) break;

        data = (paw_Char const *)data + n;
        remaining -= n;

        if (remaining == 0) {
            break;
        } else if (feof(file->inner)) {
            break;
        } else if (ferror(file->inner) && errno != EINTR) {
            return -1;
        }
    }
    return (paw_Int64)(size - remaining);
}

static paw_io_ErrorKind check_errno(void)
{
    switch (errno) {
        case ENOENT:
            return paw_io_Error_NotFound;
        case EPERM:
        case EACCES:
            return paw_io_Error_PermissionDenied;
        case ENOSPC:
        case EFBIG:
            return paw_io_Error_TooLarge;
        case EEXIST:
            return paw_io_Error_AlreadyExists;
        case EBADF:
            return paw_io_Error_NotOpened;
        case ENOSYS:
        case EOPNOTSUPP:
            return paw_io_Error_NotSupported;
        case EINVAL:
            return paw_io_Error_InvalidArgument;
        default:
            return paw_io_Error_Other;
    }
}

static int seek_kind(paw_Int64 kind)
{
    switch ((paw_io_SeekKind)kind) {
        case paw_io_Seek_Begin:
            return SEEK_SET;
        case paw_io_Seek_Current:
            return SEEK_CUR;
        case paw_io_Seek_End:
            return SEEK_END;
    }
}


// pub fn drop(*self)
void paw_io_File_drop(paw_io_File *self)
{
    os_close(self);
}

// pub fn open(pathname: str, mode: str) -> Result<Self>
PAW_IO_RESULT(io_File) paw_io_File_open(paw_Slice pathname, paw_Slice mode)
{
    paw_io_File file = {0};
    if (os_open(pathname.start, mode.start, &file) == 0) {
        return IO_RESULT_OK(io_File, file);
    } else {
        return IO_RESULT_ERR(io_File, check_errno());
    }
}

// pub fn close(self) -> Result<()>
PAW_IO_RESULT(Unit) paw_io_File_close(paw_io_File self)
{
    if (os_close(&self) == 0) {
        return IO_RESULT_OK(Unit, PAW_UNIT());
    } else {
        return IO_RESULT_ERR(Unit, check_errno());
    }
}

// pub fn seek(self, offset: int, whence: Seek) -> Result<()>
PAW_IO_RESULT(Unit) paw_io_File_seek(paw_io_File *self, paw_Int64 offset, paw_io_Seek whence)
{
    if (os_seek(self, offset, seek_kind(whence.discr)) == 0) {
        return IO_RESULT_OK(Unit, PAW_UNIT());
    } else {
        return IO_RESULT_ERR(Unit, check_errno());
    }
}

// pub fn tell(self) -> Result<int>
PAW_IO_RESULT(Int64) paw_io_File_tell(paw_io_File *self)
{
    paw_Int64 const offset = os_tell(self);
    if (offset >= 0) {
        return IO_RESULT_OK(Int64, offset);
    } else {
        return IO_RESULT_ERR(Int64, check_errno());
    }
}

// pub fn read(self, data: SliceMut<char>) -> Result<int>
PAW_IO_RESULT(Usize) paw_io_File_read(paw_io_File *self, paw_Slice data)
{
    paw_Int64 const length = os_read(self, data.start, data.length);
    return length >= 0
        ? IO_RESULT_OK(Usize, (paw_Usize)length)
        : IO_RESULT_ERR(Usize, check_errno());
}

// pub fn write(self, data: Slice<char>) -> Result<int>
PAW_IO_RESULT(Usize) paw_io_File_write(paw_io_File *self, paw_Slice data)
{
    paw_Int64 const length = os_write(self, data.start, data.length);
    return length >= 0
        ? IO_RESULT_OK(Usize, (paw_Usize)length)
        : IO_RESULT_ERR(Usize, check_errno());
}

// pub fn flush(self) -> Result<()>
PAW_IO_RESULT(Unit) paw_io_File_flush(paw_io_File *self)
{
    if (os_flush(self) == 0) {
        return IO_RESULT_OK(Unit, PAW_UNIT());
    } else {
        return IO_RESULT_ERR(Unit, check_errno());
    }
}
