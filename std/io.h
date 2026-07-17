// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_STD_IO_H
#define PAW_STD_IO_H

#include "paw.h"

// struct File {...}
typedef struct paw_io_File {
    void *inner;
} paw_io_File;

// inline enum Error {...}
typedef struct {
    paw_Int64 discr;
} paw_io_Error;

typedef enum {
    paw_io_Error_NotFound,
    paw_io_Error_TooLarge,
    paw_io_Error_PermissionDenied,
    paw_io_Error_AlreadyExists,
    paw_io_Error_NotOpened,
    paw_io_Error_NotSupported,
    paw_io_Error_InvalidArgument,
    paw_io_Error_Other,
} paw_io_ErrorKind;

// inline enum Seek {...}
typedef struct {
    paw_Int64 discr;
} paw_io_Seek;

typedef enum {
    paw_io_Seek_Begin,
    paw_io_Seek_Current,
    paw_io_Seek_End,
} paw_io_SeekKind;

PAW_DEFINE_RESULT(io_File, io_Error)
PAW_DEFINE_RESULT(Unit, io_Error)
PAW_DEFINE_RESULT(Int64, io_Error)
PAW_DEFINE_RESULT(Usize, io_Error)

// pub type Result<T> = result::Result<T, Error>;
#define PAW_IO_RESULT(T) paw_Result_##T##_io_Error

PAW_IO_RESULT(io_File) paw_io_File_open(paw_Slice, paw_Slice);
PAW_IO_RESULT(Unit) paw_io_File_close(paw_io_File);
PAW_IO_RESULT(Unit) paw_io_File_seek(paw_io_File *, paw_Int64, paw_io_Seek);
PAW_IO_RESULT(Int64) paw_io_File_tell(paw_io_File *);
PAW_IO_RESULT(Unit) paw_io_File_flush(paw_io_File *);

PAW_IO_RESULT(Usize) paw_io_File_read(paw_io_File *, paw_Slice);
PAW_IO_RESULT(Usize) paw_io_File_write(paw_io_File *, paw_Slice);
void paw_io_File_drop(paw_io_File *);

paw_io_File *paw_io_stdin(void);
paw_io_File *paw_io_stdout(void);
paw_io_File *paw_io_stderr(void);

#endif // PAW_STD_IO_H

