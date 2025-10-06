// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_STD_IO_H
#define PAW_STD_IO_H

#include "paw.h"

// struct File {...}
typedef struct paw_io_File *paw_io_File;

// inline enum Error {...}
typedef struct {
    paw_Int discr;
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
    paw_Int discr;
} paw_io_Seek;

typedef enum {
    paw_io_Seek_Begin,
    paw_io_Seek_Current,
    paw_io_Seek_End,
} paw_io_SeekKind;

PAW_DEFINE_RESULT(io_File, io_Error)
PAW_DEFINE_RESULT(Unit, io_Error)
PAW_DEFINE_RESULT(Int, io_Error)
PAW_DEFINE_RESULT(Str, io_Error)

// pub type Result<T> = result::Result<T, Error>;
#define PAW_IO_RESULT(T) paw_Result_##T##_io_Error

PAW_IO_RESULT(io_File) paw_io_File_open(void *, paw_Str, paw_Str);
PAW_IO_RESULT(Unit) paw_io_File_seek(void *, paw_io_File, paw_Int, paw_io_Seek);
PAW_IO_RESULT(Int) paw_io_File_tell(void *, paw_io_File);
PAW_IO_RESULT(Str) paw_io_File_read(void *, paw_io_File, paw_Int);
PAW_IO_RESULT(Int) paw_io_File_write(void *, paw_io_File, paw_Str);
PAW_IO_RESULT(Unit) paw_io_File_flush(void *, paw_io_File);

#endif // PAW_STD_IO_H

