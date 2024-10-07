// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "prefix.h"

#include <stdio.h>
#include "api.h"
#include "auxlib.h"
#include "env.h"
#include "lib.h"
#include "os.h"

#define ERRIF(P, X) if (X) pawO_error(P)

static int io_open(paw_Env *P)
{
    File *file = pawO_new_file(P);
    const char *pathname = paw_string(P, 1);
    const char *mode = paw_string(P, 2);
    ERRIF(P, pawO_open(file, pathname, mode));
    return 1;
}

static int file_flush(paw_Env *P)
{
    File *file = paw_pointer(P, 1);
    ERRIF(P, pawO_flush(file));
    return 1;
}

static int seek_kind(paw_Env *P, int offset)
{
    const Variant *var = V_VARIANT(P->cf->base.p[offset]);
    switch (var->k) {
        case 0:
            return SEEK_SET;
        case 1:
            return SEEK_CUR;
        default:
            return SEEK_END;
    }
}

static int file_seek(paw_Env *P)
{
    File *file = paw_pointer(P, 1);
    const paw_Int offset = paw_int(P, 2);
    const int whence = seek_kind(P, 3);
    ERRIF(P, pawO_seek(file, offset, whence));
    return 0;
}

static int file_tell(paw_Env *P)
{
    File *file = paw_pointer(P, 1);
    paw_Int offset;
    ERRIF(P, (offset = pawO_tell(file)) < 0);
    paw_push_int(P, offset);
    return 1;
}

static int file_read(paw_Env *P)
{
    Buffer buf;
    pawL_init_buffer(P, &buf);

    File *file = paw_pointer(P, 1);
    const paw_Int size = paw_int(P, 2);

    pawL_buffer_resize(P, &buf, size);
    const paw_Int nread = pawO_read(P, file, buf.data, size);
    pawL_buffer_resize(P, &buf, nread);

    pawL_push_result(P, &buf);
    return 1;
}

static int file_write(paw_Env *P)
{
    File *file = paw_pointer(P, 1);
    const char *data = paw_string(P, 2);
    paw_str_length(P, -1);
    const paw_Int size = paw_int(P, -1);

    const paw_Int nwrite = pawO_write(P, file, data, size);
    paw_push_int(P, nwrite);
    return 1;
}

void l_import_io(paw_Env *P)
{
    static const char s_io[] =
        "struct File {\n"
        "    inner: (),\n"
        "}\n"

        "enum Seek {\n"
        "    Begin,\n"
        "    Current,\n"
        "    End,\n"
        "}\n"

        "impl File {\n"
        "    pub fn open(pathname: str, mode: str) -> Self;\n"
        "    pub fn seek(self, offset: int, whence: Seek);\n"
        "    pub fn tell(self) -> int;\n"
        "    pub fn read(self, size: int) -> str;\n"
        "    pub fn write(self, data: str) -> int;\n"
        "}\n";

    pawE_push_cstr(P, CSTR_KBUILTIN);
    paw_map_getelem(P, PAW_REGISTRY_INDEX);

    pawL_add_extern_method(P, "io", "File", "open", io_open);
    pawL_add_extern_method(P, "io", "File", "seek", file_seek);
    pawL_add_extern_method(P, "io", "File", "tell", file_tell);
    pawL_add_extern_method(P, "io", "File", "read", file_read);
    pawL_add_extern_method(P, "io", "File", "write", file_write);
    paw_pop(P, 1); // paw.builtin

    pawL_chunk_reader(P, s_io, PAW_LENGTHOF(s_io));
}

