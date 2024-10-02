// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#ifndef PAW_IO_H
#define PAW_IO_H

#include "paw.h"

typedef struct File File;

File *pawO_stdout(void);

File *pawO_new_file(paw_Env *P);
void pawO_free_file(paw_Env *P, File *file);
File *pawO_detach_file(paw_Env *P, File *src);

paw_Bool pawO_is_open(const File *file);
void pawO_error(paw_Env *P);
int pawO_open(File *file, const char *pathname, const char *mode);
void pawO_close(File *file);
int pawO_seek(File *file, paw_Int offset, int whence);
paw_Int pawO_tell(File *file);
int pawO_flush(File *file);
size_t pawO_read(paw_Env *P, File *file, void *data, size_t size);
size_t pawO_write(paw_Env *P, File *file, const void *data, size_t size);
void pawO_read_exact(paw_Env *P, File *file, void *data, size_t size);
void pawO_write_all(paw_Env *P, File *file, const void *data, size_t size);

#endif // PAW_IO_H
