// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#ifndef PAW_IO_H
#define PAW_IO_H

#include "core.h"

struct Str;
typedef struct File File;
typedef struct Directory Directory;

File *os_stdout(void);

File *os_new_file(paw_Env *P);
void os_free_file(paw_Env *P, File *file);
File *os_detach_file(paw_Env *P, File *src);

paw_Bool os_is_open(File const *file);
void os_error(paw_Env *P);
int os_open(File *file, char const *pathname, char const *mode);
void os_close(File *file);
int os_seek(File *file, paw_Int offset, int whence);
paw_Int os_tell(File *file);
int os_flush(File *file);
size_t os_read(paw_Env *P, File *file, void *data, size_t size);
size_t os_write(paw_Env *P, File *file, void const *data, size_t size);
void os_read_exact(paw_Env *P, File *file, void *data, size_t size);
void os_write_all(paw_Env *P, File *file, void const *data, size_t size);

typedef paw_Bool (*AcceptEntry)(paw_Env *P, char const *name, size_t length, void *ctx);
void os_traverse_dir(paw_Env *P, char const *dirname, AcceptEntry accept, void *ctx);

paw_Bool os_is_dir(paw_Env *P, char const *name);

char const *os_find_last_sep(char const *s, size_t n, size_t *pn);

#endif // PAW_IO_H
