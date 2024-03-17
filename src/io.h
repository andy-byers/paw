// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#ifndef PAW_IO_H
#define PAW_IO_H

#include "prefix.h"

#include "error.h"
#include "paw.h"
#include <stdio.h>

FILE *pawO_open(const char *pathname, const char *mode);
void pawO_close(FILE *file);
size_t pawO_read(FILE *file, void *data, size_t size);
size_t pawO_write(FILE *file, const void *data, size_t size);
void pawO_read_exact(paw_Env *P, FILE *file, void *data, size_t size);
void pawO_write_all(paw_Env *P, FILE *file, const void *data, size_t size);

#endif // PAW_IO_H
