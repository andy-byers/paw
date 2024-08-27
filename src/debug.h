// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_DEBUG_H
#define PAW_DEBUG_H

#include "env.h"
#include "paw.h"

const char *paw_op_name(Op op);
void paw_dump_opcode(OpCode opcode);
void paw_dump_source(paw_Env *P, Proto *proto);
void paw_dump_stack(paw_Env *P);
void paw_stacktrace(paw_Env *P);
void paw_dump_value(paw_Env *P, Value v, int type);
void paw_dump_map(paw_Env *P, Map *m);

#endif // PAW_DEBUG_H
