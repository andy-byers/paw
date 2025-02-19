// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_DEBUG_H
#define PAW_DEBUG_H

#include "env.h"
#include "paw.h"

#define PAW_DEBUG_EXTRA
#if defined(PAW_DEBUG_LOG)
# define PAWD_LOG(P, ...) pawD_debug_log(P, __VA_ARGS__)
#else
# define PAWD_LOG(P, ...)
#endif

int pawD_line_number(const CallFrame *cf, const OpCode *pc);
void pawD_debug_log(paw_Env *P, const char *fmt, ...);

const char *paw_op_name(Op op);
void pawD_dump_defs(paw_Env *P);
void paw_dump_opcode(OpCode opcode);
void paw_dump_source(paw_Env *P, Proto *proto);
void paw_dump_stack(paw_Env *P);
void paw_stacktrace(paw_Env *P);
void paw_dump_value(Value v, paw_Type type);
void paw_dump_map_binary(Map *m);
void paw_dump_map(Map *m, paw_Type ktype, paw_Type vtype);

enum UnaryOp;
enum BinaryOp;
const char *paw_unop_name(enum UnaryOp unop);
const char *paw_binop_name(enum BinaryOp binop);

#endif // PAW_DEBUG_H
