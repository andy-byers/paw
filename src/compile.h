// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// compile.h: compiler entrypoint
//
// The compiler converts source code into bytecode that can be run in Paw's
// virtual machine. It works in 4 major passes:
//
//  Name    | Input -> Output | Purpose                    
// ---------|-----------------|--------------------------- 
//  parse   | source -> AST   | syntactical analysis 
//  resolve | AST -> HIR      | resolve symbols and types 
//  stencil | HIR -> HIR      | expand template instances 
//  codegen | HIR -> bytecode | generate code 

#ifndef PAW_COMPILE_H
#define PAW_COMPILE_H

#include "parse.h"

Closure *pawP_compile(paw_Env *P, paw_Reader input, struct DynamicMem *dm, 
                      const char *name, void *ud);
void pawP_cleanup(paw_Env *P, const struct DynamicMem *dm);

String *pawP_scan_nstring(paw_Env *P, Map *st, const char *s, size_t n);
static inline String *pawP_scan_string(paw_Env *P, Map *st, const char *s)
{
    return pawP_scan_nstring(P, st, s, strlen(s));
}

struct Compiler {
    struct DynamicMem *dm;
    String *modname;
    Closure *main;
    Map *strings;
    paw_Env *P;
    DefId vector_did;
    DefId map_did;
    DefId result_did;
    DefId option_did;
};

paw_Type pawP_type2code(struct Compiler *C, struct HirType *type);

#endif // PAW_COMPILE_H
