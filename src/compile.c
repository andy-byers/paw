// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "compile.h"
#include "ast.h"
#include "call.h"
#include "gc_aux.h"
#include "hir.h"
#include "map.h"
#include "parse.h"
#include "value.h"

paw_Type pawP_type2code(struct Compiler *C, struct HirType *type)
{
    if (HirIsAdt(type)) {
        struct HirAdt *adt = HirGetAdt(type);
        if (adt->base == C->vector_did) {
            return PAW_TVECTOR; 
        } else if (adt->base == C->map_did) {
            return PAW_TMAP; 
        } else {
            return adt->base;
        }
    }
    return -1;
}

String *pawP_scan_nstring(paw_Env *P, Map *st, const char *s, size_t n)
{
    const Value *pv = pawC_pushns(P, s, n);
    Value *value = pawH_action(P, st, *pv, MAP_ACTION_CREATE);
    *value = *pv; // anchor in map
    pawC_pop(P);
    check_gc(P);

    return v_string(*value);
}

Closure *pawP_compile(paw_Env *P, paw_Reader input, struct DynamicMem *dm, const char *name, void *ud)
{
    // Initialize the lexical state.
    struct Compiler C = {
        .dm = dm,
        .P = P,
    };

    // Create the main closure and push it onto the stack so that the garbage
    // collector can find it.
    Value *pv = pawC_push0(P);
    C.main = pawV_new_closure(P, 1);
    v_set_object(pv, C.main);
    Proto *f = pawV_new_proto(P);
    C.main->p = f;

    // Do the same for the compiler string map. Strings are reachable from this 
    // map during compilation. Once finished, all strings should be anchored 
    // somewhere reachable from a GC root.
    pv = pawC_push0(P);
    C.strings = pawH_new(P);
    v_set_object(pv, C.strings);

    // Store the module name.
    String *modname = pawP_scan_string(P, C.strings, name);
    C.modname = modname;
    f->modname = modname;
    P->modname = modname;
    f->name = modname;

    // Compilation phases:
    struct Ast *p_parse(struct Compiler *, paw_Reader, void *); // from parse.c
    struct Hir *p_resolve(struct Compiler *, struct Ast *); // from resolve.c
    void p_codegen(struct Compiler *, struct Hir *); // from codegen.c

    // compile the module (source -> AST -> HIR -> bytecode)
    struct Ast *ast = p_parse(&C, input, ud);
    struct Hir *hir = p_resolve(&C, ast);
    p_codegen(&C, hir);

    // Pop the lexer map. The strings it contains should be anchored elsewhere.
    // Leave the main closure on top of the stack.
    pawC_stkdec(P, 1);
    return C.main;
}
