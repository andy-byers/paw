// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "compile.h"
#include "hir.h"

// Common state for type-checking routines
struct Resolver {
    paw_Env *P;
    Map *strings;
    Unifier *U; // unification tables
    struct Compiler *C; // compiler state
    struct Ast *ast; // AST being resolved
    struct Hir *hir; // HIR being built
    struct HirType *adt; // enclosing ADT
    struct HirType *result; // enclosing function return type
    struct HirSymtab *symtab; // scoped symbol table
    struct DynamicMem *dm; // dynamic memory
    int func_depth; // number of nested functions
    int nresults;
    int vector_gid;
    int map_gid;
    paw_Bool in_closure; // 1 if the enclosing function is a closure, else 0
};

// Instantiate a polymorphic function or type
// Expects that 'decl' is already resolved, meaning the type of each symbol has been
// filled in. Works by replacing each generic type with the corresponding concrete 
// type from the given 'types'. Returns a HirInstanceDecl if 'decl' is a function, 
// and a HirAdtDecl otherwise. We avoid recursively visiting the function body here, 
// since doing so might cause further instantiations due to the presence of recursion. 
// Function instance bodies are expanded during stenciling.
struct HirDecl *pawP_instantiate(
        struct Resolver *R, 
        struct HirDecl *decl, 
        struct HirTypeList *types);
