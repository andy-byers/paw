// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "ir_type.h"
#include "map.h"
#include "unify.h"

#define NEW_NODE(C, T) (T *)pawP_alloc(C, NULL, 0, sizeof(T))

DeclId pawIr_next_did(struct Compiler *C, int mod)
{
    return (DeclId){
        .value = C->def_count++,
        .modno = mod,
    };
}

IrDef *pawIr_new_def(struct Compiler *C)
{
    return NEW_NODE(C, IrDef);
}

IrType *pawIr_new_type(struct Compiler *C)
{
    return NEW_NODE(C, IrType);
}

IrType *pawIr_get_type(struct Compiler *C, HirId hid)
{
    IrType *const * ptype = HirTypes_get(C, C->ir_types, hid);
    return ptype != NULL ? *ptype : NULL;
}

IrDef *pawIr_get_def(struct Compiler *C, DefId did)
{
    IrDef *const *pdef = DefMap_get(C, C->ir_defs, did);
    return pdef != NULL ? *pdef : NULL;
}

void pawIr_set_type(struct Compiler *C, HirId hid, IrType *type)
{
    paw_assert(type != NULL);
    // TODO: this check could be removed with more confidence if 1 is used as the default ID (HirId, DefId, etc.) instead of 0
    //       since 0 is a valid ID, we get confused when an ID is not properly initialized (zero init produces a valid ID, which is bad)
    paw_assert(hid.value > 0 || !IrIsAdt(type) || IrGetAdt(type)->did.value == 0);
    HirTypes_insert(C, C->ir_types, hid, type);
}

void pawIr_set_def(struct Compiler *C, DefId did, IrDef *def)
{
    paw_assert(def != NULL);
    DefMap_insert(C, C->ir_defs, did, def);
}

struct IrType *pawIr_resolve_trait_method(struct Compiler *C, struct IrGeneric *target, String *name)
{
    if (target->bounds == NULL) {
        TYPE_ERROR(C, "generic type missing trait bounds");
    }
    struct IrType **pbound;
    K_LIST_FOREACH(target->bounds, pbound) {
        struct IrTraitObj *bound = IrGetTraitObj(*pbound);
        struct HirDecl *trait_decl = pawHir_get_decl(C, bound->did);
        struct HirTraitDecl *trait = HirGetTraitDecl(trait_decl);

        struct HirDecl **pmethod;
        struct HirDecl *result = NULL;
        struct HirDecl *last_method = NULL;
        struct HirTraitDecl *last_trait = NULL;
        K_LIST_FOREACH(trait->methods, pmethod) {
            struct HirFuncDecl *method = HirGetFuncDecl(*pmethod);
            if (pawS_eq(method->name, name)) result = *pmethod;
            else continue;

            if (last_method != NULL) {
                NAME_ERROR(C, "found multiple applicable methods");
            }
            last_method = result;
            last_trait = trait;
        }
        if (last_method != NULL) {
            struct IrType *result = last_trait->generics == NULL ? GET_NODE_TYPE(C, last_method)
                : pawP_instantiate_method(C, trait_decl, bound->types, last_method);
            return pawIr_substitute_self(C, *pbound, IR_CAST_TYPE(target), result);
        }
    }
    return NULL;
}

void pawIr_validate_type(struct Compiler *C, struct IrType *type)
{
    if (IrIsGeneric(type)) {
        struct IrGeneric *t = IrGetGeneric(type);
        if (t->bounds != NULL) {
            struct IrType **pt;
            K_LIST_FOREACH(t->bounds, pt) {
                pawIr_validate_type(C, *pt);
            }
        }
    }

    {
        struct HirDeclList *generics;
        struct IrTypeList *types = NULL;
        if (IrIsTraitObj(type)) {
            struct HirDecl *decl = pawHir_get_decl(C, IR_TYPE_DID(type));
            generics = HirGetTraitDecl(decl)->generics;
            types = IrGetTraitObj(type)->types;
        } else if (IrIsSignature(type)) {
            struct HirDecl *decl = pawHir_get_decl(C, IR_TYPE_DID(type));
            generics = HirGetFuncDecl(decl)->generics;
            types = IrGetSignature(type)->types;
        } else if (IrIsAdt(type)) {
            struct HirDecl *decl = pawHir_get_decl(C, IR_TYPE_DID(type));
            generics = HirGetAdtDecl(decl)->generics;
            types = IrGetAdt(type)->types;
        }
        if (types != NULL && types->count != generics->count) {
                TYPE_ERROR(C, "%s type arguments (expected %d but found %d)",
                        types->count < generics->count ? "not enough" : "too many",
                        types->count, generics->count);
        }
    }
}

// TODO: get this stuff to work. requires custom hash for IrType so that pointers in C.method_contexts
//       can be changed without breaking everything (semantic equality on types instead of pointer comparison)
//       then, type folder code can make copies of types

// From https://stackoverflow.com/questions/8513911
static paw_Uint hash_combine(paw_Uint seed, paw_Uint v)
{
    // TODO: versions for other sizes of paw_Uint
    const paw_Uint mul = 0x9DDFEA08EB382D69ULL;
    paw_Uint a = (v ^ seed) * mul;
    a ^= (a >> 47);
    paw_Uint b = (seed ^ a) * mul;
    b ^= (b >> 47);
    return b * mul;
}

static paw_Uint hash_type(struct IrType *type);

static paw_Uint hash_type_list(struct IrTypeList *types)
{
    paw_Uint hash = 0;
    struct IrType **ptype;
    if (types != NULL) {
        K_LIST_FOREACH(types, ptype) {
            hash = hash_combine(hash, hash_type(*ptype));
        }
    }
    return hash;
}

static paw_Uint hash_type(struct IrType *type)
{
    return CAST(paw_Uint, type);

    paw_Uint hash = type->hdr.kind;
    switch (IR_KINDOF(type)) {
        case kIrAdt: {
            struct IrAdt *t = IrGetAdt(type);
            hash = hash_combine(hash, t->did.value);
            hash = hash_combine(hash, hash_type_list(t->types));
            break;
        }
        case kIrFuncPtr: {
            struct IrFuncPtr *t = IrGetFuncPtr(type);
            hash = hash_combine(hash, hash_type_list(t->params));
            hash = hash_combine(hash, hash_type(t->result));
            break;
        }
        case kIrSignature: {
            struct IrSignature *t = IrGetSignature(type);
            hash = hash_combine(hash, t->did.value);
            hash = hash_combine(hash, hash_type_list(t->types));
            hash = hash_combine(hash, hash_type_list(t->params));
            hash = hash_combine(hash, hash_type(t->result));
            break;
        }
        case kIrTuple: {
            struct IrTuple *t = IrGetTuple(type);
            hash = hash_combine(hash, hash_type_list(t->elems));
            break;
        }
        case kIrInfer: {
            struct IrInfer *t = IrGetInfer(type);
            hash = hash_combine(hash, t->depth);
            hash = hash_combine(hash, t->index);
            hash = hash_combine(hash, hash_type_list(t->bounds));
            break;
        }
        case kIrGeneric: {
            struct IrGeneric *t = IrGetGeneric(type);
            hash = hash_combine(hash, t->did.value);
            hash = hash_combine(hash, hash_type_list(t->bounds));
            break;
        }
        case kIrTraitObj: {
            struct IrTraitObj *t = IrGetTraitObj(type);
            hash = hash_combine(hash, t->did.value);
            hash = hash_combine(hash, hash_type_list(t->types));
            break;
        }
    }
    return hash;
}

paw_Bool pawIr_type_equals(struct Compiler *C, IrType *a, IrType *b)
{
    if (IR_KINDOF(a) != IR_KINDOF(b)) return PAW_FALSE;

    if (IrIsSignature(a)) {
        struct IrSignature *sa = IrGetSignature(a);
        struct IrSignature *sb = IrGetSignature(b);
        if (sa->did.value != sb->did.value) return PAW_FALSE;
    }

    return pawU_equals(C->U, a, b);
}

paw_Uint pawIr_type_hash(struct Compiler *C, IrType *t)
{
    PAW_UNUSED(C);
    return hash_type(t);
}

struct Printer {
    struct Compiler *C;
    Buffer *buf;
    paw_Env *P;
    int indent;
};

#define PRINT_LITERAL(P, lit) L_ADD_LITERAL(ENV(P), (P)->buf, lit)
#define PRINT_STRING(P, str) pawL_add_nstring(ENV(P), (P)->buf, (str)->text, (str)->length)
#define PRINT_FORMAT(P, ...) pawL_add_fstring(ENV(P), (P)->buf, __VA_ARGS__)
#define PRINT_CHAR(P, c) pawL_add_char(ENV(P), (P)->buf, c)

static void print_type(struct Printer *, IrType *);
static void print_type_list(struct Printer *P, struct IrTypeList *list)
{
    for (int i = 0; i < list->count; ++i) {
        print_type(P, list->data[i]);
        if (i < list->count - 1) PRINT_LITERAL(P, ", ");
    }
}

static void print_bounds(struct Printer *P, struct IrTypeList *bounds)
{
    if (bounds != NULL) {
        PRINT_LITERAL(P, ": ");
        int index;
        struct IrType **ptype;
        K_LIST_ENUMERATE(bounds, index, ptype) {
            if (index > 0) PRINT_LITERAL(P, " + ");
            print_type(P, *ptype);
        }
    }
}

static void print_type(struct Printer *P, IrType *type)
{
    switch (IR_KINDOF(type)) {
        case kIrTuple: {
            struct IrTuple *tup = IrGetTuple(type);
            PRINT_CHAR(P, '(');
            print_type_list(P, tup->elems);
            if (tup->elems->count == 1) PRINT_CHAR(P, ',');
            PRINT_CHAR(P, ')');
            break;
        }
        case kIrSignature: {
            struct IrSignature *fsig = IrGetSignature(type);
            struct HirDecl *decl = pawHir_get_decl(P->C, fsig->did);
            PRINT_LITERAL(P, "fn ");
            if (fsig->self != NULL) {
                print_type(P, fsig->self);
                PRINT_LITERAL(P, "::");
            }
            PRINT_STRING(P, decl->hdr.name);
            if (fsig->types != NULL) {
                PRINT_CHAR(P, '<');
                print_type_list(P, fsig->types);
                PRINT_CHAR(P, '>');
            }
            PRINT_LITERAL(P, "(");
            print_type_list(P, fsig->params);
            PRINT_CHAR(P, ')');
            if (!IrIsAdt(fsig->result)
                    || pawP_type2code(P->C, fsig->result) != BUILTIN_UNIT) {
                PRINT_LITERAL(P, " -> ");
                print_type(P, fsig->result);
            }
            break;
        }
        case kIrFuncPtr: {
            struct IrFuncPtr *fptr = IR_FPTR(type);
            PRINT_LITERAL(P, "fn(");
            print_type_list(P, fptr->params);
            PRINT_CHAR(P, ')');
            if (!IrIsAdt(fptr->result)
                    || pawP_type2code(P->C, fptr->result) != BUILTIN_UNIT) {
                PRINT_LITERAL(P, " -> ");
                print_type(P, fptr->result);
            }
            break;
        }
        case kIrGeneric: {
            struct IrGeneric *gen = IrGetGeneric(type);
            struct HirDecl *decl = pawHir_get_decl(P->C, gen->did);
            PRINT_STRING(P, decl->hdr.name);
            break;
        }
        case kIrInfer: {
            struct IrInfer *infer = IrGetInfer(type);
            PRINT_CHAR(P, '_');
            break;
        }
        case kIrTraitObj: {
            struct IrTraitObj *t = IrGetTraitObj(type);
            struct HirDecl *decl = pawHir_get_decl(P->C, t->did);
            PRINT_STRING(P, decl->hdr.name);
            if (t->types != NULL) {
                PRINT_CHAR(P, '<');
                print_type_list(P, t->types);
                PRINT_CHAR(P, '>');
            }
            break;
        }
        default: {
            struct IrAdt *adt = IrGetAdt(type);
            const enum BuiltinKind code = pawP_type2code(P->C, type);
            if (code == BUILTIN_UNIT) {
                PRINT_LITERAL(P, "()");
            } else if (code == BUILTIN_LIST) {
                PRINT_CHAR(P, '[');
                print_type(P, K_LIST_GET(adt->types, 0));
                PRINT_CHAR(P, ']');
            } else if (code == BUILTIN_MAP) {
                PRINT_CHAR(P, '[');
                print_type(P, K_LIST_GET(adt->types, 0));
                PRINT_LITERAL(P, ": ");
                print_type(P, K_LIST_GET(adt->types, 1));
                PRINT_CHAR(P, ']');
            } else {
                struct HirDecl *decl = pawHir_get_decl(P->C, adt->did);
                PRINT_STRING(P, decl->hdr.name);
                if (adt->types != NULL) {
                    PRINT_CHAR(P, '<');
                    print_type_list(P, adt->types);
                    PRINT_CHAR(P, '>');
                }
            }
            break;
        }
    }
}

const char *pawIr_print_type(struct Compiler *C, IrType *type)
{
    Buffer buf;
    paw_Env *P = ENV(C);
    pawL_init_buffer(P, &buf);

    print_type(&(struct Printer){
                .P = ENV(C),
                .buf = &buf,
                .C = C,
            }, type);

    pawL_push_result(P, &buf);
    return paw_string(P, -1);
}

