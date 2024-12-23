// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "ir_type.h"
#include "map.h"

#define NEW_NODE(C, T) (T *)pawK_pool_alloc(ENV(C), (C)->pool, sizeof(T))

struct IrFieldDef *pawIr_new_field(struct Compiler *C, DeclId did, String *name, paw_Bool is_pub)
{
    struct IrFieldDef *def = NEW_NODE(C, struct IrFieldDef);
    *def = (struct IrFieldDef){
        .is_pub = is_pub,
        .name = name,
        .did = did,
    };
    return def;
}

struct IrVariantDef *pawIr_new_variant_def(struct Compiler *C, DeclId xdid, int discr, String *name, struct IrFieldList *fields)
{
    struct IrVariantDef *def = NEW_NODE(C, struct IrVariantDef);
    *def = (struct IrVariantDef){
        .fields = fields,
        .discr = discr,
        .name = name,
        .xdid = xdid,
    };
    return def;
}

struct IrAdtDef *pawIr_new_adt_def(struct Compiler *C, DeclId did, String *name, paw_Bool is_pub, paw_Bool is_struct)
{
    struct IrAdtDef *def = NEW_NODE(C, struct IrAdtDef);
    *def = (struct IrAdtDef){
        .variants = pawIr_variant_list_new(C),
        .is_struct = is_struct,
        .is_pub = is_pub,
        .name = name,
        .did = did,
    };
    return def;
}


struct IrType *pawIr_new_type(struct Compiler *C, enum IrTypeKind kind)
{
    struct IrType *type = NEW_NODE(C, struct IrType);
    *type = (struct IrType){
        .hdr.kind = kind,
    };
    return type;
}

struct IrType *pawIr_get_type(struct Compiler *C, HirId hid)
{
    const Value *pv = pawH_get(C->ir_types, I2V(hid.value));
    return pv != NULL ? pv->p : NULL;
}

void pawIr_set_type(struct Compiler *C, HirId hid, struct IrType *type)
{
    paw_assert(type != NULL);
    pawH_insert(ENV(C), C->ir_types, I2V(hid.value), P2V(type));
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

static void print_type(struct Printer *, struct IrType *);
static void print_type_list(struct Printer *P, struct IrTypeList *list)
{
    for (int i = 0; i < list->count; ++i) {
        print_type(P, list->data[i]);
        if (i < list->count - 1) PRINT_LITERAL(P, ", ");
    }
}

static void print_type(struct Printer *P, struct IrType *type)
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
        case kIrSignature:
        case kIrFuncPtr: {
            struct IrFuncPtr *fptr = IR_FPTR(type);
            PRINT_LITERAL(P, "fn(");
            print_type_list(P, fptr->params);
            PRINT_CHAR(P, ')');
            if (!IrIsAdt(fptr->result)
                    || IrGetAdt(fptr->result)->did.value != PAW_TUNIT) {
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
            PRINT_FORMAT(P, "?%d", infer->index);
            break;
        }
        default: {
            struct IrAdt *adt = IrGetAdt(type);
            const paw_Type code = pawP_type2code(P->C, type);
            if (code == PAW_TUNIT) {
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

const char *pawIr_print_type(struct Compiler *C, struct IrType *type)
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

