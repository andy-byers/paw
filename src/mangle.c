// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "mangle.h"
#include "auxlib.h"
#include "ir_type.h"
#include "mir.h"


static void add_rle_string(struct Compiler *C, Buffer *b, Str const *s)
{
    pawL_add_int(ENV(C), b, (paw_Int)s->length);
    pawL_add_nstring(ENV(C), b, s->text, s->length);
}

static void start_generic_args(struct Compiler *C, Buffer *buf)
{
    pawL_add_char(ENV(C), buf, 'I');
}

static void finish_generic_args(struct Compiler *C, Buffer *buf)
{
    pawL_add_char(ENV(C), buf, 'E');
}

static void add_module_name(struct Compiler *C, Buffer *b, int modno)
{
    struct Module const info = ModuleInfo_get(C->modinfo, modno);
    add_rle_string(C, b, info.name);
}

static void add_type(struct Compiler *C, Buffer *b, IrType *type);

static void add_int_kind(paw_Env *P, Buffer *b, enum IrIntKind kind)
{
    switch (kind) {
        case IR_INT8:
            pawL_add_char(P, b, 'a');
            break;
        case IR_INT16:
            pawL_add_char(P, b, 's');
            break;
        case IR_INT32:
            pawL_add_char(P, b, 'i');
            break;
        case IR_INT64:
            pawL_add_char(P, b, 'x');
            break;
        case IR_ISIZE:
            pawL_add_char(P, b, 'j');
            break;
        case IR_UINT8:
            pawL_add_char(P, b, 'h');
            break;
        case IR_UINT16:
            pawL_add_char(P, b, 't');
            break;
        case IR_UINT32:
            pawL_add_char(P, b, 'u');
            break;
        case IR_UINT64:
            pawL_add_char(P, b, 'y');
            break;
        case IR_USIZE:
            pawL_add_char(P, b, 'k');
            break;
    }
}

static void add_float_kind(paw_Env *P, Buffer *b, enum IrFloatKind kind)
{
    switch (kind) {
        case IR_FLOAT32:
            pawL_add_char(P, b, 'f');
            break;
        case IR_FLOAT64:
            pawL_add_char(P, b, 'd');
            break;
    }
}

static void add_const(struct Compiler *C, Buffer *b, IrConst *konst)
{
    paw_Env *P = ENV(C);
    pawL_add_char(P, b, 'K');

    if (konst->kind == IR_CONST_DECL) {
        struct IrGenericDef const *def = pawIr_get_generic_def(C, konst->decl.did);
        L_ADD_STRING(P, b, def->konst.name);
    } else {
        paw_assert(konst->kind == IR_CONST_VALUE);

        // TODO: Figure out a better encoding for the constant value (currently just using type prefix + binary data)
        switch (IR_KINDOF(konst->value.type)) {
            case kIrBool:
                pawL_add_char(P, b, 'b');
                break;
            case kIrChar:
                pawL_add_char(P, b, 'c');
                break;
            case kIrInt:
                add_int_kind(P, b, IR_INT_KIND(konst->value.type));
                break;
            default:
                add_float_kind(P, b, IR_FLOAT_KIND(konst->value.type));
                break;
        }
        pawL_add_hex(P, b, (paw_Uint)konst->value.value.i);
    }
}

static void add_generic_arg(struct Compiler *C, Buffer *b, IrGenericArg arg)
{
    if (IrGenericArg_is_type(arg)) {
        IrType *t = IrGenericArg_get_type(arg);
        add_type(C, b, t);
    } else {
        IrConst *k = IrGenericArg_get_const(arg);
        pawL_add_char(ENV(C), b, 'L');
        add_const(C, b, k);
    }
}

static void add_generic_args(struct Compiler *C, Buffer *buf, IrGenericArgs *args)
{
    if (args->count > 0) {
        start_generic_args(C, buf);
        K_LIST_XFOREACH (args, IrGenericArg const, p)
            add_generic_arg(C, buf, *p);
        finish_generic_args(C, buf);
    }
}

static void add_generic_args_omitting_self(struct Compiler *C, Buffer *buf, IrGenericArgs *args)
{
    if (args->count > 1) {
        start_generic_args(C, buf);
        for (int i = 1; i < args->count; ++i)
            add_generic_arg(C, buf, IrGenericArgs_get(args, i));
        finish_generic_args(C, buf);
    }
}

static void add_type(struct Compiler *C, Buffer *b, IrType *type)
{
    paw_Env *P = ENV(C);
    switch (IR_KINDOF(type)) {
        case kIrUnit:
            // mangle like empty tuple
            L_ADD_LITERAL(P, b, "TE");
            break;
        case kIrBool:
            pawL_add_char(P, b, 'b');
            break;
        case kIrChar:
            pawL_add_char(P, b, 'c');
            break;
        case kIrInt:
            add_int_kind(P, b, IR_INT_KIND(type));
            break;
        case kIrFloat:
            add_float_kind(P, b, IR_FLOAT_KIND(type));
            break;
        case kIrString:
            pawL_add_char(P, b, 'w');
            break;
        case kIrPtr:
            pawL_add_char(P, b, 'p');
            add_type(C, b, ir_deref(type));
            break;
        case kIrAdt: {
            struct IrAdt const *t = IrGetAdt(type);
            struct IrAdtDef const *def = pawIr_get_adt_def(C, t->did);
            add_module_name(C, b, (int)def->did.modno);
            add_rle_string(C, b, def->name);
            add_generic_args(C, b, t->args);
            break;
        }
        case kIrClosure:
        case kIrSignature:
        case kIrFnPtr: {
            struct IrFnPtr const *fn = IrGetFnPtr(IR_GET_FN(C, type));
            pawL_add_char(P, b, 'F');
            add_type(C, b, fn->result);
            K_LIST_XFOREACH (fn->params, IrType *const, p)
                add_type(C, b, *p);
            pawL_add_char(P, b, 'E');
            break;
        }
        case kIrTuple: {
            struct IrTuple const *t = IrGetTuple(type);
            pawL_add_char(P, b, 'T');
            K_LIST_XFOREACH (t->elems, IrType *const, p)
                add_type(C, b, *p);
            pawL_add_char(P, b, 'E');
            break;
        }
        case kIrSlice: {
            struct IrSlice const *t = IrGetSlice(type);
            pawL_add_char(P, b, 'S');
            add_type(C, b, t->type);
            break;
        }
        case kIrArray: {
            struct IrArray const *t = IrGetArray(type);
            pawL_add_char(P, b, 'A');
            add_const(C, b, t->length);
            pawL_add_char(P, b, '_');
            add_type(C, b, t->type);
            break;
        }
        default:
            paw_assert(IrIsNever(type));
            pawL_add_char(P, b, 'x');
            break;
    }
}

static void add_trait(struct Compiler *C, Buffer *b, IrTrait *trait)
{
    struct IrTraitDef const *def = pawIr_get_trait_def(C, trait->did);
    add_rle_string(C, b, def->name);
    add_generic_args_omitting_self(C, b, trait->args);
}

static void add_fn_part(struct Compiler *C, Buffer *b, IrType *type)
{
    struct IrFnDef const *fn_def = pawIr_get_fn_def(C, IR_TYPE_DID(type));
    add_module_name(C, b, (int)fn_def->did.modno);
    add_rle_string(C, b, fn_def->name);
    add_generic_args(C, b, IR_GENERIC_ARGS(type));
}

Str *mangle_type(struct Compiler *C, IrType *type)
{
    Buffer b;
    paw_Env *P = ENV(C);
    pawL_init_buffer(P, &b);

    L_ADD_LITERAL(P, &b, "_P");

    if (IrIsSignature(type)) {
        IrType *self = pawIr_get_context(C, type);
        if (self == NULL) {
            // mangling a free function
            pawL_add_char(P, &b, 'N');
            add_fn_part(C, &b, type);
        } else {
            // mangling a method (trait or inherent impl)
            struct IrFnDef const *fn_def = pawIr_get_fn_def(C, IR_TYPE_DID(type));
            struct IrImpl const *impl_def = pawIr_get_impl_def(C, fn_def->parent);
            if (impl_def->trait == NULL) {
                pawL_add_char(P, &b, 'M');
                add_type(C, &b, self);
            } else {
                IrTrait *trait = pawIr_get_trait_context(C, type);
                pawL_add_char(P, &b, 'X');
                add_type(C, &b, self);
                add_trait(C, &b, trait);
            }
            add_fn_part(C, &b, type);
        }
    } else if (IrIsClosure(type)) {
        struct IrClosure const *t = IrGetClosure(type);
        pawL_add_fstring(P, &b, "C%d", t->did.value);
        add_generic_args(C, &b, t->args);
    } else {
        add_type(C, &b, type);
    }

    return pawL_buffer_finish(P, &b);
}

