// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "mangle.h"
#include "auxlib.h"
#include "ir_type.h"


//  Paw construct | mangling format
// ---------------|-----------------
//  bool          | "b"
//  char          | "c"
//  int8          | "a"
//  int16         | "s"
//  int32         | "i"
//  int64         | "x"
//  isize         | "j"
//  uint8         | "h"
//  uint16        | "t"
//  uint32        | "u"
//  uint64        | "y"
//  usize         | "k"
//  float32       | "f"
//  float64       | "d"
//  str           | "w"
//  pointer       | "p" Type
//  ADT           | name "::" name {Arg}
//  fn            | "F" Type {Type} "E"
//  tuple         | "T" {Type} "E"
//  slice         | "S" Type
//  array         | "A" Const "_" Type
//
// Type = Primitive | Pointer | Adt | Fn | Closure | Tuple | Slice | Array .
// Const = "K" Primitive Value .
// Value =
//
// Pointer = "p" Type .
// Adt     = Rle "::" Rle {Arg}
// Fn      = "F" Type {Type} "E"
// Tuple   = "T" {Type} "E"
// Slice   = "S" Type
// Array   = "A" Const "_" Type
//
// Rle            = integer name .
// name           = letter {letter | decimal_digit} .
// letter         = "A".."Z" | "a".."z" | "_" .
// integer        = "1".."9" {decimal_digit} .
// decimal_digit  = "0".."9" .

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
        // TODO: this branch should never be hit since we only mangle concrete instantiations of generic constant values
        PAW_UNREACHABLE();
        struct IrGenericDef const *def = pawIr_get_generic_def(C, konst->decl.did);
        L_ADD_STRING(P, b, def->konst.name);
    } else {
        paw_assert(konst->kind == IR_CONST_VALUE);
        add_type(C, b, konst->value.type);
        if (IrIsString(konst->value.type)) {
            pawL_add_char(P, b, 'A');
            Str const *s = konst->value.value.s;
            for (size_t i = 0; i < s->length; ++i) {
                pawL_add_char(P, b, 'c');
                unsigned char c = (unsigned char)s->text[i];
                if (c > 0xF) pawL_add_char(P, b, '0' + (c >> 4));
                pawL_add_char(P, b, '0' + (c & 0xF));
                pawL_add_char(P, b, '_');
            }
            pawL_add_char(P, b, 'E');
        } else {
            pawL_add_hex(P, b, konst->value.value.u64);
        }
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

static void add_closure_type(struct Compiler *C, Buffer *b, struct IrClosure const *t)
{
    pawL_add_fstring(ENV(C), b, "C%d", t->did.value);
    add_generic_args(C, b, t->args);
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
        case kIrClosure: {
            struct IrClosure const *t = IrGetClosure(type);
            struct IrFnDef const *def = pawIr_get_fn_def(C, t->did);
            if (def->has_captures) {
                // closures that capture variables have types that are incompatible with any
                // other function type
                add_closure_type(C, b, t);
                break;
            }
            // (fallthrough)
        }
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

Str const *mangle_type(struct Compiler *C, IrType *type)
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
        add_closure_type(C, &b, t);
    } else {
        add_type(C, &b, type);
    }

    return pawL_buffer_finish(P, &b);
}

