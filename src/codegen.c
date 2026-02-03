// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "api.h"
#include "auxlib.h"
#include "code.h"
#include "compile.h"
#include "debug.h"
#include "error.h"
#include "hir.h"
#include "impl.h"
#include "ir_type.h"
#include "lib.h"
#include "map.h"
#include "match.h"
#include "mem.h"
#include "mir.h"
#include "parse.h"
#include "regstack.h"
#include "ssa.h"
#include "unify.h"

#include "codegen/codegen.h"
#include "glue.h"

// TODO: need module name, this always reports main module, but error may be elsewhere
#define CODEGEN_ERROR(G_, Kind_, ...) pawErr_##Kind_((G_)->C, (G_)->C->modname, __VA_ARGS__)

#define IS_POINTER(G, r) REG_DATA(G, r).is_pointer
#define REG_DATA(G, r) MirRegisterDataList_get((G)->fs->mir->registers, (r).value)
#define TYPE_CODE(G, type) pawP_type2code((G)->C, type)
#define REG(Place_) vm_register_for(fs, Place_)
#define BASE_REG(Places_) REG(K_LIST_FIRST(Places_))

struct Generator {
    struct Compiler *C;
    struct BodyList *items;
    struct Pool *pool;
    paw_Env *P;
};

static Str const *module_prefix(struct Generator *G, int modno)
{
    return modno != TARGET_MODNO
        ? ModuleInfo_get(G->C->modinfo, modno).name
        : NULL;
}

static void VisitType(struct Compiler *, IrType *);

static void VisitTypeList(struct Compiler *C, IrTypeList *list)
{
    if (list != NULL) {
        IrType *const *ptype;
        K_LIST_FOREACH (list, ptype)
            VisitType(C, *ptype);
    }
}

static void VisitPtr(struct Compiler *C, struct IrPtr *t)
{
    VisitType(C, t->pointee);
}

static void VisitAdt(struct Compiler *C, struct IrAdt *t)
{
    VisitTypeList(C, t->types);

    struct IrAdtDef const *def = pawIr_get_adt_def(C, t->did);
    for (int discr = 0; discr < def->variants->count; ++discr) {
        IrTypeList *fields = pawP_instantiate_variant_fields(C, t, discr);
        VisitTypeList(C, fields);
    }
}

static void VisitSignature(struct Compiler *C, struct IrSignature *t)
{
    VisitTypeList(C, t->types);
    VisitType(C, pawIr_materialize_fn(C, t->did, t->types));
    VisitType(C, pawIr_get_context(C, IR_CAST_TYPE(t)));
}

static void VisitFnPtr(struct Compiler *C, struct IrFnPtr *t)
{
    VisitTypeList(C, t->params);
    VisitType(C, t->result);
}

static void VisitTuple(struct Compiler *C, struct IrTuple *t)
{
    VisitTypeList(C, t->elems);
}

static void VisitType(struct Compiler *C, IrType *type)
{
    if (type == NULL) return;

    switch (IR_KINDOF(type)) {
        case kIrUnit:
            C->typesystem.primitives.unit_t = type;
            return;
        case kIrBool:
            C->typesystem.primitives.bool_t = type;
            return;
        case kIrChar:
            C->typesystem.primitives.char_t = type;
            return;
        case kIrInt:
            C->typesystem.primitives.int_t = type;
            return;
        case kIrFloat:
            C->typesystem.primitives.float_t = type;
            return;
        case kIrStr:
            C->typesystem.primitives.str_t = type;
            return;
        case kIrAdt:
            switch (pawP_type2code(C, type)) {
                case BUILTIN_UNIT:
                    C->typesystem.primitives.unit_t = type;
                    return;
                case BUILTIN_BOOL:
                    C->typesystem.primitives.bool_t = type;
                    return;
                case BUILTIN_CHAR:
                    C->typesystem.primitives.char_t = type;
                    return;
                case BUILTIN_INT:
                    C->typesystem.primitives.int_t = type;
                    return;
                case BUILTIN_FLOAT:
                    C->typesystem.primitives.float_t = type;
                    return;
                case BUILTIN_STR:
                    C->typesystem.primitives.str_t = type;
                    return;
                case BUILTIN_LIST: {
                    void *const *p = TypeCollection_get(C, C->typesystem.lists, type);
                    if (p != NULL) return;

                    TypeCollection_insert(C, C->typesystem.lists, type, NULL);

                    struct Instantiation *inst = pawP_find_method(C, type, SCAN_STR(C, "iterator"));
                    paw_assert(inst != NULL);
                    TypeCollection_insert(C, C->typesystem.iterators.list, type, ir_fn_result(C, inst->inst));
                    break;
                }
                case BUILTIN_MAP: {
                    void *const *p = TypeCollection_get(C, C->typesystem.maps, type);
                    if (p != NULL) return;

                    TypeCollection_insert(C, C->typesystem.maps, type, NULL);

                    struct Instantiation *inst = pawP_find_method(C, type, SCAN_STR(C, "iterator"));
                    paw_assert(inst != NULL);
                    TypeCollection_insert(C, C->typesystem.iterators.map, type, ir_fn_result(C, inst->inst));
                    break;
                }
                default: {
                    void *const *p = TypeCollection_get(C, C->typesystem.adts, type);
                    if (p != NULL) return;

                    struct IrAdtDef const *def = pawIr_get_adt_def(C, IR_TYPE_DID(type));
                    if (pawS_eq(def->name, SCAN_STR(C, "ListIterator"))) {
                        // field_types[0] = [T], where type = ListIterator<T>
                        IrTypeList *field_types = pawP_instantiate_struct_fields(C, IrGetAdt(type));
                        TypeCollection_insert(C, C->typesystem.iterators.list, K_LIST_FIRST(field_types), type);
                    } else if (pawS_eq(def->name, SCAN_STR(C, "MapIterator"))) {
                        // field_types[0] = [K: V], where type = MapIterator<K, V>
                        IrTypeList *field_types = pawP_instantiate_struct_fields(C, IrGetAdt(type));
                        TypeCollection_insert(C, C->typesystem.iterators.map, K_LIST_FIRST(field_types), type);
                    }

                    TypeCollection_insert(C, C->typesystem.adts, type, NULL);
                    break;
                }
            }
            VisitAdt(C, IrGetAdt(type));
            break;
        case kIrPtr:
            TypeCollection_insert(C, C->typesystem.types, type, NULL);
            VisitPtr(C, IrGetPtr(type));
            break;
        case kIrFnPtr:
            TypeCollection_insert(C, C->typesystem.types, type, NULL);
            VisitFnPtr(C, IrGetFnPtr(type));
            break;
        case kIrSignature:
            TypeCollection_insert(C, C->typesystem.types, type, NULL);
            VisitSignature(C, IrGetSignature(type));
            break;
        case kIrTuple:
            TypeCollection_insert(C, C->typesystem.types, type, NULL);
            VisitTuple(C, IrGetTuple(type));
            break;
        case kIrNever:
            C->typesystem.primitives.never_t = type;
            break;
        case kIrInfer:
        case kIrGeneric:
            PAW_UNREACHABLE();
    }
}

static void collect_type(struct Mir *mir, IrType *type)
{
    VisitType(mir->C, type);
}

static void collect_types(struct Mir *mir)
{
    collect_type(mir, mir->type);
    collect_type(mir, mir->self);

    struct MirLocalData const *plocal;
    K_LIST_FOREACH (mir->local_data, plocal)
        collect_type(mir, plocal->type);

    struct MirPlace const *pplace;
    K_LIST_FOREACH (mir->locals, pplace)
        collect_type(mir, pplace->type);

    struct MirRegisterData const *preg;
    K_LIST_FOREACH (mir->registers, preg)
        collect_type(mir, preg->type);

    struct MirUpvalueInfo const *pup;
    K_LIST_FOREACH (mir->upvalues, pup)
        collect_type(mir, pup->type);

    struct Mir *const *pchild;
    K_LIST_FOREACH (mir->children, pchild)
        collect_types(*pchild);
}

static void code_items(struct Generator *G)
{
    struct Compiler *C = G->C;
    BodyList *bodies = BodyList_new(C);

    pawU_enter_binder(C->U, SCAN_STR(C, "(method_query)"));

    VisitType(C, C->strtab_type);
    VisitType(C, C->main_args_type);

    struct Mir *const *pitem;
    K_LIST_FOREACH (G->items, pitem) {
        BodyList_push(C, bodies, *pitem);
        collect_types(*pitem);
    }

    C->typesystem.primitives.never_t = pawIr_new_never(C);
    C->typesystem.primitives.bool_t = pawP_builtin_type(C, BUILTIN_BOOL);
    C->typesystem.primitives.char_t = pawP_builtin_type(C, BUILTIN_CHAR);
    C->typesystem.primitives.int_t = pawP_builtin_type(C, BUILTIN_INT);
    C->typesystem.primitives.float_t = pawP_builtin_type(C, BUILTIN_FLOAT);
    C->typesystem.primitives.str_t = pawP_builtin_type(C, BUILTIN_STR);
//    TypeCollection_insert(C, C->typesystem.maps, C->strtab_type, NULL);
//    TypeCollection_insert(C, C->typesystem.lists, C->main_args_type, NULL);

    pawCodegen_generate(C, &(struct TranslationUnit){
        .modname = C->modname->text,
        .mir_count = bodies->count,
        .mirs = bodies->data,
    });

    pawU_discard_variables(C->U);
    pawU_leave_binder(C->U);

    BodyList_delete(C, bodies);
}

static void register_items(struct Generator *G)
{
    struct Compiler *C = G->C;
    struct MonoResult const mr = pawP_monomorphize(C, C->bodies);
    G->items = mr.bodies;
}

void pawP_generate_code(struct Compiler *C)
{
    paw_Env *P = ENV(C);

    struct Generator G = {
        .pool = pawP_pool_new(C, C->aux_stats),
        .P = P,
        .C = C,
    };
    register_items(&G);
    code_items(&G);

    pawP_pool_free(C, G.pool);

    // report compilation statistics
    pawP_callback(C, "paw.stats_reporter", C->stats);
}

