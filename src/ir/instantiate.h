// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_IR_INSTANTIATE_H
#define PAW_IR_INSTANTIATE_H

#include "type.h"

struct Substitution {
    struct IrGenericArgs *params;
    struct IrGenericArgs *args;
};

// Type representing the result of a type instantiation
// Contains the instantiated type, as well as the substitution applied to it.
struct Instantiation {
    struct Substitution subst;
    struct IrType *inst;
};

struct Instantiation pawP_instantiate_assoc(struct Compiler *C, struct IrType *type, struct IrType *method);

// Substitute types in "subst.params" for types in "subst.args" in the context of the given "type"
// Note that "type" is not modified by this operation.
struct IrType *substitute_type(struct Compiler *C, struct IrType *type, struct Substitution subst);
struct IrTrait *substitute_trait(struct Compiler *C, struct IrTrait *trait, struct Substitution subst);
struct IrGenericArg substitute_arg(struct Compiler *C, struct IrGenericArg arg, struct Substitution subst);
struct IrConst *substitute_const(struct Compiler *C, struct IrConst *k, struct Substitution subst);

EXTERN_C struct IrTypeList *instantiate_struct_fields(struct Compiler *C, struct IrAdt const *inst);
EXTERN_C struct IrTypeList *instantiate_variant_fields(struct Compiler *C, struct IrAdt const *inst, int index);

void init_substitution_folder(struct IrTypeFolder *F, struct Compiler *C, struct Substitution *subst,
                                   struct IrGenericArgs *params, struct IrGenericArgs *args);

#endif // PAW_IR_INSTANTIATE_H

