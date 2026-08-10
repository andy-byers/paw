// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_CODEGEN_ABI_H
#define PAW_CODEGEN_ABI_H

#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/DebugInfoMetadata.h>

namespace paw::cg {

class Context;
class Type;

class AbiInfo {
public:
    enum class Kind {
        EMPTY,

        // object is passed and returned by value
        VALUE,

        // same as `VALUE`, but expands into multiple parameters (`.param_ty` contains a
        // structure type with fields corresponding to expanded parameters)
        EXPAND,

        // object is passed and returned indirectly (`.param_ty = ptr` and `.return_ty = void`,
        // caller is expected to invoke `sret` machinery to return values of this type)
        MEMORY,
    } kind;

    llvm::Type *return_ty;
    llvm::Type *param_ty;

    struct {
        bool requires_byval = false;
        llvm::MaybeAlign alignment = {};
    } m;

    bool is_value() const { return kind == Kind::VALUE; }
    bool is_expand() const { return kind == Kind::EXPAND; }
    bool is_memory() const { return kind == Kind::MEMORY; }
};

AbiInfo get_abi_info(Context &X, Type const &type);

} // namespace paw::cg

#endif // PAW_CODEGEN_ABI_H


