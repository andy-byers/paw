// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_CODEGEN_STATE_H
#define PAW_CODEGEN_STATE_H

#include "codegen.h"
#include "context.h"

namespace paw::cg {

class ScratchMap {
public:
    explicit ScratchMap(Context &X);
    llvm::Value *get(llvm::Type *type);

private:
    struct SizeAlign {
        llvm::TypeSize size;
        llvm::Align align;
    };

    std::function<unsigned long(SizeAlign)> za_hash_ = [](auto za) {
        return std::hash<unsigned long>()(za.size) * 0x9E3779B97F4A7C15ULL
            + std::hash<unsigned long>()(za.align.value());
    };

    std::function<bool(SizeAlign, SizeAlign)> za_equals_ = [](auto lhs, auto rhs) {
        return lhs.size == rhs.size && lhs.align == rhs.align;
    };

    Context *X;

    std::unordered_map<
        SizeAlign,
        llvm::Value *,
        decltype(za_hash_),
        decltype(za_equals_)
    > za_temp_{0, za_hash_, za_equals_};

    std::unordered_map<
        Type const *,
        std::vector<llvm::Value *>,
        decltype(hash_type) *,
        decltype(type_equals) *
    > call_temp_{0, hash_type, type_equals};
};

// Object representing a function in the process of being generated
class State {
public:
    explicit State(Context &X, Fn *fn);
    void load_args();

    State(State const &) = delete;
    State &operator=(State const &) = delete;

    Module *get_module() const;
    llvm::IRBuilder<> *get_builder() const;
    Context *get_context() const { return X; }
    llvm::BasicBlock *get_entry() const { return entry_; }
    Fn *get_fn() const { return fn_; }

    llvm::Value *get_arg(unsigned index) const;

    llvm::Value *get_scratch(llvm::Type *type);

    void create_return(llvm::Value *value = nullptr);
    llvm::Value *create_call(Callable const &call, llvm::Value *env, llvm::ArrayRef<llvm::Value *> args);
    llvm::Value *create_call(Callable const &call, llvm::Value *env, llvm::Value *arg) { return create_call(call, env, llvm::ArrayRef<llvm::Value *>{arg}); }
    llvm::Value *create_call(Callable const &call, llvm::Value *env) { return create_call(call, env, llvm::ArrayRef<llvm::Value *>{}); }

protected:
    Context *X;
    Fn *fn_;

    llvm::BasicBlock *entry_;
    std::vector<llvm::Value *> args_;
    ScratchMap scratch_;
};

} // namespace paw::cg

#endif // PAW_CODEGEN_STATE_H


