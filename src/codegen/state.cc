// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "compile.h"
#include "context.h"
#include "state.h"
#include "value.h"

namespace paw::cg {

ScratchMap::ScratchMap(Context &X)
    : X(&X)
{
}

llvm::Value *ScratchMap::get(llvm::Type *type)
{
    auto *B = X->get_builder();
    auto *bb = B->GetInsertBlock();
    // add a local to the end of the entry block
    B->SetInsertPointPastAllocas(bb->getParent());
    auto *temp = B->CreateAlloca(type);
    B->SetInsertPoint(bb);
    return temp;
}

State::State(Context &X, Fn *fn)
    : X(&X)
    , fn_(fn)
    , args_(fn->get_num_args())
    , scratch_(X)
{
    auto *B = X.get_builder();
    auto *c = X.get_context();
    auto *type = fn->get_type();

    entry_ = llvm::BasicBlock::Create(*c, "entry", *fn);
    B->SetInsertPoint(entry_);

    for (auto i = 0U; i < fn_->get_num_args(); ++i) {
        auto *param_type = type->get_param_type(i);
        auto *stack_slot = B->CreateAlloca(*param_type);
        X.store_value(fn_->get_arg(i), stack_slot);
        args_[i] = stack_slot;
    }
}

Module *State::get_module() const
{
    return X->get_module();
}

llvm::IRBuilder<> *State::get_builder() const
{
    return X->get_builder();
}

void State::create_return(llvm::Value *value)
{
    auto *B = X->get_builder();
    auto *fn_type = fn_->get_type();
    auto *return_type = fn_type->get_return_type();

    switch (fn_type->get_return_kind()) {
        case ReturnKind::VOID:
            B->CreateRetVoid();
            break;
        case ReturnKind::NORMAL: {
            auto *abi_ty = return_type->get_abi_ty();
            auto *abi = get_scratch(abi_ty);
            B->CreateStore(value, abi);

            auto *result = B->CreateLoad(abi_ty, abi);
            B->CreateRet(result);
            break;
        }
        case ReturnKind::SRET: {
            auto *sret = fn_->get_fn()->getArg(0);
            B->CreateStore(value, sret);
            B->CreateRetVoid();
            break;
        }
    }
}

static llvm::Value *into_arg(Context &X, State &state, llvm::Value *arg, Type *type, Type *fntype, unsigned index)
{
    auto *B = X.get_builder();
    switch (type->get_abi_class()) {
        case ABIClass::EMPTY:
        case ABIClass::SCALAR:
        case ABIClass::HXA_STRUCT:
            return arg;
        case ABIClass::SMALL_STRUCT:
        case ABIClass::BINARY_STRUCT: {
            auto *scratch = state.get_scratch(*type);
            B->CreateStore(arg, scratch);
            return B->CreateLoad(type->get_abi_ty(), scratch);
        }
        case ABIClass::LARGE_STRUCT: {
            auto *scratch = state.get_scratch(*type);
            B->CreateStore(arg, scratch);
            return scratch;
        }
    }
}

llvm::Value *State::get_scratch(llvm::Type *type)
{
    return scratch_.get(type);
}

llvm::Value *State::create_call(Callable const &call, llvm::ArrayRef<llvm::Value *> args)
{
    auto *B = X->get_builder();
    auto const callee = call.get_callee();
    auto *type = call.get_type();

    std::vector<llvm::Value *> rewrite = {call.env_};
    rewrite.reserve(1 + args.size());

    for (auto i = 0U; i < args.size(); ++i)
        rewrite.push_back(into_arg(*X, *this, args[i], type->get_param_type(i), type, i));

    auto *return_type = type->get_return_type();
    auto *return_ty = return_type->get_ty();
    switch (type->get_return_kind()) {
        case ReturnKind::VOID:
            B->CreateCall(callee, rewrite);
            return X->create_unit();
        case ReturnKind::NORMAL: {
            llvm::Value *value = B->CreateCall(callee, rewrite);
            if (return_type->is_abi_struct_type()) {
                // convert from ABI return type to actual type
                auto *scratch = get_scratch(*type);
                B->CreateStore(value, scratch);
                value = B->CreateLoad(return_ty, scratch);
            }
            return value;
        }
        case ReturnKind::SRET: {
            auto *sret = get_scratch(*type->get_return_type());
            rewrite.insert(begin(rewrite), sret);
            auto *c = B->CreateCall(callee, rewrite);
            c->addParamAttr(0, llvm::Attribute::getWithStructRetType(
                        *X->get_context(), return_ty));
            return B->CreateLoad(return_ty, sret);
        }
    }
}

} // namespace paw::cg
