// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "abi.h"
#include "context.h"
#include "state.h"
#include "value.h"

namespace paw::cg {

ScratchMap::ScratchMap(Context &X)
    : X(&X)
{
}

llvm::Value *ScratchMap::get(Type *type)
{
    auto *B = X->get_builder();
    auto *bb = B->GetInsertBlock();
    // add a local to the end of the entry block
    B->SetInsertPointPastAllocas(bb->getParent());
    auto *temp = X->create_alloca(type);
    B->SetInsertPoint(bb);
    return temp;
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

static void store_abi_arg(Context &X, llvm::Value *arg, llvm::Value *out, llvm::Type *abi_ty, llvm::Type *out_ty)
{
    auto *B = X.get_builder();
    if (X.bitsize_of(out_ty) < X.bitsize_of(abi_ty)) {
        if (abi_ty->isIntegerTy()) {
            auto const bitsize = PAW_ROUND_UP(X.bitsize_of(out_ty), 8);
            arg = B->CreateTrunc(arg, X.get_sized_int_ty(bitsize));
        } else {
            paw_assert(abi_ty->isDoubleTy());
            paw_assert(X.bitsize_of(out_ty) == 32);
            arg = B->CreateFPTrunc(arg, X.get_f32_ty());
        }
    }
    B->CreateStore(arg, out);
}

State::State(Context &X, Fn *fn)
    : X(&X)
    , fn_(fn)
    , scratch_(X)
{
    auto *B = X.get_builder();
    auto *c = X.get_context();
    auto *fn_type = fn->get_type();

    entry_ = llvm::BasicBlock::Create(*c, "entry", *fn);
    B->SetInsertPoint(entry_);

    args_.reserve(fn->get_num_args());
    auto const fn_info = get_abi_info_(X, *fn_type);
    for (auto iparam = 0U, iarg = 0U; iparam < fn_type->get_num_params(); ++iparam) {
        auto *type = fn_type->get_param_type(iparam);
        auto const info = fn_info.param_info[iparam];
        switch (info.kind) {
            case AbiInfo::Kind::EMPTY: {
                auto *alloca = X.create_alloca(type);
                B->CreateStore(X.create_unit(), alloca);
                args_.push_back(alloca);
                break;
            }
            case AbiInfo::Kind::VALUE: {
                auto *value = fn_->get_arg(iarg++);
                auto *alloca = X.create_alloca(info.param_ty);
                store_abi_arg(X, value, alloca, info.param_ty, *type);
                args_.push_back(alloca);
                break;
            }
            case AbiInfo::Kind::EXPAND: {
                auto *alloca = X.create_alloca(info.param_ty);
                auto *fields = llvm::cast<llvm::StructType>(info.param_ty);
                for (unsigned j = 0; j < fields->getNumElements(); ++j) {
                    auto *ptr = B->CreateStructGEP(info.param_ty, alloca, j);
                    auto *value = fn_->get_arg(iarg++);
                    store_abi_arg(X, value, ptr, info.param_ty, *type);
                }
                args_.push_back(alloca);
                break;
            }
            case AbiInfo::Kind::MEMORY: {
                auto *alloca = X.create_alloca(type);
                auto *value = B->CreateLoad(type->get_ty(),
                        fn_->get_arg(iarg++));
                B->CreateStore(value, alloca);
                args_.push_back(alloca);
                break;
            }
        }
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
    auto const return_info = get_abi_info_(*X, *fn_type).return_info;
    switch (return_info.kind) {
        case AbiInfo::Kind::EMPTY:
            B->CreateRetVoid();
            break;
        case AbiInfo::Kind::VALUE:
        case AbiInfo::Kind::EXPAND: {
            auto *scratch = get_scratch(return_info.return_ty);
            B->CreateStore(value, scratch);

            auto *result = B->CreateLoad(return_info.return_ty, scratch);
            B->CreateRet(result);
            break;
        }
        case AbiInfo::Kind::MEMORY: {
            auto *sret = fn_->get_fn()->getArg(0);
            B->CreateStore(value, sret);
            B->CreateRetVoid();
            break;
        }
    }
}

llvm::Value *State::get_scratch(llvm::Type *type)
{
    return scratch_.get(type);
}

llvm::Value *State::get_scratch(Type *type)
{
    return scratch_.get(type);
}

llvm::Value *State::get_arg(unsigned index) const
{
    return args_.at(index);
}

static void add_abi_args(Context &X, State &state, llvm::Value *value, Type &type, AbiInfo info, std::vector<llvm::Value *> &out, std::vector<llvm::AttributeSet> &attrs)
{
    auto *B = X.get_builder();
    auto *c = X.get_context();
    switch (info.kind) {
        case AbiInfo::Kind::EMPTY:
            break;
        case AbiInfo::Kind::VALUE: {
            auto *scratch = state.get_scratch(type);
            B->CreateStore(value, scratch);
            auto *arg = B->CreateLoad(info.param_ty, scratch);
            attrs.push_back(llvm::AttributeSet());
            out.push_back(arg);
            break;
        }
        case AbiInfo::Kind::EXPAND: {
            auto *scratch = state.get_scratch(type);
            B->CreateStore(value, scratch);
            auto *fields = llvm::cast<llvm::StructType>(info.param_ty);
            for (unsigned i = 0; i < fields->getNumElements(); ++i) {
                auto *ptr = B->CreateStructGEP(info.param_ty, scratch, i);
                auto *arg = B->CreateLoad(fields->getElementType(i), ptr);
                attrs.push_back(llvm::AttributeSet());
                out.push_back(arg);
            }
            break;
        }
        case AbiInfo::Kind::MEMORY: {
            auto *arg = state.get_scratch(type);
            llvm::AttrBuilder a(*c);
            if (info.m.requires_byval)
                a.addByValAttr(type.get_ty());
            if (info.m.alignment.has_value())
                a.addAlignmentAttr(info.m.alignment.value());
            attrs.push_back(llvm::AttributeSet::get(*c, a));
            B->CreateStore(value, arg);
            out.push_back(arg);
            break;
        }
    }
}

static void set_attributes(Context &X, llvm::CallInst *call, std::vector<llvm::AttributeSet> arg_attrs)
{
    call->setAttributes(
            llvm::AttributeList::get(*X.get_context(),
                {}, {}, // FnAttrs, RetAttrs
                arg_attrs));
}

llvm::Value *State::create_call(Callable const &call, llvm::Value *env, llvm::ArrayRef<llvm::Value *> args)
{
    auto *B = X->get_builder();
    auto callee = call.get_callee();
    auto *type = call.get_type();
    std::vector<llvm::Value *> rewrite;
    std::vector<llvm::AttributeSet> attrs;
    rewrite.reserve((env != nullptr) + args.size());

    auto const fn_info = get_abi_info_(*X, *type);
    if (fn_info.has_env)
        add_abi_args(*X, *this, env, *type->get_env_type(), fn_info.env_info, rewrite, attrs);
    auto param_info = begin(fn_info.param_info);
    for (auto i = 0U; i < args.size(); ++i)
        add_abi_args(*X, *this, args[i], *type->get_param_type(i), *param_info++, rewrite, attrs);

    auto const return_info = fn_info.return_info;
    auto *return_type = type->get_return_type();
    auto *return_ty = return_type->get_ty();
    switch (return_info.kind) {
        case AbiInfo::Kind::EMPTY: {
            auto *c = B->CreateCall(callee, rewrite);
            set_attributes(*X, c, attrs);
            return X->create_unit();
        }
        case AbiInfo::Kind::VALUE:
        case AbiInfo::Kind::EXPAND: {
            auto *c = B->CreateCall(callee, rewrite);
            set_attributes(*X, c, attrs);
            // convert from ABI return type to actual type
            auto *scratch = get_scratch(return_type);
            B->CreateStore(c, scratch);
            return B->CreateLoad(return_ty, scratch);
        }
        case AbiInfo::Kind::MEMORY: {
            auto *sret = get_scratch(type->get_return_type());
            attrs.insert(begin(attrs), llvm::AttributeSet());
            rewrite.insert(begin(rewrite), sret);
            auto *c = B->CreateCall(callee, rewrite);
            set_attributes(*X, c, attrs);
            c->addParamAttr(0, llvm::Attribute::getWithStructRetType(
                        *X->get_context(), return_ty));
            return B->CreateLoad(return_ty, sret);
        }
    }
}

} // namespace paw::cg
