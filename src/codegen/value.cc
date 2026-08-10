// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "abi.h"
#include "context.h"
#include "state.h"
#include "type.h"
#include "value.h"

#define S(Str_) std::string((Str_)->text, (size_t)(Str_)->length)
#define DEFERRED_INIT ((llvm::Value *)42)

namespace paw::cg {

Context *Value::get_context() const
{
    return state_->get_context();
}


static unsigned env_args_count(Context &X, FnType const *type)
{
    if (!type->has_env())
        return 0;

    auto *env_type = type->get_env_type();
    auto const env_info = get_abi_info(X, *env_type);
    if (env_info.kind != AbiInfo::Kind::EXPAND)
        return 1;
    return llvm::cast<llvm::StructType>(env_info.param_ty)
        ->getNumElements();
}

static unsigned env_arg_offset(FnType const *type)
{
    return type->get_return_kind() == ReturnKind::SRET;
}

static unsigned user_args_offset(Context &X, FnType const *type)
{
    return env_arg_offset(type) + env_args_count(X, type);
}


Callable::Callable(State &state, llvm::Value *fn, FnType *type)
    : Value(state, fn, type)
{
}

llvm::Value *Callable::get_value() const
{
    return value_;
}


static unsigned add_attr(Context &X, llvm::Function *fn, unsigned base_arg, Type &param_type, unsigned iarg)
{
    auto *c = X.get_context();
    auto const param_info = get_abi_info(X, param_type);
    switch (param_info.kind) {
        case AbiInfo::Kind::EMPTY:
            break;
        case AbiInfo::Kind::VALUE:
            ++iarg;
            break;
        case AbiInfo::Kind::EXPAND: {
            auto *fields = llvm::cast<llvm::StructType>(param_info.param_ty);
            iarg += fields->getNumElements();
            break;
        }
        case AbiInfo::Kind::MEMORY: {
            auto *arg = fn->getArg(base_arg + iarg);
            if (param_info.m.requires_byval) {
                auto attr = llvm::Attribute::getWithByValType(*c, param_type.get_ty());
                arg->addAttr(attr);
            }
            if (param_info.m.alignment.has_value()) {
                auto attr = llvm::Attribute::getWithAlignment(*c, param_info.m.alignment.value());
                arg->addAttr(attr);
            }
            ++iarg;
            break;
        }
    }
    return iarg;
}

Fn::Fn(Context &X, std::string name,
        llvm::Function::LinkageTypes linkage,
        FnType *type)
    : X(&X)
    , type_(type)
{
    auto *c = X.get_context();
    llvm::Module *m = *X.get_module();
    value_ = m->getFunction(name);
    if (value_ == nullptr)
        // NOTE: This check is necessary because some generic functions in the
        //   core modules are implemented by a single C function, i.e. the same C
        //   function is called for each instantiation. Prevents extra functions
        //   from being created in such cases.
        value_ = llvm::Function::Create(type->get_fn_ty(),
                    linkage, name, *X.get_module());

    auto *fn = get_fn();
    fn->setDoesNotThrow();

    if (type->never_returns())
        fn->setDoesNotReturn();

    if (type->has_struct_return()) {
        // add struct return attribute to first parameter
        auto *sret = fn->getArg(0);
        auto *return_ty = type->get_return_type()->get_ty();
        sret->addAttr(llvm::Attribute::getWithStructRetType(*c, return_ty));
        sret->addAttr(llvm::Attribute::getWithAlignment(*c, X.align_of(return_ty)));
        sret->addAttr(llvm::Attribute::get(*c, llvm::Attribute::Writable));
        sret->addAttr(llvm::Attribute::get(*c, llvm::Attribute::NoAlias));
    }

    if (type->has_env())
        add_attr(X, fn, env_arg_offset(type), *type->get_env_type(), 0);

    auto const offset = user_args_offset(X, type);
    for (auto iparam = 0U, iarg = 0U; iparam < type->get_num_params(); ++iparam) {
        auto *param_type = type->get_param_type(iparam);
        auto const param_info = get_abi_info(X, *param_type);
        switch (param_info.kind) {
            case AbiInfo::Kind::EMPTY:
                break;
            case AbiInfo::Kind::VALUE:
                ++iarg;
                break;
            case AbiInfo::Kind::EXPAND: {
                auto *fields = llvm::cast<llvm::StructType>(param_info.param_ty);
                iarg += fields->getNumElements();
                break;
            }
            case AbiInfo::Kind::MEMORY: {
                auto *arg = fn->getArg(offset + iarg);
                if (param_info.m.requires_byval) {
                    auto attr = llvm::Attribute::getWithByValType(*c, *param_type);
                    arg->addAttr(attr);
                }
                if (param_info.m.alignment.has_value()) {
                    auto attr = llvm::Attribute::getWithAlignment(*c, param_info.m.alignment.value());
                    arg->addAttr(attr);
                }
                ++iarg;
                break;
            }
        }
    }
}

llvm::Value *Fn::get_value() const
{
    return value_;
}

Callable Fn::as_callable(State &state) const
{
    return Callable(state, value_, type_);
}

std::string Fn::get_name() const
{
    return get_fn()->getName().str();
}

llvm::Value *Fn::load_env() const
{
    auto *B = X->get_builder();
    auto *fn_type = get_type();
    auto *env_type = fn_type->get_env_type();
    auto const env_info = get_abi_info(*X, *env_type);
    auto const offset = env_arg_offset(fn_type);
    switch (env_info.kind) {
        case AbiInfo::Kind::EMPTY:
            return X->create_unit();
        case AbiInfo::Kind::VALUE:
            return get_fn()->getArg(offset);
        case AbiInfo::Kind::EXPAND: {
            auto *scratch = B->CreateAlloca(*env_type);
            auto *fields = llvm::cast<llvm::StructType>(env_info.param_ty);
            for (unsigned i = 0; i < fields->getNumElements(); ++i) {
                auto *ptr = B->CreateStructGEP(env_info.param_ty, scratch, i);
                auto *arg = get_fn()->getArg(offset + i);
                B->CreateStore(arg, ptr);
            }
            return B->CreateLoad(*env_type, scratch);
        }
        case AbiInfo::Kind::MEMORY: {
            auto *ptr = get_fn()->getArg(offset);
            return B->CreateLoad(*env_type, ptr);
        }
    }
}

llvm::Value *Fn::get_arg(unsigned index) const
{
    return get_fn()->getArg(user_args_offset(*X, get_type()) + index);
}

Str::Str(State &state, llvm::Value *str, Str::Methods const *methods)
    : Value(state, str, state.get_context()->get_str_type())
    , value_(str)
    , methods_(methods)
{
}

llvm::Value *Str::get_text() const
{
    return state_->get_context()->get_builder()
        ->CreateExtractValue(value_, 0ULL);
}

llvm::Value *Str::get_length() const
{
    return state_->get_context()->get_builder()
        ->CreateExtractValue(value_, 1ULL);
}

llvm::Value *Str::get_element_ptr(llvm::Value *index)
{
    auto *X = state_->get_context();
    auto *B = X->get_builder();
    // TODO: check bounds here?
    return B->CreateInBoundsGEP(X->get_char_ty(),
            get_text(), {index});
}


Object::Object(State &state, llvm::Value *object, ObjectType *type)
    : Value(state, object, type)
{
}

// TODO: have the caller pass in a pointer
static llvm::Value *create_object(State &state, ObjectType const *type)
{
    return state.get_scratch(type->get_struct_ty());
}

Object::Object(State &state, ObjectType *type, Object::CreationTag)
    : Object(state, create_object(state, type), type)
{
}

llvm::Value *Object::get_field_ptr(Discriminant discr, unsigned index)
{
    auto *B = state_->get_builder();
    auto *variant_ty = get_type()->get_variant_ty(discr);
    return B->CreateStructGEP(variant_ty, get_value(), index);
}

llvm::Value *Object::get_field(Discriminant discr, unsigned index)
{
    auto *B = state_->get_builder();
    auto *field_type = get_type()->get_field_type(discr, index);
    return B->CreateLoad(*field_type, get_field_ptr(discr, index));
}

void Object::set_field(Discriminant discr, unsigned index, llvm::Value *value)
{
    auto *B = state_->get_builder();
    B->CreateStore(value, get_field_ptr(discr, index));
}

} // namespace paw::cg
