// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "value.h"
#include "context.h"
#include "ir_type.h"
#include "mir.h"
#include "state.h"
#include "type.h"

#define S(Str_) std::string((Str_)->text, (size_t)(Str_)->length)
#define DEFERRED_INIT ((llvm::Value *)42)

namespace paw::cg {

Context *Value::get_context() const
{
    return state_->get_context();
}


static unsigned env_pointer_offset(FnType const *type)
{
    return type->get_return_kind() == ReturnKind::SRET;
}

static unsigned user_args_offset(FnType const *type)
{
    return env_pointer_offset(type) + type->has_env();
}


Callable::Callable(State &state, llvm::Value *fn, FnType *type)
    : Value(state, fn, type)
{
}

llvm::Value *Callable::get_value() const
{
    return value_;
}


static void setup_fn(Context &X, llvm::Function *fn, FnType *type)
{
    fn->setDoesNotThrow();

    if (type->never_returns())
        fn->setDoesNotReturn();

    if (type->get_return_kind() == ReturnKind::SRET) {
        // add struct return attribute to first parameter
        using A = llvm::Attribute;
        fn->addParamAttr(0, A::getWithStructRetType(
                    *X.get_context(), *type->get_return_type()));
    }
}

Fn::Fn(Context &X, std::string name,
        llvm::Function::LinkageTypes linkage,
        FnType *type)
    : X(&X)
    , type_(type)
{
    llvm::Module *m = *X.get_module();
    value_ = m->getFunction(name);
    if (value_ == nullptr)
        // NOTE: This check is necessary because some generic functions in the
        //   core modules are implemented by a single C function, i.e. the same C
        //   function is called for each instantiation. Prevents extra functions
        //   from being created in such cases.
        value_ = llvm::Function::Create(type->get_fn_ty(),
                    linkage, name, *X.get_module());
    setup_fn(X, get_fn(), type);
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
    auto *arg_type = fn_type->get_env_type();
    if (arg_type == nullptr) return nullptr;
    llvm::Value *arg = get_fn()->getArg(env_pointer_offset(fn_type));
    return arg_type->get_abi_class() == ABIClass::LARGE_STRUCT
        ? B->CreateLoad(*arg_type, arg)
        : arg;
}

llvm::Value *Fn::get_arg(unsigned index) const
{
    auto *B = X->get_builder();
    auto *fn_type = get_type();
    auto *arg_type = fn_type->get_param_type(index);
    llvm::Value *arg = get_fn()->getArg(user_args_offset(fn_type) + index);
    return arg_type->get_abi_class() == ABIClass::LARGE_STRUCT
        ? B->CreateLoad(*arg_type, arg)
        : arg;
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
