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

Unit::Unit(State &state, llvm::Value *value, Methods const *methods)
    : Value(state, value, state.get_context()->get_unit_type())
    , methods_(methods)
{
}

Bool::Bool(State &state, llvm::Value *value, Methods const *methods)
    : Value(state, value, state.get_context()->get_bool_type())
    , methods_(methods)
{
}

Char::Char(State &state, llvm::Value *value, Methods const *methods)
    : Value(state, value, state.get_context()->get_char_type())
    , methods_(methods)
{
}

Int::Int(State &state, llvm::Value *value, Methods const *methods)
    : Value(state, value, state.get_context()->get_int_type())
    , methods_(methods)
{
}

Float::Float(State &state, llvm::Value *value, Methods const *methods)
    : Value(state, value, state.get_context()->get_float_type())
    , methods_(methods)
{
}


Callable::Callable(State &state, llvm::Value *fn, llvm::Value *env, FnType *type)
    : Value(state, fn, type)
    , env_(env)
{
}

llvm::Value *Callable::get_value() const
{
    auto *X = state_->get_context();
    auto *B = X->get_builder();
    llvm::Value *object = llvm::UndefValue::get(X->get_callable_ty());
    object = B->CreateInsertValue(object, value_, 0);
    object = B->CreateInsertValue(object, env_, 1);
    return object;
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
    , value_(llvm::Function::Create(type->get_fn_ty(),
                linkage, name, *X.get_module()))
    , env_(X.create_null_ptr())
{
    setup_fn(X, get_fn(), type);
}

Fn::Fn(Context &X, llvm::Value *env, FnType *type)
    : X(&X)
    , type_(type)
    , value_(llvm::Function::Create(type->get_fn_ty(),
                llvm::Function::PrivateLinkage,
                "closure", *X.get_module()))
    , env_(env)
{
    setup_fn(X, get_fn(), type);
}

llvm::Value *Fn::get_value() const
{
    auto *B = X->get_builder();
    llvm::Value *object = llvm::UndefValue::get(X->get_callable_ty());
    object = B->CreateInsertValue(object, value_, 0);
    object = B->CreateInsertValue(object, env_, 1);
    return object;
}

Callable Fn::as_callable(State &state) const
{
    return Callable(state, value_, env_, type_);
}

std::string Fn::get_name() const
{
    return get_fn()->getName().str();
}

llvm::Value *Fn::get_env_ptr() const
{
    return get_fn()->getArg(env_pointer_offset(get_type()));
}

llvm::Value *Fn::get_arg(unsigned index) const
{
    auto *B = X->get_builder();
    auto *fn_type = get_type();
    auto *arg_type = fn_type->get_param_type(index);
    auto const offset = user_args_offset(fn_type);
    llvm::Value *arg = get_fn()->getArg(offset + index);
    return arg_type->get_abi_class() == ABIClass::LARGE_STRUCT
        ? B->CreateLoad(*arg_type, arg)
        : arg;
}

Str::Str(State &state, llvm::Value *str, Str::Methods const *methods)
    : Value(state, str, state.get_context()->get_str_type())
    , methods_(methods)
{
    auto *X = state.get_context();
    auto *B = X->get_builder();
    length_ptr_ = B->CreateStructGEP(X->get_str_ty(), str, 0);
    hash_ptr_ = B->CreateStructGEP(X->get_str_ty(), str, 1);
    text_ = B->CreateStructGEP(X->get_str_ty(), str, 2);
}

static llvm::Value *size_of_str(Context &X, llvm::Value *length)
{
    auto *extra_size = X.create_int((paw_Int)X.size_of(X.get_str_ty()) + 1);
    return X.create_ckd_iadd(extra_size, length);
}

static llvm::Value *create_empty_str(State &state, llvm::Value *length)
{
    auto *X = state.get_context();
    return X->create_alloc(size_of_str(*X, length));
}

Str::Str(State &state, llvm::Value *length, Str::Methods const *methods, Str::CreationTag)
    : Str(state, create_empty_str(state, length), methods)
{
    auto *X = state.get_context();
    auto *B = X->get_builder();

    B->CreateStore(length, get_length_ptr());

    // write the null terminator
    auto *end_ptr = get_element_ptr(length);
    B->CreateStore(X->create_char('\0'), end_ptr);
}

Str::Str(State &state, llvm::Value *text, llvm::Value *length, Methods const *methods, CreationTag tag)
    : Str(state, text, state.get_context()->create_hash_bytes(text, length), length, methods, tag)
{
}

Str::Str(State &state, llvm::Value *text, llvm::Value *hash, llvm::Value *length, Str::Methods const *methods, Str::CreationTag tag)
    : Str(state, length, methods, tag)
{
    auto *X = state.get_context();
    auto *B = X->get_builder();
    X->create_memcpy(get_text(), text, length);
    B->CreateStore(hash, hash_ptr_);
}

void Str::finalize()
{
    auto *X = state_->get_context();
    auto *B = X->get_builder();
    auto *hash = X->create_hash_bytes(text_, get_length());
    B->CreateStore(hash, hash_ptr_);
}

llvm::Value *Str::get_hash() const
{
    auto *X = state_->get_context();
    auto *B = X->get_builder();
    return B->CreateLoad(X->get_i32_ty(), hash_ptr_);
}

llvm::Value *Str::get_length() const
{
    auto *X = state_->get_context();
    auto *B = X->get_builder();
    return B->CreateLoad(X->get_int_ty(), length_ptr_);
}

llvm::Value *Str::get_element_ptr(llvm::Value *index)
{
    auto *X = state_->get_context();
    auto *B = X->get_builder();
    return B->CreateInBoundsGEP(X->get_char_ty(),
            text_, {index});
}


List::List(State &state, llvm::Value *list, ListType *type, Methods const *methods)
    : Value(state, list, type)
    , methods_(methods)
{
    auto *X = state_->get_context();
    auto *B = X->get_builder();
    data_ptr_ = B->CreateStructGEP(X->get_list_ty(), list, 0);
    length_ptr_ = B->CreateStructGEP(X->get_list_ty(), list, 1);
    capacity_ptr_ = B->CreateStructGEP(X->get_list_ty(), list, 2);
}

static llvm::Value *create_empty_list(State &state)
{
    auto *X = state.get_context();
    return X->create_alloc(X->get_list_ty());
}

List::List(State &state, llvm::Value *length, ListType *type, List::Methods const *methods, List::CreationTag)
    : List(state, create_empty_list(state), type, methods)
{
    auto *X = state_->get_context();
    auto *next_pow2 = X->create_next_pow2(length);
    auto *capacity = X->create_imax(next_pow2,
            X->create_int(ListType::MIN_CAPACITY));

    auto *data = new_buffer(capacity);
    set_data(data);
    set_length(length);
    set_capacity(capacity);
}

llvm::Value *List::get_data() const { return state_->get_context()->load_ptr(get_data_ptr()); }
llvm::Value *List::get_length() const { return state_->get_context()->load_int(get_length_ptr()); }
llvm::Value *List::get_capacity() const { return state_->get_context()->load_int(get_capacity_ptr()); }
void List::set_data(llvm::Value *value) const { state_->get_context()->store_value(value, get_data_ptr()); }
void List::set_length(llvm::Value *value) const { state_->get_context()->store_value(value, get_length_ptr()); }
void List::set_capacity(llvm::Value *value) const { state_->get_context()->store_value(value, get_capacity_ptr()); }

llvm::Value *List::get_element(llvm::Value *index)
{
    return state_->get_context()->load_value(*get_type()->element_type_, get_element_ptr(index));
}

void List::set_element(llvm::Value *index, llvm::Value *element)
{
    return state_->get_context()->store_value(element, get_element_ptr(index));
}

llvm::Value *List::new_buffer(llvm::Value *capacity)
{
    auto *X = state_->get_context();
    auto const element_size = (paw_Int)X->size_of(*get_type()->element_type_);
    auto *buffer_size = X->create_ckd_imul(capacity, X->create_int(element_size));
    return X->create_alloc(buffer_size);
}

llvm::Value *List::get_element_ptr(llvm::Value *index)
{
    auto *X = state_->get_context();
    auto *B = X->get_builder();
    auto *data = X->load_ptr(data_ptr_);
    return B->CreateInBoundsGEP(*get_type()->element_type_,
            data, {index});
}

llvm::Value *List::get_element_ptr(paw_Int index)
{
    auto *X = state_->get_context();
    return get_element_ptr(X->create_int(index));
}

void List::create_grow(llvm::Value *old_capacity)
{
    auto *X = state_->get_context();
    auto *B = X->get_builder();
    auto *element_size = X->create_int((paw_Int)X->size_of(
                *get_type()->element_type_));

    // new_capacity = old_capacity * 1.5
    auto *half_capacity = B->CreateSDiv(old_capacity, X->create_int(2));
    auto *new_capacity = X->create_ckd_iadd(old_capacity, half_capacity);
    auto *new_size = X->create_ckd_imul(new_capacity, element_size);
    // overflow check elided since "old_capacity < new_capacity"
    auto *old_size = B->CreateMul(old_capacity, element_size);

    auto *old_data = X->load_ptr(data_ptr_);
    auto *new_data = new_buffer(new_size);
    X->create_memcpy(new_data, old_data, old_size);

    set_data(new_data);
    set_capacity(new_capacity);
}

void List::generate_methods(Context &X, ListType *type, List::Methods &m)
{
    auto *element_type = type->get_element_type();
    auto *B = X.get_builder();
    auto *c = X.get_context();

    // generate "fn List<T>::push(self, value: T)"
    {
        auto *fn = m.push;
        State state(X, fn);

        auto *grow_block = llvm::BasicBlock::Create(*c, "grow", fn->get_fn());
        auto *push_block = llvm::BasicBlock::Create(*c, "push", fn->get_fn());

        List list(state, fn->get_arg(0), type, &m);
        auto *element = fn->get_arg(1);
        auto *length = list.get_length();
        auto *capacity = list.get_capacity();
        auto *has_space = B->CreateICmpSLT(length, capacity);
        B->CreateCondBr(has_space, push_block, grow_block);

        B->SetInsertPoint(grow_block);
        list.create_grow(capacity);
        B->CreateBr(push_block);

        B->SetInsertPoint(push_block);
        list.set_element(length, element);
        list.set_length(X.create_iadd1(length));
        state.create_return();
    }

    // generate "fn List<T>::pop(self) -> T"
    {
        auto *fn = m.pop;
        State state(X, fn);

        auto *error_block = llvm::BasicBlock::Create(*c, "error", fn->get_fn());
        auto *pop_block = llvm::BasicBlock::Create(*c, "pop", fn->get_fn());

        List list(state, fn->get_arg(0), type, &m);
        auto *length = list.get_length();
        auto *is_nonempty = B->CreateICmpSGT(length, X.create_int(0));
        B->CreateCondBr(is_nonempty, pop_block, error_block);

        B->SetInsertPoint(error_block);
        CG_PANIC_LITERAL(X, "pop from empty list");
        B->CreateUnreachable();

        B->SetInsertPoint(pop_block);
        auto *index = X.create_isub1(length);
        auto *element_ptr = list.get_element_ptr(index);
        auto *element = X.load_value(*element_type, element_ptr);
        list.set_length(index);
        state.create_return(element);
    }

    // generate "fn List<T>::insert(self, index: int, value: T)"
    {
        auto *fn = m.insert;
        State state(X, fn);

        auto *check_block = llvm::BasicBlock::Create(*c, "check", fn->get_fn());
        auto *grow_block = llvm::BasicBlock::Create(*c, "grow", fn->get_fn());
        auto *insert_block = llvm::BasicBlock::Create(*c, "insert", fn->get_fn());

        List list(state, fn->get_arg(0), type, &m);
        auto *index = fn->get_arg(1);
        auto *element = fn->get_arg(2);
        auto *length = list.get_length();
        auto *capacity = list.get_capacity();
        auto *in_bounds = B->CreateICmpSLT(length, capacity);
        B->CreateCondBr(in_bounds, insert_block, grow_block);

        B->SetInsertPoint(check_block);
        auto *has_space = B->CreateICmpSLT(length, capacity);
        B->CreateCondBr(has_space, insert_block, grow_block);

        B->SetInsertPoint(grow_block);
        list.create_grow(capacity);
        B->CreateBr(insert_block);

        B->SetInsertPoint(insert_block);
        auto *data = list.get_data();
        // move existing elements up by 1 and write the new element
        auto *difference = B->CreateSub(length, index);
        auto *src_ptr = X.create_array_gep(*element_type, data, index);
        auto *dest_ptr = X.create_array_gep(*element_type, data, X.create_iadd1(index));
        auto *element_size = X.create_int((paw_Int)X.size_of(*element_type));
        X.create_memmove(dest_ptr, src_ptr, B->CreateMul(difference, element_size));
        X.create_array_set(*element_type, data, index, element);
        list.set_length(X.create_iadd1(length));
        state.create_return();
    }

    // generate "fn List<T>::remove(self, index: int) -> T"
    {
        auto *fn = m.remove;
        State state(X, fn);

        auto *check_block = llvm::BasicBlock::Create(*c, "check", fn->get_fn());
        auto *error_block = llvm::BasicBlock::Create(*c, "error", fn->get_fn());
        auto *remove_block = llvm::BasicBlock::Create(*c, "remove", fn->get_fn());

        List list(state, fn->get_arg(0), type, &m);
        auto *index = fn->get_arg(1);
        auto *length = list.get_length();
        auto *first_cond = B->CreateICmpSGT(length, X.create_int(0));
        B->CreateCondBr(first_cond, check_block, error_block);

        B->SetInsertPoint(check_block);
        auto *second_cond = B->CreateICmpSGT(length, X.create_int(0));
        B->CreateCondBr(second_cond, remove_block, error_block);

        B->SetInsertPoint(error_block);
        CG_PANIC_LITERAL(X, "out-of-bounds list access");
        B->CreateUnreachable();

        B->SetInsertPoint(remove_block);
        auto *element_ptr = list.get_element_ptr(index);
        auto *element = B->CreateLoad(*element_type, element_ptr);
        // move elements after the removed element to the left by 1, i.e. call
        // "memcpy(ptr, ptr + 1, (length - index - 1) * sizeof(element_type))"
        auto *difference = B->CreateSub(B->CreateSub(length, index), X.create_int(1));
        auto *element_size = X.create_int(paw_Int(X.size_of(*element_type)));
        X.create_memmove(element_ptr,
                list.get_element_ptr(X.create_iadd1(index)),
                B->CreateMul(difference, element_size));
        list.set_length(X.create_isub1(length));
        state.create_return(element);
    }
}


static const int MAP_FILL_FACTOR = 4;

Map::Map(State &state, llvm::Value *map, MapType *type, Map::Methods const *methods)
    : Value(state, map, type)
    , methods_(methods)
{
    auto *X = state.get_context();
    auto *B = X->get_builder();
    data_ptr_ = B->CreateStructGEP(X->get_map_ty(), map, 0);
    length_ptr_ = B->CreateStructGEP(X->get_map_ty(), map, 1);
    capacity_ptr_ = B->CreateStructGEP(X->get_map_ty(), map, 2);
}

static llvm::Value *create_empty_map(State &state)
{
    auto *X = state.get_context();
    return X->create_alloc(X->get_map_ty());
}

Map::Map(State &state, llvm::Value *length_hint, MapType *type, Map::Methods const *methods, Map::CreationTag tag)
    : Map(state, create_empty_map(state), type, methods)
{
    auto *X = state.get_context();
    auto const max_align = std::max(
            X->align_of(*type->get_key_type()),
            X->align_of(*type->get_value_type()));

    // "capacity" is a power of 2 greater than or equal to the maximum alignment
    // needed by a key or value. This makes it so that if the start of the map
    // is aligned properly (guaranteed by malloc), then both the start of the key
    // and value sections will also be aligned properly.
    auto *capacity = X->create_imax(
            X->create_next_pow2(length_hint),
            X->create_int((paw_Int)max_align.value()));

    auto *data = new_buffer(capacity);
    set_capacity(capacity);
    set_data(data);
}

llvm::Value *Map::get_data() const { return state_->get_context()->load_ptr(get_data_ptr()); }
llvm::Value *Map::get_length() const { return state_->get_context()->load_int(get_length_ptr()); }
llvm::Value *Map::get_capacity() const { return state_->get_context()->load_int(get_capacity_ptr()); }
void Map::set_data(llvm::Value *value) const { state_->get_context()->store_value(value, get_data_ptr()); }
void Map::set_length(llvm::Value *value) const { state_->get_context()->store_value(value, get_length_ptr()); }
void Map::set_capacity(llvm::Value *value) const { state_->get_context()->store_value(value, get_capacity_ptr()); }

// "capacity" must be a power of 2 greater than 0
static llvm::Value *clamp_to_capacity(Context &X, llvm::Value *value, llvm::Value *capacity)
{
    auto *B = X.get_builder();
    auto *mask = B->CreateSub(capacity, X.create_int(1));
    return B->CreateAnd(value, mask);
}

llvm::Value *Map::first_index(llvm::Value *key, llvm::Value *capacity)
{
    auto *X = state_->get_context();
    auto *hash_fn = methods_->key_hash;
    auto *hash = state_->create_call(hash_fn->as_callable(*state_), key);
    return clamp_to_capacity(*X, hash, capacity);
}

llvm::Value *Map::next_index(llvm::Value *index, llvm::Value *capacity)
{
    auto *X = state_->get_context();
    auto *B = X->get_builder();
    auto *next = B->CreateAdd(index, X->create_int(1));
    return clamp_to_capacity(*X, next, capacity);
}

Map::Components Map::unpack()
{
    auto *X = state_->get_context();
    auto *B = X->get_builder();
    auto *capacity = get_capacity();
    auto *flags = B->CreateLoad(X->get_ptr_ty(), data_ptr_);
    auto *keys = B->CreateInBoundsGEP(X->get_char_ty(), flags, {capacity});
    auto *values = B->CreateInBoundsGEP(*get_type()->get_key_type(), keys, {capacity});

    return {
        .flags = flags,
        .keys = keys,
        .values = values,
    };
}

llvm::Value *Map::lookup(llvm::Value *key)
{
    auto *fn = methods_->lookup.value.get();
    return state_->create_call(fn->as_callable(*state_), {*this, key});
}

llvm::Value *Map::access(llvm::Value *key)
{
    auto *fn = methods_->access.value.get();
    return state_->create_call(fn->as_callable(*state_), {*this, key});
}

llvm::Value *Map::new_buffer(llvm::Value *capacity)
{
    auto *X = state_->get_context();
    auto *B = X->get_builder();
    auto const key_stride = (paw_Int)X->stride_of(*get_type()->get_key_type());
    auto const value_stride = (paw_Int)X->stride_of(*get_type()->get_value_type());
    auto *item_size = X->create_int(1 + key_stride + value_stride);
    auto *buffer_size = B->CreateMul(capacity, item_size);
    auto *buffer = X->create_alloc(buffer_size);
    // clear the "flags" region of the buffer
    X->create_memset(buffer, X->create_i8(0), capacity);
    return buffer;
}

llvm::Value *Map::get_element_ptr(llvm::Value *key)
{
    auto *fn = methods_->gep.value.get();
    return state_->create_call(fn->as_callable(*state_), {*this, key});
}

llvm::Value *Map::new_element_ptr(llvm::Value *key)
{
    auto *fn = methods_->nep.value.get();
    return state_->create_call(fn->as_callable(*state_), {*this, key});
}

llvm::Value *Map::get_element(llvm::Value *key)
{
    auto *X = state_->get_context();
    return X->load_value(
            *get_type()->get_value_type(),
            get_element_ptr(key));
}

void Map::set_element(llvm::Value *key, llvm::Value *value)
{
    auto *X = state_->get_context();
    X->store_value(value, new_element_ptr(key));
}

void Map::generate_methods(Context &X, MapType *type, Map::Methods &methods)
{
    auto *key_type = type->get_key_type();
    auto *value_type = type->get_value_type();
    auto *B = X.get_builder();
    auto *c = X.get_context();

    auto const create_value_info = [&X, B](llvm::Value *flag_ptr, llvm::Value *value_ptr) {
        llvm::Value *result = llvm::UndefValue::get(
                llvm::StructType::get(*X.get_context(), {
                    X.get_ptr_ty(),
                    X.get_ptr_ty(),
                }, false));
        result = B->CreateInsertValue(result, flag_ptr, 0);
        result = B->CreateInsertValue(result, value_ptr, 1);
        return result;
    };
    auto const get_flag_ptr = [B](llvm::Value *info) {
        return B->CreateExtractValue(info, 0);
    };
    auto const get_value_ptr = [B](llvm::Value *info) {
        return B->CreateExtractValue(info, 1);
    };

    // generate "fn Map<K, V>::lookup(self, key: K) -> (*i8, *V)"
    {
        auto *fn = methods.lookup.value.get();
        State state(X, fn);

        auto *entry_block = state.get_entry();
        auto *loop_header_block = llvm::BasicBlock::Create(*c, "loop_header", fn->get_fn());
        auto *loop_footer_block = llvm::BasicBlock::Create(*c, "loop_footer", fn->get_fn());
        auto *found_exists_block = llvm::BasicBlock::Create(*c, "found_exists", fn->get_fn());
        auto *found_equals_block = llvm::BasicBlock::Create(*c, "found_equals", fn->get_fn());
        auto *exit_block = llvm::BasicBlock::Create(*c, "exit", fn->get_fn());

        Map map(state, fn->get_arg(0), type, &methods);
        auto *search_key = fn->get_arg(1);
        auto *capacity = map.get_capacity();
        auto *index0 = map.first_index(search_key, capacity);
        auto *rflag0 = X.create_null_ptr();
        auto *rvalue0 = X.create_null_ptr();
        auto const m = map.unpack();
        B->CreateBr(loop_header_block);

        B->SetInsertPoint(loop_header_block);
        auto *index = B->CreatePHI(X.get_int_ty(), 2);
        index->addIncoming(index0, entry_block);
        // switch on the value of the flag
        auto *flag_ptr = B->CreateInBoundsGEP(X.get_i8_ty(), m.flags, {index});
        auto *flag = B->CreateLoad(X.get_i8_ty(), flag_ptr);
        auto *switch_flag = B->CreateSwitch(flag, found_exists_block, 2);
        switch_flag->addCase(X.create_i8(int8_t(Flag::VACANT)), exit_block);
        switch_flag->addCase(X.create_i8(int8_t(Flag::ERASED)), loop_footer_block);

        // check if the key at "index" matches the "search_key"
        B->SetInsertPoint(found_exists_block);
        auto *key_ptr = B->CreateInBoundsGEP(*key_type, m.keys, {index});
        auto *key = B->CreateLoad(*key_type, key_ptr);
        auto *key_equals = state.create_call(methods.key_eq->as_callable(state), {search_key, key});
        B->CreateBr(loop_footer_block);

        B->SetInsertPoint(loop_footer_block);
        auto *key_found = B->CreatePHI(X.get_bool_ty(), 2);
        key_found->addIncoming(X.create_bool(false), loop_header_block);
        key_found->addIncoming(key_equals, found_exists_block);
        auto *index1 = map.next_index(index, capacity);
        index->addIncoming(index1, loop_footer_block);
        B->CreateCondBr(key_found, found_equals_block, loop_header_block);

        B->SetInsertPoint(found_equals_block);
        auto *rflag1 = B->CreateInBoundsGEP(X.get_i8_ty(), m.flags, {index});
        auto *rvalue1 = B->CreateInBoundsGEP(*value_type, m.values, {index});
        B->CreateBr(exit_block);

        B->SetInsertPoint(exit_block);
        auto *rflag = B->CreatePHI(X.get_ptr_ty(), 2);
        rflag->addIncoming(rflag0, loop_header_block);
        rflag->addIncoming(rflag1, found_equals_block);
        auto *rvalue = B->CreatePHI(X.get_ptr_ty(), 2);
        rvalue->addIncoming(rvalue0, loop_header_block);
        rvalue->addIncoming(rvalue1, found_equals_block);
        state.create_return(create_value_info(rflag, rvalue));
    }

    // generate "fn Map<K, V>::access(self, key: K) -> (*i8, *V)"
    {
        auto *fn = methods.access.value.get();
        State state(X, fn);

        auto *entry_block = state.get_entry();
        auto *loop_header_block = llvm::BasicBlock::Create(*c, "loop_header", fn->get_fn());
        auto *loop_footer_block = llvm::BasicBlock::Create(*c, "loop_footer", fn->get_fn());
        auto *found_erased_block = llvm::BasicBlock::Create(*c, "found_erased", fn->get_fn());
        auto *found_exists_block = llvm::BasicBlock::Create(*c, "found_exists", fn->get_fn());
        auto *found_vacant_block = llvm::BasicBlock::Create(*c, "found_vacant", fn->get_fn());
        auto *exit_block = llvm::BasicBlock::Create(*c, "exit", fn->get_fn());

        Map map(state, fn->get_arg(0), type, &methods);
        auto *search_key = fn->get_arg(1);
        auto *capacity = map.get_capacity();
        auto *index0 = map.first_index(search_key, capacity);
        auto *erased0 = X.create_int(-1);
        auto const m = map.unpack();
        B->CreateBr(loop_header_block);

        B->SetInsertPoint(loop_header_block);
        auto *index = B->CreatePHI(X.get_int_ty(), 2);
        auto *erased = B->CreatePHI(X.get_int_ty(), 2);
        index->addIncoming(index0, entry_block);
        erased->addIncoming(erased0, entry_block);

        // check if a key exists or was erased at the given "index"
        auto *flag_ptr = B->CreateInBoundsGEP(X.get_i8_ty(), m.flags, {index});
        auto *flag = B->CreateLoad(X.get_i8_ty(), flag_ptr);
        auto *switch_flag = B->CreateSwitch(flag, found_exists_block, 2);
        switch_flag->addCase(X.create_i8((int8_t)Flag::VACANT), found_vacant_block);
        switch_flag->addCase(X.create_i8((int8_t)Flag::ERASED), found_erased_block);

        B->SetInsertPoint(found_erased_block);
        auto *is_first_erased = B->CreateICmpEQ(erased, X.create_int(-1));
        auto *erased1 = B->CreateSelect(is_first_erased, index, erased);
        B->CreateBr(loop_footer_block);

        B->SetInsertPoint(found_exists_block);
        auto *key_ptr = B->CreateInBoundsGEP(*key_type, m.keys, {index});
        auto *key = B->CreateLoad(*key_type, key_ptr);
        auto *key_equals = state.create_call(methods.key_eq->as_callable(state), {search_key, key});
        B->CreateCondBr(key_equals, exit_block, loop_footer_block);

        B->SetInsertPoint(loop_footer_block);
        auto *erased2 = B->CreatePHI(X.get_int_ty(), 2);
        erased2->addIncoming(erased, found_exists_block);
        erased2->addIncoming(erased1, found_erased_block);
        erased->addIncoming(erased2, loop_footer_block);
        auto *index1 = map.next_index(index, capacity);
        index->addIncoming(index1, loop_footer_block);
        B->CreateBr(loop_header_block);

        // add a new key-value pair to the map (falls through to "exit_block"
        // to handle emitting the pointer-to-value)
        B->SetInsertPoint(found_vacant_block);
        auto *not_erased = B->CreateICmpEQ(erased, X.create_int(-1));
        auto *insert_loc = B->CreateSelect(not_erased, index, erased);
        // m.flags[insert_loc] = Flag::EXISTS
        flag_ptr = B->CreateInBoundsGEP(X.get_i8_ty(), m.flags, {insert_loc});
        B->CreateStore(X.create_i8(int8_t(Flag::EXISTS)), flag_ptr);
        // m.keys[insert_loc] = key
        key_ptr = B->CreateInBoundsGEP(*key_type, m.keys, {insert_loc});
        B->CreateStore(search_key, key_ptr);
        map.set_length(X.create_iadd1(map.get_length()));
        B->CreateBr(exit_block);

        // set the value of an existing key-value pair
        B->SetInsertPoint(exit_block);
        // return (&m.flags[write_loc], &m.values[write_loc])
        auto *write_loc = B->CreatePHI(X.get_int_ty(), 2);
        write_loc->addIncoming(index, found_exists_block);
        write_loc->addIncoming(insert_loc, found_vacant_block);
        auto *rflag = B->CreateInBoundsGEP(X.get_i8_ty(), m.flags, {write_loc});
        auto *rvalue = B->CreateInBoundsGEP(*value_type, m.values, {write_loc});
        state.create_return(create_value_info(rflag, rvalue));
    }

    // generate "fn Map<K, V>::gep1(self, key: K) -> (*i8, *V)"
    {
        auto *fn = methods.gep1.value.get();
        State state(X, fn);

        auto *error_block = llvm::BasicBlock::Create(*c, "missing", fn->get_fn());
        auto *exit_block = llvm::BasicBlock::Create(*c, "exit", fn->get_fn());

        Map map(state, fn->get_arg(0), type, &methods);
        auto *value_ptr = map.get_element_ptr(fn->get_arg(1));
        auto *is_nonnull = B->CreateICmpNE(value_ptr, X.create_null_ptr());
        B->CreateCondBr(is_nonnull, exit_block, error_block);

        B->SetInsertPoint(error_block);
        CG_PANIC_LITERAL(X, "missing key in map");
        B->CreateUnreachable();

        B->SetInsertPoint(exit_block);
        state.create_return(value_ptr);
    }

    // generate "fn Map<K, V>::grow(self)"
    {
        auto *fn = methods.grow.value.get();
        State state(X, fn);

        auto *entry_block = state.get_entry();
        auto *loop_header_block = llvm::BasicBlock::Create(*c, "loop_header", fn->get_fn());
        auto *loop_body_block = llvm::BasicBlock::Create(*c, "loop_body", fn->get_fn());
        auto *found_element_block = llvm::BasicBlock::Create(*c, "found_element", fn->get_fn());
        auto *loop_footer_block = llvm::BasicBlock::Create(*c, "loop_footer", fn->get_fn());
        auto *exit_block = llvm::BasicBlock::Create(*c, "exit", fn->get_fn());

        Map map(state, fn->get_arg(0), type, &methods);
        auto const rehash = map.unpack();

        auto *old_capacity = map.get_capacity();
        auto *new_capacity = X.create_ckd_imul(old_capacity, X.create_int(2));
        auto *new_data = map.new_buffer(new_capacity);
        map.set_capacity(new_capacity);
        map.set_data(new_data);

        auto *index0 = X.create_int(0);
        B->CreateBr(loop_header_block);

        B->SetInsertPoint(loop_header_block);
        auto *index = B->CreatePHI(X.get_int_ty(), 2);
        index->addIncoming(index0, entry_block);
        auto *should_continue = B->CreateICmpSLT(index, old_capacity);
        B->CreateCondBr(should_continue, loop_body_block, exit_block);

        B->SetInsertPoint(loop_body_block);
        auto *flag = X.create_array_get(X.get_i8_ty(), rehash.flags, index);
        auto *element_exists = B->CreateICmpEQ(flag, X.create_i8(int8_t(Flag::EXISTS)));
        B->CreateCondBr(element_exists, found_element_block, loop_footer_block);

        B->SetInsertPoint(found_element_block);
        auto *key = X.create_array_get(*key_type, rehash.keys, index);
        auto *value = X.create_array_get(*value_type, rehash.values, index);
        auto *value_info = map.access(key);
        auto *value_ptr = get_value_ptr(value_info);
        B->CreateStore(value, value_ptr);
        B->CreateBr(loop_footer_block);

        B->SetInsertPoint(loop_footer_block);
        auto *index1 = B->CreateAdd(index, X.create_int(1));
        index->addIncoming(index1, loop_footer_block);
        B->CreateBr(loop_header_block);

        B->SetInsertPoint(exit_block);
        B->CreateRetVoid();
    }

    // generate "fn Map<K, V>::get(self, key: K) -> Option<V>"
    {
        auto *fn = methods.get;
        State state(X, fn);

        auto *some_block = llvm::BasicBlock::Create(*c, "some", fn->get_fn());
        auto *none_block = llvm::BasicBlock::Create(*c, "none", fn->get_fn());
        auto *exit_block = llvm::BasicBlock::Create(*c, "exit", fn->get_fn());

        // TODO: elide the alloca, use more PHI nodes
        auto *return_type = fn->get_type()->get_return_type();
        auto *return_ptr = B->CreateAlloca(*return_type);
        auto *discr_ptr = B->CreateStructGEP(*return_type, return_ptr, 0);
        auto *result_ptr = B->CreateStructGEP(*return_type, return_ptr, 1);

        auto *object = fn->get_arg(0);
        auto *key = fn->get_arg(1);
        Map map(state, object, type, &methods);

        auto *value_ptr = map.get_element_ptr(key);
        auto *is_nonnull = B->CreateICmpNE(value_ptr, X.create_null_ptr());
        B->CreateCondBr(is_nonnull, some_block, none_block);

        B->SetInsertPoint(some_block);
        auto *some = X.create_int(PAW_OPTION_SOME);
        auto *value = B->CreateLoad(*value_type, value_ptr);
        B->CreateStore(value, result_ptr);
        B->CreateBr(exit_block);

        B->SetInsertPoint(none_block);
        auto *none = X.create_int(PAW_OPTION_NONE);
        B->CreateBr(exit_block);

        B->SetInsertPoint(exit_block);
        auto *discr = B->CreatePHI(X.get_int_ty(), 2);
        discr->addIncoming(some, some_block);
        discr->addIncoming(none, none_block);
        B->CreateStore(discr, discr_ptr);
        auto *result = B->CreateLoad(*return_type, return_ptr);
        state.create_return(result);
    }

    // generate "fn Map<K, V>::remove(self, key: K)"
    {
        auto *fn = methods.remove;
        State state(X, fn);

        auto *found_block = llvm::BasicBlock::Create(*c, "found", fn->get_fn());
        auto *exit_block = llvm::BasicBlock::Create(*c, "exit", fn->get_fn());

        auto *object = fn->get_arg(0);
        auto *key = fn->get_arg(1);

        Map map(state, object, type, &methods);
        auto *value_info = map.lookup(key);
        auto *flag_ptr = get_flag_ptr(value_info);
        auto *is_nonnull = B->CreateICmpNE(flag_ptr, X.create_null_ptr());
        B->CreateCondBr(is_nonnull, found_block, exit_block);

        B->SetInsertPoint(found_block);
        B->CreateStore(X.create_i8(int8_t(Map::Flag::ERASED)), flag_ptr);
        map.set_length(X.create_isub1(map.get_length()));
        B->CreateBr(exit_block);

        B->SetInsertPoint(exit_block);
        B->CreateRetVoid();
    }

    // generate "fn Map<K, V>::gep(self, key: K) -> *V"
    {
        auto *fn = methods.gep.value.get();
        State state(X, fn);

        auto *object = fn->get_arg(0);
        auto *key = fn->get_arg(1);

        Map map(state, object, type, &methods);
        auto *result = map.lookup(key);
        state.create_return(get_value_ptr(result));
    }

    // generate "fn Map<K, V>::nep(self, key: K) -> *V"
    {
        auto *fn = methods.nep.value.get();
        State state(X, fn);

        auto *grow_block = llvm::BasicBlock::Create(*c, "grow", fn->get_fn());
        auto *access_block = llvm::BasicBlock::Create(*c, "access", fn->get_fn());

        auto *object = fn->get_arg(0);
        auto *key = fn->get_arg(1);

        Map map(state, object, type, &methods);
        auto *limit = B->CreateSDiv(map.get_capacity(),
                X.create_int(MAP_FILL_FACTOR));
        auto *has_space = B->CreateICmpSLT(map.get_length(), limit);
        B->CreateCondBr(has_space, access_block, grow_block);

        B->SetInsertPoint(grow_block);
        state.create_call(methods.grow.value->as_callable(state), object);
        B->CreateBr(access_block);

        B->SetInsertPoint(access_block);
        auto *result = map.access(key);
        state.create_return(get_value_ptr(result));
    }

    // generate "fn MapIterator<K, V>::next(self) -> Option<K>"
    {
        auto *fn = methods.iterator_next;
        State state(X, fn);

        auto *loop_header_block = llvm::BasicBlock::Create(*c, "loop_header", fn->get_fn());
        auto *loop_body_block = llvm::BasicBlock::Create(*c, "loop_body", fn->get_fn());
        auto *some_block = llvm::BasicBlock::Create(*c, "some", fn->get_fn());
        auto *none_block = llvm::BasicBlock::Create(*c, "none", fn->get_fn());
        auto *exit_block = llvm::BasicBlock::Create(*c, "exit", fn->get_fn());

        auto *return_type = fn->get_type()->get_return_type();
        auto *return_ptr = B->CreateAlloca(*return_type);
        auto *discr_ptr = B->CreateStructGEP(*return_type, return_ptr, 0);
        auto *result_ptr = B->CreateStructGEP(*return_type, return_ptr, 1);

        Object iterator(state, fn->get_arg(0), methods.iterator_type);
        auto *object_ptr = iterator.get_field_ptr(Discriminant::base(), 0);
        auto *index_ptr = iterator.get_field_ptr(Discriminant::base(), 1);
        auto *object = B->CreateLoad(X.get_ptr_ty(), object_ptr);
        Map map(state, object, type, &methods);
        auto *capacity = map.get_capacity();
        auto const m = map.unpack();
        B->CreateBr(loop_header_block);

        B->SetInsertPoint(loop_header_block);
        auto *index = X.load_int(index_ptr);
        auto *should_continue = B->CreateICmpSLT(index, capacity);
        B->CreateCondBr(should_continue, loop_body_block, none_block);

        B->SetInsertPoint(loop_body_block);
        auto *index1 = X.create_iadd1(index);
        B->CreateStore(index1, index_ptr);
        auto *flag = X.create_array_get(X.get_i8_ty(), m.flags, index);
        auto *element_exists = B->CreateICmpEQ(flag, X.create_i8(int8_t(Flag::EXISTS)));
        B->CreateCondBr(element_exists, some_block, loop_header_block);

        B->SetInsertPoint(none_block);
        auto *discr0 = X.create_int(PAW_OPTION_NONE);
        B->CreateStore(discr0, discr_ptr);
        B->CreateBr(exit_block);

        B->SetInsertPoint(some_block);
        auto *discr1 = X.create_int(PAW_OPTION_SOME);
        auto *key1 = X.create_array_get(*key_type, m.keys, index);
        B->CreateStore(discr1, discr_ptr);
        B->CreateStore(key1, result_ptr);
        B->CreateBr(exit_block);

        B->SetInsertPoint(exit_block);
        auto *result = B->CreateLoad(*return_type, return_ptr);
        state.create_return(result);
    }
}

Object::Object(State &state, llvm::Value *object, ObjectType *type)
    : Value(state, object, type)
{
}

static llvm::Value *create_object(State &state, ObjectType const *type)
{
    auto *X = state.get_context();
    return type->is_inline()
        ? state.get_scratch(type->get_struct_ty())
        : X->create_alloc(type->get_struct_ty());
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
