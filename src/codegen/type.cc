// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "abi.h"
#include "context.h"

// TODO: remove DEFERRED_INIT thing, not necessary now that reference types don't exist
#define DEFERRED_INIT ((llvm::Type *)42)

namespace paw::cg {

namespace {

unsigned long static constexpr const HASH_SEED = 0x9E3779B97F4A7C15ULL;

// Splitmix64 PRNG
template<class T>
T splitmix64(T &state)
{
    state += HASH_SEED;

    auto t = state;
    t = (t ^ (t >> 30)) * 0xBF58476D1CE4E5B9ULL;
    t = (t ^ (t >> 27)) * 0x94D049BB133111EBULL;
    return t ^ (t >> 31);
}

template<class T>
T hash_combine(T x, T y) {
    T t = x ^ y;
    return splitmix64(t);
}

} // (anonymous namespace)

unsigned Type::get_alignment() const
{
    return X->align_of(get_ty()).value();
}

uint64_t Type::get_size() const
{
    return X->size_of(get_ty());
}

uint64_t Type::get_bitsize() const
{
    return X->bitsize_of(get_ty());
}

bool Type::is_signed_int() const
{
    return is_int_type() && ((IntType *)this)->is_signed();
}


UnitType::UnitType(Context &X)
    : PrimitiveType(X, X.get_unit_ty(), Kind::UNIT)
{
}


BoolType::BoolType(Context &X)
    : PrimitiveType(X, X.get_i1_ty(), Kind::BOOL)
{
}


CharType::CharType(Context &X)
    : PrimitiveType(X, X.get_i8_ty(), Kind::CHAR)
{
}


namespace {

llvm::Type *int_kind_to_llvm_type(Context &X, IntKind kind)
{
    switch (kind) {
        case IntKind::INT8:
        case IntKind::UINT8:
            return X.get_i8_ty();
        case IntKind::INT16:
        case IntKind::UINT16:
            return X.get_i16_ty();
        case IntKind::INT32:
        case IntKind::UINT32:
            return X.get_i32_ty();
        case IntKind::INT64:
        case IntKind::UINT64:
            return X.get_i64_ty();
        case IntKind::ISIZE:
        case IntKind::USIZE:
            return X.get_isize_ty();
    }
}

llvm::Type *float_kind_to_llvm_type(Context &X, FloatKind kind)
{
    switch (kind) {
        case FloatKind::FLOAT32:
            return X.get_f32_ty();
        case FloatKind::FLOAT64:
            return X.get_f64_ty();
    }
}

} // namespace


IntType::IntType(Context &X, IntKind kind)
    : PrimitiveType(X, int_kind_to_llvm_type(X, kind), Kind::INT)
    , ikind_(kind)
{
}

unsigned long IntType::hash() const
{
    return hash_combine((unsigned long)ikind_,
            hash_combine((unsigned long)get_kind(), HASH_SEED));
}

bool IntType::equals(Type const *rhs) const
{
    return rhs->is_int_type() && ikind_ == static_cast<IntType const *>(rhs)->ikind_;
}


FloatType::FloatType(Context &X, FloatKind kind)
    : PrimitiveType(X, float_kind_to_llvm_type(X, kind), Kind::FLOAT)
    , fkind_(kind)
{
}

unsigned long FloatType::hash() const
{
    return hash_combine((unsigned long)fkind_,
            hash_combine((unsigned long)get_kind(), HASH_SEED));
}

bool FloatType::equals(Type const *rhs) const
{
    return rhs->is_float_type() && fkind_ == static_cast<FloatType const *>(rhs)->fkind_;
}



SliceType::SliceType(Context &X, Type *element_type)
    : Type(X, X.get_slice_ty(), Kind::SLICE)
    , element_type_(element_type)
{
}

llvm::StructType *SliceType::get_struct_ty() const
{
    return X->get_slice_ty();
}

unsigned long SliceType::hash() const
{
    auto const h = hash_combine((unsigned long)get_kind(), HASH_SEED);
    return hash_combine(h, element_type_->hash());
}

bool SliceType::equals(Type const *rhs) const
{
    return rhs->is_slice_type()
        && element_type_->equals(
                ((SliceType *)rhs)->element_type_);
}


StrType::StrType(Context &X)
    : Type(X, X.get_str_ty(), Kind::STR)
{
}

llvm::StructType *StrType::get_struct_ty() const
{
    return X->get_str_ty();
}

unsigned long StrType::hash() const
{
    return hash_combine((unsigned long)get_kind(), HASH_SEED);
}

bool StrType::equals(Type const *rhs) const
{
    return rhs->is_str_type();
}


unsigned long PrimitiveType::hash() const
{
    return hash_combine((unsigned long)get_kind(), HASH_SEED);
}

bool PrimitiveType::equals(Type const *rhs) const
{
    return get_kind() == rhs->get_kind();
}


static void add_abi_params(Context &X, Type *type, std::vector<llvm::Type *> &out)
{
    auto const info = get_abi_info(X, *type);
    if (info.kind == AbiInfo::Kind::EXPAND) {
        auto *struct_ty = llvm::cast<llvm::StructType>(info.param_ty);
        for (auto const e: struct_ty->elements())
            out.push_back(e);
    } else if (info.kind != AbiInfo::Kind::EMPTY) {
        out.push_back(info.param_ty);
    }
}

FnType::FnType(Context &X, Type *return_type,
        llvm::ArrayRef<Type *> param_types,
        Type *env_type, bool never_returns)
    : Type(X, DEFERRED_INIT, Kind::FN)
    , return_kind_(ReturnKind::NORMAL)
    , param_types_(param_types)
    , return_type_(return_type)
    , env_type_(env_type)
    , never_returns_(never_returns)
{
    std::vector<llvm::Type *> param_tys;
    param_tys.reserve(!!env_type + param_types.size());

    if (env_type != nullptr)
        add_abi_params(X, env_type, param_tys);

    for (auto *type: param_types)
        add_abi_params(X, type, param_tys);

    auto const return_info = get_abi_info(X, *return_type);
    auto *return_ty = return_info.return_ty;
    switch (return_info.kind) {
        case AbiInfo::Kind::EMPTY:
            return_kind_ = ReturnKind::VOID;
            break;
        case AbiInfo::Kind::VALUE:
        case AbiInfo::Kind::EXPAND:
            return_kind_ = ReturnKind::NORMAL;
            break;
        case AbiInfo::Kind::MEMORY:
            return_kind_ = ReturnKind::SRET;
            // add return pointer parameter
            param_tys.insert(begin(param_tys), return_info.param_ty);
            break;
    }

    ty_ = llvm::FunctionType::get(return_ty, param_tys, false);
}

llvm::Type *FnType::get_ty() const
{
    return X->get_ptr_ty();
}

unsigned long FnType::hash() const
{
    auto h = hash_combine((unsigned long)get_kind(), HASH_SEED);
    for (auto i = 0U; i < get_num_params(); ++i) {
        auto *param_type = get_param_type(i);
        h = hash_combine(h, param_type->hash());
    }
    return hash_combine(h, get_return_type()->hash());
}

static bool are_types_equal(llvm::ArrayRef<Type *> lhs, llvm::ArrayRef<Type *> rhs)
{
    if (lhs.size() != rhs.size())
        return false;

    for (auto i = 0U; i < lhs.size(); ++i) {
        if (lhs[i] != rhs[i])
            return false;
    }

    return true;
}

bool FnType::equals(Type const *rhs) const
{
    if (!rhs->is_fn_type()) return false;

    auto *fn = (FnType *)rhs;
    if (return_kind_ != fn->return_kind_) return false;
    if (never_returns_ != fn->never_returns_) return false;
    if (!env_type_ != !fn->env_type_) return false;
    if (env_type_ != nullptr && !env_type_->equals(fn->env_type_)) return false;

    if (return_type_ != fn->return_type_) return false;
    return are_types_equal(param_types_, fn->param_types_);
}

std::string FnType::to_string() const
{
    std::string string;
    llvm::raw_string_ostream rso(string);
    ty_->print(rso);
    return string;
}


PtrType::PtrType(Context &X, Type *pointee_type)
    : Type(X, X.get_ptr_ty(), Kind::PTR)
    , pointee_type_(pointee_type)
{
}

unsigned long PtrType::hash() const
{
    auto h = hash_combine((unsigned long)get_kind(), HASH_SEED);
    if (pointee_type_ == nullptr) return h;
    return hash_combine(h, pointee_type_->hash());
}

bool PtrType::equals(Type const *rhs) const
{
    return rhs->is_ptr_type()
        && pointee_type_ == ((PtrType *)rhs)->pointee_type_;
}

std::string PtrType::to_string() const
{
    return "*" + (pointee_type_ != nullptr ? pointee_type_->to_string() : "");
}


ArrayType::ArrayType(Context &X, Type *element_type, uint64_t length)
    : Type(X, X.get_array_ty(element_type->get_ty(), length), Kind::ARRAY)
    , element_type_(element_type)
    , length_(length)
{
}

unsigned long ArrayType::hash() const
{
    auto h = hash_combine((unsigned long)get_kind(), HASH_SEED);
    h = hash_combine(h, element_type_->hash());
    return hash_combine(h, (unsigned long)length_);
}

bool ArrayType::equals(Type const *rhs) const
{
    if (rhs->get_kind() == Kind::ARRAY) {
        auto const *r = (ArrayType *)rhs;
        return element_type_->equals(r->get_element_type())
            && length_ == r->get_length();
    }
    return false;
}

std::string ArrayType::to_string() const
{
    return "[" + std::to_string(length_) + "]"
        + element_type_->to_string();
}


ObjectType::ObjectType(Context &X, llvm::ArrayRef<ObjectType::FieldTypes> variants, std::string name)
    : Type(X, DEFERRED_INIT, Type::Kind::OBJECT)
    , min_alignment_(1)
    , name_(std::move(name))
{
    set_variants(variants);
}


static llvm::Type *create_underlying_ty(Context &X, size_t size, unsigned alignment)
{
    if (alignment == 8)
        return X.get_array_ty(X.get_i64_ty(), size / 8);
    if (alignment == 4)
        return X.get_array_ty(X.get_i32_ty(), size / 4);
    if (alignment == 2)
        return X.get_array_ty(X.get_i16_ty(), size / 2);
    return X.get_array_ty(X.get_i8_ty(), size);
}

void ObjectType::set_variants(llvm::ArrayRef<ObjectType::FieldTypes> variants)
{
    auto *c = X->get_context();
    variants_.resize(variants.size());

    struct {
        uint64_t size = 0;
        std::vector<llvm::Type *> field_tys;
        llvm::Type *ty;
    } largest_variant;
    unsigned strictest_alignment = 1;

    paw_assert(!variants.empty());
    for (auto i = 0U; i < variants.size(); ++i) {
        auto const field_types = variants[i];
        std::vector<llvm::Type *> field_tys;
        llvm::StructType *variant_ty;
        {
            field_tys.reserve(field_types.size());
            for (auto *field_type: field_types)
                field_tys.push_back(*field_type);
            variant_ty = llvm::StructType::get(*c, field_tys, false);
        }

        variants_[i] = {
            .field_types = field_types,
            .ty = variant_ty,
        };

        auto const size = X->size_of(variant_ty);
        if (largest_variant.size <= size) {
            largest_variant.field_tys = std::move(field_tys);
            largest_variant.ty = variant_ty;
            largest_variant.size = size;
        }
        auto const alignment = X->align_of(variant_ty);
        if (strictest_alignment < alignment.value())
            strictest_alignment = alignment.value();
    }

    if (variants.empty() || largest_variant.size == 0) {
        ty_ = llvm::StructType::get(*c, {}, false);
    } else {
        ty_ = variants.size() == 1 ? largest_variant.ty
            : create_underlying_ty(*X, largest_variant.size, strictest_alignment);
        min_alignment_ = strictest_alignment;
    }
}

llvm::Type *ObjectType::get_ty() const
{
    return ty_;
}

unsigned long ObjectType::hash() const
{
    auto h = hash_combine((unsigned long)get_kind(), HASH_SEED);
    for (auto const &variant: variants_) {
        for (auto *field_type: variant.field_types)
            h = hash_combine(h, field_type->hash());
    }
    return h;
}

bool ObjectType::equals(Type const *rhs) const
{
    if (!rhs->is_object_type()) return false;

    auto *obj = (ObjectType *)rhs;
    if (variants_.size() != obj->variants_.size())
        return false;

    for (auto i = 0U; i < variants_.size(); ++i) {
        if (!are_types_equal(variants_[i].field_types,
                    obj->variants_[i].field_types))
            return false;
    }

    return true;
}


std::string ObjectType::to_string() const
{
    std::string string;
    llvm::raw_string_ostream rso(string);
    ty_->print(rso);
    return string;
}

} // namespace paw::cg
