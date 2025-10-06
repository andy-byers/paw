// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "context.h"

#define DEFERRED_INIT ((llvm::Type *)42)

namespace paw::cg {

unsigned long static constexpr HASH_SEED = 0x9E3779B97F4A7C15ULL;

// Splitmix64 PRNG
template<class T>
static T splitmix64(T &state)
{
    state += 0x9E3779B97F4A7C15ULL;

    auto t = state;
    t = (t ^ (t >> 30)) * 0xBF58476D1CE4E5B9ULL;
    t = (t ^ (t >> 27)) * 0x94D049BB133111EBULL;
    return t ^ (t >> 31);
}

template<class T>
static T hash_combine(T x, T y) {
    T t = x ^ y;
    return splitmix64(t);
}


bool Type::is_boxed_type() const
{
    return is_str_type()
        || is_list_type()
        || is_map_type()
        || (is_object_type() && static_cast<ObjectType const *>(this)->is_inline());
}

UnitType::UnitType(Context &X)
    : PrimitiveType(X, X.get_unit_ty(), Kind::UNIT, ABIClass::EMPTY)
{
}


BoolType::BoolType(Context &X)
    : PrimitiveType(X, X.get_i1_ty(), Kind::BOOL, ABIClass::SCALAR)
{
}


CharType::CharType(Context &X)
    : PrimitiveType(X, X.get_i8_ty(), Kind::CHAR, ABIClass::SCALAR)
{
}


Int32Type::Int32Type(Context &X)
    : PrimitiveType(X, X.get_i32_ty(), Kind::INT32, ABIClass::SCALAR)
{
}


IntType::IntType(Context &X)
    : PrimitiveType(X, X.get_i64_ty(), Kind::INT, ABIClass::SCALAR)
{
}


FloatType::FloatType(Context &X)
    : PrimitiveType(X, X.get_float_ty(), Kind::FLOAT, ABIClass::SCALAR)
{
}


StrType::StrType(Context &X)
    : PrimitiveType(X, X.get_ptr_ty(), Kind::STR, ABIClass::SCALAR)
{
}

llvm::StructType *StrType::get_struct_ty() const
{
    return X->get_str_ty();
}


unsigned long PrimitiveType::hash() const
{
    return hash_combine((unsigned long)get_kind(), HASH_SEED);
}

bool PrimitiveType::equals(Type const *rhs) const
{
    return get_kind() == rhs->get_kind();
}


// WARNING: The ABIClass must be updated if the layout of this type changes.
// Currently, "FnType" is a structure containing 2 pointer-sized members,
// making it a ABIClass::BINARY_STRUCT.
FnType::FnType(Context &X, Type *return_type,
        llvm::ArrayRef<Type *> param_types,
        FnKind fn_kind, bool has_env,
        bool never_returns)
    : Type(X, DEFERRED_INIT, Kind::FN, ABIClass::BINARY_STRUCT)
    , fn_kind_(fn_kind)
    , return_kind_(ReturnKind::NORMAL)
    , param_types_(param_types)
    , return_type_(return_type)
    , never_returns_(never_returns)
    , has_env_(has_env)
{
    auto *B = X.get_builder();
    std::vector<llvm::Type *> param_tys(has_env + param_types.size());
    auto first = begin(param_tys);

    if (has_env)
        *first++ = X.get_ptr_ty();

    auto param = first;
    for (auto *type: param_types)
        *param++ = type->get_abi_ty();

    llvm::Type *return_ty;
    switch (return_type->get_abi_class()) {
        case ABIClass::EMPTY:
            return_kind_ = ReturnKind::VOID;
            return_ty = X.get_void_ty();
            break;
        case ABIClass::SCALAR:
            // TODO: i1 is a special case, should be rounded up to i8
//            if (return_type->is_bool_type()) {
//                return_ty = X.get_i8_ty();
//                break;
//            }
            // (fallthrough)
        case ABIClass::HXA_STRUCT:
            return_ty = *return_type;
            break;
        case ABIClass::SMALL_STRUCT:
            return_ty = B->getIntNTy(X.bitsize_of(*return_type));
            break;
        case ABIClass::BINARY_STRUCT:
            return_ty = X.get_array_ty(X.get_i64_ty(), 2);
            break;
        case ABIClass::LARGE_STRUCT:
            return_kind_ = ReturnKind::SRET;
            // large structure is written to output pointer
            return_ty = X.get_void_ty();
            // add return pointer parameter
            param_tys.insert(begin(param_tys), X.get_ptr_ty());
            break;
    }

    ty_ = llvm::FunctionType::get(return_ty, param_tys, false);
}

llvm::Type *FnType::get_ty() const
{
    return X->get_callable_ty();
}

static llvm::Type *object_to_abi(Context &X, llvm::Type *ty, ABIClass abi_class)
{
    switch (abi_class) {
        case ABIClass::EMPTY:
        case ABIClass::SCALAR:
        case ABIClass::HXA_STRUCT:
            return ty;
        case ABIClass::SMALL_STRUCT:
            // NOTE: "Context::size_of" rounds up to the nearest byte
            return X.get_sized_int_ty(X.size_of(ty) * 8);
        case ABIClass::BINARY_STRUCT:
            return X.get_array_ty(X.get_i64_ty(), 2);
        case ABIClass::LARGE_STRUCT:
            return X.get_ptr_ty();
    }
}

llvm::Type *FnType::get_abi_ty() const
{
    return object_to_abi(*X, get_ty(), get_abi_class());
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
    if (fn_kind_ != fn->fn_kind_) return false;
    if (return_kind_ != fn->return_kind_) return false;
    if (never_returns_ != fn->never_returns_) return false;
    if (has_env_ != fn->has_env_) return false;

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
    : Type(X, X.get_ptr_ty(), Kind::PTR, ABIClass::SCALAR)
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


ListType::ListType(Context &X, Type *element_type)
    : Type(X, X.get_ptr_ty(), Kind::LIST, ABIClass::SCALAR)
    , element_type_(element_type)
{
}

llvm::StructType *ListType::get_struct_ty() const
{
    return X->get_list_ty();
}

unsigned long ListType::hash() const
{
    auto h = hash_combine((unsigned long)get_kind(), HASH_SEED);
    return hash_combine(h, element_type_->hash());
}

bool ListType::equals(Type const *rhs) const
{
    return rhs->is_list_type()
        && element_type_ == ((ListType *)rhs)->element_type_;
}


std::string ListType::to_string() const
{
    return "[" + element_type_->to_string() + "]";
}

MapType::MapType(Context &X, Type *key_type, Type *value_type)
    : Type(X, X.get_ptr_ty(), Kind::MAP, ABIClass::SCALAR)
    , key_type_(key_type)
    , value_type_(value_type)
{
}

llvm::StructType *MapType::get_struct_ty() const
{
    return X->get_map_ty();
}

unsigned long MapType::hash() const
{
    auto h = hash_combine((unsigned long)get_kind(), HASH_SEED);
    h = hash_combine(h, key_type_->hash());
    h = hash_combine(h, value_type_->hash());
    return h;
}

bool MapType::equals(Type const *rhs) const
{
    return rhs->is_map_type()
        && key_type_ == ((MapType *)rhs)->key_type_
        && value_type_ == ((MapType *)rhs)->value_type_;
}

std::string MapType::to_string() const
{
    return "[" + key_type_->to_string()
        + ": " + value_type_->to_string() + "]";
}


struct FieldCounter {
    int floats;
    int total;
};

static FieldCounter count_fields(llvm::Type *ty)
{
    if (ty->isDoubleTy()) return FieldCounter{1, 1};
    if (!ty->isStructTy()) return FieldCounter{0, 1};

    struct FieldCounter counter = {};
    auto *st = llvm::cast<llvm::StructType>(ty);
    for (auto i = 0U; i < st->getNumElements(); ++i) {
        auto *field_ty = st->getElementType(i);
        auto const [floats, total] = count_fields(field_ty);
        counter.floats += floats;
        counter.total += total;
    }
    return counter;
}

// Return true if the given type is a homogeneous floating-point aggregate (HFA)
// or a homogeneous short vector aggregate (HVA), false otherwise
// TODO: only works for HFA
static bool is_hxa_struct_ty(llvm::Type *ty)
{
    if (!ty->isStructTy()) return false;
    auto const [floats, total] = count_fields(ty);
    return 0 < total && total == floats && total <= 4;
}

ObjectType::ObjectType(Context &X, llvm::ArrayRef<ObjectType::FieldTypes> variants, Location location)
    : Type(X, DEFERRED_INIT, Type::Kind::OBJECT, (ABIClass)0)
    , is_inline_(location == Location::STACK)
{
    set_variants(variants);
}

ObjectType::ObjectType(Context &X, std::string name, Location location)
    : Type(X, llvm::StructType::create(*X.get_context(), name),
            Type::Kind::OBJECT, (ABIClass)0)
    , name_(std::move(name))
    , is_inline_(location == Location::STACK)
{
    paw_assert(!name_.empty());
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

    paw_assert(!variants.empty());
    for (auto i = 0; i < variants.size(); ++i) {
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
    }

    // set the underlying type to that of the largest variant

//    for (auto const &v: variants_) {
//        auto const size = X->size_of(v.ty);
//        if (size > largest_variant.size) {
//            largest_variant.size = size;
//            largest_variant.ty = v.ty;
//        }
//    }

    if (ty_ != DEFERRED_INIT) {
        llvm::cast<llvm::StructType>(ty_)
            ->setBody(largest_variant.field_tys);
    } else {
        ty_ = largest_variant.ty;
    }

    if (!is_inline_) {
        abi_class_ = ABIClass::SCALAR;
    } else if (is_hxa_struct_ty(ty_)) {
        abi_class_ = ABIClass::HXA_STRUCT;
    } else if (X->size_of(ty_) == 0) {
        abi_class_ = ABIClass::EMPTY;
    } else if (X->size_of(ty_) <= 8) {
        abi_class_ = ABIClass::SMALL_STRUCT;
    } else if (X->size_of(ty_) <= 16) {
        abi_class_ = ABIClass::BINARY_STRUCT;
    } else {
        abi_class_ = ABIClass::LARGE_STRUCT;
    }
}

bool ObjectType::is_opaque() const
{
    return ((llvm::StructType *)ty_)->isOpaque();
}

llvm::Type *ObjectType::get_ty() const
{
    return is_inline_ ? ty_ : X->get_ptr_ty();
}

llvm::Type *ObjectType::get_abi_ty() const
{
    return object_to_abi(*X, get_ty(), get_abi_class());
}

unsigned long ObjectType::hash() const
{
    if (!name_.empty())
        return std::hash<std::string>{}(name_);

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
    if (!name_.empty() || !obj->name_.empty())
        return !name_.empty() && !obj->name_.empty()
            && name_ == obj->name_;

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
