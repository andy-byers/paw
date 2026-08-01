// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_CODEGEN_TYPE_H
#define PAW_CODEGEN_TYPE_H

#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/DebugInfoMetadata.h>

namespace paw::cg {

class Context;


// Describes how an object of this type is handled at function
// call boundaries
enum class ABIClass {
    EMPTY,
    SCALAR,
    SMALL_STRUCT,
    BINARY_STRUCT,
    LARGE_STRUCT,
    HXA_STRUCT,
};

enum class ReturnKind {
    VOID,
    NORMAL,
    SRET,
};

class Type {
public:
    enum class Kind {
        UNIT,
        BOOL,
        CHAR,
        INT,
        FLOAT,
        STR,
        SLICE,
        ARRAY,
        OBJECT,
        FN,
        PTR,
    };

    explicit Type(Context &X, llvm::Type *ty, Kind kind, ABIClass abi_class)
        : X(&X)
        , ty_(ty)
        , abi_class_(abi_class)
        , kind_(kind)
    {
    }

    virtual ~Type() = default;

    Kind get_kind() const { return kind_; }
    ABIClass get_abi_class() const { return abi_class_; }
    bool is_unit_type() const { return kind_ == Kind::UNIT; }
    bool is_bool_type() const { return kind_ == Kind::BOOL; }
    bool is_char_type() const { return kind_ == Kind::CHAR; }
    bool is_int_type() const { return kind_ == Kind::INT; }
    bool is_float_type() const { return kind_ == Kind::FLOAT; }
    bool is_str_type() const { return kind_ == Kind::STR; }
    bool is_object_type() const { return kind_ == Kind::OBJECT; }
    bool is_slice_type() const { return kind_ == Kind::SLICE; }
    bool is_array_type() const { return kind_ == Kind::ARRAY; }
    bool is_fn_type() const { return kind_ == Kind::FN; }
    bool is_ptr_type() const { return kind_ == Kind::PTR; }

    bool is_signed_int() const;

    virtual unsigned get_alignment() const;

    bool is_abi_struct_type() const
    {
        return abi_class_ == ABIClass::SMALL_STRUCT
            || abi_class_ == ABIClass::BINARY_STRUCT
            || abi_class_ == ABIClass::LARGE_STRUCT
            || abi_class_ == ABIClass::HXA_STRUCT;
    }

    llvm::DIType *get_dity() const
    {
        return dity_;
    }

    void set_dity(llvm::DIType *dity)
    {
        dity_ = dity;
    }

    virtual unsigned long hash() const = 0;
    virtual bool equals(Type const *rhs) const = 0;

    bool operator==(Type const *rhs) const
    {
        return equals(rhs);
    }

    // Return the base LLVM type
    // The base type is defined as the type "T" as used in the
    // instruction "alloca T".
    virtual llvm::Type *get_ty() const
    {
        return ty_;
    }

    virtual llvm::Type *get_abi_ty() const
    {
        return get_ty();
    }

    // Implicit conversion to "llvm::Type *" for convenience
    operator llvm::Type *() const { return get_ty(); }

    virtual std::string to_string() const = 0;

protected:
    Context *X;
    llvm::Type *ty_;
    llvm::DIType *dity_;
    ABIClass abi_class_;
    Kind kind_;
};

static inline std::ostream &operator<<(std::ostream &os, Type const &v)
{
    return os << v.to_string();
}


class PrimitiveType: public Type {
public:
    explicit PrimitiveType(Context &X, llvm::Type *ty, Kind kind, ABIClass abi_class)
        : Type(X, ty, kind, abi_class)
    {
    }

    ~PrimitiveType() override = default;

    llvm::IntegerType *get_integer_ty() const
    {
        return llvm::cast<llvm::IntegerType>(ty_);
    }

    unsigned long hash() const override;
    bool equals(Type const *rhs) const override;
};


class UnitType: public PrimitiveType {
public:
    friend class Unit;

    explicit UnitType(Context &X);
    ~UnitType() override = default;

    std::string to_string() const override
    {
        return "()";
    }
};


class BoolType: public PrimitiveType {
public:
    friend class Bool;

    explicit BoolType(Context &X);
    ~BoolType() override = default;

    std::string to_string() const override
    {
        return "bool";
    }
};


class CharType: public PrimitiveType {
public:
    friend class Char;

    explicit CharType(Context &X);
    ~CharType() override = default;

    std::string to_string() const override
    {
        return "char";
    }
};


enum class IntKind {
    INT8,
    INT16,
    INT32,
    INT64,
    ISIZE,
    UINT8,
    UINT16,
    UINT32,
    UINT64,
    USIZE,
};

static constexpr size_t NUM_INT_KINDS = size_t(IntKind::USIZE) + 1;


enum class Signedness {
    SIGNED,
    UNSIGNED,
};

class IntType: public PrimitiveType {
public:
    friend class Int;

    explicit IntType(Context &X, IntKind kind);
    ~IntType() override = default;

    unsigned long hash() const override;
    bool equals(Type const *rhs) const override;

    bool is_signed() const
    {
        return get_signedness() == Signedness::SIGNED;
    }

    Signedness get_signedness() const
    {
        switch (ikind_) {
            case IntKind::INT8:
            case IntKind::INT16:
            case IntKind::INT32:
            case IntKind::INT64:
            case IntKind::ISIZE:
                return Signedness::SIGNED;
            case IntKind::UINT8:
            case IntKind::UINT16:
            case IntKind::UINT32:
            case IntKind::UINT64:
            case IntKind::USIZE:
                return Signedness::UNSIGNED;
        }
    }

    std::string to_string() const override
    {
        switch (ikind_) {
            case IntKind::INT8:
                return "int8";
            case IntKind::INT16:
                return "int16";
            case IntKind::INT32:
                return "int32";
            case IntKind::INT64:
                return "int64";
            case IntKind::ISIZE:
                return "isize";
            case IntKind::UINT8:
                return "uint8";
            case IntKind::UINT16:
                return "uint16";
            case IntKind::UINT32:
                return "uint32";
            case IntKind::UINT64:
                return "uint64";
            case IntKind::USIZE:
                return "usize";
        }
    }

private:
    IntKind ikind_;
};


enum class FloatKind {
    FLOAT32,
    FLOAT64,
};

static constexpr size_t NUM_FLOAT_KINDS = size_t(FloatKind::FLOAT64) + 1;

class FloatType: public PrimitiveType {
public:
    friend class Float;

    explicit FloatType(Context &X, FloatKind kind);
    ~FloatType() override = default;

    unsigned long hash() const override;
    bool equals(Type const *rhs) const override;

    std::string to_string() const override
    {
        switch (fkind_) {
            case FloatKind::FLOAT32:
                return "float";
            case FloatKind::FLOAT64:
                return "double";
        }
    }

private:
    FloatKind fkind_;
};



class ArrayType: public Type {
public:
    friend class Array;

    explicit ArrayType(Context &X, Type *element_type, uint64_t length);
    ~ArrayType() override = default;
    llvm::Type *get_abi_ty() const override;

    llvm::ArrayType *get_array_ty() const
    {
        return llvm::cast<llvm::ArrayType>(get_ty());
    }

    unsigned long hash() const override;
    bool equals(Type const *rhs) const override;

    uint64_t get_length() const { return length_; }
    Type *get_element_type() const { return element_type_; }

    std::string to_string() const override;

private:
    Type *element_type_;
    uint64_t length_;
};


struct Discriminant {
    explicit Discriminant(int value) : value(value) { }
    static Discriminant base() { return Discriminant(0); }
    unsigned value;
};

class ObjectType: public Type {
public:
    friend class Object;

    enum class Kind {
        STRUCT,
        ENUM,
    };

    using FieldTypes = std::vector<Type *>;
    explicit ObjectType(Context &X,
            llvm::ArrayRef<FieldTypes> variant_types,
            std::string name = "");
    ~ObjectType() override = default;

    llvm::Type *get_ty() const override;
    llvm::Type *get_abi_ty() const override;
    llvm::StructType *get_struct_ty() const
    {
        return llvm::cast<llvm::StructType>(ty_);
    }

    std::string const &get_name() const { return name_; }
    unsigned get_num_variants() const { return variants_.size(); }
    unsigned get_alignment() const override { return min_alignment_; }

    llvm::StructType *get_variant_ty(Discriminant discr) const
    {
        return variants_.at(discr.value).ty;
    }

    unsigned get_num_fields(Discriminant discr) const
    {
        return variants_.at(discr.value).field_types.size();
    }

    Type *get_field_type(Discriminant discr, unsigned index) const
    {
        return variants_.at(discr.value).field_types.at(index);
    }

    unsigned long hash() const override;
    bool equals(Type const *rhs) const override;

    std::string to_string() const override;

private:
    void set_variants(llvm::ArrayRef<ObjectType::FieldTypes> variants);

    struct Variant {
        std::vector<Type *> field_types;
        llvm::StructType *ty;
    };

    unsigned min_alignment_;
    std::vector<Variant> variants_;
    std::string name_;
};

class SliceType: public Type {
public:
    friend class Slice;

    explicit SliceType(Context &X, Type *element_type);
    ~SliceType() override = default;

    llvm::StructType *get_struct_ty() const;
    llvm::Type *get_abi_ty() const override;

    Type *get_element_type() const
    {
        return element_type_;
    }

    std::string to_string() const override
    {
        return "*[" + element_type_->to_string() + "]";
    }

    unsigned long hash() const override;
    bool equals(Type const *rhs) const override;

private:
    Type *element_type_;
};


class StrType: public Type {
public:
    friend class Str;

    explicit StrType(Context &X);
    ~StrType() override = default;

    llvm::StructType *get_struct_ty() const;
    llvm::Type *get_abi_ty() const override;

    std::string to_string() const override
    {
        return "str";
    }

    unsigned long hash() const override;
    bool equals(Type const *rhs) const override;
};


class FnType: public Type {
public:
    explicit FnType(Context &X, Type *return_type,
            llvm::ArrayRef<Type *> param_types,
            Type *env_type = nullptr,
            bool never_returns = false);
    ~FnType() override = default;

    llvm::Type *get_ty() const override;
    llvm::Type *get_abi_ty() const override;
    llvm::FunctionType *get_fn_ty() const
    {
        return llvm::cast<llvm::FunctionType>(ty_);
    }

    bool has_env() const { return env_type_ != nullptr; }
    Type *get_env_type() const { return env_type_; }
    ReturnKind get_return_kind() const { return return_kind_; }
    bool never_returns() const { return never_returns_; }
    Type *get_return_type() const { return return_type_; }
    unsigned get_num_params() const { return param_types_.size(); }

    bool has_void_return() const { return return_kind_ == ReturnKind::VOID; }
    bool has_normal_return() const { return return_kind_ == ReturnKind::NORMAL; }
    bool has_struct_return() const { return return_kind_ == ReturnKind::SRET; }

    unsigned long hash() const override;
    bool equals(Type const *rhs) const override;

    Type *get_param_type(unsigned index) const
    {
        return param_types_.at(index);
    }

    std::string to_string() const override;

private:
    ReturnKind return_kind_;
    std::vector<Type *> param_types_;
    Type *return_type_;
    Type *env_type_;
    bool never_returns_;
};


class PtrType: public Type {
public:
    explicit PtrType(Context &X, Type *pointee_type = nullptr);
    ~PtrType() override = default;

    llvm::PointerType *get_pointer_ty() const
    {
        return llvm::cast<llvm::PointerType>(ty_);
    }

    Type *get_pointee_type() const { return pointee_type_; }

    unsigned long hash() const override;
    bool equals(Type const *rhs) const override;

    std::string to_string() const override;

private:
    Type *pointee_type_;
};

} // namespace paw::cg

#endif // PAW_CODEGEN_TYPE_H

