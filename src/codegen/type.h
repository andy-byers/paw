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
        INT32,
        FLOAT,
        STR,
        OBJECT,
        LIST,
        MAP,
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
    bool is_int32_type() const { return kind_ == Kind::INT32; }
    bool is_float_type() const { return kind_ == Kind::FLOAT; }
    bool is_str_type() const { return kind_ == Kind::STR; }
    bool is_object_type() const { return kind_ == Kind::OBJECT; }
    bool is_list_type() const { return kind_ == Kind::LIST; }
    bool is_map_type() const { return kind_ == Kind::MAP; }
    bool is_fn_type() const { return kind_ == Kind::FN; }
    bool is_ptr_type() const { return kind_ == Kind::PTR; }
    bool is_value_type() const { return !is_boxed_type(); }
    bool is_boxed_type() const;

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


class Int32Type: public PrimitiveType {
public:
    explicit Int32Type(Context &X);
    ~Int32Type() override = default;

    std::string to_string() const override
    {
        return "i32";
    }
};


class IntType: public PrimitiveType {
public:
    friend class Int;

    explicit IntType(Context &X);
    ~IntType() override = default;

    std::string to_string() const override
    {
        return "int";
    }
};


class FloatType: public PrimitiveType {
public:
    friend class Float;

    explicit FloatType(Context &X);
    ~FloatType() override = default;

    std::string to_string() const override
    {
        return "float";
    }
};


class StrType: public PrimitiveType {
public:
    friend class Str;

    explicit StrType(Context &X);
    ~StrType() override = default;

    llvm::StructType *get_struct_ty() const;

    std::string to_string() const override
    {
        return "str";
    }
};


struct Discriminant {
    explicit Discriminant(int value) : value(value) { }
    static Discriminant base() { return Discriminant(0); }
    unsigned value;
};

class ObjectType: public Type {
public:
    friend class Object;

    // Describes where an object of this type is allocated
    enum class Location {
        STACK,
        HEAP,
    };

    enum class Kind {
        STRUCT,
        ENUM,
    };

    using FieldTypes = std::vector<Type *>;
    explicit ObjectType(Context &X, llvm::ArrayRef<FieldTypes> variant_types, Location location = Location::HEAP);
    explicit ObjectType(Context &X, std::string name, Location location = Location::HEAP);
    ~ObjectType() override = default;

    void set_variants(llvm::ArrayRef<FieldTypes> variant_types);

    llvm::Type *get_ty() const override;
    llvm::Type *get_abi_ty() const override;
    llvm::StructType *get_struct_ty() const
    {
        return llvm::cast<llvm::StructType>(ty_);
    }

    bool is_opaque() const;
    bool is_inline() const { return is_inline_; }
    std::string const &get_name() const { return name_; }
    unsigned get_num_variants() const { return variants_.size(); }

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
    llvm::Type *create_inner_type(llvm::ArrayRef<ObjectType::FieldTypes> variant_types);

    struct Variant {
        std::vector<Type *> field_types;
        llvm::StructType *ty;
    };

    std::vector<Variant> variants_;
    std::string name_;
    bool is_inline_ : 1;
};


class ListType: public Type {
public:
    friend class List;
    paw_Int static constexpr MIN_CAPACITY = 2;

    explicit ListType(Context &X, Type *element_type);
    ~ListType() override = default;

    llvm::StructType *get_struct_ty() const;

    Type *get_element_type() { return element_type_; }

    unsigned long hash() const override;
    bool equals(Type const *rhs) const override;
    std::string to_string() const override;

private:
    Type *element_type_;
};

class MapType: public Type {
public:
    friend class Map;

    explicit MapType(Context &X, Type *key_type, Type *value_type);
    ~MapType() override = default;

    llvm::StructType *get_struct_ty() const;

    Type *get_key_type() const { return key_type_; }
    Type *get_value_type() const { return value_type_; }

    unsigned long hash() const override;
    bool equals(Type const *rhs) const override;
    std::string to_string() const override;

private:
    Type *key_type_;
    Type *value_type_;
};

class FnType: public Type {
public:
    explicit FnType(Context &X, Type *return_type,
            llvm::ArrayRef<Type *> param_types,
            FnKind fn_kind, bool has_env = true,
            bool never_returns = false);
    ~FnType() override = default;

    llvm::Type *get_ty() const override;
    llvm::Type *get_abi_ty() const override;
    llvm::FunctionType *get_fn_ty() const
    {
        return llvm::cast<llvm::FunctionType>(ty_);
    }

    bool has_env() const { return has_env_; }
    FnKind get_fn_kind() const { return fn_kind_; }
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
    FnKind fn_kind_;
    ReturnKind return_kind_;
    std::vector<Type *> param_types_;
    Type *return_type_;
    bool never_returns_;
    bool has_env_;
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

