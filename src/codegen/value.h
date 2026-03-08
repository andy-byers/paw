// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_CODEGEN_VALUE_H
#define PAW_CODEGEN_VALUE_H

#include <string>

#include <llvm/IR/Function.h>
#include <llvm/IR/Value.h>

#include "mir.h"
#include "type.h"

namespace paw::cg {

class Context;
class Fn;
class State;


template<class T>
struct Owned {
    std::unique_ptr<typename T::Type> type;
    std::unique_ptr<T> value;
};


class Fn {
public:
    using Type = FnType;

    // Constructor for toplevel functions and functions generated
    // by the compiler
    explicit Fn(Context &X, std::string name,
            llvm::Function::LinkageTypes linkage,
            FnType *type);

    // Constructor for closures
    explicit Fn(Context &X, llvm::Value *env, FnType *type);

    ~Fn() = default;

    operator llvm::Function *() const
    {
        return llvm::cast<llvm::Function>(value_);
    }

    class Callable as_callable(State &state) const;

    llvm::Value *get_value() const;

    llvm::Function *get_fn() const
    {
        return llvm::cast<llvm::Function>(value_);
    }

    std::string get_name() const;

    FnType *get_type() const
    {
        return type_;
    }

    unsigned get_num_args() const
    {
        return get_type()->get_num_params();
    }

    llvm::Value *get_env_ptr() const;
    llvm::Value *get_arg(unsigned index) const;

private:
    Context *X;
    FnType *type_;
    llvm::Value *value_;
    llvm::Value *env_;
};


class Value {
public:
    enum Kind {
        OBJECT,
        STR,
        LIST,
        MAP,
        FN,
    };

    using Type = Type;

    struct CreationTag { };

    explicit Value(State &state, llvm::Value *value, Type *type)
        : state_(&state)
        , value_(value)
        , type_(type)
    {
        paw_assert(value != nullptr);
        paw_assert(type != nullptr);
    }

    virtual ~Value() = default;

    Context *get_context() const;

    virtual llvm::Value *V() const
    {
        return value_;
    }

    // TODO: use this instead of V()
    virtual llvm::Value *get_value() const
    {
        return value_;
    }

    // Implicit conversion to "llvm::Value *" for convenience
    operator llvm::Value *() const { return get_value(); }

    virtual Type *get_type() const = 0;

protected:
    State *state_;
    llvm::Value *value_;
    Type *type_;
};


class Callable: public Value {
public:
    friend class State;
    using Type = FnType;

    explicit Callable(State &state, llvm::Value *fn, llvm::Value *env, FnType *type);
    ~Callable() override = default;

    FnType *get_type() const override
    {
        return (FnType *)type_;
    }

    llvm::FunctionCallee get_callee() const
    {
        return llvm::FunctionCallee(
                get_type()->get_fn_ty(),
                value_);
    }

    llvm::Value *get_value() const override;

    unsigned get_num_args() const { return get_type()->get_num_params(); }

//TODO    llvm::Value *call(llvm::ArrayRef<llvm::Value *> args, llvm::Value *sret = nullptr);
//TODO    llvm::Value *call(llvm::Value *arg) { return call(llvm::ArrayRef<llvm::Value *>{arg}); }
//TODO    llvm::Value *call() { return call(llvm::ArrayRef<llvm::Value *>{}); }

private:
    llvm::Value *env_;
};


class Unit: public Value {
public:
    struct Methods {
        Fn *hash;
        Fn *to_str;
    };

    using Type = UnitType;

    explicit Unit(State &state, llvm::Value *value, Methods const *methods);
    ~Unit() override = default;

    UnitType *get_type() const override
    {
        return (UnitType *)type_;
    }

private:
    Methods const *methods_;
};


class Bool: public Value {
public:
    struct Methods {
        Fn *hash;
        Fn *to_str;
    };

    using Type = BoolType;

    explicit Bool(State &state, llvm::Value *value, Methods const *methods);
    ~Bool() override = default;

    BoolType *get_type() const override
    {
        return (BoolType *)type_;
    }

private:
    Methods const *methods_;
};


class Char: public Value {
public:
    struct Methods {
        Fn *hash;
        Fn *to_str;
    };

    using Type = CharType;

    explicit Char(State &state, llvm::Value *value, Methods const *methods);
    ~Char() override = default;

    CharType *get_type() const override
    {
        return (CharType *)type_;
    }

private:
    Methods const *methods_;
};


class Int: public Value {
public:
    struct Methods {
        Fn *hash;
        Fn *to_str;
    };

    using Type = IntType;

    explicit Int(State &state, llvm::Value *value, Methods const *methods);
    ~Int() override = default;

    IntType *get_type() const override
    {
        return (IntType *)type_;
    }

private:
    Methods const *methods_;
};


class Float: public Value {
public:
    struct Methods {
        Fn *hash;
        Fn *to_str;
    };

    using Type = FloatType;

    explicit Float(State &state, llvm::Value *value, Methods const *methods);
    ~Float() override = default;

    FloatType *get_type() const override
    {
        return (FloatType *)type_;
    }

private:
    Methods const *methods_;
};


class Str: public Value {
public:
    struct Methods {
        Fn *hash;
        Fn *to_str;
    };

    using Type = StrType;

    explicit Str(State &state, llvm::Value *str, Methods const *methods);
    explicit Str(State &state, llvm::Value *length, Methods const *methods, CreationTag);
    explicit Str(State &state, llvm::Value *text, llvm::Value *hash, llvm::Value *length, Methods const *methods, CreationTag);

    explicit Str(State &state, llvm::Value *text, llvm::Value *length, Methods const *methods, CreationTag);

    void finalize();

    StrType *get_type() const override
    {
        return (StrType *)type_;
    }

    llvm::Value *get_length_ptr() const { return length_ptr_; }
    llvm::Value *get_hash_ptr() const { return hash_ptr_; }
    llvm::Value *get_text() const { return text_; }

    llvm::Value *get_hash() const;
    llvm::Value *get_length() const;

    llvm::Value *get_element_ptr(llvm::Value *index);

private:
    llvm::Value *text_;
    llvm::Value *hash_ptr_;
    llvm::Value *length_ptr_;
    Methods const *methods_;
};


class Object: public Value {
public:
    struct Methods { };

    using Type = ObjectType;

    explicit Object(State &state, ObjectType *type, CreationTag);
    explicit Object(State &state, llvm::Value *object, ObjectType *type);
    ~Object() override = default;

    ObjectType *get_type() const override
    {
        return (ObjectType *)type_;
    }

    llvm::Value *get_field_ptr(Discriminant discr, unsigned index);
    llvm::Value *get_field(Discriminant discr, unsigned index);
    void set_field(Discriminant discr, unsigned index, llvm::Value *value);

private:
    Methods const *methods_;
};


class List: public Value {
public:
    struct Methods {
        Fn *push;
        Fn *pop;
        Fn *insert;
        Fn *remove;
        Fn *get_element_ptr;
    };

    using Type = ListType;

    friend class Context;
    friend class ListType;

    explicit List(State &state, llvm::Value *list, ListType *type, Methods const *methods);
    explicit List(State &state, llvm::Value *length, ListType *type, Methods const *methods, CreationTag);
    static void generate_methods(Context &X, ListType *type, Methods &m);

    ListType *get_type() const override
    {
        return (ListType *)type_;
    }

    llvm::Value *get_data_ptr() const { return data_ptr_; }
    llvm::Value *get_length_ptr() const { return length_ptr_; }
    llvm::Value *get_capacity_ptr() const { return capacity_ptr_; }

    llvm::Value *get_data() const;
    llvm::Value *get_length() const;
    llvm::Value *get_capacity() const;
    void set_data(llvm::Value *value) const;
    void set_length(llvm::Value *value) const;
    void set_capacity(llvm::Value *value) const;

    llvm::Value *get_element_ptr(llvm::Value *index);
    llvm::Value *get_element_ptr(paw_Int index);

    llvm::Value *get_element(llvm::Value *index);
    void set_element(llvm::Value *index, llvm::Value *element);

private:
    llvm::Value *new_buffer(llvm::Value *capacity);
    void create_grow(llvm::Value *old_capacity);

    Methods const *methods_;
    llvm::Value *data_ptr_;
    llvm::Value *length_ptr_;
    llvm::Value *capacity_ptr_;
};


class Map: public Value {
public:
    struct Methods {
        Fn *get;
        Fn *remove;
        Fn *key_hash;
        Fn *key_eq;

        ObjectType *iterator_type;
        Fn *iterator_next;

        Owned<Fn> gep;
        Owned<Fn> nep;
        Owned<Fn> gep1;
        Owned<Fn> grow;
        Owned<Fn> lookup;
        Owned<Fn> access;
    };

    using Type = MapType;

    friend class Context;
    friend class MapType;

    explicit Map(State &state, llvm::Value *map, MapType *type, Methods const *methods);
    explicit Map(State &state, llvm::Value *length_hint, MapType *type, Methods const *methods, CreationTag);
    static void generate_methods(Context &X, MapType *type, Methods &m);

    MapType *get_type() const override
    {
        return (MapType *)type_;
    }

    operator llvm::Value *() const
    {
        return get_value();
    }

    llvm::Value *get_data_ptr() const { return data_ptr_; }
    llvm::Value *get_length_ptr() const { return length_ptr_; }
    llvm::Value *get_capacity_ptr() const { return capacity_ptr_; }

    llvm::Value *get_data() const;
    llvm::Value *get_length() const;
    llvm::Value *get_capacity() const;
    void set_data(llvm::Value *data) const;
    void set_length(llvm::Value *capacity) const;
    void set_capacity(llvm::Value *capacity) const;

    llvm::Value *get_element_ptr(llvm::Value *search_key);
    llvm::Value *new_element_ptr(llvm::Value *search_key);

    llvm::Value *get_element(llvm::Value *key);
    void set_element(llvm::Value *key, llvm::Value *value);

private:
    enum class Flag: int8_t {
        VACANT = 0b00,
        ERASED = 0b01,
        EXISTS = 0b10,
    };

    struct Components {
        llvm::Value *flags;
        llvm::Value *keys;
        llvm::Value *values;
    };

    llvm::Value *first_index(llvm::Value *key, llvm::Value *capacity);
    llvm::Value *next_index(llvm::Value *index, llvm::Value *capacity);
    llvm::Value *new_buffer(llvm::Value *capacity);
    llvm::Value *access(llvm::Value *search_key);
    llvm::Value *lookup(llvm::Value *search_key);
    Components unpack();

    Methods const *methods_;
    llvm::Value *data_ptr_;
    llvm::Value *length_ptr_;
    llvm::Value *capacity_ptr_;
};


} // namespace paw::cg

#endif // PAW_CODEGEN_VALUE_H
