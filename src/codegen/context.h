// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_CODEGEN_CONTEXT_H
#define PAW_CODEGEN_CONTEXT_H

#include <string>
#include <unordered_map>
#include <unordered_set>

#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DIBuilder.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Verifier.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>
#include <llvm/TargetParser/Host.h>
#include <llvm/Transforms/Instrumentation/AddressSanitizer.h>
#include <llvm/Transforms/Instrumentation/SanitizerCoverage.h>
#include <llvm/Transforms/Utils/Cloning.h>
#include <llvm/Transforms/Utils/ModuleUtils.h>

#include "codegen.h"
#include "compile.h"
#include "type.h"
#include "value.h"

namespace paw::cg {

template<class T>
struct Span {
    explicit Span(T const *p, size_t n)
        : data(p), length(n) {}

    T const *data;
    size_t length;
};

using StringView = Span<paw_Char>;

#define CG_LITERAL(Lit_) StringView(Lit_ "", sizeof(Lit_) - 1)
#define CG_STRING(Str_) StringView((Str_).data(), (Str_).size())

static inline std::string to_string(::Str const *str)
{
    return {str->text, (size_t)str->length};
}


enum class BuiltinFn {
    // dynamic memory management functions
    PAW_ALLOC,
    PAW_REALLOC,
    PAW_DEALLOC,
    PAW_ALIGNED_ALLOC,

    // miscellaneous helper functions
    PAW_BKPT,
    HASH_BYTES,
    CHECK_BOUNDS,
    RAWCMP,

    // frequently-called prelude functions
    PANIC,
    PRINT,
    PRINTLN,

    NUM_BUILTINS
};

char const *get_builtin_name(BuiltinFn kind);


class Module {
public:
    friend class Context;

    explicit Module(Context &ctx, std::string name);

    std::string const &get_name() const { return name_; }
    llvm::Module *get_module() const { return M.get(); }
    operator llvm::Module *() const { return M.get(); }

    llvm::Function *get_builtin(BuiltinFn kind) const;

    llvm::DataLayout get_data_layout() const
    {
        return M->getDataLayout();
    }

    void set_data_layout(llvm::DataLayout layout)
    {
        M->setDataLayout(layout);
    }

private:
    explicit Module(Context &ctx, std::unique_ptr<llvm::Module> m);

    Context *X;
    std::string name_;
    std::unique_ptr<llvm::Module> M;

    char const *builtin_names_[(size_t)BuiltinFn::NUM_BUILTINS];
};


static inline unsigned long hash_type(Type const *type)
{
    return type->hash();
}

static inline bool type_equals(Type const *lhs, Type const *rhs)
{
    return lhs->equals(rhs);
}


class Context {
public:
    // Helper for arguments that might be aligned to a value greater than 1
    class MaybeAlignedValue {
    public:
        // allows implicit conversion from "llvm::Value *"
        MaybeAlignedValue(llvm::Value *v)
            : MaybeAlignedValue(v, llvm::MaybeAlign(1)) {}

        explicit MaybeAlignedValue(llvm::Value *v, llvm::MaybeAlign a)
            : value_(v)
            , align_(a) {}

        llvm::Value *value() const { return value_; }
        llvm::MaybeAlign align() const { return align_; }

    private:
        llvm::Value *value_;
        llvm::MaybeAlign align_;
    };

    explicit Context(llvm::LLVMContext &ctx, Compiler &compiler, std::string name, CodegenOptions cgopt);

    std::string const &get_modname() const { return M->get_name(); }
    Module *get_module() const { return M.get(); }
    CodegenOptions get_options() const { return options_; }
    llvm::LLVMContext *get_context() const { return ctx_; }
    Compiler *get_compiler() const { return C; }
    llvm::IRBuilder<> *get_builder() const { return B.get(); }
    llvm::DIBuilder *get_dibuilder() const { return DI.get(); }
    llvm::DICompileUnit *get_compile_unit() const { return dcu_; }

    std::unique_ptr<Context> clone() const;

    llvm::TypeSize bitsize_of(llvm::Type *ty) const
    {
        return M->get_data_layout().getTypeSizeInBits(ty);
    }

    llvm::TypeSize size_of(llvm::Type *ty) const
    {
        return M->get_data_layout().getTypeAllocSize(ty);
    }

    llvm::Align align_of(llvm::Type *ty) const
    {
        return M->get_data_layout().getABITypeAlign(ty);
    }

    uint64_t stride_of(llvm::Type *ty) const
    {
        auto const size = size_of(ty);
        auto const align = align_of(ty);
        return llvm::alignTo(size, align);
    }

    void create_paw_bkpt(llvm::Value *ptr) const
    {
        auto *fn = M->get_builtin(BuiltinFn::PAW_BKPT);
        B->CreateCall(fn, {ptr});
    }

    void create_panic(StringView message) const
    {
        create_panic(create_char_slice(message));
    }

    // NOTE: it is the caller's responsibility to create an "unreachable" basic block
    //       terminator after this function is called
    void create_panic(llvm::Value *message) const
    {
        auto *fn = M->get_builtin(BuiltinFn::PANIC);
        B->CreateCall(fn, {message});
    }

    llvm::Value *create_check_bounds(llvm::Value *index, llvm::Value *length) const
    {
        auto *fn = M->get_builtin(BuiltinFn::CHECK_BOUNDS);
        return B->CreateCall(fn, {index, length});
    }

    llvm::Value *create_hash_bytes(llvm::Value *bytes, llvm::Value *length) const
    {
        auto *fn = M->get_builtin(BuiltinFn::HASH_BYTES);
        return B->CreateCall(fn, {bytes, length, create_i64(0)});
    }

    llvm::Value *create_hash_bytes(llvm::Value *bytes, llvm::Value *length, llvm::Value *hash) const
    {
        auto *fn = M->get_builtin(BuiltinFn::HASH_BYTES);
        return B->CreateCall(fn, {bytes, length, hash});
    }

    void create_print(StringView message) const
    {
        create_print(create_char_slice(message));
    }

    void create_print(llvm::Value *message) const
    {
        auto *fn = M->get_builtin(BuiltinFn::PRINT);
        B->CreateCall(fn, {message});
    }

    void create_println(StringView message) const
    {
        create_println(create_char_slice(message));
    }

    void create_println(llvm::Value *message) const
    {
        auto *fn = M->get_builtin(BuiltinFn::PRINTLN);
        B->CreateCall(fn, {message});
    }

    void create_memset(MaybeAlignedValue ptr, llvm::Value *value, size_t size, bool is_volatile = false)
    {
        create_memset(ptr, value, create_isize(size), is_volatile);
    }

    void create_memset(MaybeAlignedValue ptr, llvm::Value *value, llvm::Value *size, bool is_volatile = false)
    {
        B->CreateMemSet(ptr.value(), value, size, ptr.align(), is_volatile);
    }

    void create_memmove(MaybeAlignedValue dest, MaybeAlignedValue src, size_t size, bool is_volatile = false)
    {
        create_memmove(dest, src, create_isize(size), is_volatile);
    }

    void create_memmove(MaybeAlignedValue dest, MaybeAlignedValue src, llvm::Value *size, bool is_volatile = false)
    {
        B->CreateMemMove(dest.value(), dest.align(),
                src.value(), src.align(), size, is_volatile);
    }

    void create_memcpy(MaybeAlignedValue dest, MaybeAlignedValue src, size_t size, bool is_volatile = false)
    {
        create_memcpy(dest, src, create_isize(size), is_volatile);
    }

    void create_memcpy(MaybeAlignedValue dest, MaybeAlignedValue src, llvm::Value *size, bool is_volatile = false)
    {
        B->CreateMemCpy(dest.value(), dest.align(),
                src.value(), src.align(), size, is_volatile);
    }

    llvm::FunctionCallee get_puts_callee() const
    {
        return M->get_module()->getOrInsertFunction("puts",
            llvm::FunctionType::get(get_i32_ty(),
                {get_ptr_ty()}, false));
    }

    llvm::FunctionCallee get_strlen_callee() const
    {
        return M->get_module()->getOrInsertFunction("strlen",
            llvm::FunctionType::get(get_isize_ty(),
                {get_ptr_ty()}, false));
    }

    llvm::Value *call_strlen(llvm::Value *str) const
    {
        return B->CreateCall(get_strlen_callee(), str);
    }

    llvm::FunctionCallee get_exit_callee() const
    {
        return M->get_module()->getOrInsertFunction("exit",
            llvm::FunctionType::get(get_void_ty(),
                {get_i32_ty()}, false));
    }

    llvm::FunctionCallee get_rawcmp_callee() const
    {
        return M->get_builtin(BuiltinFn::RAWCMP);
    }

    llvm::Value *create_imin(llvm::Value *a, llvm::Value *b)
    {
        auto *a_lt_b = B->CreateICmpSLT(a, b);
        return B->CreateSelect(a_lt_b, a, b);
    }

    llvm::Value *create_imax(llvm::Value *a, llvm::Value *b)
    {
        auto *a_gt_b = B->CreateICmpSGT(a, b);
        return B->CreateSelect(a_gt_b, a, b);
    }

    llvm::ConstantPointerNull *create_null_ptr() const
    {
        return llvm::ConstantPointerNull::get(get_ptr_ty());
    }

    llvm::Value *create_unit() const
    {
        // TODO: ZSTs
        return create_i8(0);
//        return llvm::ConstantStruct::get(get_unit_ty(), {});
    }

    llvm::ConstantInt *create_bool(paw_Bool value) const
    {
        return create_i1(value);
    }

    llvm::ConstantInt *create_char(paw_Char value) const
    {
        return create_i8(value);
    }

    llvm::ConstantInt *create_i1(bool value) const
    {
        return B->getInt1(value);
    }

    llvm::ConstantInt *create_i8(int8_t value) const
    {
        return B->getInt8((uint8_t)value);
    }

    llvm::ConstantInt *create_i16(int16_t value) const
    {
        return B->getInt16((uint16_t)value);
    }

    llvm::ConstantInt *create_i32(int32_t value) const
    {
        return B->getInt32((uint32_t)value);
    }

    llvm::ConstantInt *create_i64(int64_t value) const
    {
        return B->getInt64((uint64_t)value);
    }

    llvm::ConstantInt *create_isize(size_t value) const
    {
        return llvm::ConstantInt::get(B->getIntPtrTy(M->get_data_layout()), value);
    }

    llvm::Constant *create_f32(paw_Float32 value) const
    {
        return llvm::ConstantFP::get(get_f32_ty(), value);
    }

    llvm::Constant *create_f64(paw_Float64 value) const
    {
        return llvm::ConstantFP::get(get_f64_ty(), value);
    }

    // Return the type of an opaque pointer
    llvm::PointerType *get_ptr_ty() const
    {
        return llvm::PointerType::getUnqual(*ctx_);
    }

    // Return the type of a statically-sized array
    llvm::ArrayType *get_array_ty(llvm::Type *type, uint64_t length) const
    {
        return llvm::ArrayType::get(type, length);
    }

    llvm::Type *get_void_ty() const
    {
        return B->getVoidTy();
    }

    llvm::Type *get_unit_ty() const
    {
        // TODO: ZSTs
        return get_i8_ty();
//        return llvm::StructType::get(*ctx_, {}, false);
    }

    llvm::IntegerType *get_bool_ty() const
    {
        return get_i1_ty();
    }

    llvm::IntegerType *get_char_ty() const
    {
        return get_i8_ty();
    }

    llvm::Type *get_f32_ty() const
    {
        return B->getFloatTy();
    }

    llvm::Type *get_f64_ty() const
    {
        return B->getDoubleTy();
    }

    llvm::Type *get_isize_ty() const
    {
        return M->get_data_layout().getIntPtrType(get_ptr_ty());
    }

    llvm::StructType *get_fatptr_ty() const
    {
        return llvm::StructType::get(*ctx_, {
                get_ptr_ty(), // ptr
                get_isize_ty(), // len
            }, false);
    }

    llvm::StructType *get_str_ty() const
    {
        return get_fatptr_ty();
    }

    llvm::StructType *get_slice_ty() const
    {
        return get_fatptr_ty();
    }

    llvm::IntegerType *get_i1_ty() const
    {
        return B->getInt1Ty();
    }

    llvm::IntegerType *get_i8_ty() const
    {
        return B->getInt8Ty();
    }

    llvm::IntegerType *get_i16_ty() const
    {
        return B->getInt16Ty();
    }

    llvm::IntegerType *get_i32_ty() const
    {
        return B->getInt32Ty();
    }

    llvm::IntegerType *get_i64_ty() const
    {
        return B->getInt64Ty();
    }

    llvm::IntegerType *get_sized_int_ty(unsigned num_bytes) const
    {
        return B->getIntNTy(num_bytes);
    }

    UnitType *get_unit_type() const { return &scalar_types_.u; }
    BoolType *get_bool_type() const { return &scalar_types_.b; }
    CharType *get_char_type() const { return &scalar_types_.c; }
    IntType *get_int_type(IntKind kind) const { return &scalar_types_.i[(size_t)kind]; }
    FloatType *get_float_type(FloatKind kind) const { return &scalar_types_.f[(size_t)kind]; }
    StrType *get_str_type() const { return &scalar_types_.s; }

    SliceType *get_slice_type(Type *element_type);
    ArrayType *get_array_type(Type *element_type, uint64_t length);

    // TODO: rename to get_enum_type
    ObjectType *get_object_type(llvm::ArrayRef<ObjectType::FieldTypes> field_types);

    ObjectType *get_tuple_type(llvm::ArrayRef<Type *> field_types)
    {
        return get_struct_type(field_types);
    }

    ObjectType *get_struct_type(llvm::ArrayRef<Type *> field_types)
    {
        return get_object_type({field_types});
    }

    FnType *get_fn_type(
            Type *return_type,
            llvm::ArrayRef<Type *> param_types,
            Type *env_type,
            bool never_returns);

    PtrType *get_ptr_type(Type *pointee_type = nullptr);

    // Emit code to build a stack-allocated string out of a C-style string
    // The lifetime of the returned string is the same as that of the current
    // function (at most). Note that the ".hash" field is not filled out. The
    // returned "str" should only be used for printing error messages.
    llvm::Value *create_char_slice(StringView view) const
    {
        paw_assert(view.length < 100); // short strings only
        auto *message = llvm::ConstantDataArray::getString(
                *ctx_, llvm::StringRef(view.data, view.length), true);
        auto *storage = new llvm::GlobalVariable(
                        **M, get_ptr_ty(), false,
                        llvm::GlobalValue::PrivateLinkage,
                        create_null_ptr(), "temp_str");
        B->CreateStore(message, storage);
        temp_strings_.push_back(0);

        llvm::Value *slice = llvm::UndefValue::get(get_slice_ty());
        slice = B->CreateInsertValue(slice, storage, 0);
        slice = B->CreateInsertValue(slice, create_isize(view.length), 1);
        return slice;
    }

    llvm::DIFile *get_difile(int modno)
    {
        return difiles_.at(unsigned(modno));
    }

private:
    // Private constructor used in "clone" method
    explicit Context(Context const &rhs, std::unique_ptr<llvm::Module> mod);

    template<class DerivedType>
    DerivedType *intern_type(DerivedType const *type)
    {
        auto itr = types_.find(type);
        if (itr == end(types_)) {
            auto p = std::unique_ptr<Type>(new DerivedType(*type));
            itr = types_.emplace(p.get(), std::move(p)).first;
        }
        return (DerivedType *)itr->second.get();
    }

    llvm::LLVMContext *ctx_;
    std::unique_ptr<llvm::IRBuilder<>> B;
    std::unique_ptr<Module> M;
    Compiler *C;

    mutable std::vector<void *> temp_strings_;
    std::unique_ptr<llvm::DIBuilder> DI;
    llvm::DICompileUnit *dcu_;
    std::vector<llvm::DIFile *> difiles_;

    std::string modname_;
    CodegenOptions options_;

    using TypeInterner = std::unordered_map<
        Type const *,
        std::unique_ptr<Type>,
        decltype(hash_type) *,
        decltype(type_equals) *>;

    TypeInterner types_{0 /* buckets */, hash_type, type_equals};

    struct ScalarTypes {
        UnitType u;
        BoolType b;
        CharType c;
        IntType i[NUM_INT_KINDS];
        FloatType f[NUM_FLOAT_KINDS];
        StrType s;
    } mutable scalar_types_;
};


#define CG_PANIC_STRING(Ctx_, Text_) (Ctx_).create_panic(CG_STRING(Text_))
#define CG_PRINT_STRING(Ctx_, Text_) (Ctx_).create_print(CG_STRING(Text_))
#define CG_PRINTLN_STRING(Ctx_, Text_) (Ctx_).create_println(CG_STRING(Text_))
#define CG_PANIC_LITERAL(Ctx_, Text_) (Ctx_).create_panic(CG_LITERAL(Text_))
#define CG_PRINT_LITERAL(Ctx_, Text_) (Ctx_).create_print(CG_LITERAL(Text_))
#define CG_PRINTLN_LITERAL(Ctx_, Text_) (Ctx_).create_println(CG_LITERAL(Text_))

} // namespace paw::cg

#endif // PAW_CODEGEN_CONTEXT_H


