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
        return B->CreateCall(fn, {bytes, length, create_i32(0)});
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

    void create_memset(MaybeAlignedValue ptr, llvm::Value *value, paw_Int size, bool is_volatile = false)
    {
        create_memset(ptr, value, create_int(size), is_volatile);
    }

    void create_memset(MaybeAlignedValue ptr, llvm::Value *value, llvm::Value *size, bool is_volatile = false)
    {
        B->CreateMemSet(ptr.value(), value, size, ptr.align(), is_volatile);
    }

    void create_memmove(MaybeAlignedValue dest, MaybeAlignedValue src, paw_Int size, bool is_volatile = false)
    {
        create_memmove(dest, src, create_int(size), is_volatile);
    }

    void create_memmove(MaybeAlignedValue dest, MaybeAlignedValue src, llvm::Value *size, bool is_volatile = false)
    {
        B->CreateMemMove(dest.value(), dest.align(),
                src.value(), src.align(), size, is_volatile);
    }

    void create_memcpy(MaybeAlignedValue dest, MaybeAlignedValue src, paw_Int size, bool is_volatile = false)
    {
        create_memcpy(dest, src, create_int(size), is_volatile);
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
            llvm::FunctionType::get(get_i64_ty(),
                {get_ptr_ty()}, false));
    }

    llvm::Value *call_strlen(llvm::Value *str) const
    {
        auto *len = B->CreateCall(get_strlen_callee(), {str});
        return B->CreateSExt(len, get_int_ty());
    }

    llvm::FunctionCallee get_exit_callee() const
    {
        return M->get_module()->getOrInsertFunction("exit",
            llvm::FunctionType::get(get_void_ty(),
                {get_i32_ty()}, false));
    }

    llvm::FunctionCallee get_rawcmp_callee() const
    {
        return M->get_module()
            ->getFunction(get_builtin_name(BuiltinFn::RAWCMP));

    }

    llvm::Value *create_alloc(llvm::Value *size)
    {
        auto *fn = M->get_builtin(BuiltinFn::PAW_ALLOC);
        return B->CreateCall(fn, {size});
    }

    llvm::Value *create_alloc(paw_Int size)
    {
        return create_alloc(create_int(size));
    }

    llvm::Value *create_alloc(llvm::Type *ty)
    {
        return create_alloc(stride_of(ty));
    }

    llvm::Value *create_dealloc(llvm::Value *ptr)
    {
        auto *fn = M->get_builtin(BuiltinFn::PAW_DEALLOC);
        return B->CreateCall(fn, {ptr});
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

    // Source: https://graphics.stanford.edu/~seander/bithacks.html#RoundUpPowerOf2
    // Modified to operate on 64-bit integers (requires 1 additional shift). Uses
    // the suggestion to fix the edge case at 0.
    llvm::Value *create_next_pow2(llvm::Value *v)
    {
        v = B->CreateSub(v, create_int(1));
        v = B->CreateOr(v, B->CreateLShr(v, create_int(1)));
        v = B->CreateOr(v, B->CreateLShr(v, create_int(2)));
        v = B->CreateOr(v, B->CreateLShr(v, create_int(4)));
        v = B->CreateOr(v, B->CreateLShr(v, create_int(8)));
        v = B->CreateOr(v, B->CreateLShr(v, create_int(16)));
        v = B->CreateOr(v, B->CreateLShr(v, create_int(32)));
        v = B->CreateAdd(v, create_int(1));

        auto *eq0 = B->CreateCmp(llvm::CmpInst::ICMP_EQ, v, create_int(0));
        return B->CreateAdd(v, B->CreateZExt(eq0, get_int_ty()));
    }

    llvm::Value *create_gep(llvm::Type *element_ty, llvm::Value *array, llvm::Value *index)
    {
        return B->CreateInBoundsGEP(element_ty, array, {index});
    }

    llvm::Value *create_gep(llvm::Type *element_ty, llvm::Value *array, paw_Int index)
    {
        return create_gep(element_ty, array, create_int(index));
    }

    llvm::Value *create_getter(llvm::Type *element_ty, llvm::Value *array, llvm::Value *index)
    {
        auto *element_ptr = create_gep(element_ty, array, index);
        return B->CreateLoad(element_ty, element_ptr);
    }

    llvm::Value *create_setter(llvm::Type *element_ty, llvm::Value *array, llvm::Value *index, llvm::Value *element)
    {
        auto *element_ptr = create_gep(element_ty, array, index);
        B->CreateStore(element, element_ptr);
        return element_ptr;
    }

    llvm::Value *create_cast(llvm::Value *target, BuiltinKind from, BuiltinKind to)
    {
        paw_assert(from != BUILTIN_UNIT && IS_SCALAR_TYPE(from));
        paw_assert(to != BUILTIN_UNIT && IS_SCALAR_TYPE(to));

        switch (from) {
            case BUILTIN_BOOL:
                if (to == BUILTIN_CHAR) {
                    return B->CreateZExt(target, get_char_ty());
                } else if (to == BUILTIN_INT) {
                    return B->CreateZExt(target, get_int_ty());
                } else { // to == BUILTIN_FLOAT
                    auto *temp = B->CreateZExt(target, get_int_ty());
                    return B->CreateSIToFP(temp, get_float_ty());
                }
            case BUILTIN_CHAR:
                if (to == BUILTIN_BOOL) {
                    return B->CreateCmp(llvm::CmpInst::ICMP_NE, target,
                            create_char(0));
                } else if (to == BUILTIN_INT) {
                    return B->CreateZExt(target, get_int_ty());
                } else { // to == BUILTIN_FLOAT
                    auto *temp = B->CreateZExt(target, get_int_ty());
                    return B->CreateSIToFP(temp, get_float_ty());
                }
            case BUILTIN_INT:
                if (to == BUILTIN_BOOL) {
                    return B->CreateCmp(llvm::CmpInst::ICMP_NE, target,
                            create_int(0));
                } else if (to == BUILTIN_CHAR) {
                    return B->CreateTrunc(target, get_char_ty());
                } else { // to == BUILTIN_FLOAT
                    return B->CreateSIToFP(target, get_float_ty());
                }
            default: // from == BUILTIN_FLOAT
                if (to == BUILTIN_BOOL) {
                    return B->CreateCmp(llvm::CmpInst::FCMP_ONE, target,
                            create_float(0.0));
                } else if (to == BUILTIN_CHAR) {

                    return B->CreateFPToSI(target, get_char_ty());
                } else { // to == BUILTIN_INT
                    return B->CreateFPToSI(target, get_int_ty());
                }
        }
    }

    llvm::Value *create_iadd1(llvm::Value *value)
    {
        return B->CreateAdd(value, create_int(1));
    }

    llvm::Value *create_isub1(llvm::Value *value)
    {
        return B->CreateSub(value, create_int(1));
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

    llvm::ConstantInt *create_uint(paw_Uint value) const
    {
        return B->getInt64(value);
    }

    llvm::ConstantInt *create_int(paw_Int value) const
    {
        return create_i64(value);
    }

    llvm::ConstantInt *create_i1(bool value) const
    {
        return B->getInt1(value);
    }

    llvm::ConstantInt *create_i8(int8_t value) const
    {
        return B->getInt8((uint8_t)value);
    }

    llvm::ConstantInt *create_i32(int32_t value) const
    {
        return B->getInt32((uint32_t)value);
    }

    llvm::ConstantInt *create_i64(int64_t value) const
    {
        return B->getInt64((uint64_t)value);
    }

    llvm::IntegerType *get_iptr_ty() const
    {
        return B->getIntPtrTy(M->get_data_layout());
    }

    llvm::ConstantInt *create_iptr(uint64_t iptr) const
    {
        return llvm::ConstantInt::get(get_iptr_ty(), iptr);
    }

    llvm::Value *load_int(llvm::Value *int_ptr) const
    {
        return load_value(get_int_ty(), int_ptr);
    }

    llvm::Value *load_ptr(llvm::Value *ptr_ptr) const
    {
        return load_value(get_ptr_ty(), ptr_ptr);
    }

    llvm::Value *load_value(llvm::Type *ty, llvm::Value *ptr) const
    {
        paw_assert(ty && ptr);
        return B->CreateLoad(ty, ptr);
    }

    void store_value(llvm::Value *value, llvm::Value *ptr) const
    {
        paw_assert(value && ptr);
        B->CreateStore(value, ptr);
    }

    llvm::IntegerType *get_index_ty() const
    {
        return B->getIndexTy(M->get_data_layout(), 0);
    }

    llvm::ConstantInt *create_index(uint64_t index) const
    {
        return llvm::ConstantInt::get(get_index_ty(), index);
    }

    llvm::Constant *create_float(paw_Float value) const
    {
        return llvm::ConstantFP::get(get_float_ty(), value);
    }

    llvm::Value *create_array_gep(llvm::Type *element_ty, llvm::Value *array, llvm::Value *index)
    {
        return B->CreateInBoundsGEP(element_ty, array, {index});
    }

    llvm::Value *create_array_gep(llvm::Type *element_ty, llvm::Value *array, paw_Int index)
    {
        return create_array_gep(element_ty, array, create_int(index));
    }

    llvm::Value *create_array_get(llvm::Type *element_ty, llvm::Value *array, llvm::Value *index)
    {
        auto *element_ptr = create_array_gep(element_ty, array, index);
        return B->CreateLoad(element_ty, element_ptr);
    }

    llvm::Value *create_array_set(llvm::Type *element_ty, llvm::Value *array, llvm::Value *index, llvm::Value *element)
    {
        auto *element_ptr = create_array_gep(element_ty, array, index);
        B->CreateStore(element, element_ptr);
        return element_ptr;
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

    llvm::Type *get_float_ty() const
    {
        return B->getDoubleTy();
    }

    llvm::IntegerType *get_int_ty() const
    {
        return get_i64_ty();
    }

    llvm::StructType *get_fatptr_ty() const
    {
        return llvm::StructType::get(*ctx_, {
                get_ptr_ty(), // ptr
                get_int_ty(), // len
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

    llvm::StructType *get_list_ty() const
    {
        return llvm::StructType::get(*ctx_, {
                get_ptr_ty(), // data
                get_int_ty(), // length
                get_int_ty(), // capacity
            }, false);
    }

    llvm::StructType *get_map_ty() const
    {
        return llvm::StructType::get(*ctx_, {
                get_ptr_ty(), // data
                get_int_ty(), // length
                get_int_ty(), // capacity
            }, false);
    }

    llvm::StructType *get_callable_ty() const
    {
        return llvm::StructType::get(*ctx_, {
                get_ptr_ty(), // callee
                get_ptr_ty(), // env
            }, false);
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
    IntType *get_int_type() const { return &scalar_types_.i; }
    Int32Type *get_int32_type() const { return &scalar_types_.i32; }
    FloatType *get_float_type() const { return &scalar_types_.f; }
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
        slice = B->CreateInsertValue(slice, create_uint(view.length), 1);
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
        IntType i;
        Int32Type i32;
        FloatType f;
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


