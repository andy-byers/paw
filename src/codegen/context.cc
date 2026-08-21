// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "context.h"
#include "../value.h"

namespace paw::cg {

std::string to_string(::Str const *str)
{
    return {str->text, str->length};
}

static constexpr char const *BUILTIN_NAMES[(size_t)BuiltinFn::NUM_BUILTINS] = {
    "paw_mem_raw_alloc",
    "paw_mem_raw_realloc",
    "paw_mem_raw_dealloc",
    "paw_mem_raw_aligned_alloc",
    "paw_builtin_bkpt",
    "_PN3ops18builtin_hash_bytes",
    "paw_builtin_check_bounds",
    "_PN3ops14builtin_rawcmp",
    "paw_panic_handler",
    "_PN7prelude5printIScE",
    "_PN7prelude7printlnIScE",
};

char const *get_builtin_name(BuiltinFn kind)
{
    paw_assert(kind < BuiltinFn::NUM_BUILTINS);
    return BUILTIN_NAMES[(unsigned)kind];
}


static std::vector<llvm::DIFile *> create_di_modules(Compiler *C, llvm::DIBuilder &DI)
{
    std::vector<llvm::DIFile *> difiles;
//    difiles.reserve(unsigned(C->modinfo->count));
//
//    ::Module const *pmod;
//    K_LIST_FOREACH (C->modinfo, pmod) {
//        difiles.push_back(DI.createFile(
//                to_string(pmod->name) + PAW_MODULE_EXT,
//                to_string(pmod->dirname)));
//    }

    return difiles;
}

Context::Context(llvm::LLVMContext &ctx, Compiler &compiler, std::string name, CodegenOptions cgopt)
    : ctx_(&ctx)
    , B(std::make_unique<llvm::IRBuilder<>>(ctx))
    , M(std::make_unique<Module>(*this, name))
    , C(&compiler)
    , DI(std::make_unique<llvm::DIBuilder>(*M->get_module()))
    , difiles_(create_di_modules(C, *DI))
    , options_(cgopt)
    , scalar_types_{
        .u = UnitType(*this),
        .b = BoolType(*this),
        .c = CharType(*this),
        .i = {
            IntType(*this, IntKind::INT8),
            IntType(*this, IntKind::INT16),
            IntType(*this, IntKind::INT32),
            IntType(*this, IntKind::INT64),
            IntType(*this, IntKind::ISIZE),
            IntType(*this, IntKind::UINT8),
            IntType(*this, IntKind::UINT16),
            IntType(*this, IntKind::UINT32),
            IntType(*this, IntKind::UINT64),
            IntType(*this, IntKind::USIZE),
        },
        .f = {
            FloatType(*this, FloatKind::FLOAT32),
            FloatType(*this, FloatKind::FLOAT64),
        },
        .s = StrType(*this),
    }
{
    scalar_types_.u.set_dity(DI->createBasicType("int", 0, llvm::dwarf::DW_ATE_signed));
    scalar_types_.b.set_dity(DI->createBasicType("int", 1, llvm::dwarf::DW_ATE_signed));
    scalar_types_.c.set_dity(DI->createBasicType("int", 8, llvm::dwarf::DW_ATE_signed));
    scalar_types_.i[size_t(IntKind::INT8)].set_dity(DI->createBasicType("int8", 8, llvm::dwarf::DW_ATE_signed));
    scalar_types_.i[size_t(IntKind::INT16)].set_dity(DI->createBasicType("int16", 16, llvm::dwarf::DW_ATE_signed));
    scalar_types_.i[size_t(IntKind::INT32)].set_dity(DI->createBasicType("int32", 32, llvm::dwarf::DW_ATE_signed));
    scalar_types_.i[size_t(IntKind::INT64)].set_dity(DI->createBasicType("int64", 64, llvm::dwarf::DW_ATE_signed));
    scalar_types_.i[size_t(IntKind::ISIZE)].set_dity(DI->createBasicType("isize", 64, llvm::dwarf::DW_ATE_signed));
    scalar_types_.i[size_t(IntKind::UINT8)].set_dity(DI->createBasicType("uint8", 8, llvm::dwarf::DW_ATE_unsigned));
    scalar_types_.i[size_t(IntKind::UINT16)].set_dity(DI->createBasicType("uint16", 16, llvm::dwarf::DW_ATE_unsigned));
    scalar_types_.i[size_t(IntKind::UINT32)].set_dity(DI->createBasicType("uint32", 32, llvm::dwarf::DW_ATE_unsigned));
    scalar_types_.i[size_t(IntKind::UINT64)].set_dity(DI->createBasicType("uint64", 64, llvm::dwarf::DW_ATE_unsigned));
    scalar_types_.i[size_t(IntKind::USIZE)].set_dity(DI->createBasicType("usize", 64, llvm::dwarf::DW_ATE_unsigned));
    scalar_types_.f[size_t(FloatKind::FLOAT32)].set_dity(DI->createBasicType("float32", 32, llvm::dwarf::DW_ATE_float));
    scalar_types_.f[size_t(FloatKind::FLOAT64)].set_dity(DI->createBasicType("float64", 64, llvm::dwarf::DW_ATE_float));
    scalar_types_.s.set_dity(DI->createStringType("int", 8));
}

Context::Context(Context const &rhs, std::unique_ptr<llvm::Module> mod)
    : ctx_(rhs.ctx_)
    , B(std::make_unique<llvm::IRBuilder<>>(*ctx_))
    , M(std::unique_ptr<Module>(new Module(std::move(mod))))
    , C(rhs.C)
    , options_(rhs.options_)
    , scalar_types_{
        .u = UnitType(*this),
        .b = BoolType(*this),
        .c = CharType(*this),
        .i = {
            IntType(*this, IntKind::INT8),
            IntType(*this, IntKind::INT16),
            IntType(*this, IntKind::INT32),
            IntType(*this, IntKind::INT64),
            IntType(*this, IntKind::ISIZE),
            IntType(*this, IntKind::UINT8),
            IntType(*this, IntKind::UINT16),
            IntType(*this, IntKind::UINT32),
            IntType(*this, IntKind::UINT64),
            IntType(*this, IntKind::USIZE),
        },
        .f = {
            FloatType(*this, FloatKind::FLOAT32),
            FloatType(*this, FloatKind::FLOAT64),
        },
        .s = StrType(*this),
    }
{
    scalar_types_.u.set_dity(rhs.scalar_types_.u.get_dity());
    scalar_types_.b.set_dity(rhs.scalar_types_.b.get_dity());
    scalar_types_.c.set_dity(rhs.scalar_types_.c.get_dity());
    for (size_t i = 0; i < NUM_INT_KINDS; ++i)
        scalar_types_.i[i].set_dity(rhs.scalar_types_.i[i].get_dity());
    for (size_t i = 0; i < NUM_FLOAT_KINDS; ++i)
        scalar_types_.f[i].set_dity(rhs.scalar_types_.f[i].get_dity());
    scalar_types_.s.set_dity(rhs.scalar_types_.s.get_dity());
}

std::unique_ptr<Context> Context::clone() const
{
    return std::unique_ptr<Context>(new Context(*this, llvm::CloneModule(**M)));
}

SliceType *Context::get_slice_type(Type *element_type)
{
    SliceType slice_type(*this, element_type);
    return (SliceType *)intern_type(&slice_type);
}

ArrayType *Context::get_array_type(Type *element_type, uint64_t length)
{
    ArrayType array_type(*this, element_type, length);
    return (ArrayType *)intern_type(&array_type);
}

ObjectType *Context::get_object_type(llvm::ArrayRef<ObjectType::FieldTypes> field_types)
{
    ObjectType object_type(*this, field_types);
    return (ObjectType *)intern_type(&object_type);
}

FnType *Context::get_fn_type(Type *return_type, llvm::ArrayRef<Type *> param_types,
        Type *env_type, bool never_returns)
{
    FnType fn_type(*this, return_type, param_types, env_type, never_returns);
    return (FnType *)intern_type(&fn_type);
}

PtrType *Context::get_ptr_type(Type *pointee_type)
{
    PtrType ptr_type(*this, pointee_type);
    return (PtrType *)intern_type(&ptr_type);
}


Module::Module(Context &X, std::string name)
    : name_(name)
    , M(std::make_unique<llvm::Module>(name, *X.get_context()))
{
}

Module::Module(std::unique_ptr<llvm::Module> m)
    : name_(m->getName())
    , M(std::move(m))
{
}

llvm::Function *Module::get_builtin(BuiltinFn kind) const
{
    return M->getFunction(get_builtin_name(kind));
}

} // namespace paw::cg
