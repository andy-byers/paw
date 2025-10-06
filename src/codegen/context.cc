// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "context.h"
#include "ir_type.h"

namespace paw::cg {

static constexpr char const *BUILTIN_NAMES[(size_t)BuiltinFn::NUM_BUILTINS] = {
    "GC_init",
    "GC_malloc",
    "GC_free",
    "paw_builtin_ckd_imul",
    "paw_builtin_ckd_iadd",
    "paw_builtin_bkpt",
    "paw_builtin_hash_bytes",
    "paw_builtin_abs_index",
    "paw_builtin_rawcmp",
    "paw_prelude_panic",
    "paw_prelude_print",
    "paw_prelude_println",
};

char const *get_builtin_name(BuiltinFn kind)
{
    return BUILTIN_NAMES[(unsigned)kind];
}


static std::vector<llvm::DIFile *> create_di_modules(Compiler *C, llvm::DIBuilder &DI)
{
    std::vector<llvm::DIFile *> difiles;
    difiles.reserve(unsigned(C->modinfo->count));

    ::Module const *pmod;
    K_LIST_FOREACH (C->modinfo, pmod) {
        difiles.push_back(DI.createFile(
                to_string(pmod->name) + PAW_MODULE_EXT,
                to_string(pmod->dirname)));
    }

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
        .i = IntType(*this),
        .i32 = Int32Type(*this),
        .f = FloatType(*this),
        .s = StrType(*this),
    }
{
    scalar_types_.u.set_dity(DI->createBasicType("int", 0, llvm::dwarf::DW_ATE_signed));
    scalar_types_.b.set_dity(DI->createBasicType("int", 1, llvm::dwarf::DW_ATE_signed));
    scalar_types_.c.set_dity(DI->createBasicType("int", 8, llvm::dwarf::DW_ATE_signed));
    scalar_types_.i.set_dity(DI->createBasicType("int", 64, llvm::dwarf::DW_ATE_signed));
    scalar_types_.i32.set_dity(DI->createBasicType("int", 32, llvm::dwarf::DW_ATE_signed));
    scalar_types_.f.set_dity(DI->createBasicType("int", 64, llvm::dwarf::DW_ATE_float));
    scalar_types_.s.set_dity(DI->createStringType("int", 8));
}

Context::Context(Context const &rhs, std::unique_ptr<llvm::Module> mod)
    : ctx_(rhs.ctx_)
    , B(std::make_unique<llvm::IRBuilder<>>(*ctx_))
    , M(std::unique_ptr<Module>(new Module(*this, std::move(mod))))
    , C(rhs.C)
    , options_(rhs.options_)
    , scalar_types_{
        .u = UnitType(*this),
        .b = BoolType(*this),
        .c = CharType(*this),
        .i = IntType(*this),
        .i32 = Int32Type(*this),
        .f = FloatType(*this),
        .s = StrType(*this),
    }
{
    scalar_types_.u.set_dity(rhs.scalar_types_.u.get_dity());
    scalar_types_.b.set_dity(rhs.scalar_types_.b.get_dity());
    scalar_types_.c.set_dity(rhs.scalar_types_.c.get_dity());
    scalar_types_.i.set_dity(rhs.scalar_types_.i.get_dity());
    scalar_types_.i32.set_dity(rhs.scalar_types_.i32.get_dity());
    scalar_types_.f.set_dity(rhs.scalar_types_.f.get_dity());
    scalar_types_.s.set_dity(rhs.scalar_types_.s.get_dity());
}

std::unique_ptr<Context> Context::clone() const
{
    return std::unique_ptr<Context>(new Context(*this, llvm::CloneModule(**M)));
}

ListType *Context::get_list_type(Type *element_type)
{
    ListType list_type(*this, element_type);
    return (ListType *)intern_type(&list_type);
}

MapType *Context::get_map_type(Type *key_type, Type *value_type)
{
    MapType map_type(*this, key_type, value_type);
    return (MapType *)intern_type(&map_type);
}

ObjectType *Context::get_object_type(llvm::ArrayRef<ObjectType::FieldTypes> field_types,
        ObjectType::Location location)
{
    ObjectType object_type(*this, field_types, location);
    return (ObjectType *)intern_type(&object_type);
}

ObjectType *Context::get_named_type(std::string name, ObjectType::Location location)
{
    ObjectType object_type(*this, std::move(name), location);
    return (ObjectType *)intern_type(&object_type);
}

FnType *Context::get_fn_type(Type *return_type, llvm::ArrayRef<Type *> param_types,
        FnKind fn_kind, bool has_env, bool never_returns)
{
    FnType fn_type(*this, return_type, param_types, fn_kind, has_env, never_returns);
    return (FnType *)intern_type(&fn_type);
}

PtrType *Context::get_ptr_type(Type *pointee_type)
{
    PtrType ptr_type(*this, pointee_type);
    return (PtrType *)intern_type(&ptr_type);
}


Module::Module(Context &X, std::string name)
    : X(&X)
    , name_(name)
    , M(std::make_unique<llvm::Module>(name, *X.get_context()))
{
}

Module::Module(Context &X, std::unique_ptr<llvm::Module> m)
    : X(&X)
    , name_(m->getName())
    , M(std::move(m))
{
}

llvm::Function *Module::get_builtin(BuiltinFn kind) const
{
    return M->getFunction(get_builtin_name(kind));
}

} // namespace paw::cg
