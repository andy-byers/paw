// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// TODO: Prevent reference arguments from being captured in closures
// TODO: Restrict reference parameters so they cannot be bound to container elements, including recursive (inline)
// TODO:     sub-fields of container elements. Otherwise it is possible to have a dangling reference after the container
// TODO:     has been resized. Boehm GC should prevent the use-after-free, but it would still cause unexpected behavior.
// TODO:     Could also use "fat" pointers containing a tag, maybe a pointer-to-container and index/key, or a pointer-to-
// TODO:     local variable.
// TODO: For enum, use strictest alignment among all variants

#include <algorithm>
#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <functional>
#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

#include <llvm/Analysis/ConstantFolding.h>
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
#include <llvm/Support/raw_os_ostream.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>
#include <llvm/TargetParser/Host.h>
#include <llvm/Transforms/Instrumentation/AddressSanitizer.h>
#include <llvm/Transforms/Instrumentation/SanitizerCoverage.h>
#include <llvm/Transforms/Utils/Cloning.h>
#include <llvm/Transforms/Utils/ModuleUtils.h>

#include "codegen.h"
#include "glue.h"
#include "ir_type.h"
#include "mir.h"

#include "context.h"
#include "linker.h"
#include "state.h"
#include "type.h"


namespace paw::cg {

template<class Ty>
static Ty *cast(Type *type)
{
    if (std::is_same_v<Ty, UnitType>) {
        paw_assert(type->is_unit_type());
    } else if (std::is_same_v<Ty, BoolType>) {
        paw_assert(type->is_bool_type());
    } else if (std::is_same_v<Ty, CharType>) {
        paw_assert(type->is_char_type());
    } else if (std::is_same_v<Ty, IntType>) {
        paw_assert(type->is_int_type());
    } else if (std::is_same_v<Ty, FloatType>) {
        paw_assert(type->is_float_type());
    } else if (std::is_same_v<Ty, StrType>) {
        paw_assert(type->is_str_type());
    } else if (std::is_same_v<Ty, ListType>) {
        paw_assert(type->is_list_type());
    } else if (std::is_same_v<Ty, MapType>) {
        paw_assert(type->is_map_type());
    } else if (std::is_same_v<Ty, FnType>) {
        paw_assert(type->is_fn_type());
    } else if (std::is_same_v<Ty, ObjectType>) {
        paw_assert(type->is_object_type());
    }
    return (Ty *)type;
}

static bool is_empty_irtype(Compiler *C, IrType *irtype)
{
    if (!IrIsAdt(irtype)) return false;
    auto const kind = pawP_type2code(C, irtype);
    if (kind == BUILTIN_UNIT) return true;
    if (IS_BUILTIN_TYPE(kind)) return false;

    auto const *def = pawIr_get_adt_def(C, IR_TYPE_DID(irtype));
    if (!def->is_struct) return false;
    if (!def->is_inline) return false;

    paw_assert(def->variants->count == 1);
    auto const *variant = def->variants->data[0];
    return variant->fields->count == 0;
}

static ::Str *core_name(struct Compiler *C, IrType *type)
{
    switch (IR_KINDOF(type)) {
        case kIrUnit:
            return SCAN_STR(C, "unit");
        case kIrBool:
            return SCAN_STR(C, "bool");
        case kIrChar:
            return SCAN_STR(C, "char");
        case kIrInt:
            return SCAN_STR(C, "int");
        case kIrFloat:
            return SCAN_STR(C, "float");
        case kIrStr:
            return SCAN_STR(C, "str");
        case kIrTuple:
            // TODO: redo name mangling
            return pawP_format_string(C, "tuple%d", IrGetTuple(type)->elems->count);
        default:
            return pawIr_get_adt_def(C, IR_TYPE_DID(type))->name;
    }
}

// TODO: Ensure mangling produces a unique name.
static std::string mangle_fn_name(Compiler *C, ::Str const *modname, IrType *type, IrType *self)
{
    paw_assert(type->hdr.kind == kIrSignature);
    auto const fsig = *IrGetSignature(type);

    auto const *fdef = pawIr_get_fn_def(C, fsig.did);
    if (self == nullptr) return to_string(pawP_mangle_name(C, modname, fdef->name, fsig.types));
    if (IrIsAdt(self)) {
        auto const *adef = pawIr_get_adt_def(C, IR_TYPE_DID(self));

        IrTypeList *types = NULL;
        if (fdef->generics != NULL) {
            types = IrTypeList_new(C);
            IrTypeList_reserve(C, types, fdef->generics->count);
            paw_assert(fsig.types->count >= fdef->generics->count);
            IrType *const *p = &K_LIST_AT(fsig.types, fsig.types->count - fdef->generics->count);
            while (p != K_LIST_END(fsig.types)) IrTypeList_push(C, types, *p++);
        }

        return to_string(pawP_mangle_attr(C, modname, adef->name, IR_TYPE_SUBTYPES_(self), fdef->name, types));
    }
    auto *self_name = core_name(C, self);
    return to_string(pawP_mangle_attr(C, modname, self_name, NULL, fdef->name, NULL));
}

static std::string mangle_adt_name(Compiler *C, ::Str const *modname, IrType *type)
{
    paw_assert(type->hdr.kind == kIrAdt);
    auto const adt = type->Adt_;

    auto const *def = pawIr_get_adt_def(C, adt.did);
    return to_string(pawP_mangle_name(C, modname, def->name, adt.types));
}

static std::string format_core_name(char const *mod, char const *self, char const *item)
{
    char buffer[4096] = "";
    if (self != nullptr) {
        strcpy(buffer + 1, self);
        buffer[0] = '_';
    }
    // format name as "paw_mod[_self]_item"
    std::string const PREFIX = "paw_";
    return PREFIX + mod + buffer + "_" + item;
}

static std::string mangle_mir_name(Mir const *mir)
{
    if (mir->fn_kind == FUNC_CLOSURE)
        return "closure";

    auto const *modname = ModuleInfo_get(mir->C->modinfo, mir->modno).name;

    Annotation annotation;
    if (pawP_check_extern(mir->C, mir->annotations, &annotation)) {
        if (annotation.has_value) {
            paw_assert(annotation.kind == BUILTIN_STR);
            auto const *a = (::Str const *)annotation.value.p;
            if (a->length == 1 && (a->text[0] == 'c' || a->text[0] == 'C'))
                return to_string(mir->name); // skip mangling for C functions
        }
    } else if (pawP_contains_core_annotation(mir->C, mir->annotations)) {
        char const *self = mir->self == nullptr ? nullptr
            : core_name(mir->C, mir->self)->text;
        return format_core_name(modname->text, self, mir->name->text);
    }
    return mangle_fn_name(mir->C, modname, mir->type, mir->self);
}

static std::string mangle_internal_method_name(Compiler *C, IrType *self, std::string name)
{
    auto const *modname = ModuleInfo_get(C->modinfo, (int)IR_TYPE_DID(self).modno).name;
    return mangle_adt_name(C, modname, self)
            + std::to_string(name.size())
            + name;
}

static bool has_annotation(Compiler *C, Annotations const *annotations, char const *name)
{
    if (annotations != nullptr) {
        for (int i = 0; i < annotations->count; ++i) {
            Annotation const a = annotations->data[i];
            if (pawS_eq(a.name, SCAN_STR(C, name)))
                return true;
        }
    }
    return false;
}


template<class Value>
class IrTypeHashMap {
public:
    explicit IrTypeHashMap(Compiler &C)
        : C(&C)
    {
    }

    virtual ~IrTypeHashMap() = default;

    Value &operator[](IrType *key)
    {
        return types_[key];
    }

    Value *lookup(IrType *key)
    {
        auto const itr = types_.find(key);
        return itr != end(types_)
            ? &itr->second
            : nullptr;
    }

    Value const *lookup(IrType *key) const
    {
        auto const itr = types_.find(key);
        return itr != end(types_)
            ? &itr->second
            : nullptr;
    }

    template<class V>
    void insert(IrType *key, V &&value)
    {
        paw_assert(lookup(key) == nullptr); // must be unique
        types_.emplace(key, std::forward<V>(value));
    }

private:
    Compiler *C;

    std::function<unsigned long(IrType *)> type_hash_ = [this](auto *type) {
        return pawIr_type_hash(C, type);
    };

    std::function<bool(IrType *, IrType *)> type_equals_ = [this](auto *lhs, auto *rhs) {
        return pawIr_type_equals(C, lhs, rhs);
    };

    std::unordered_map<
        IrType *,
        Value,
        decltype(type_hash_),
        decltype(type_equals_)
    > types_{0, type_hash_, type_equals_};
};


class TypeTranslator {
public:
    explicit TypeTranslator(Context &X, IrTypeHashMap<Type *> &types)
        : C(X.get_compiler())
        , X(&X)
        , types_(&types)
    {
    }

    void declare_builtin(IrType *irtype)
    {
        if (IrIsNever(irtype)) {
            types_->insert(irtype, X->get_unit_type());
            return;
        }

        Type *type;
        auto const kind = pawP_type2code(C, irtype);
        switch (kind) {
            case BUILTIN_UNIT:
                type = X->get_unit_type();
                break;
            case BUILTIN_BOOL:
                type = X->get_bool_type();
                break;
            case BUILTIN_CHAR:
                type = X->get_char_type();
                break;
            case BUILTIN_INT:
                type = X->get_int_type();
                break;
            case BUILTIN_FLOAT:
                type = X->get_float_type();
                break;
            case BUILTIN_STR:
                type = X->get_str_type();
                break;
            case BUILTIN_LIST:
                type = X->get_list_type(
                        get_or_declare_type(ir_list_elem(irtype)));
                break;
            default:
                paw_assert(kind == BUILTIN_MAP);
                type = X->get_map_type(
                        get_or_declare_type(ir_map_key(irtype)),
                        get_or_declare_type(ir_map_value(irtype)));
                break;
        }
        if (get_type(irtype) == nullptr)
            types_->insert(irtype, type);
    }

    void declare_adt(IrType *irtype)
    {
        if (get_type(irtype) == nullptr) {
            auto const did = IR_TYPE_DID(irtype);
            IrAdtDef const *def = pawIr_get_adt_def(C, did);
            auto const location = def->is_inline
                ? ObjectType::Location::STACK
                : ObjectType::Location::HEAP;
            auto const *modname = C->modinfo->data[did.modno].name;
            auto const name = mangle_adt_name(C, modname, irtype);
            auto *type = X->get_named_type(name, location);
            types_->insert(irtype, type);
        }
    }

    Type *define_adt(IrType *irtype)
    {
        auto *type = cast<ObjectType>(get_type(irtype));
        if (!type->is_opaque()) return type;

        // define ADT variants and fields
        IrAdtDef const *def = pawIr_get_adt_def(C, IR_TYPE_DID(irtype));
        std::vector<ObjectType::FieldTypes> variant_types;
        variant_types.reserve(unsigned(def->variants->count));
        if (def->is_struct) {
            define_struct_variant(irtype, variant_types);
        } else {
            define_enum_variants(irtype, variant_types);
        }

        type->set_variants(variant_types);
        // NOTE: "type" already in "types_"
        return type;
    }

    Type *define_type(IrType *irtype)
    {
        if (IrIsAdt(irtype)) {
            return define_adt_(irtype);
        } else {
            return get_type(irtype);
        }
//        switch (irtype->hdr.kind) {
//            case kIrAdt:
//                return define_adt_(irtype);
//            case kIrSignature:
//            case kIrFnPtr:
//                return get_fn_type(irtype);
//            case kIrTuple:
//                return get_tuple_type(irtype);
//            case kIrNever:
//                return get_type(irtype);
//            case kIrTraitObj:
//            case kIrGeneric:
//            case kIrInfer:
//                // these types never appear at this phase of compilation
//                PAW_UNREACHABLE();
//        }
    }


private:
    Type *get_or_declare_type(IrType *irtype)
    {
        auto *type = get_type(irtype);
        if (type == nullptr) {
            declare_builtin(irtype);
            return get_type(irtype);
        }
        return type;
    }

    Type *get_type(IrType *irtype)
    {
        if (IR_IS_FUNC_TYPE(irtype)) {
            return get_fn_type(irtype);
        } else if (IrIsTuple(irtype)) {
            return get_tuple_type(irtype);
        } else if (IrIsPtr(irtype)) {
            return get_ptr_type(irtype);
        }
        auto *const *type_ptr = types_->lookup(irtype);
        if (type_ptr == nullptr) return nullptr;
        return *type_ptr;
    }

    Type *get_fn_type(IrType *irtype)
    {
        auto *itr = types_->lookup(irtype);
        if (itr != nullptr) return *itr;
        auto *params = ir_fn_params(C, irtype);
        auto *result = ir_fn_result(C, irtype);
        auto const is_closure = !IrIsSignature(irtype);
        auto const is_method = is_method_type(irtype);
        auto *return_type = define_type(result);
        auto *type = X->get_fn_type(return_type,
                define_irtypes(params),
                is_closure ? FUNC_CLOSURE :
                    is_method ? FUNC_METHOD :
                    FUNC_FUNCTION,
                true /* has_env */,
                IrIsNever(result));
        types_->insert(irtype, type);
        return type;
    }

    Type *get_tuple_type(IrType *irtype)
    {
        auto *itr = types_->lookup(irtype);
        if (itr != nullptr) return *itr;
        auto const field_types = define_irtypes(irtype->Tuple_.elems);
        auto *type = X->get_tuple_type(field_types);
        types_->insert(irtype, type);
        return type;
    }

    Type *get_ptr_type(IrType *irtype)
    {
        auto *itr = types_->lookup(irtype);
        if (itr != nullptr) return *itr;
        auto const pointee_type = define_type(irtype->Ptr_.pointee);
        auto *type = X->get_ptr_type(pointee_type);
        types_->insert(irtype, type);
        return type;
    }

    bool is_method_type(IrType *irtype)
    {
        if (irtype->hdr.kind == kIrSignature && pawIr_get_context(C, irtype) != nullptr) {
            auto const *def = pawIr_get_fn_def(C, IR_TYPE_DID(irtype));
            if (def->params->count > 0) {
                auto const *name = def->params->data[0].name;
                return pawS_eq(name, SCAN_STR(C, "self"));
            }
        }
        return false;
    }

    Type *define_enum_type(IrType *irtype)
    {
        auto const *def = pawIr_get_adt_def(C, IR_TYPE_DID(irtype));
        auto *discr_type = define_discr_type(def->variants->count);
        paw_assert(def->variants->count > 0);

        std::vector<ObjectType::FieldTypes> variant_types(unsigned(def->variants->count));
        for (int i = 0; i < def->variants->count; ++i) {
            auto *field_irtypes = pawP_instantiate_variant_fields(C, &irtype->Adt_, i);
            auto field_types = define_irtypes(field_irtypes);
            field_types.insert(begin(field_types), discr_type);
            variant_types[unsigned(i)] = field_types;
        }

        auto const location = def->is_inline
            ? ObjectType::Location::STACK
            : ObjectType::Location::HEAP;
        return X->get_object_type(variant_types, location);
    }

    Type *define_discr_type(int num_variants)
    {
        return X->get_int_type();
        // TODO: Need some way to convey the discriminant size to code that loads
        //       and stores the discriminant. Could add sized integer types to Paw
        //       and use for discriminant (sized integer types could be hidden from
        //       the user if necessary, just exposing "int").
//        return num_variants < 0x100ULL ? get_i8_ty() :
//           num_variants < 0x10000ULL ? get_i16_ty() :
//           num_variants < 0x100000000ULL ? get_i32_ty() :
//           get_i64_ty();
    }

    Type *define_adt_(IrType *irtype)
    {
        switch (pawP_type2code(C, irtype)) {
            case BUILTIN_UNIT:
            case BUILTIN_BOOL:
            case BUILTIN_CHAR:
            case BUILTIN_INT:
            case BUILTIN_FLOAT:
            case BUILTIN_STR:
            case BUILTIN_LIST:
            case BUILTIN_MAP:
                return get_type(irtype);
            default: {
                auto *type = cast<ObjectType>(get_type(irtype));
                if (!type->is_inline()) return type; // indirection, maybe recursive
                if (!type->is_opaque()) return type; // already computed variants
                return define_adt(irtype);
            }
        }
    }

    void define_enum_variants(IrType *irtype, std::vector<ObjectType::FieldTypes> &variant_types)
    {
        auto const *def = pawIr_get_adt_def(C, IR_TYPE_DID(irtype));
        auto *discr_type = define_discr_type(def->variants->count);
        paw_assert(def->variants->count > 0);

        for (int i = 0; i < def->variants->count; ++i) {
            auto *field_irtypes = pawP_instantiate_variant_fields(C, &irtype->Adt_, i);
            auto field_types = define_irtypes(field_irtypes);
            field_types.insert(begin(field_types), discr_type);
            variant_types.push_back(field_types);
        }
    }

    void define_struct_variant(IrType *irtype, std::vector<ObjectType::FieldTypes> &variant_types)
    {
        auto const *def = pawIr_get_adt_def(C, IR_TYPE_DID(irtype));
        paw_assert(def->variants->count == 1);

        auto *field_irtypes = pawP_instantiate_struct_fields(C, &irtype->Adt_);
        auto field_types = define_irtypes(field_irtypes);
        variant_types.push_back(field_types);
    }

    std::vector<Type *> define_irtypes(IrTypeList const *irtypes)
    {
        std::vector<Type *> types(unsigned(irtypes->count));
        for (int i = 0; i < irtypes->count; ++i)
            types[unsigned(i)] = define_type(irtypes->data[i]);
        return types;
    }

    Compiler *C;
    Context *X;

    IrTypeHashMap<Type *> *types_;
};


class CodeGenerator;

struct Upvalue {
    llvm::Value *ptr;
    llvm::Value *val;
};

struct Closure {
    explicit Closure() = default;
    explicit Closure(std::unique_ptr<Fn> fn, size_t num_upvalues)
        : upvalues(num_upvalues)
        , fn(std::move(fn))
        , env_ptr(nullptr)
        , env_ty(nullptr)
    {
    }

    std::vector<Upvalue> upvalues;
    std::unique_ptr<Fn> fn;
    llvm::Value *env_ptr;
    llvm::Type *env_ty;
};


struct PhiInput {
    llvm::PHINode *phi;
    llvm::BasicBlock *b;
    struct MirPlace r;
};

class PawState final: public State {
public:
    friend class CodeGenerator;

    explicit PawState(CodeGenerator &G, Fn *fn, Mir const *mir, std::vector<Upvalue> *upvalues, PawState *outer);
    ~PawState();

    Closure *get_closure(unsigned index)
    {
        return &closures_.at(index);
    }

    llvm::Value *get_local_ptr(unsigned index);
    llvm::Value *get_upvalue_ptr(unsigned index);

private:
    CodeGenerator *G;
    llvm::IRBuilder<> *B;

    Mir const *mir_;
    MirBlockData const *current_;

    llvm::BasicBlock *before_block_;
    PawState *outer_;

    std::vector<PhiInput> phi_inputs_;

    std::vector<llvm::BasicBlock *> blocks_;
    std::vector<llvm::Value *> locals_;
    std::vector<llvm::Value *> registers_;
    std::vector<llvm::Value *> constants_;
    std::vector<llvm::Value *> captured_;
    std::vector<Upvalue> *upvalues_;
    std::vector<Closure> closures_;

    llvm::DISubprogram *disub_;
};

static void remove_function_if_exists(llvm::Module &M, std::string name)
{
    auto *fn = M.getFunction(name);
    if (fn != nullptr) fn->eraseFromParent();
}

static void fatal_error(std::string const &message)
{
    llvm::errs() << message << '\n';
    std::exit(EXIT_FAILURE);
}

template<class IR, class Target>
static void print_ir(IR const &ir, std::ostream &os)
{
    llvm::raw_os_ostream ros(os);
    ir.print(ros);
    ros.flush();
}

template<class IR>
static void print_ir(IR const &ir, std::string const &pathname)
{
    std::error_code ec;
    llvm::raw_fd_ostream ros(pathname, ec, llvm::sys::fs::OF_Text);
    if (ec) {
        fatal_error(ec.message());
    } else {
        ir.print(ros, nullptr);
    }
    ros.flush();
}

static std::string opt_name(llvm::OptimizationLevel opt)
{
    if (opt == llvm::OptimizationLevel::O1) {
        return "O1";
    } else if (opt == llvm::OptimizationLevel::O2) {
        return "O2";
    } else if (opt == llvm::OptimizationLevel::O3) {
        return "O3";
    } else if (opt == llvm::OptimizationLevel::Os) {
        return "Os";
    } else if (opt == llvm::OptimizationLevel::Oz) {
        return "Oz";
    } else {
        paw_assert(opt == llvm::OptimizationLevel::O0);
        return "O0";
    }
}

static llvm::OptimizationLevel opt_level(char suffix)
{
    switch (suffix) {
        default:  return llvm::OptimizationLevel::O0;
        case '1': return llvm::OptimizationLevel::O1;
        case '2': return llvm::OptimizationLevel::O2;
        case '3': return llvm::OptimizationLevel::O3;
        case 's': return llvm::OptimizationLevel::Os;
        case 'z': return llvm::OptimizationLevel::Oz;
    }
}

static void compile_object(Context &X, llvm::TargetMachine &machine, std::string modname, CodegenOptions options)
{
    auto *M = X.get_module();
    auto *m = M->get_module();

    if (options.verify_module && llvm::verifyModule(*m, &llvm::errs())) {
        print_ir(*m, modname + "_failure.ll");
        fatal_error("module verification failed");
    }

    llvm::PassBuilder pb;
    llvm::LoopAnalysisManager lam;
    llvm::FunctionAnalysisManager fam;
    llvm::CGSCCAnalysisManager cgam;
    llvm::ModuleAnalysisManager mam;

    pb.registerModuleAnalyses(mam);
    pb.registerCGSCCAnalyses(cgam);
    pb.registerFunctionAnalyses(fam);
    pb.registerLoopAnalyses(lam);
    pb.crossRegisterProxies(lam, fam, cgam, mam);

    auto const opt = opt_level(options.opt_suffix);
    auto mpm = pb.buildPerModuleDefaultPipeline(opt);
//TODO    if (options.enable_asan)
//TODO        mpm.addPass(llvm::AddressSanitizerPass({}));
    mpm.run(*m, mam);

    if (options.print_ir)
        print_ir(*m, modname + "_" + opt_name(opt) + ".ll");

    std::error_code ec;
    auto const pathname = modname + ".o";
    llvm::raw_fd_ostream os(pathname, ec, llvm::sys::fs::OF_None);
    if (ec) {
        llvm::errs() << "could not open file: " << ec.message();
        return;
    }

    llvm::legacy::PassManager pm;
    if (machine.addPassesToEmitFile(pm, os, nullptr,
                llvm::CodeGenFileType::ObjectFile)) {
        llvm::errs() << "TargetMachine cannot emit a file of this type\n";
        return;
    }

    pm.run(*m);
    os.flush();
}

static void generate_test_driver(Context &base, llvm::TargetMachine &machine, std::string modname, std::vector<std::string> const &test_names, CodegenOptions options)
{
    auto X = base.clone();
    auto *M = X->get_module();
    auto *B = X->get_builder();
    auto *c = X->get_context();
    auto *m = M->get_module();

    m->setModuleIdentifier(modname);

    remove_function_if_exists(*m, "paw_main");
    remove_function_if_exists(*m, "main");
    remove_function_if_exists(*m, "_PN4code4main");

    auto *main_fn = llvm::Function::Create(
            llvm::FunctionType::get(X->get_i32_ty(),
                // int argc, char **argv
                {X->get_i32_ty(), X->get_ptr_ty()}, false),
            llvm::Function::ExternalLinkage,
            "main", m);
    {
        main_fn->setDoesNotThrow();

        auto *block = llvm::BasicBlock::Create(*c, "entry", main_fn);
        B->SetInsertPoint(block);

        for (auto const &name: test_names) {
            auto *print_fn = m->getFunction("paw_prelude_println");
            B->CreateCall(print_fn, {
                        X->create_null_ptr(), // environment
                        X->create_temp_str(CG_STRING("TEST " + name)),
                    });
            B->CreateCall(m->getFunction(name),
                    X->create_null_ptr());
        }

        B->CreateRet(X->create_i32(0));
    }

    compile_object(*X, machine, modname, options);
}


class CodeGenerator final {
public:
    friend class PawState;

    explicit CodeGenerator(Compiler *C, std::string name, CodegenOptions options)
        : ctx_(std::make_unique<llvm::LLVMContext>())
        , X(*ctx_, *C, name, options)
        , M(X.get_module())
        , C(C)
        , B(X.get_builder())
        , modname_(name)
        , options_(options)
        , types_(*C)
        , fns_(*C)
        , methods_(*C)
        , list_methods_(*C)
        , map_methods_(*C)
    {
        llvm::InitializeNativeTarget();
        llvm::InitializeNativeTargetAsmPrinter();

        auto *m = X.get_module()->get_module();
        llvm::Triple const target_triple(llvm::sys::getDefaultTargetTriple());
        m->setTargetTriple(target_triple);

        std::string error;
        const auto *target = llvm::TargetRegistry::lookupTarget(target_triple, error);
        if (target == nullptr) {
            llvm::errs() << error;
            std::exit(EXIT_FAILURE);
        }

        llvm::TargetOptions target_opts;
        auto rm = std::optional<llvm::Reloc::Model>();
        machine_ = target->createTargetMachine(target_triple, "generic", "", target_opts, rm);
        m->setDataLayout(machine_->createDataLayout());
    }

    ~CodeGenerator()
    {
    }

    Context *get_context() { return &X; }
    Context const *get_context() const { return &X; }

    void define_fn(Mir const *mir)
    {
        Fn *fn;
        std::vector<Upvalue> *upvalues = nullptr;
        if (mir->fn_kind == FUNC_CLOSURE) {
            auto *closure = state_->get_closure(unsigned(mir->child_id));
            upvalues = &closure->upvalues;
            fn = closure->fn.get();
        } else {
            fn = get_fn(mir->type);
        }

        if (mir->self == nullptr && pawS_eq(mir->name, C->main_name)) {
            auto const *fptr = IrGetFnPtr(IR_GET_FN(C, mir->type));
            paw_Bool const materialize_args = fptr->params->count > 0;
            paw_Bool const materialize_return = builtin_kind(fptr->result) != BUILTIN_INT;
            create_main_fn_wrapper(*fn, materialize_args, materialize_return);
        }

        if (mir->blocks->count == 0)
            return;

        PawState state(*this, fn, mir, upvalues, state_);
        enter_fn(state);

        for (int b = 0; b < mir->blocks->count; ++b) {
            state.current_ = state.mir_->blocks->data[b];
            B->SetInsertPoint(state.blocks_[(size_t)b]);

            auto *bb = mir->blocks->data[b];
            for (int i = 0; i < bb->joins->count; ++i)
                create_instruction(bb->joins->data[i]);
            for (int i = 0; i < bb->instructions->count; ++i)
                create_instruction(bb->instructions->data[i]);
        }

        leave_fn();
    }

    void startup_module(Mir *const *mirs, int mir_count)
    {
        auto *C = X.get_compiler();
        auto *c = X.get_context();
        auto *M = X.get_module();

#define FOREACH_TYPE(Types_, Name_, Code_) do { \
        TypeCollectionIterator iter_; \
        TypeCollectionIterator_init(Types_, &iter_); \
        while (TypeCollectionIterator_is_valid(&iter_)) { \
            auto *Name_ = TypeCollectionIterator_key(&iter_); \
            Code_ \
            TypeCollectionIterator_next(&iter_); \
        } \
    } while (0)

        TypeTranslator translator(X, types_);

        // Declare ADTs, which require 2-phase initialization to support
        // recursive types. Recursive types require indirection, otherwise
        // the size cannot be computed (enforced by the frontend).
        FOREACH_TYPE(C->typesystem.adts, irtype, {
                    translator.declare_adt(irtype);
                });

        translator.declare_builtin(C->typesystem.primitives.never_t);
        translator.declare_builtin(C->typesystem.primitives.unit_t);
        translator.declare_builtin(C->typesystem.primitives.bool_t);
        translator.declare_builtin(C->typesystem.primitives.char_t);
        translator.declare_builtin(C->typesystem.primitives.int_t);
        translator.declare_builtin(C->typesystem.primitives.float_t);
        translator.declare_builtin(C->typesystem.primitives.str_t);
        FOREACH_TYPE(C->typesystem.lists, irtype, {
                    translator.declare_builtin(irtype);
                });
        FOREACH_TYPE(C->typesystem.maps, irtype, {
                    translator.declare_builtin(irtype);
                });
        FOREACH_TYPE(C->typesystem.iterators.list, irtype, {
                    translator.declare_builtin(irtype);
                    IrType *iterator = (IrType *)*TypeCollectionIterator_valuep(&iter_);
                    translator.declare_adt(iterator);
                });
        FOREACH_TYPE(C->typesystem.iterators.map, irtype, {
                    IrType *iterator = (IrType *)*TypeCollectionIterator_valuep(&iter_);
                    translator.declare_builtin(irtype);
                    translator.declare_adt(iterator);
                });

        FOREACH_TYPE(C->typesystem.adts, irtype, {
                    translator.define_adt(irtype);
                });
        FOREACH_TYPE(C->typesystem.types, irtype, {
                    translator.define_type(irtype);
                });

        // declare all toplevel functions
        for (int i = 0; i < mir_count; ++i) {
            auto const *mir = mirs[i];
            declare_fn(mir);
        }

        // declare/generate internal functions that are only called from
        // generated code
        generate_builtins();

        FOREACH_TYPE(C->typesystem.lists, irtype, {
                    generate_list_methods(irtype);
                });
        FOREACH_TYPE(C->typesystem.maps, irtype, {
                    generate_map_methods(irtype);
                });

#undef FOREACH_TYPE

        FnType constructor_type(X, X.get_unit_type(), {}, FUNC_FUNCTION);
        auto constructor_fn = Fn(
                X, "paw_constructor",
                llvm::GlobalValue::InternalLinkage,
                &constructor_type);
        {
            State state(X, &constructor_fn);

            // call GC_init() to initialize BDWGC collector
            X.create_gc_init();

            StringMapIterator iter;
            StringMapIterator_init(C->strings, &iter);
            while (StringMapIterator_is_valid(&iter)) {
                auto const *s = StringMapIterator_key(&iter);
                auto *array = llvm::ConstantDataArray::getString(*c, s->text, true);

                // TODO: allocatae once outside loop, use longest string length
                auto *temp = B->CreateAlloca(array->getType());
                B->CreateStore(array, temp);

                auto *global = new llvm::GlobalVariable(
                        **M, X.get_ptr_ty(), false,
                        llvm::GlobalValue::PrivateLinkage,
                        X.create_null_ptr(), "str");

                Str str(state, temp, X.create_i32((int32_t)s->hash),
                        X.create_int((paw_Int)s->length),
                        get_str_methods(), Str::CreationTag());
                B->CreateStore(str.get_value(), global);
                strings_[s] = global;

//                // NOTE: strings are intern'd by the compiler, so "str" is guaranteed
//                //       to be absent from the string table
//                map.create_set(str, X.get_unit());
                StringMapIterator_next(&iter);
            }

            state.create_return();
        }

        FnType destructor_type(X, X.get_unit_type(), {}, FUNC_FUNCTION);
        auto destructor_fn = Fn(
                X, "paw_destructor",
                llvm::GlobalValue::InternalLinkage,
                &destructor_type);
        {
            State state(X, &destructor_fn);

            state.create_return();
        }

        int static constexpr PRIORITY_USER = 65535;
        llvm::appendToGlobalCtors(**M, constructor_fn, PRIORITY_USER);
        llvm::appendToGlobalDtors(**M, destructor_fn, PRIORITY_USER);
    }

    void teardown_module()
    {
    }

    llvm::Value *create_cstr_to_str(State &state, llvm::Value *cstr)
    {
        auto *length = B->CreateCall(X.get_strlen_callee(), {cstr});
        Str str(state, cstr, length, get_str_methods(), Str::CreationTag());
        return str.get_value();
    }

    void create_main_fn_wrapper(llvm::Function *inner, bool materialize_args, bool materialize_return)
    {
        auto *c = X.get_context();
        auto *C = X.get_compiler();

        // Set up the entrypoint function. The entrypoint code converts the standard
        // arguments ("argc"/"argv") into a single variable of type "[str]", then calls
        // the user-defined "main" (renamed to "paw_main") and forwards its integer
        // return value.
        inner->setName("paw_main");
        inner->setLinkage(llvm::Function::PrivateLinkage);

        FnType main_type(X, X.get_int32_type(), {
                    X.get_int32_type(),
                    X.get_ptr_type(),
                }, FUNC_FUNCTION, false);
        Fn main_fn(X, "main", llvm::Function::ExternalLinkage, &main_type);
        State state(X, &main_fn);

        auto *entry = state.get_entry();
        llvm::Value *args = X.create_null_ptr();
        if (materialize_args) {
            // Convert the "argc"/"argv" pair into a list of type "[str]"
            // to pass to "paw_main".
            auto *header = llvm::BasicBlock::Create(*c, "header", main_fn);
            auto *body = llvm::BasicBlock::Create(*c, "body", main_fn);
            auto *exit = llvm::BasicBlock::Create(*c, "exit", main_fn);

            auto *index1 = X.create_int(0);
            auto *argc32 = main_fn.get_arg(0);
            auto *argv = main_fn.get_arg(1);

            auto *argc1 = B->CreateSExt(argc32, X.get_int_ty());
            auto *args_irtype = C->main_args_type;
            List list(state, argc1, cast<ListType>(get_type(args_irtype)),
                    get_list_methods(args_irtype),
                    List::CreationTag());
            args = list.get_value();
            B->CreateBr(header);

            B->SetInsertPoint(header);
            auto *argc = B->CreatePHI(X.get_int_ty(), 2);
            auto *index = B->CreatePHI(X.get_int_ty(), 2);
            argc->addIncoming(argc1, entry);
            index->addIncoming(index1, entry);

            auto *condition = B->CreateCmp(llvm::CmpInst::ICMP_SGT,
                    argc, X.create_int(0));
            B->CreateCondBr(condition, body, exit);

            B->SetInsertPoint(body);
            auto *element_ptr = list.get_element_ptr(index);
            auto *cstr = B->CreateLoad(X.get_ptr_ty(),
                    B->CreateInBoundsGEP(X.get_ptr_ty(), argv, {index}));
            auto *str = create_cstr_to_str(state, cstr);
            B->CreateStore(str, element_ptr); // write "str" element of "[str]"
            auto *index2 = B->CreateAdd(index, X.create_int(1), "index");
            auto *argc2 = B->CreateSub(argc, X.create_int(1), "argc");

            B->CreateBr(header);

            argc->addIncoming(argc2, body);
            index->addIncoming(index2, body);

            B->SetInsertPoint(exit);
        }

        auto *ret = B->CreateCall(inner, {X.create_null_ptr(), args});
        state.create_return(materialize_return ? X.create_i32(0)
                : B->CreateTrunc(ret, X.get_i32_ty()));
    }

    void compile_module(std::string prefix, std::string filename)
    {
        compile_object(X, *machine_, prefix + filename, options_);

        if (options_.build_tests)
            generate_test_driver(X, *machine_,
                    prefix + "test_" + filename,
                    test_names_, options_);
    }

    Type *get_type(IrType *irtype) const
    {
        auto *itr = types_.lookup(irtype);
        if (itr != nullptr) return *itr;
        return nullptr;
    }

    std::vector<Type *> get_types(IrTypeList const *irtypes) const
    {
        std::vector<Type *> types(irtypes->count);
        for (int i = 0; i < irtypes->count; ++i)
            types[i] = get_type(irtypes->data[i]);
        return types;
    }

    Unit::Methods const *get_unit_methods() const { return &scalar_info_.u.methods; }
    Bool::Methods const *get_bool_methods() const { return &scalar_info_.b.methods; }
    Char::Methods const *get_char_methods() const { return &scalar_info_.c.methods; }
    Int::Methods const *get_int_methods() const { return &scalar_info_.i.methods; }
    Float::Methods const *get_float_methods() const { return &scalar_info_.f.methods; }
    Str::Methods const *get_str_methods() const { return &scalar_info_.s.methods; }

    List::Methods const *get_list_methods(IrType *irtype) const
    {
        auto *itr = list_methods_.lookup(irtype);
        paw_assert(itr != nullptr);
        return itr;
    }

    Map::Methods const *get_map_methods(IrType *irtype) const
    {
        auto *itr = map_methods_.lookup(irtype);
        paw_assert(itr != nullptr);
        return itr;
    }

    Fn *get_fn(IrType *irtype) const
    {
        auto *itr = fns_.lookup(irtype);
        paw_assert(itr != nullptr);
        return itr->get();
    }

    llvm::Value *create_constant(MirConstantData kdata)
    {
        switch (kdata.kind) {
            case BUILTIN_UNIT:
                return X.create_unit();
            case BUILTIN_BOOL:
                return X.create_bool(kdata.value.i);
            case BUILTIN_CHAR:
                return X.create_char(kdata.value.c);
            case BUILTIN_INT:
                return X.create_int(kdata.value.i);
            case BUILTIN_FLOAT:
                return X.create_float(kdata.value.f);
            default:
                paw_assert(kdata.kind == BUILTIN_STR);
                return get_constant_str((::Str const *)kdata.value.p);
        }
    }

private:
    void enter_fn(PawState &state)
    {
        state_ = &state;
    }

    void leave_fn()
    {
        for (auto const &p: state_->phi_inputs_)
            p.phi->addIncoming(operand(p.r), p.b);

        auto *before = state_->before_block_;
        state_ = state_->outer_;

        if (state_ != nullptr)
            B->SetInsertPoint(before);
    }

    Owned<Fn> create_internal_method(IrType *irself, std::string name, Type *return_type, llvm::ArrayRef<Type *> param_types)
    {
        auto type = std::make_unique<FnType>(X,
                return_type, param_types, FUNC_METHOD);
        auto value = std::make_unique<Fn>(X,
                mangle_internal_method_name(C, irself, name),
                llvm::Function::PrivateLinkage,
                type.get());

        return {
            std::move(type),
            std::move(value),
        };
    }

    void generate_builtins()
    {
        auto *c = X.get_context();

        // declare "void @GC_init()" builtin
        {
            auto *fn = llvm::Function::Create(
                    llvm::FunctionType::get(X.get_void_ty(), false),
                    llvm::GlobalValue::ExternalLinkage,
                    get_builtin_name(BuiltinFn::GC_INIT),
                    *M);
            fn->setDoesNotThrow();
        }

        // declare "ptr @GC_malloc(i64)" builtin
        {
            auto *fn = llvm::Function::Create(
                    llvm::FunctionType::get(X.get_ptr_ty(),
                        {X.get_i64_ty()}, false),
                    llvm::GlobalValue::ExternalLinkage,
                    get_builtin_name(BuiltinFn::GC_MALLOC),
                    *M);
            fn->setDoesNotThrow();
        }

        // declare "void @GC_free(ptr)" builtin
        {
            auto *fn = llvm::Function::Create(
                    llvm::FunctionType::get(X.get_void_ty(),
                        {X.get_ptr_ty()}, false),
                    llvm::GlobalValue::ExternalLinkage,
                    get_builtin_name(BuiltinFn::GC_FREE),
                    *M);
            fn->setDoesNotThrow();
        }

        // generate "void @paw_bkpt(ptr)" builtin
        {
            auto *fn = llvm::Function::Create(
                    llvm::FunctionType::get(X.get_void_ty(),
                        {X.get_ptr_ty()}, false),
                    llvm::GlobalValue::ExternalLinkage,
                    get_builtin_name(BuiltinFn::PAW_BKPT),
                    *M);
            fn->setDoesNotThrow();

            auto *block = llvm::BasicBlock::Create(*c, "", fn);
            B->SetInsertPoint(block);
            B->CreateRetVoid();
        }

        // declare "i32 @paw_builtin_hash_bytes(ptr, i64, i32)" builtin
        {
            auto *fn = llvm::Function::Create(
                    llvm::FunctionType::get(X.get_i32_ty(), {
                        X.get_ptr_ty(),
                        X.get_i64_ty(),
                        X.get_i32_ty(),
                    }, false),
                    llvm::GlobalValue::ExternalLinkage,
                    get_builtin_name(BuiltinFn::HASH_BYTES),
                    *M);
            fn->setDoesNotThrow();
        }

        // declare "i64 @abs_index(i64, i64)" builtin
        {
            auto *fn = llvm::Function::Create(
                    llvm::FunctionType::get(X.get_i64_ty(), {
                        X.get_i64_ty(),
                        X.get_i64_ty(),
                    }, false),
                    llvm::GlobalValue::ExternalLinkage,
                    get_builtin_name(BuiltinFn::ABS_INDEX),
                    *M);
            fn->setDoesNotThrow();
        }

        // declare "i64 @paw_builtin_rawcmp(ptr, i64, ptr, i64)" builtin
        {
            auto *fn = llvm::Function::Create(
                    llvm::FunctionType::get(X.get_i64_ty(), {
                        X.get_ptr_ty(),
                        X.get_i64_ty(),
                        X.get_ptr_ty(),
                        X.get_i64_ty(),
                    }, false),
                    llvm::GlobalValue::ExternalLinkage,
                    get_builtin_name(BuiltinFn::RAWCMP),
                    *M);
            fn->setDoesNotThrow();
        }

        // generate "i64 @ckd_iadd(i64, i64)" builtin
        {
            auto *fn = llvm::Function::Create(
                    llvm::FunctionType::get(X.get_i64_ty(), {
                        X.get_i64_ty(),
                        X.get_i64_ty(),
                    }, false),
                    llvm::GlobalValue::ExternalLinkage,
                    get_builtin_name(BuiltinFn::CKD_IADD),
                    *M);
            fn->setDoesNotThrow();

            auto *entry_block = llvm::BasicBlock::Create(*c, "entry", fn);
            auto *ok_block = llvm::BasicBlock::Create(*c, "ok", fn);
            auto *error_block = llvm::BasicBlock::Create(*c, "error", fn);

            B->SetInsertPoint(entry_block);
            auto *result = B->CreateCall(
                    llvm::Intrinsic::getOrInsertDeclaration(*M,
                        llvm::Intrinsic::sadd_with_overflow,
                        {X.get_i64_ty()}),
                    {fn->getArg(0), fn->getArg(1)});
            auto *overflow = B->CreateExtractValue(result, 1);
            B->CreateCondBr(overflow, error_block, ok_block);

            B->SetInsertPoint(error_block);
            CG_PANIC_LITERAL(X, "encountered signed integer overflow during operator \"*\"");
            B->CreateUnreachable();

            B->SetInsertPoint(ok_block);
            auto *value = B->CreateExtractValue(result, 0);
            B->CreateRet(value);
        }

        // generate "i64 @ckd_imul(i64, i64)" builtin
        {
            auto *fn = llvm::Function::Create(
                    llvm::FunctionType::get(X.get_i64_ty(), {
                        X.get_i64_ty(),
                        X.get_i64_ty(),
                    }, false),
                    llvm::GlobalValue::ExternalLinkage,
                    get_builtin_name(BuiltinFn::CKD_IMUL),
                    *M);
            fn->setDoesNotThrow();

            auto *entry_block = llvm::BasicBlock::Create(*c, "entry", fn);
            auto *ok_block = llvm::BasicBlock::Create(*c, "ok", fn);
            auto *error_block = llvm::BasicBlock::Create(*c, "error", fn);

            B->SetInsertPoint(entry_block);
            auto *result = B->CreateCall(
                    llvm::Intrinsic::getOrInsertDeclaration(*M,
                        llvm::Intrinsic::smul_with_overflow,
                        {X.get_i64_ty()}),
                    {fn->getArg(0), fn->getArg(1)});
            auto *overflow = B->CreateExtractValue(result, 1);
            B->CreateCondBr(overflow, error_block, ok_block);

            B->SetInsertPoint(error_block);
            CG_PANIC_LITERAL(X, "encountered signed integer overflow during operator \"*\"");
            B->CreateUnreachable();

            B->SetInsertPoint(ok_block);
            auto *value = B->CreateExtractValue(result, 0);
            B->CreateRet(value);
        }
    }

    void generate_list_methods(IrType *irself)
    {
        auto *self_type = cast<ListType>(get_type(irself));

        List::Methods m;
        m.push = get_method(irself, "push");
        m.pop = get_method(irself, "pop");
        m.insert = get_method(irself, "insert");
        m.remove = get_method(irself, "remove");
        m.get_element_ptr = get_method(irself, "get_element_ptr");

        List::generate_methods(X, self_type, m);
        list_methods_.insert(irself, std::move(m));
    }

    void generate_map_methods(IrType *irself)
    {
        auto *self_type = cast<MapType>(get_type(irself));
        auto *key_type = self_type->get_key_type();

        Map::Methods m;
        m.key_hash = get_method(irself, "key_hash");
        m.key_eq = get_method(irself, "key_eq");
        m.get = get_method(irself, "get");
        m.remove = get_method(irself, "remove");
        m.gep = create_internal_method(irself, "gep", X.get_ptr_type(), {self_type, key_type});
        m.nep = create_internal_method(irself, "nep", X.get_ptr_type(), {self_type, key_type});
        m.gep1 = create_internal_method(irself, "gep1", X.get_ptr_type(), {self_type, key_type});
        m.grow = create_internal_method(irself, "grow", X.get_unit_type(), {self_type});
        {
            // { ptr %flag_ptr, ptr %value_ptr }
            auto *result_type = X.get_tuple_type({X.get_ptr_type(), X.get_ptr_type()});
            m.lookup = create_internal_method(irself, "lookup", result_type, {self_type, key_type});
            m.access = create_internal_method(irself, "access", result_type, {self_type, key_type});
        }

        m.iterator_type = NULL;
        m.iterator_next = NULL;
        void *iterator_irtype = TypeCollection_get(C, C->typesystem.iterators.map, irself);
        paw_assert(iterator_irtype != nullptr);
        m.iterator_type = cast<ObjectType>(get_type(*(IrType **)iterator_irtype));
        m.iterator_next = get_method(*(IrType **)iterator_irtype, "next");

        Map::generate_methods(X, self_type, m);
        map_methods_.insert(irself, std::move(m));
    }

    Fn *get_method(IrType *self, std::string name)
    {
        auto &methods = methods_[self];
        auto const itr = methods.find(name);
        return itr != end(methods) ? itr->second : NULL;
    }

    void declare_fn(Mir const *mir)
    {
        auto *type = cast<FnType>(get_type(mir->type));
        auto const mangled_name = mangle_mir_name(mir);
        auto fn = std::make_unique<Fn>(X, mangled_name, mir->is_pub
                    ? llvm::Function::ExternalLinkage
                    : llvm::Function::InternalLinkage,
                type);

        if (mir->self != nullptr)
            methods_[mir->self][to_string(mir->name)] = fn.get();

        if (has_annotation(C, mir->annotations, "test"))
            test_names_.push_back(mangled_name);

        fns_.insert(mir->type, std::move(fn));
    }

    void create_instruction(MirInstruction *instr)
    {
        switch (MIR_KINDOF(instr)) {
            case kMirNoop:
                return;
            case kMirPhi:
                create_phi(instr->Phi_);
                break;
            case kMirMove:
                create_move(instr->Move_);
                break;
            case kMirAddrOf:
                create_addrof(instr->AddrOf_);
                break;
            case kMirLoad:
                create_load(instr->Load_);
                break;
            case kMirStore:
                create_store(instr->Store_);
                break;
            case kMirGlobal:
                create_global(instr->Global_);
                break;
            case kMirAllocLocal:
                create_alloclocal(instr->AllocLocal_);
                break;
            case kMirAggregate:
                create_aggregate(instr->Aggregate_);
                break;
            case kMirContainer:
                create_container(instr->Container_);
                break;
            case kMirStructGEP:
                create_structgep(instr->StructGEP_);
                break;
            case kMirStrGEP:
                create_strgep(instr->StrGEP_);
                break;
            case kMirListGEP:
                create_listgep(instr->ListGEP_);
                break;
            case kMirMapGEP:
                create_mapgep(instr->MapGEP_);
                break;
            case kMirGetRange:
                create_getrange(instr->GetRange_);
                break;
            case kMirSetRange:
                create_setrange(instr->SetRange_);
                break;
            case kMirCall:
                create_call(instr->Call_);
                break;
            case kMirCast:
                create_cast_instr(instr->Cast_);
                break;
            case kMirCapture:
                create_capture(instr->Capture_);
                break;
            case kMirClose:
                create_close(instr->Close_);
                break;
            case kMirClosure:
                create_closure(instr->Closure_);
                break;
            case kMirUnaryOp:
                create_unaryop(instr->UnaryOp_);
                break;
            case kMirBinaryOp:
                create_binaryop(instr->BinaryOp_);
                break;
            case kMirConcat:
                create_concat(instr->Concat_);
                break;
            case kMirUnreachable:
                create_unreachable(instr->Unreachable_);
                break;
            case kMirReturn:
                create_return(instr->Return_);
                break;
            case kMirBranch:
                create_branch(instr->Branch_);
                break;
            case kMirSwitch:
                create_switch(instr->Switch_);
                break;
            case kMirGoto:
                create_goto(instr->Goto_);
                break;
        }
    }

    llvm::BasicBlock *get_block(int b)
    {
        return state_->blocks_.at(unsigned(b));
    }

    llvm::BasicBlock *get_predecessor_block(int index)
    {
        paw_assert(state_->current_ != nullptr);
        return get_block(state_->current_->predecessors->data[index].value);
    }

    llvm::BasicBlock *get_successor_block(int index)
    {
        paw_assert(state_->mir_ != nullptr);
        return get_block(state_->current_->successors->data[index].value);
    }

    llvm::Value *get_constant_str(::Str const *k)
    {
        auto const itr = strings_.find(k);
        paw_assert(itr != end(strings_));
        return B->CreateLoad(X.get_ptr_ty(), itr->second);
    }

    void create_phi(MirPhi const &x)
    {
        auto *phi = B->CreatePHI(*get_type(x.output.type),
                unsigned(x.inputs->count));
        for (auto i = 0; i < x.inputs->count; ++i) {
            auto const r = x.inputs->data[i];
            auto *b = get_predecessor_block(i);
            state_->phi_inputs_.push_back(PhiInput{
                .phi = phi,
                .r = r,
                .b = b,
            });
        }
        set_result(x.output, phi);
    }

    void create_move(MirMove const &x)
    {
        if (x.output.kind == MIR_PLACE_LOCAL) {
            // only used when storing local corresponding to reference argument
            auto const L = unsigned(x.output.L.value);
            state_->locals_.at(L) = operand(x.target);
        } else {
            paw_assert(x.output.kind == MIR_PLACE_REGISTER);
            set_result(x.output, operand(x.target));
        }
    }

    void create_addrof(MirAddrOf const &x)
    {
        PAW_UNUSED(x);
        PAW_UNREACHABLE();
    }

    void create_load(MirLoad const &x)
    {
        auto *pointer = operand(x.pointer);
        auto *output_type = get_type(x.output.type);
        auto *output = B->CreateLoad(*output_type, pointer);
        set_result(x.output, output);
    }

    void create_store(MirStore const &x)
    {
        auto *value = operand(x.value);
        auto *pointer = operand(x.pointer);
        B->CreateStore(value, pointer);
    }

    void create_global(MirGlobal const &x)
    {
        auto *itr = fns_.lookup(x.output.type);
        paw_assert(itr != nullptr);

        auto *fn = (*itr)->get_value();
        set_result(x.output, fn);
    }

    void create_alloclocal(MirAllocLocal const &x)
    {
        auto *fn = state_->fn_;
        auto const L = unsigned(x.output.L.value);

        auto const data = state_->mir_->local_data->data[x.output.L.value];
        if (data.is_captured && L > fn->get_num_args()) {
            // NOTE: the return value (L0) cannot be captured and slots for captured
            //   arguments (L1..LN) are allocated and initialized at the start of the
            //   function.
            auto &local = state_->locals_.at(L);
            auto *slot = X.create_alloc(*get_type(data.type));
            local = slot;
        }
    }

    void create_aggregate(MirAggregate const &x)
    {
        auto *obj_type = cast<ObjectType>(get_type(x.output.type));
        Object obj(*state_, obj_type, Object::CreationTag());

        Discriminant const discr(x.discr);
        for (auto i = 0; i < x.fields->count; ++i) {
            auto *value = operand(x.fields->data[i]);
            obj.set_field(discr, unsigned(i), value);
        }

        auto *object = !x.is_boxed
            ? B->CreateLoad(*obj_type, obj.get_value())
            : obj.get_value();
        set_result(x.output, object);
    }

    llvm::Value *operand(MirPlace const place)
    {
        switch (place.kind) {
            case MIR_PLACE_LOCAL:
                return state_->get_local_ptr(unsigned(place.L.value));
            case MIR_PLACE_REGISTER:
                return state_->registers_.at(unsigned(place.r.value));
            case MIR_PLACE_CONSTANT:
                return state_->constants_.at(unsigned(place.k.value));
            case MIR_PLACE_UPVALUE:
                return state_->get_upvalue_ptr(unsigned(place.up));
        }
    }

    llvm::Value *create_literal_list(MirContainer const &x)
    {
        paw_assert(x.b_kind == BUILTIN_LIST);
        auto *list_type = cast<ListType>(get_type(x.output.type));
        List list(*state_, X.create_int(x.elems->count), list_type,
                get_list_methods(x.output.type), List::CreationTag());

        for (int i = 0; i < x.elems->count; ++i) {
            auto *value = operand(x.elems->data[i]);
            list.set_element(X.create_int(i), value);
        }

        return list.get_value();
    }

    llvm::Value *create_literal_map(MirContainer const &x)
    {
        paw_assert(x.b_kind == BUILTIN_MAP);
        auto *map_type = cast<MapType>(get_type(x.output.type));
        Map map(*state_, X.create_int(x.elems->count), map_type,
                get_map_methods(x.output.type), Map::CreationTag());

        for (int i = 0; i < x.elems->count; i += 2) {
            auto *key = operand(x.elems->data[i]);
            auto *value = operand(x.elems->data[i + 1]);
            map.set_element(key, value);
        }

        return map.get_value();
    }

    void create_container(MirContainer const &x)
    {
        auto *result = x.b_kind == BUILTIN_LIST
            ? create_literal_list(x)
            : create_literal_map(x);
        set_result(x.output, result);
    }

    Type *get_deref_type(IrType *irtype)
    {
        return get_type(ir_remove_indirection(irtype));
    }

    void create_structgep(struct MirStructGEP const &x)
    {
        auto *value = operand(x.object);
        auto *obj_type = (ObjectType *)get_deref_type(x.object.type);
        Object obj(*state_, value, obj_type);

        Discriminant const discr(x.discr);
        auto *field_ptr = obj.get_field_ptr(discr, unsigned(x.field));
        set_result(x.output, field_ptr);
    }

    void create_strgep(MirStrGEP const &x)
    {
        Str str(*state_, operand(x.object), get_str_methods());
        auto *index = X.create_abs_index(operand(x.index), str.get_length());
        auto *element_ptr = str.get_element_ptr(index);
        set_result(x.output, element_ptr);
    }

    void create_listgep(MirListGEP const &x)
    {
        auto *list_type = (ListType *)get_deref_type(x.object.type);
        List list(*state_, operand(x.object), list_type,
                get_list_methods(x.object.type));
        auto *index = X.create_abs_index(operand(x.index), list.get_length());
        auto *element_ptr = list.get_element_ptr(index);
        set_result(x.output, element_ptr);
    }

    void create_mapgep(MirMapGEP const &x)
    {
        auto *map_type = (MapType *)get_deref_type(x.object.type);
        Map map(*state_, operand(x.object), map_type,
                get_map_methods(x.object.type));

        auto *value_ptr = x.create_if_missing
            ? map.new_element_ptr(operand(x.key))
            : map.get_element_ptr(operand(x.key));
        set_result(x.output, value_ptr);
    }

    void create_setrange(MirSetRange const &x)
    {
        PAW_UNREACHABLE();
    }

    void create_getrange(struct MirGetRange const &x)
    {
        PAW_UNREACHABLE();
    }

    bool is_boxed_aggregate(IrType *type)
    {
        return mir_is_boxed_aggregate(C, type);
    }

    bool is_inline_aggregate(IrType *type)
    {
        return mir_is_inline_aggregate(C, type);
    }

    BuiltinKind builtin_kind(IrType *type)
    {
        return pawP_type2code(C, type);
    }

    std::string get_inherent_context_name(IrType *type)
    {
        if (IrIsSignature(type)) {
            auto const *fn_def = pawIr_get_fn_def(C, IR_TYPE_DID(type));
            if (fn_def->parent.value != (unsigned)-1) {
                if (pawIr_get_kind(C, fn_def->parent) == IR_IMPL_DEF) {
                    auto const *impl_def = pawIr_get_impl_def(C, fn_def->parent);
                    if (impl_def->trait == NULL && IrIsAdt(impl_def->type)) {
                        auto const *adt_def = pawIr_get_adt_def(C, IR_TYPE_DID(impl_def->type));
                        return to_string(adt_def->name);
                    }
                }
            }
        }
        return "";
    }

    std::string get_fn_name(IrType *type)
    {
        if (IrIsSignature(type)) {
            auto const *fn_def = pawIr_get_fn_def(C, IR_TYPE_DID(type));
            return to_string(fn_def->name);
        }
        return "";
    }

    void generate_pointer_add(Type *element_type,  llvm::Value *pointer, llvm::Value *offset, MirPlace output)
    {
        auto *result = B->CreateInBoundsGEP(element_type->get_ty(), pointer, offset);
        set_result(output, result);
    }

    void generate_pointer_read(Type *element_type,  llvm::Value *pointer, MirPlace output)
    {
        auto *result = B->CreateLoad(element_type->get_ty(), pointer);
        set_result(output, result);
    }

    void generate_pointer_write(llvm::Value *pointer, llvm::Value *value)
    {
        B->CreateStore(value, pointer);
    }

    // Generate code for performing a function call
    void create_call(MirCall const &x)
    {
        if (get_inherent_context_name(x.target.type) == "Pointer") {
            auto *element_type = get_type(IrTypeList_first(
                        IR_TYPE_SUBTYPES(pawIr_get_context(C, x.target.type))));
            auto const pointer = operand(MirPlaceList_get(x.args, 0));
            if (get_fn_name(x.target.type) == "add") {
                auto const index = operand(MirPlaceList_get(x.args, 1));
                generate_pointer_add(element_type, pointer, index, x.output);
            } else if (get_fn_name(x.target.type) == "read") {
                generate_pointer_read(element_type, pointer, x.output);
            } else if (get_fn_name(x.target.type) == "write") {
                auto const value = operand(MirPlaceList_get(x.args, 1));
                generate_pointer_write(pointer, value);
            }
            return;
        }

        auto *value = operand(x.target);
        auto *fn = B->CreateExtractValue(value, 0);
        auto *env = B->CreateExtractValue(value, 1);

        auto *fn_type = cast<FnType>(get_type(x.target.type));
        Callable callable(*state_, fn, env, fn_type);

        std::vector<llvm::Value *> args((size_t)x.args->count);
        for (int i = 0; i < x.args->count; ++i)
            args[(size_t)i] = operand(x.args->data[i]);

        auto *result = state_->create_call(callable, args);
        set_result(x.output, result);
    }

    llvm::Value *create_cast(llvm::Value *target, BuiltinKind from, BuiltinKind to)
    {
        paw_assert(from != BUILTIN_UNIT && IS_SCALAR_TYPE(from));
        paw_assert(to != BUILTIN_UNIT && IS_SCALAR_TYPE(to));

        switch (from) {
            case BUILTIN_BOOL:
                if (to == BUILTIN_CHAR) {
                    return B->CreateZExt(target, X.get_char_ty());
                } else if (to == BUILTIN_INT) {
                    return B->CreateZExt(target, X.get_int_ty());
                } else { // to == BUILTIN_FLOAT
                    auto *temp = B->CreateZExt(target, X.get_int_ty());
                    return B->CreateSIToFP(temp, X.get_float_ty());
                }
            case BUILTIN_CHAR:
                if (to == BUILTIN_BOOL) {
                    return B->CreateCmp(llvm::CmpInst::ICMP_NE, target, X.create_char(0));
                } else if (to == BUILTIN_INT) {
                    return B->CreateZExt(target, X.get_int_ty());
                } else { // to == BUILTIN_FLOAT
                    auto *temp = B->CreateZExt(target, X.get_int_ty());
                    return B->CreateSIToFP(temp, X.get_float_ty());
                }
            case BUILTIN_INT:
                if (to == BUILTIN_BOOL) {
                    return B->CreateCmp(llvm::CmpInst::ICMP_NE, target, X.create_int(0));
                } else if (to == BUILTIN_CHAR) {
                    return B->CreateTrunc(target, X.get_char_ty());
                } else { // to == BUILTIN_FLOAT
                    return B->CreateSIToFP(target, X.get_float_ty());
                }
            default: // from == BUILTIN_FLOAT
                if (to == BUILTIN_BOOL) {
                    return B->CreateCmp(llvm::CmpInst::FCMP_ONE, target, X.create_float(0.0));
                } else if (to == BUILTIN_CHAR) {
                    return B->CreateFPToSI(target, X.get_char_ty());
                } else { // to == BUILTIN_INT
                    return B->CreateFPToSI(target, X.get_int_ty());
                }
        }
    }

    void create_cast_instr(MirCast const &x)
    {
        auto *target = operand(x.target);
        auto *result = create_cast(target, x.from, x.to);
        set_result(x.output, result);
    }

    void create_capture(MirCapture const &x)
    {
    }

    void create_close(MirClose const &x)
    {
    }

    llvm::Type *create_env_ty(MirUpvalueList const *upvalues)
    {
        std::vector<llvm::Type *> upvalue_tys(unsigned(upvalues->count));
        for (int i = 0; i < upvalues->count; ++i)
            upvalue_tys[unsigned(i)] = X.get_ptr_ty();
        return X.get_env_ty(upvalue_tys);
    }

    void create_closure(MirClosure const &x)
    {
        auto *block = B->GetInsertBlock(); // save position
        auto *child = MirBodyList_get(state_->mir_->children, x.child_id);
        auto const num_upvalues = child->upvalues->count;

        auto *env_ty = create_env_ty(child->upvalues);
        auto *env_ptr = num_upvalues > 0
            ? X.create_alloc(env_ty)
            : X.create_null_ptr();

        // initialize upvalues from parent locals or environment
        for (int i = 0; i < num_upvalues; ++i) {
            auto const up = MirUpvalueList_get(child->upvalues, i);
            // If "up.is_local", then "up.index" refers to a local variable in the current function.
            // Otherwise, it refers to an upvalue in the current function backed by a local in one of
            // its callers.
            auto *source = up.is_local
                ? state_->get_local_ptr(up.index)
                : state_->get_upvalue_ptr(up.index);
            auto *source_ptr = B->CreateStructGEP(env_ty, env_ptr, unsigned(i));
            B->CreateStore(source, source_ptr);
        }

        auto *closure = state_->get_closure(unsigned(child->child_id));
        closure->fn = std::make_unique<Fn>(X, env_ptr,
                cast<FnType>(get_type(child->type)));
        define_fn(child);

        B->SetInsertPoint(block); // restore position
        set_result(x.output, closure->fn->get_value());
    }

    llvm::Value *new_unary_op(MirUnaryOpKind op, llvm::Value *value)
    {
        paw_assert(value != nullptr);

        switch (op) {
            case MIR_UNARY_STRLEN:
                value = B->CreateStructGEP(X.get_str_ty(), value, 0);
                return B->CreateLoad(X.get_int_ty(), value);
            case MIR_UNARY_LISTLEN:
                value = B->CreateStructGEP(X.get_list_ty(), value, 1);
                return B->CreateLoad(X.get_int_ty(), value);
            case MIR_UNARY_MAPLEN:
                value = B->CreateStructGEP(X.get_map_ty(), value, 1);
                return B->CreateLoad(X.get_int_ty(), value);
            case MIR_UNARY_IBITNOT:
                return B->CreateNot(value);
            case MIR_UNARY_INEG:
            case MIR_UNARY_FNEG:
                return B->CreateNeg(value);
            case MIR_UNARY_NOT:
                return B->CreateCmp(llvm::CmpInst::ICMP_EQ, value, X.create_i1(0));
        }
    }

    void create_unaryop(MirUnaryOp const &x)
    {
        auto *value = operand(x.val);
        auto *result = new_unary_op(x.op, value);
        set_result(x.output, result);
    }

    // NOTE: "strcmp" C stdlib function will not work here, since "str" might
    //       contain embedded null characters.
    llvm::Value *create_strcmp(llvm::Value *lhs, llvm::Value *rhs)
    {
        Str a(*state_, lhs, get_str_methods());
        Str b(*state_, rhs, get_str_methods());
        return B->CreateCall(X.get_rawcmp_callee(), {
                a.get_text(), a.get_length(),
                b.get_text(), b.get_length()});
    }

    llvm::Value *new_binary_op(MirBinaryOpKind op, llvm::Value *lhs, llvm::Value *rhs)
    {
        paw_assert(lhs != nullptr && rhs != nullptr);

        switch (op) {
            case MIR_BINARY_CEQ:
                return B->CreateCmp(llvm::CmpInst::ICMP_EQ, lhs, rhs);
            case MIR_BINARY_CNE:
                return B->CreateCmp(llvm::CmpInst::ICMP_NE, lhs, rhs);
            case MIR_BINARY_CLT:
                return B->CreateCmp(llvm::CmpInst::ICMP_ULT, lhs, rhs);
            case MIR_BINARY_CLE:
                return B->CreateCmp(llvm::CmpInst::ICMP_ULE, lhs, rhs);
            case MIR_BINARY_IEQ:
                return B->CreateCmp(llvm::CmpInst::ICMP_EQ, lhs, rhs);
            case MIR_BINARY_INE:
                return B->CreateCmp(llvm::CmpInst::ICMP_NE, lhs, rhs);
            case MIR_BINARY_ILT:
                return B->CreateCmp(llvm::CmpInst::ICMP_SLT, lhs, rhs);
            case MIR_BINARY_ILE:
                return B->CreateCmp(llvm::CmpInst::ICMP_SLE, lhs, rhs);
            case MIR_BINARY_FEQ:
                return B->CreateCmp(llvm::CmpInst::FCMP_OEQ, lhs, rhs);
            case MIR_BINARY_FNE:
                return B->CreateCmp(llvm::CmpInst::FCMP_ONE, lhs, rhs);
            case MIR_BINARY_FLT:
                return B->CreateCmp(llvm::CmpInst::FCMP_OLT, lhs, rhs);
            case MIR_BINARY_FLE:
                return B->CreateCmp(llvm::CmpInst::FCMP_OLE, lhs, rhs);
            case MIR_BINARY_STREQ:
                return B->CreateCmp(llvm::CmpInst::ICMP_EQ,
                        create_strcmp(lhs, rhs), X.create_int(0));
            case MIR_BINARY_STRNE:
                return B->CreateCmp(llvm::CmpInst::ICMP_NE,
                        create_strcmp(lhs, rhs), X.create_int(0));
            case MIR_BINARY_STRLT:
                return B->CreateCmp(llvm::CmpInst::ICMP_SLT,
                        create_strcmp(lhs, rhs), X.create_int(0));
            case MIR_BINARY_STRLE:
                return B->CreateCmp(llvm::CmpInst::ICMP_SLE,
                        create_strcmp(lhs, rhs), X.create_int(0));
            case MIR_BINARY_IADD:
                return B->CreateAdd(lhs, rhs);
            case MIR_BINARY_ISUB:
                return B->CreateSub(lhs, rhs);
            case MIR_BINARY_IMUL:
                return B->CreateMul(lhs, rhs);
            case MIR_BINARY_IDIV:
                return B->CreateSDiv(lhs, rhs);
            case MIR_BINARY_IMOD:
                return B->CreateSRem(lhs, rhs);
            case MIR_BINARY_FADD:
                return B->CreateFAdd(lhs, rhs);
            case MIR_BINARY_FSUB:
                return B->CreateFSub(lhs, rhs);
            case MIR_BINARY_FMUL:
                return B->CreateFMul(lhs, rhs);
            case MIR_BINARY_FDIV:
                return B->CreateFDiv(lhs, rhs);
            case MIR_BINARY_FMOD:
                return B->CreateFRem(lhs, rhs);
            case MIR_BINARY_IBITAND:
                return B->CreateAnd(lhs, rhs);
            case MIR_BINARY_IBITOR:
                return B->CreateOr(lhs, rhs);
            case MIR_BINARY_IBITXOR:
                return B->CreateXor(lhs, rhs);
            case MIR_BINARY_ISHL:
                return B->CreateShl(lhs, rhs);
            case MIR_BINARY_ISHR:
                return B->CreateLShr(lhs, rhs);
        }
    }

    void create_binaryop(MirBinaryOp const &x)
    {
        auto *lhs = operand(x.lhs);
        auto *rhs = operand(x.rhs);
        auto *result = new_binary_op(x.op, lhs, rhs);
        set_result(x.output, result);
    }

    llvm::Value *create_strcat(llvm::Value *a, llvm::Value *b)
    {
        Str lhs(*state_, a, get_str_methods());
        Str rhs(*state_, b, get_str_methods());
        auto *lhs_length = lhs.get_length();
        auto *rhs_length = rhs.get_length();

        auto *length = X.create_ckd_iadd(lhs_length, rhs_length);
        Str result(*state_, length, get_str_methods(), Str::CreationTag());

        // NOTE: sizeof(char) == 1
        X.create_memcpy(
                result.get_text(),
                lhs.get_text(),
                lhs_length);
        X.create_memcpy(
                result.get_element_ptr(lhs_length),
                rhs.get_text(),
                rhs_length);
        result.finalize();
        return result.get_value();
    }

    llvm::Value *create_listcat(IrType *irtype, llvm::Value *a, llvm::Value *b)
    {
        auto *type = cast<ListType>(get_type(irtype));
        List lhs(*state_, a, type, get_list_methods(irtype));
        List rhs(*state_, b, type, get_list_methods(irtype));
        auto *lhs_length = lhs.get_length();
        auto *rhs_length = rhs.get_length();

        auto *length = X.create_ckd_iadd(lhs_length, rhs_length);
        List result(*state_, length, type, get_list_methods(irtype),
                List::CreationTag());

        auto const element_size = paw_Int(X.size_of(*type->get_element_type()));
        auto *lhs_size = X.create_ckd_imul(lhs_length, X.create_int(element_size));
        auto *rhs_size = X.create_ckd_imul(rhs_length, X.create_int(element_size));

        X.create_memcpy(
                result.get_data(),
                lhs.get_data(),
                lhs_size);
        X.create_memcpy(
                result.get_element_ptr(lhs_length),
                rhs.get_data(),
                rhs_size);
        return result.get_value();
    }

    void create_concat(MirConcat const &x)
    {
        auto *result = operand(x.inputs->data[0]);
        for (int i = 1; i < x.inputs->count; ++i) {
            auto *value = operand(x.inputs->data[i]);
            result = x.b_kind == BUILTIN_LIST
                ? create_listcat(x.output.type, result, value)
                : create_strcat(result, value);
        }
        set_result(x.output, result);
    }

    void create_unreachable(MirUnreachable const &x)
    {
        B->CreateUnreachable();
    }

    llvm::Value *load_result(llvm::Value *value)
    {
        auto *result_type = get_type(ir_fn_result(C, state_->mir_->type));
        return B->CreateLoad(*result_type, value);
    }

    void create_return(MirReturn const &x)
    {
        auto const ret = K_LIST_FIRST(state_->mir_->locals);
        state_->create_return(is_empty_irtype(C, ret.type)
                ? nullptr : load_result(operand(ret)));
    }

    void create_branch(MirBranch const &x)
    {
        auto *condition = operand(x.cond);
        auto *then_block = get_successor_block(0);
        auto *else_block = get_successor_block(1);
        B->CreateCondBr(condition, then_block, else_block);
    }

    llvm::ConstantInt *into_constant_integral(MirConstant k)
    {
        auto const kdata = state_->mir_
            ->kcache->data->data[k.value];
        switch (kdata.kind) {
            case BUILTIN_BOOL:
                return X.create_bool(kdata.value.i);
            case BUILTIN_CHAR:
                return X.create_char(kdata.value.c);
            case BUILTIN_INT:
                return X.create_int(kdata.value.i);
            default:
                paw_assert(kdata.kind == BUILTIN_FLOAT);
                return llvm::cast<llvm::ConstantInt>(
                        llvm::ConstantExpr::getBitCast(
                            X.create_float(kdata.value.f),
                            X.get_int_ty()));
        }
    }

    llvm::BasicBlock *create_unreachable_block() const
    {
        auto *c = X.get_context();
        auto *before = B->GetInsertBlock();
        auto *block = llvm::BasicBlock::Create(*c, "unreachable", *state_->get_fn());
        B->SetInsertPoint(block);
        B->CreateUnreachable();
        B->SetInsertPoint(before);
        return block;
    }

    void create_direct_switch(MirSwitch const &x)
    {
        auto *discr = operand(x.discr);
        if (builtin_kind(x.discr.type) == BUILTIN_FLOAT)
            discr = B->CreateBitCast(discr, X.get_int_ty());
        auto *node = B->CreateSwitch(discr, x.has_otherwise
                    ? get_successor_block(x.arms->count)
                    : create_unreachable_block(),
                unsigned(x.arms->count));
        for (int i = 0; i < x.arms->count; ++i) {
            auto *k = into_constant_integral(x.arms->data[i].k);
            node->addCase(k, get_successor_block(i));
        }
    }

    Str get_str(llvm::Value *value)
    {
        return Str(*state_, value, get_str_methods());
    }

    void create_indirect_switch(MirSwitch const &x)
    {
        auto *c = X.get_context();
        auto *fn = state_->get_fn();
        paw_assert(x.has_otherwise);

        auto *discr = operand(x.discr);
        for (int i = 0; i < x.arms->count; ++i) {
            auto const kdata = *mir_const_data((Mir *)state_->mir_, x.arms->data[i].k);
            paw_assert(kdata.kind == BUILTIN_STR);

            auto *target = get_constant_str((::Str const *)kdata.value.p);
            auto *cond = B->CreateICmpEQ(create_strcmp(discr, target), X.create_int(0));

            auto *false_block = llvm::BasicBlock::Create(*c, "", *fn);
            B->CreateCondBr(cond, get_successor_block(i), false_block);
            B->SetInsertPoint(false_block);
        }

        B->CreateBr(get_successor_block(x.arms->count));
    }

    bool can_switch_directly(IrType *irtype)
    {
        switch (pawP_type2code(C, irtype)) {
            case BUILTIN_BOOL:
            case BUILTIN_CHAR:
            case BUILTIN_INT:
            case BUILTIN_FLOAT:
                return true;
            default:
                return false;
        }
    }

    void create_switch(MirSwitch const &x)
    {
        if (can_switch_directly(x.discr.type))
            return create_direct_switch(x);
        return create_indirect_switch(x);
    }

    void create_goto(MirGoto const &)
    {
        auto *target_block = get_successor_block(0);
        B->CreateBr(target_block);
    }

    void set_result(MirPlace place, llvm::Value *value)
    {
        paw_assert(place.kind == MIR_PLACE_REGISTER);
        state_->registers_.at(place.r.value) = value;
    }


    std::unique_ptr<llvm::LLVMContext> ctx_;

    Context X;
    Module *M;
    Compiler *C;
    llvm::IRBuilder<> *B;
    std::string modname_;
    llvm::TargetMachine *machine_;
    CodegenOptions options_;
    PawState *state_;

    // mapping from IR strings to runtime global variables
    std::unordered_map<::Str const *, llvm::GlobalVariable *> strings_;

    // List of mangled names referring to functions marked with the "test" annotation.
    // When the "CodegenOptions::build_tests" flag is set, an executable is generated
    // that runs each test function in sequence.
    std::vector<std::string> test_names_;

    // translation from IR types to cg module types
    IrTypeHashMap<Type *> types_;

    IrTypeHashMap<std::unique_ptr<Fn>> fns_;

    using MethodTable = std::unordered_map<std::string, Fn *>;
    IrTypeHashMap<MethodTable> methods_;

    template<class V>
    struct ScalarInfo {
        std::unique_ptr<typename V::Type> type;
        typename V::Methods methods;
    };

    struct ScalarInfoTable {
        ScalarInfo<Unit> u;
        ScalarInfo<Bool> b;
        ScalarInfo<Char> c;
        ScalarInfo<Int> i;
        ScalarInfo<Float> f;
        ScalarInfo<Str> s;
    } scalar_info_;

    IrTypeHashMap<List::Methods> list_methods_;
    IrTypeHashMap<Map::Methods> map_methods_;
};


PawState::PawState(CodeGenerator &G, Fn *fn, Mir const *mir, std::vector<Upvalue> *upvalues, PawState *outer)
    : State(*G.get_context(), fn)
    , G(&G)
    , B(X->get_builder())
    , mir_(mir)
    , before_block_(B->GetInsertBlock())
    , outer_(outer)
    , blocks_(unsigned(mir->blocks->count))
    , locals_(unsigned(mir->locals->count))
    , registers_(unsigned(mir->registers->count))
    , constants_(unsigned(mir->kcache->data->count))
    , captured_(unsigned(mir->captured->count))
    , upvalues_(upvalues)
    , closures_(unsigned(mir->children->count))
{
    auto *X = G.get_context();
    auto *B = X->get_builder();
    auto *c = X->get_context();

    // create debug information for the function
    {
//        auto const mod = ModuleInfo_get(G.C->modinfo, mir->modno);
//        auto const dirname = to_string(mod.dirname);
//        auto const modname = to_string(mod.name);
//        auto *DI = X->get_dibuilder();
//        auto const start = mir->span.start;
//        auto *type = fn->get_type();
//        auto *func = fn->get_fn();
//
//        disub_ = DI->createFunction(
//            X->get_compile_unit(),
//            modname,
//            modname,
//            X->get_difile(mir->modno),
//            unsigned(start.line),
//            (llvm::DISubroutineType *)type->get_dity(),
//            unsigned(start.line),
//            llvm::DINode::FlagZero,
//            llvm::DISubprogram::SPFlagDefinition);
//        func->setSubprogram(disub_);
    }

    paw_assert(!blocks_.empty());
    if (locals_.empty()) locals_.resize(1 + fn->get_num_args());
    B->SetInsertPointPastAllocas(*fn);

    {
        // allocate memory for local variables
        auto *type = fn->get_type();
        if (type->get_return_kind() != ReturnKind::SRET) {
            auto *return_type = type->get_return_type();
            locals_.front() = B->CreateAlloca(*return_type);
        } else {
            locals_.front() = fn->get_fn()->getArg(0);
        }
        // copy arguments from base class
        for (auto i = 0U; i < fn->get_num_args(); ++i)
            locals_[1 + i] = get_arg(i);
        // allocate stack memory for the rest of the locals
        for (auto i = 1 + fn->get_num_args(); i < mir->locals->count; ++i) {
            auto const place = MirPlaceList_get(mir->locals, i);
            auto *type = IrGetPtr(place.type)->pointee;
            locals_[i] = B->CreateAlloca(*G.get_type(type));
        }
    }

    // There should be no alloca instructions after this point. alloca can
    // only appear at the start of the entry block. Note that the entry
    // block is not contained in the "blocks_" list. It is a special basic
    // blocks not represented in the MIR.
    B->SetInsertPoint(get_entry());

    for (auto i = 0U; i < captured_.size(); ++i) {
        auto const L = unsigned(MirCaptureList_get(mir->captured, i).local.value);
        auto *type = G.get_type(MirLocalDataList_get(mir->local_data, L).type);
        paw_assert(L > 0);

        if (L <= fn->get_num_args()) {
            auto *capture_slot = X->create_alloc(*type);
            // initialize with value of argument
            auto *arg = X->load_value(*type, get_local_ptr(L));
            X->store_value(arg, capture_slot);
            locals_.at(L) = capture_slot;
        }
    }

    for (int i = 0; i < mir->blocks->count; ++i) {
        auto const name = "bb" + std::to_string(i);
        blocks_[i] = llvm::BasicBlock::Create(*c, name, *fn);
    }

    for (int i = 0; i < mir->kcache->data->count; ++i)
        constants_[i] = G.create_constant(mir->kcache->data->data[i]);

    for (int i = 0; i < mir->captured->count; ++i) {
        auto const capture = mir->captured->data[i];
        captured_[i] = locals_[capture.local.value];
    }

    if (mir->fn_kind == FUNC_CLOSURE) {
        auto *env_ty = G.create_env_ty(mir->upvalues);
        // initialize upvalues from parent locals or environment
        auto *env_ptr = fn->get_env_ptr();
        auto const n = upvalues_->size();
        for (auto i = 0U; i < n; ++i) {
            (*upvalues_)[i].ptr = B->CreateStructGEP(env_ty, env_ptr, i);
        }
    }

    for (int i = 0; i < mir->children->count; ++i) {
        auto *child = mir->children->data[i];
        closures_[i] = Closure(nullptr,
                (size_t)child->upvalues->count);
    }

    B->CreateBr(blocks_.front());
    G.state_ = this;
}

PawState::~PawState()
{
    if (llvm::verifyFunction(*fn_->get_fn(), &llvm::errs()))
        llvm::errs() << "\nfunction verification failed for "
            << fn_->get_fn()->getName() << "\n";

    G->state_ = outer_;
}

llvm::Value *PawState::get_local_ptr(unsigned index)
{
    return locals_.at(index);
}

llvm::Value *PawState::get_upvalue_ptr(unsigned index)
{
    return X->load_ptr(upvalues_->at(index).ptr);
}

static void link_compilation_artifact(paw_Env *P, std::string prefix)
{
    std::string const root_dir(PAW_ROOT_DIR);
    std::string const libgc_dir(PAW_GC_DIR);

    auto linker = Linker(P)
        .with_object(prefix + ".o")
        .with_arg("-L" + libgc_dir + "/lib")
        .with_staticlib("gc")
        .with_arg("-L" + root_dir)
        .with_arg("--coverage") // TODO
        .with_staticlib("paw_stdc");

    auto const o = P->options;
    for (int i = 0; i < o.num_linker_paths; ++i) {
        std::string const path(o.linker_paths[i]);
        linker.add_arg("-L" + path);
    }
    for (int i = 0; i < o.num_linker_specs; ++i) {
        std::string spec(o.linker_specs[i]);

        bool is_static = true;
        auto const pos = spec.find('=');
        if (pos != std::string::npos) {
            is_static = spec.starts_with("static");
            spec = spec.substr(pos + 1);
        }

        if (is_static) {
            linker.link_staticlib(spec);
        } else {
            linker.link_dylib(spec);
        }
    }

    // invoke the linker
    std::move(linker)
        .finalize(prefix);
}

} // namespace paw::cg

void pawCodegen_generate(Compiler *C, TranslationUnit const *tu)
{
    using namespace paw::cg;
    auto *P = ENV(C);

    CodegenOptions const cgopt = {
        .compile_only = P->options.compile_only,
        .build_tests = P->options.build_tests,
        .verify_module = P->options.verify_ir,
        .print_ir = P->options.dump_ir,
        .enable_asan = P->options.enable_asan,
        .opt_suffix = P->options.opt_suffix,
    };
    std::string prefix, filename;
    if (P->options.output_filename != NULL) {
        if (P->options.output_dirname != NULL) {
            std::string const dirname(P->options.output_dirname);
            prefix = dirname + PAW_FOLDER_SEPS[0];
        }
        filename = P->options.output_filename;
    } else {
        filename = tu->modname;
    }

    CodeGenerator cg(C, tu->modname, cgopt);
    cg.startup_module(tu->mirs, tu->mir_count);
    for (int i = 0; i < tu->mir_count; ++i)
        cg.define_fn(tu->mirs[i]);
    cg.compile_module(prefix, filename);
    cg.teardown_module();

    if (!cgopt.compile_only)
        link_compilation_artifact(P, prefix + filename);
    if (cgopt.build_tests)
        link_compilation_artifact(P, prefix + "test_" + filename);
}

