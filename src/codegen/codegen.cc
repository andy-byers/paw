// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// TODO: Prevent reference arguments from being captured in closures
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
#include "mangle.h"
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
    } else if (std::is_same_v<Ty, ArrayType>) {
        paw_assert(type->is_array_type());
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

    paw_assert(def->variants->count == 1);
    auto const *variant = def->variants->data[0];
    return variant->fields->count == 0;
}

static std::string mangle_mir_name(Mir const *mir)
{
    Annotation annotation;
    if (pawP_check_extern(mir->C, mir->annotations, &annotation)) {
        if (annotation.has_value) {
            // #[extern = "function_name"]
            paw_assert(annotation.kind == BUILTIN_STR);
            return to_string((::Str const *)annotation.value.p);
        }
    }

    return to_string(mangle_type(mir->C, mir->type));
}

static std::string mangle_internal_method_name(Compiler *C, IrType *self, std::string name)
{
    return to_string(mangle_type(C, self))
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

    Type *get_or_create_type(IrType *irtype)
    {
        auto *const *type_ptr = types_->lookup(irtype);
        if (type_ptr != nullptr) return *type_ptr;
        auto *type = create_type(irtype);
        types_->insert(irtype, type);
        return type;
    }

private:
    Type *create_type(IrType *irtype)
    {
        switch (IR_KINDOF(irtype)) {
            case kIrNever:
            case kIrUnit:
                return X->get_unit_type();
            case kIrBool:
                return X->get_bool_type();
            case kIrChar:
                return X->get_char_type();
            case kIrInt:
                return X->get_int_type();
            case kIrFloat:
                return X->get_float_type();
            case kIrPtr:
                return create_ptr_type();
            case kIrString:
                return X->get_str_type();
            case kIrSlice:
                return create_slice_type(irtype);
            case kIrTuple:
                return create_tuple_type(irtype);
            case kIrArray:
                return create_array_type(irtype);
            case kIrAdt:
                return create_adt(irtype);
            default:
                paw_assert(IR_IS_FUNC_TYPE(irtype));
                return create_fn_type(irtype);
        }
    }

    Type *create_fn_type(IrType *irtype)
    {
        auto *params = ir_fn_params(C, irtype);
        auto *result = ir_fn_result(C, irtype);
        auto const is_closure = !IrIsSignature(irtype);
        auto const is_method = is_method_type(irtype);
        auto *return_type = get_or_create_type(result);
        return X->get_fn_type(return_type,
                get_or_create_types(params),
                is_closure ? FUNC_CLOSURE :
                    is_method ? FUNC_METHOD :
                    FUNC_FUNCTION,
                true /* has_env */,
                IrIsNever(result));
    }

    Type *create_tuple_type(IrType *irtype)
    {
        auto const field_types = get_or_create_types(IrGetTuple(irtype)->elems);
        return X->get_tuple_type(field_types);
    }

    Type *create_ptr_type()
    {
        return X->get_ptr_type();
    }

    Type *create_array_type(IrType *irtype)
    {
        auto const *t = IrGetArray(irtype);
        paw_assert(t->length->kind == IR_CONST_VALUE);
        auto const length = t->length->value;
        paw_assert(IrIsInt(length.type));
        return X->get_array_type(
                    get_or_create_type(t->type),
                    uint64_t(length.value.i));
    }

    Type *create_slice_type(IrType *irtype)
    {
        auto const elem_type = get_or_create_type(IrGetSlice(irtype)->type);
        return X->get_struct_type({
                    X->get_ptr_type(elem_type),
                    X->get_int_type(),
                });
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
            auto field_types = get_or_create_types(field_irtypes);
            field_types.insert(begin(field_types), discr_type);
            variant_types[unsigned(i)] = field_types;
        }

        return X->get_object_type(variant_types);
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

    Type *create_adt(IrType *irtype)
    {
        auto const *def = pawIr_get_adt_def(C, IR_TYPE_DID(irtype));
        auto variant_types = create_adt_variants(irtype, def->variants->count);
        if (!def->is_struct) {
            // add the discriminant to the start of each variant
            auto *discr_type = define_discr_type(def->variants->count);
            for (auto &field_types: variant_types)
                field_types.insert(begin(field_types), discr_type);
        }
        return X->get_object_type(variant_types);
    }

    std::vector<ObjectType::FieldTypes> create_adt_variants(IrType *irtype, int num_variants)
    {
        std::vector<ObjectType::FieldTypes> variant_types;
        for (int discr = 0; discr < num_variants; ++discr) {
            auto const *field_irtypes = pawP_instantiate_variant_fields(C, IrGetAdt(irtype), discr);
            auto field_types = get_or_create_types(field_irtypes);
            variant_types.push_back(field_types);
        }
        return variant_types;
    }

    std::vector<Type *> get_or_create_types(IrTypeList const *irtypes)
    {
        std::vector<Type *> types;
        types.reserve(size_t(irtypes->count));
        K_LIST_XFOREACH (irtypes, IrType *const, p)
            types.push_back(get_or_create_type(*p));
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

enum class ValueKind {
    // Value is a static single assignment (SSA) variable, i.e. it is assigned exactly
    // once and can be used 0 or more times. Used for scalar values that do not require
    // an address.
    SSA,

    // Value is backed by either an "alloca" or a heap-allocated "upvalue" slot.
    MEMORY,
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

    ValueKind get_value_kind(MirRegister r) const
    {
        return value_kinds_.at(unsigned(r.value));
    }

    bool has_address(MirRegister r) const
    {
        return get_value_kind(r) == ValueKind::MEMORY;
    }

    void set_raw_value(MirRegister r, llvm::Value *value)
    {
        values_.at(unsigned(r.value)) = value;
    }

    llvm::Value *get_raw_value(MirRegister r) const
    {
        return values_.at(unsigned(r.value));
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
    std::vector<llvm::Value *> values_;
    std::vector<ValueKind> value_kinds_;
    std::vector<llvm::Value *> constants_;
    std::vector<llvm::Value *> captured_;
    std::vector<Upvalue> *upvalues_;
    std::vector<Closure> closures_;
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

    if (options.verify_module) {
        std::string error;
        llvm::raw_string_ostream os(error);
        if (llvm::verifyModule(*m, &os)) {
            print_ir(*m, modname + "_failure.ll");
            fatal_error("module verification failed:\n" + error);
        }
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
    if (options.enable_asan)
        mpm.addPass(llvm::AddressSanitizerPass({}));
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
            auto *fn_ty = llvm::FunctionType::get(B->getInt32Ty(), B->getPtrTy(), true);
            auto callee = m->getOrInsertFunction("printf", fn_ty);
            B->CreateCall(callee, {
                        B->CreateGlobalString("TEST " + name + '\n'),
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

    bool is_core_op(Mir const *mir, char const *module_name, char const *name)
    {
        if (mir->self == nullptr && IrIsSignature(mir->type)) {
            // TODO: could cause problems if a user module is called "ptr"
            auto const modno = (int)IR_TYPE_DID(mir->type).modno;
            auto const *modname = ModuleInfo_get(C->modinfo, modno).name;
            return pawS_eq(modname, SCAN_STR(C, module_name))
                && pawS_eq(mir->name, SCAN_STR(C, name));
        }
        return false;
    }

    bool is_builtin(Mir const *mir, char const *name)
    {
        return mir->self == nullptr && IrIsSignature(mir->type)
            && IR_TYPE_DID(mir->type).modno == PRELUDE_MODNO
            && pawS_eq(mir->name, SCAN_STR(C, name));
    }

    // fn ptr::read<T>(p: *T) -> T
    void generate_ptr_read(Mir const *mir, Fn *fn)
    {
        auto *pointee_type = get_type(IrGenericArg_get_type(
                    IR_FIRST_GENERIC_ARG(mir->type)));

        State state(X, fn);
        auto *result = B->CreateLoad(pointee_type->get_ty(), fn->get_arg(0));
        state.create_return(result);
    }

    // fn ptr::write<T>(p: *T, value: T)
    void generate_ptr_write(Mir const *mir, Fn *fn)
    {
        State state(X, fn);
        B->CreateStore(fn->get_arg(1), fn->get_arg(0));
        state.create_return();
    }

    // fn ptr::add<T>(p: *T, n: int) -> *T
    void generate_ptr_add(Mir const *mir, Fn *fn)
    {
        auto *pointee_type = get_type(IrGenericArg_get_type(
                    IR_FIRST_GENERIC_ARG(mir->type)));

        State state(X, fn);

        auto *result = B->CreateInBoundsGEP(
                pointee_type->get_ty(),
                fn->get_arg(0),
                fn->get_arg(1));

        state.create_return(result);
    }

    // fn ptr::drop<T>(p: *T)
    void generate_ptr_drop(Mir const *mir, Fn *fn)
    {
        State state(X, fn);

        // TODO: if "T: Drop", then call "<T as Drop>::drop()"

        state.create_return();
    }

    // fn sizeof<T>() -> int
    void generate_sizeof_intrinsic(Mir const *mir, Fn *fn)
    {
        auto *type = get_type(IrGenericArg_get_type(
                    IR_FIRST_GENERIC_ARG(mir->type)));

        State state(X, fn);
        auto *result = X.create_int(X.size_of(*type));
        state.create_return(result);
    }

    // fn alignof<T>() -> int
    void generate_alignof_intrinsic(Mir const *mir, Fn *fn)
    {
        auto *type = get_type(IrGenericArg_get_type(
                    IR_FIRST_GENERIC_ARG(mir->type)));

        State state(X, fn);
        auto *result = X.create_int(X.align_of(*type).value());
        state.create_return(result);
    }

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
            paw_Bool const materialize_return = builtin_kind(fptr->result) != BUILTIN_INT;
            create_main_fn_wrapper(*fn, materialize_return);
        }

        if (is_core_op(mir, "ptr", "read"))
            generate_ptr_read(mir, fn);

        if (is_core_op(mir, "ptr", "write"))
            generate_ptr_write(mir, fn);

        if (is_core_op(mir, "ptr", "add"))
            generate_ptr_add(mir, fn);

        if (is_core_op(mir, "ptr", "drop"))
            generate_ptr_drop(mir, fn);

        if (is_core_op(mir, "mem", "sizeof"))
            generate_sizeof_intrinsic(mir, fn);

        if (is_core_op(mir, "mem", "alignof"))
            generate_alignof_intrinsic(mir, fn);

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
        FOREACH_TYPE(C->typesystem.types, irtype, {
                    translator.get_or_create_type(irtype);
                });

#undef FOREACH_TYPE

        // declare all toplevel functions
        for (int i = 0; i < mir_count; ++i) {
            auto const *mir = mirs[i];
            declare_fn(mir);
        }

        // declare/generate internal functions that are only called from
        // generated code
        generate_builtins();

        FnType constructor_type(X, X.get_unit_type(), {}, FUNC_FUNCTION);
        auto constructor_fn = Fn(
                X, "paw_constructor",
                llvm::GlobalValue::InternalLinkage,
                &constructor_type);
        {
            State state(X, &constructor_fn);

            StringMapIterator iter;
            StringMapIterator_init(C->strings, &iter);
            while (StringMapIterator_is_valid(&iter)) {
                auto const *s = StringMapIterator_key(&iter);
                auto *array = llvm::ConstantDataArray::getString(*c, s->text, true);

                auto *global = new llvm::GlobalVariable(
                        **M, array->getType(), false,
                        llvm::GlobalValue::PrivateLinkage,
                        array, "str");
                global->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
                global->setAlignment(llvm::Align(1));

                strings_[s] = StringDescriptor{
                    .text = llvm::ConstantExpr::getGetElementPtr(
                        global->getType(), global, X.create_int(0)),
                    .length = X.create_uint(s->length),
                };

                StringMapIterator_next(&iter);
            }

            state.create_return();
        }

        FnType destructor_type(X, X.get_unit_type(), {}, FUNC_FUNCTION);
        auto destructor_fn = Fn(X, "paw_destructor",
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

    llvm::Value *create_cstr_to_str(llvm::Value *cstr)
    {
        auto *len = B->CreateCall(X.get_strlen_callee(), {cstr});
        llvm::Value *str = llvm::UndefValue::get(X.get_str_ty());
        str = B->CreateInsertValue(str, cstr, 0);
        str = B->CreateInsertValue(str, len, 1);
        return str;
    }

    void create_main_fn_wrapper(llvm::Function *inner, bool materialize_return)
    {
        inner->setName("paw_main");
        inner->setLinkage(llvm::Function::PrivateLinkage);

        FnType main_type(X, X.get_int32_type(), {
                    X.get_int32_type(),
                    X.get_ptr_type(),
                }, FUNC_FUNCTION, false);
        Fn main_fn(X, "main", llvm::Function::ExternalLinkage, &main_type);
        State state(X, &main_fn);

        auto *ret = B->CreateCall(inner, {X.create_null_ptr(), X.create_null_ptr()});
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

    Unit::Methods const *get_unit_methods() const { return &scalar_info_.u.methods; }
    Bool::Methods const *get_bool_methods() const { return &scalar_info_.b.methods; }
    Char::Methods const *get_char_methods() const { return &scalar_info_.c.methods; }
    Int::Methods const *get_int_methods() const { return &scalar_info_.i.methods; }
    Float::Methods const *get_float_methods() const { return &scalar_info_.f.methods; }
    Str::Methods const *get_str_methods() const { return &scalar_info_.s.methods; }

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
        auto *m = M->get_module();

        // TODO: Define a minimal panic handler to be shipped with the core and a more informative one to be
        //   linked in when using the standard library. Just trap or enter an infinite loop in the minimal
        //   version. Write the message to stderr and exit(1) in the stdlib version.

        // define default panic handler
        {
            static constexpr char const *const STDERR_NAME =
#if defined(PAW_OS_MACOS)
                "__stderrp"
#else
                "stderr"
#endif
                ;

            // extern FILE *stderr;
            auto *stream_ptr = new llvm::GlobalVariable(
                *m, B->getPtrTy(), false,
                llvm::GlobalValue::ExternalLinkage,
                nullptr, STDERR_NAME);

            // size_t fwrite(const void* restrict buffer, size_t size, size_t count, FILE* restrict stream);
            auto *write = llvm::cast<llvm::Function>(
                    m->getOrInsertFunction("fwrite",
                        llvm::FunctionType::get(X.get_i64_ty(),
                        {X.get_ptr_ty(), X.get_i64_ty(),
                         X.get_i64_ty(), X.get_ptr_ty()}, false))
                    .getCallee());

            // ABI type of "*[char]" in argument position
            auto *payload_ty = X.get_slice_type(
                    X.get_char_type())->get_abi_ty();
            auto callee = M->get_module()
                ->getOrInsertFunction("paw_panic_handler",
                    llvm::FunctionType::get(X.get_void_ty(),
                        {X.get_ptr_ty(), payload_ty}, false));
            auto *fn = llvm::cast<llvm::Function>(callee.getCallee());
            fn->setDoesNotThrow();
            fn->setDoesNotReturn();

            auto *block = llvm::BasicBlock::Create(*c, "block", fn);
            B->SetInsertPoint(block);

            auto *stream = B->CreateLoad(
                B->getPtrTy(),
                stream_ptr,
                "stream");

            auto *ptr = B->CreateExtractValue(fn->getArg(1), 0ULL);
            auto *len = B->CreateExtractValue(fn->getArg(1), 1ULL);
            B->CreateCall(write, {ptr, X.create_i64(1), len, stream});

            auto *trap = llvm::Intrinsic::getOrInsertDeclaration(*M, llvm::Intrinsic::trap);
            B->CreateCall(trap);

            B->CreateUnreachable();
        }

        // declare "ptr @paw_mem_alloc(i64)" builtin
        {
            auto *fn = llvm::Function::Create(
                    llvm::FunctionType::get(X.get_ptr_ty(),
                        {X.get_ptr_ty(), X.get_i64_ty()}, false),
                    llvm::GlobalValue::ExternalLinkage,
                    get_builtin_name(BuiltinFn::PAW_ALLOC),
                    *M);
            fn->setDoesNotThrow();
        }

        // declare "ptr @paw_mem_realloc(ptr, i64)" builtin
        {
            auto *fn = llvm::Function::Create(
                    llvm::FunctionType::get(X.get_ptr_ty(),
                        {X.get_ptr_ty(), X.get_ptr_ty(), X.get_i64_ty()}, false),
                    llvm::GlobalValue::ExternalLinkage,
                    get_builtin_name(BuiltinFn::PAW_REALLOC),
                    *M);
            fn->setDoesNotThrow();
        }

        // declare "void @paw_mem_dealloc(ptr)" builtin
        {
            auto *fn = llvm::Function::Create(
                    llvm::FunctionType::get(X.get_void_ty(),
                        {X.get_ptr_ty(), X.get_ptr_ty()}, false),
                    llvm::GlobalValue::ExternalLinkage,
                    get_builtin_name(BuiltinFn::PAW_DEALLOC),
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

        // declare "void @paw_builtin_check_bounds(i64, i64)" builtin
        {
            auto *fn = llvm::Function::Create(
                    llvm::FunctionType::get(X.get_void_ty(), {
                        X.get_i64_ty(),
                        X.get_i64_ty(),
                    }, false),
                    llvm::GlobalValue::ExternalLinkage,
                    get_builtin_name(BuiltinFn::CHECK_BOUNDS),
                    *M);
            fn->setDoesNotThrow();
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

        // declare "i64 @paw_builtin_rawcmp(ptr, i64, ptr, i64)" builtin
        {
            auto *fn = llvm::Function::Create(
                    llvm::FunctionType::get(X.get_i64_ty(), {
                        X.get_ptr_ty(),
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

        // TODO: ckd_i* are broken b/c panic() cannot be easily called anymore (it takes a
        // TODO: slice by value which needs to have type "i64[2]" per the ABI), implement directly in Paw

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

    Fn *get_method(IrType *self, std::string name)
    {
        auto &methods = methods_[self];
        auto const itr = methods.find(name);
        return itr != end(methods) ? itr->second : nullptr;
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

        if (has_annotation(C, mir->annotations, "test")) {
            struct IrFnPtr const *fn = IrGetFnPtr(IR_GET_FN(C, mir->type));
            // TODO: perform this check much earlier? like in collect_items.c
            if (fn->params->count > 0) {
                IncorrectArityError const error = {
                    .modname = SCAN_STR(C, modname_.c_str()),
                    .have = fn->params->count,
                    .want = 0,
                    .span = mir->span,
                };
                pawErr_throw(C, kErrIncorrectArity, (void *)&error);
            }
            test_names_.push_back(mangled_name);
        }

        fns_.insert(mir->type, std::move(fn));
    }

    bool is_thin_ptr(IrType *type)
    {
        return IrIsPtr(type) && !IrIsString(ir_deref(type)) && !IrIsSlice(ir_deref(type));
    }

    void create_instruction(MirInstruction *instr)
    {
        switch (MIR_KINDOF(instr)) {
            case kMirNoop:
            case kMirKill:
                return;
            case kMirPhi:
                create_phi(instr->Phi_);
                break;
            case kMirDrop:
                create_drop(instr->Drop_);
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
            case kMirArray:
                create_array(instr->Array_);
                break;
            case kMirArrayGep:
                create_arraygep(instr->ArrayGep_);
                break;
            case kMirStructGEP:
                create_structgep(instr->StructGEP_);
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
            case kMirGetRange:
            case kMirSetRange:
                PAW_UNREACHABLE();
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

        auto const [text, len] = itr->second;
        llvm::Value *str = llvm::UndefValue::get(X.get_str_ty());
        str = B->CreateInsertValue(str, text, 0);
        str = B->CreateInsertValue(str, len, 1);
        return str;
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

    // TODO: locals that became SSA registers need their drops removed, then use ir_deref instead of ir_auto_deref
    // TODO: also remove check for pawIr_needs_drop
    void create_drop(MirDrop const &x)
    {
        auto *target_type = x.target.type;
        llvm::Value *target;
        if (is_thin_ptr(target_type)) {
            // must be dropping a field
            target_type = ir_deref(target_type);
            target = operand(x.target);
        } else {
            target = state_->get_raw_value(x.target.r);
        }
        if (pawIr_needs_drop(C, target_type)) {
            auto *irtype = pawIr_get_custom_drop_type(C, target_type);
            B->CreateCall(get_fn(irtype)->get_fn(), {
                    X.create_null_ptr(), target});
        }
    }

    void create_move(MirMove const &x)
    {
        set_result(x.output, operand(x.target));
    }

    void create_addrof(MirAddrOf const &x)
    {
        paw_assert(x.input.kind == MIR_PLACE_REGISTER);
        paw_assert(state_->has_address(x.input.r));
        set_result(x.output, state_->get_raw_value(x.input.r));
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
        X.store_value(value, pointer);
    }

    void create_global(MirGlobal const &x)
    {
        auto *itr = fns_.lookup(x.output.type);
        paw_assert(itr != nullptr);

        auto *fn = (*itr)->get_value();
        set_result(x.output, fn);
    }

    bool needs_upvalue_slot(MirRegister r)
    {
        auto *fn = state_->fn_;

        // NOTE: the return value (%0) cannot be captured and slots for captured
        //   arguments (%1..%N) are allocated and initialized at the start of the
        //   function.
        auto const data = *mir_reg_data((Mir *)state_->mir_, r);
        return data.is_captured && unsigned(r.value) > fn->get_num_args();
    }

    void create_alloclocal(MirAllocLocal const &x)
    {
        if (needs_upvalue_slot(x.output.r)) {
            paw_assert(state_->has_address(x.output.r));
            auto *slot = X.create_alloc(*get_type(x.output.type));
            state_->set_raw_value(x.output.r, slot);
        }
    }

    void create_aggregate(MirAggregate const &x)
    {
        auto *object_type = cast<ObjectType>(get_type(x.output.type));
        auto *variant_ty = object_type->get_variant_ty(Discriminant(x.discr));
        llvm::Value *object = llvm::UndefValue::get(variant_ty);
        for (auto i = 0U; i < unsigned(x.fields->count); ++i) {
            auto *element = operand(x.fields->data[i]);
            object = B->CreateInsertValue(object, element, i);
        }
        set_result(x.output, object);
    }

    llvm::Value *operand(MirPlace const place)
    {
        switch (place.kind) {
            case MIR_PLACE_REGISTER:
                switch (state_->get_value_kind(place.r)) {
                    case ValueKind::SSA:
                        return state_->get_raw_value(place.r);
                    case ValueKind::MEMORY:
                        return B->CreateLoad(*get_type(place.type),
                                state_->get_raw_value(place.r));
                }
            case MIR_PLACE_UPVALUE:
                return B->CreateLoad(*get_type(place.type),
                        state_->get_upvalue_ptr(unsigned(place.up)));
            case MIR_PLACE_CONSTANT:
                return state_->constants_.at(unsigned(place.k.value));
        }
    }

    void create_array(MirArray const &x)
    {
        auto *array_type = cast<ArrayType>(get_type(x.output.type));
        llvm::Value *array = llvm::UndefValue::get(array_type->get_ty());
        for (unsigned i = 0; i < (unsigned)x.elems->count; ++i) {
            auto *elem = operand(x.elems->data[i]);
            array = B->CreateInsertValue(array, elem, i);
        }
        set_result(x.output, array);
    }

    Type *get_deref_type(IrType *irtype)
    {
        return get_type(pawIr_remove_indirection(C, irtype));
    }

    void create_structgep(struct MirStructGEP const &x)
    {
        auto *value = !is_thin_ptr(x.object.type)
            ? state_->get_raw_value(x.object.r)
            : operand(x.object);
        auto *obj_type = (ObjectType *)get_deref_type(x.object.type);
        Object obj(*state_, value, obj_type);

        Discriminant const discr(x.discr);
        auto *field_ptr = obj.get_field_ptr(discr, unsigned(x.field));
        set_result(x.output, field_ptr);
    }

    void create_arraygep(MirArrayGep const &x)
    {
        auto *array = operand(x.array);
        auto *index = operand(x.index);

        {
            auto *konst = IrGetArray(ir_deref(x.array.type))->length;
            paw_assert(konst->kind == IR_CONST_VALUE);
            auto const length = konst->value.value.i;
            X.create_check_bounds(index, X.create_int(length));
        }

        auto *element_type = get_type(ir_deref(x.output.type));
        auto *element_ptr = B->CreateInBoundsGEP(element_type->get_ty(), array, index);
        set_result(x.output, element_ptr);
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
                    if (impl_def->trait == nullptr && IrIsAdt(impl_def->type)) {
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
            auto const arg = IR_FIRST_GENERIC_ARG(
                        pawIr_get_context(C, x.target.type));
            auto *element_type = get_type(IrGenericArg_get_type(arg));
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

        std::vector<llvm::Value *> args;
        args.reserve((size_t)x.args->count);
        K_LIST_XFOREACH (x.args, MirPlace const, p)
            args.push_back(operand(*p));

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
                ? state_->values_.at(up.index)
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
                X.create_null_ptr(),
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

    void create_unreachable(MirUnreachable const &x)
    {
        B->CreateUnreachable();
    }

//TODO    llvm::Value *load_result(llvm::Value *value)
//TODO    {
//TODO        auto *result_type = get_type(ir_fn_result(C, state_->mir_->type));
//TODO        return B->CreateLoad(*result_type, value);
//TODO    }

    void create_return(MirReturn const &)
    {
        auto const result = pawMir_get_register(state_->mir_, MirRegister{0});
        state_->create_return(is_empty_irtype(C, result.type)
                ? nullptr : operand(result));
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
//TODO        if (is_thin_ptr(x.discr.type)) // TODO: hack
//TODO            discr = B->CreateLoad(get_type(ir_deref(x.discr.type))->get_ty(), discr);
        if (IrIsFloat(x.discr.type))
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
        irtype = pawIr_remove_indirection(C, irtype);
        switch (IR_KINDOF(irtype)) {
            case kIrBool:
            case kIrChar:
            case kIrInt:
            case kIrFloat:
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
        switch (state_->get_value_kind(place.r)) {
            case ValueKind::SSA:
                state_->values_.at(unsigned(place.r.value)) = value;
                break;
            case ValueKind::MEMORY:
                B->CreateStore(value, state_->get_raw_value(place.r));
                break;
        }
    }


    std::unique_ptr<llvm::LLVMContext> ctx_;

    mutable Context X;
    Module *M;
    Compiler *C;
    llvm::IRBuilder<> *B;
    std::string modname_;
    llvm::TargetMachine *machine_;
    CodegenOptions options_;
    PawState *state_;

    // mapping from IR strings to runtime global variables
    struct StringDescriptor { llvm::Value *text; llvm::Value *length; };
    std::unordered_map<::Str const *, StringDescriptor> strings_;

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
};


PawState::PawState(CodeGenerator &G, Fn *fn, Mir const *mir, std::vector<Upvalue> *upvalues, PawState *outer)
    : State(*G.get_context(), fn)
    , G(&G)
    , B(X->get_builder())
    , mir_(mir)
    , before_block_(B->GetInsertBlock())
    , outer_(outer)
    , blocks_(unsigned(mir->blocks->count))
    , values_(unsigned(mir->registers->count))
    , value_kinds_(unsigned(mir->registers->count))
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
    if (values_.empty()) {
        values_.resize(1 + fn->get_num_args());
        value_kinds_.resize(1 + fn->get_num_args());
    }
    B->SetInsertPointPastAllocas(*fn);

    {
        // allocate memory for local variables
        auto *type = fn->get_type();
        if (type->get_return_kind() != ReturnKind::SRET) {
            auto *return_type = type->get_return_type();
            values_.front() = B->CreateAlloca(*return_type);
        } else {
            values_.front() = fn->get_fn()->getArg(0);
        }
        // copy arguments from base class
        for (auto i = 0U; i < fn->get_num_args(); ++i)
            values_[1 + i] = get_arg(i);
        for (auto i = 0U; i <= fn->get_num_args(); ++i)
            value_kinds_[i] = ValueKind::MEMORY;
        // allocate stack memory for the rest of the locals
        for (auto i = 1 + fn->get_num_args(); i < mir->registers->count; ++i) {
            auto const data = MirRegisterDataList_get(mir->registers, i);
            if (data.is_nontrivial) {
                if (!data.is_captured)
                    values_[i] = B->CreateAlloca(*G.get_type(data.type));
                value_kinds_[i] = ValueKind::MEMORY;
            } else {
                value_kinds_[i] = ValueKind::SSA;
            }
        }
    }

    // There should be no alloca instructions after this point. alloca can
    // only appear at the start of the entry block. Note that the entry
    // block is not contained in the "blocks_" list. It is a special basic
    // blocks not represented in the MIR.
    B->SetInsertPoint(get_entry());

    for (auto i = 0U; i < captured_.size(); ++i) {
        auto const r = unsigned(MirCaptureList_get(mir->captured, i).local.value);
        auto *type = G.get_type(MirRegisterDataList_get(mir->registers, r).type);
        paw_assert(r > 0);

        if (r <= fn->get_num_args()) {
            auto *capture_slot = X->create_alloc(*type);
            // initialize with value of argument
            auto *arg = X->load_value(*type, values_.at(r));
            X->store_value(arg, capture_slot);
            values_.at(r) = capture_slot;
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
        captured_[i] = values_[capture.local.value];
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
    std::string error;
    llvm::raw_string_ostream os(error);
    if (llvm::verifyFunction(*fn_->get_fn(), &os))
        llvm::errs() << "\nfunction verification failed for "
            << fn_->get_fn()->getName() << ":\n" << error << "\n";

    G->state_ = outer_;
}

llvm::Value *PawState::get_local_ptr(unsigned index)
{
    return values_.at(index);
}

llvm::Value *PawState::get_upvalue_ptr(unsigned index)
{
    return X->load_ptr(upvalues_->at(index).ptr);
}

static void link_compilation_artifact(paw_Env *P, std::string prefix)
{
    std::string const root_dir(PAW_ROOT_DIR);

    auto linker = Linker(P)
        .with_object(prefix + ".o")
        .with_arg("-L" + root_dir)
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
    if (P->options.output_filename != nullptr) {
        if (P->options.output_dirname != nullptr) {
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

