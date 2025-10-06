// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include <algorithm>
#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <map>
#include <memory>
#include <string>
#include <vector>

#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/TargetParser/Host.h"
#include "llvm/Transforms/Instrumentation/AddressSanitizer.h"
#include "llvm/Transforms/Instrumentation/SanitizerCoverage.h"
#include "llvm/Transforms/Utils/ModuleUtils.h"

#include "codegen.h"
#include "glue.h"
#include "ir_type.h"
#include "mir.h"


static const int PRIORITY_USER = 65535;

// "no debug" flag
static bool s_ndebug = false;

static bool is_empty_ty(llvm::Type *ty)
{
    return ty->isStructTy() && ty->getStructNumElements() == 0;
}

static std::string opt_name(llvm::OptimizationLevel opt)
{
    if (opt == llvm::OptimizationLevel::O0) {
        return "O0";
    } else if (opt == llvm::OptimizationLevel::O1) {
        return "O1";
    } else if (opt == llvm::OptimizationLevel::O2) {
        return "O2";
    } else if (opt == llvm::OptimizationLevel::O3) {
        return "O3";
    } else if (opt == llvm::OptimizationLevel::Os) {
        return "Os";
    } else if (opt == llvm::OptimizationLevel::Oz) {
        return "Oz";
    }
}

enum StreamKind {
    STREAM_STDIN = 0,
    STREAM_STDOUT = 1,
    STREAM_STDERR = 2,
};

enum BuiltinFn {
    PRELUDE_ASSERT,
    PRELUDE_PRINT,
    PRELUDE_PRINTLN,
    PRELUDE_PANIC,
    STR_HASH,
    NOT_BUILTIN
};

#define NUM_BUILTINS NOT_BUILTIN

struct Key {
    int *type;
    Compiler *C;

    explicit Key(Compiler *C, int *type)
        : C(C), type(type) {}

    bool operator==(Key const &rhs) const
    {
        int *a = type;
        int *b = rhs.type;
        return a[0] == b[0];
    }
};

template <>
struct std::hash<Key>
{
    size_t operator()(Key const &key) const
    {
        int *v = key.type;
        return (size_t)v[0];
    }
};

struct TypeKey {
    Compiler *C;
    IrType *type;

    explicit TypeKey(Compiler *C, IrType *type)
        : C(C), type(type) {}

    bool operator==(TypeKey const &rhs) const
    {
        IrType *a = type;
        IrType *b = rhs.type;
        return pawIr_type_equals(C, a, b);
    }
};

template <>
struct std::hash<TypeKey>
{
    size_t operator()(TypeKey const &key) const
    {
        IrType *v = key.type;
        return (size_t)pawIr_type_hash(key.C, v);
    }
};

class Context;

struct CgList {
    friend class Context;

    explicit CgList(Context &ctx, llvm::Value *list, llvm::Type *element_ty);
    static CgList construct(Context &ctx, llvm::Type *element_ty, llvm::Value *length);
    static CgList construct(Context &ctx, llvm::Type *element_ty, paw_Int length);

    llvm::Value *gep(llvm::Value *index);
    llvm::Value *gep(paw_Int index);
    llvm::Value *push(llvm::Value *value);
    llvm::Value *pop();

    llvm::Value *get_data() const;
    llvm::Value *get_length() const;
    llvm::Value *get_capacity() const;

    llvm::Value *get_list() const { return list_; }
    llvm::Value *get_data_ptr() const { return L_.data_ptr; }
    llvm::Value *get_length_ptr() const { return L_.length_ptr; }
    llvm::Value *get_capacity_ptr() const { return L_.capacity_ptr; }

private:
    llvm::Value *new_buffer(llvm::Value *capacity);
    void grow();

    Context *ctx_;
    llvm::Type *element_ty_;
    llvm::Value *list_;

    struct {
        llvm::Value *data_ptr;
        llvm::Value *length_ptr;
        llvm::Value *capacity_ptr;
    } L_;
};

enum MapFlag: int8_t {
    MAP_VACANT = 0b00,
    MAP_ERASED = 0b01,
    MAP_EXISTS = 0b10,
};

struct CgMap {
    friend class Context;
    explicit CgMap(Context &ctx, IrType *type);

    llvm::Value *gep(llvm::IRBuilder<> &b, llvm::Value *map, llvm::Value *key);
    llvm::Value *nep(llvm::IRBuilder<> &b, llvm::Value *map, llvm::Value *key);
    llvm::Value *construct(llvm::IRBuilder<> &b, llvm::Value *length);

private:
    struct Components {
        llvm::Value *data_ptr;
        llvm::Value *length_ptr;
        llvm::Value *capacity_ptr;

        llvm::Value *data;
        llvm::Value *length;
        llvm::Value *capacity;

        llvm::Value *flags;
        llvm::Value *keys;
        llvm::Value *values;
    };

    llvm::Value *clamp_to_capacity(llvm::IRBuilder<> &b, llvm::Value *value, llvm::Value *capacity);
    llvm::Value *first_index(llvm::IRBuilder<> &b, llvm::Value *key, llvm::Value *capacity);
    llvm::Value *next_index(llvm::IRBuilder<> &b, llvm::Value *index, llvm::Value *capacity);
    llvm::Value *access(llvm::IRBuilder<> &b, llvm::Function *fn, Components &m, llvm::Value *search_key);
    llvm::Value *lookup(llvm::IRBuilder<> &b, llvm::Function *fn, Components &m, llvm::Value *search_key);
    llvm::Value *new_buffer(llvm::IRBuilder<> &b, llvm::Value *capacity);
    Components unpack(llvm::IRBuilder<> &b, llvm::Value *map);
    void grow(llvm::IRBuilder<> &b, llvm::Function *fn, Components &m);

    void create_getp(llvm::IRBuilder<> &b);
    void create_newp(llvm::IRBuilder<> &b);

    Context *ctx_;
    std::unique_ptr<llvm::IRBuilder<>> builder_;

    IrType *map_type_;
    llvm::Type *key_ty_;
    llvm::Type *value_ty_;
    llvm::FunctionType *hash_ty_;
    llvm::FunctionType *equals_ty_;
    llvm::Value *hash_fn_;
    llvm::Value *equals_fn_;

    llvm::Function *gep_fn_;
    llvm::Function *nep_fn_;
};

#define TODO() PAW_UNREACHABLE()

enum ReturnKind {
    RETURN_EMPTY,
    RETURN_SINGLE,
    RETURN_NEVER,
    RETURN_WIDE,
};

struct CgFn {
    explicit CgFn(Context &ctx, llvm::Function *fn);
    explicit CgFn(Context &ctx, Mir const mir);

    llvm::Value *get_arg(unsigned index);
    void return_value(llvm::Value *value);

    Context *ctx;

    CgFn *outer;
    llvm::Function *fn;
    std::vector<std::unique_ptr<CgFn>> closures;
    std::vector<llvm::BasicBlock *> blocks;
    std::vector<llvm::Value *> constants;
    std::vector<llvm::Value *> captures;
    std::vector<llvm::Value *> upvalues;
    std::vector<llvm::Value *> values;
    llvm::BasicBlock *current;
    llvm::Type *return_ty;
    MirBlockData bb;
    Mir mir;

    ReturnKind ret;
};

static std::string S(Str const *str)
{
    return std::string(str->text, str->length);
}

static std::string mangle_fn_name(Compiler *C, Str const *modname, IrType *type, IrType *self)
{
    paw_assert(type->hdr.kind == kIrSignature);
    IrSignature const fsig = type->Signature_;

    IrFnDef const *fdef = pawIr_get_fn_def(C, fsig.did);
    IrTypeList *fdef_types = fdef->is_extern ? NULL : fsig.types;
    if (fsig.self == NULL) return S(pawP_mangle_name(C, modname, fdef->name, fdef_types));

    IrAdtDef const *adef = pawIr_get_adt_def(C, IR_TYPE_DID(self));
    IrTypeList const *adef_types = fdef->is_extern ? NULL : IR_TYPE_SUBTYPES(self);
    return S(pawP_mangle_attr(C, modname, adef->name, adef_types, fdef->name, fdef_types));
}

static std::string mangle_adt_name(Compiler *C, Str const *modname, IrType *type)
{
    paw_assert(type->hdr.kind == kIrAdt);
    IrAdt const adt = type->Adt_;

    IrAdtDef const *def = pawIr_get_adt_def(C, adt.did);
    return S(pawP_mangle_name(C, modname, def->name, adt.types));
}


struct StringLiteral {
    explicit StringLiteral(char const *text, uint64_t length)
        : text(text), length(length) {}
    char const *text;
    uint64_t length;
};
#define STRING_LITERAL(Lit_) StringLiteral(Lit_ "", sizeof(Lit_) - 1)


class Context {
public:
#define TEMP_LITERAL(Text_) get_temp_str(STRING_LITERAL(Text_))
#define PANIC_LITERAL(Text_) call_panic(STRING_LITERAL(Text_))
#define PRINT_LITERAL(Text_) call_print(STRING_LITERAL(Text_))
#define PRINTLN_LITERAL(Text_) call_println(STRING_LITERAL(Text_))

    friend struct CgFn;
    friend struct CgMap;

    explicit Context(Compiler *compiler, std::string name)
        : context_(std::make_unique<llvm::LLVMContext>())
        , module_(std::make_unique<llvm::Module>(name, *context_))
        , builder_(std::make_unique<llvm::IRBuilder<>>(*context_))
        , compiler_(compiler)
        , modname_(name)
        , f_(nullptr)
    {
        llvm::InitializeNativeTarget();
        llvm::InitializeNativeTargetAsmPrinter();

        auto const target_triple = llvm::sys::getDefaultTargetTriple();
        module_->setTargetTriple(target_triple);

        std::string error;
        const auto *target = llvm::TargetRegistry::lookupTarget(target_triple, error);
        if (target == nullptr) {
            llvm::errs() << error;
            return;
        }

        llvm::TargetOptions options;
        auto rm = std::optional<llvm::Reloc::Model>();
        machine_ = target->createTargetMachine(target_triple, "generic", "", options, rm);

        module_->setDataLayout(machine_->createDataLayout());
        malloc_fn_ = llvm::cast<llvm::Function>(
                module_->getOrInsertFunction("malloc",
                    llvm::FunctionType::get(
                        get_ptr_ty(),
                        get_i64_ty(),
                        false)).getCallee());

        unit_ty_ = llvm::StructType::get(*context_, {}, false);

        str_ty_ = llvm::StructType::get(*context_, {
                get_int_ty(), // length
                get_i32_ty(), // hash
                get_array_ty(get_char_ty(), 0), // text[]
            }, false);
    }

    void startup_module()
    {
        create_sdbm();
        create_rawcmp();
        create_print();
        create_println();
        create_panic();
        create_assert();
        create_str_hash();

        auto *void_fn_ty = llvm::FunctionType::get(get_void_ty(), false);
        auto *null_ptr = llvm::ConstantPointerNull::get(get_ptr_ty());

        auto *constructor_fn = llvm::Function::Create(
                void_fn_ty, llvm::GlobalValue::InternalLinkage,
                "paw_constructor", *module_);
        constructor_fn->setDoesNotThrow();
        {
            CgFn f(*this, constructor_fn);
            f_ = &f;

            auto *entry = llvm::BasicBlock::Create(*context_, "entry", constructor_fn);
            builder_->SetInsertPoint(entry);

            // allocate the runtime string internalization table
            auto *cgmap = get_cgmap(compiler_->strtab_type);
            auto *map = cgmap->construct(*builder_,
                    get_int(pawMap_length(compiler_->strings)));
            strtab_ = new llvm::GlobalVariable(
                    *module_, get_ptr_ty(), false,
                    llvm::GlobalValue::ExternalLinkage,
                    null_ptr, "strtab");
            builder_->CreateStore(map, strtab_);

            auto *str_ty = get_str_ty();
            paw_Int iter = PAW_ITER_INIT;
            while (pawMap_iter(compiler_->strings, &iter)) {
                auto const *s = (Str *)pawMap_key(compiler_->P, compiler_->strings, iter)->p;
                auto *array = llvm::ConstantDataArray::getString(*context_, s->text, true);

                auto *temp = builder_->CreateAlloca(array->getType());
                builder_->CreateStore(array, temp);
                auto *data = builder_->CreateConstInBoundsGEP1_32(array->getType(), temp, 0);

                auto *global = new llvm::GlobalVariable(
                        *module_, get_ptr_ty(), false,
                        llvm::GlobalValue::PrivateLinkage,
                        null_ptr, "str");

                auto *str = emit_new_str(temp, get_int(s->length));
                builder_->CreateStore(str, global);
                strings_[s] = global;

//                // internalize the string
//                auto *value_ptr = cgmap->nep(*builder_, map, str);
//                builder_->CreateStore(get_unit(), value_ptr);
            }

            builder_->CreateRetVoid();
            f_ = nullptr;
        }
        llvm::appendToGlobalCtors(*module_, constructor_fn, PRIORITY_USER);

        auto *destructor_fn = llvm::Function::Create(
                void_fn_ty, llvm::GlobalValue::InternalLinkage,
                "paw_destructor", *module_);
        destructor_fn->setDoesNotThrow();
        {
            CgFn f(*this, destructor_fn);
            f_ = &f;

            auto *entry = llvm::BasicBlock::Create(*context_, "entry", destructor_fn);
            builder_->SetInsertPoint(entry);

            builder_->CreateRetVoid();
            f_ = nullptr;
        }
        llvm::appendToGlobalDtors(*module_, destructor_fn, PRIORITY_USER);
    }

    void teardown_module()
    {
    }

    llvm::Module *get_module() const
    {
        return module_.get();
    }

    llvm::LLVMContext *get_context() const
    {
        return context_.get();
    }

    llvm::IRBuilder<> *get_builder() const
    {
        return builder_.get();
    }

    llvm::TypeSize bitsize_of(llvm::Type *ty) const
    {
        return module_->getDataLayout().getTypeSizeInBits(ty);
    }

    llvm::TypeSize size_of(llvm::Type *ty) const
    {
        return module_->getDataLayout().getTypeAllocSize(ty);
    }

    llvm::Align align_of(llvm::Type *ty) const
    {
        return module_->getDataLayout().getABITypeAlign(ty);
    }

    uint64_t stride_of(llvm::Type *ty) const
    {
        auto const size = size_of(ty);
        auto const align = align_of(ty);
        return llvm::alignTo(size, align);
    }

    BuiltinFn get_builtin_kind(IrType *type)
    {
        paw_assert(type->hdr.kind == kIrSignature);
        auto const sig = type->Signature_;

        auto const *def = pawIr_get_fn_def(compiler_, sig.did);
        if (!def->is_extern) return NOT_BUILTIN;

        auto const modname = S(compiler_->modnames->data[sig.did.modno]);
        auto const fnname = S(def->name);

        if (sig.self != NULL) {
            paw_assert(sig.self->hdr.kind == kIrAdt);
            auto const adt = sig.self->Adt_;
            auto const *def = pawIr_get_adt_def(compiler_, adt.did);
            auto const selfname = S(def->name);
            if (selfname == "str") {
                if (fnname == "hash") {
                    return STR_HASH;
                }
            }
            return NOT_BUILTIN;
        }

        if (modname == "prelude") {
            if (fnname == "assert") {
                return PRELUDE_ASSERT;
            } else if (fnname == "print") {
                return PRELUDE_PRINT;
            } else if (fnname == "println") {
                return PRELUDE_PRINTLN;
            } else if (fnname == "panic") {
                return PRELUDE_PANIC;
            }
        }
        return NOT_BUILTIN;
    }

    void create_main_fn_wrapper(llvm::Function *fn)
    {
        // Set up the entrypoint function. The entrypoint code converts the standard
        // arguments ("argc"/"argv") into a single variable of type "[str]", then calls
        // the user-defined "main" (renamed to "paw_main") and forwards its integer
        // return value.
        fn->setName("paw_main");
        fn->setLinkage(llvm::Function::PrivateLinkage);

        auto *const char_ty = get_i8_ty();
        auto *const int_ty = get_i32_ty();
        auto *const pchar_ty = char_ty->getPointerTo();
        auto *const ppchar_ty = pchar_ty->getPointerTo();
        auto *const pstr_ty = get_str_ty()->getPointerTo();

        auto *main_fn = llvm::Function::Create(
                llvm::FunctionType::get(int_ty,
                    // int argc, char **argv
                    {int_ty, ppchar_ty}, false),
                llvm::Function::ExternalLinkage,
                "main", *module_);
        main_fn->setDoesNotThrow();

        auto *entry = llvm::BasicBlock::Create(*context_, "entry", main_fn);
        auto *header = llvm::BasicBlock::Create(*context_, "header", main_fn);
        auto *body = llvm::BasicBlock::Create(*context_, "body", main_fn);
        auto *exit = llvm::BasicBlock::Create(*context_, "exit", main_fn);
        builder_->SetInsertPoint(entry);

        auto *index1 = get_int(0);
        auto *argc32 = main_fn->getArg(0);
        auto *argv = main_fn->getArg(1);

        auto *argc1 = builder_->CreateSExt(argc32, get_int_ty());
        auto args = CgList::construct(*this, get_ptr_ty(), argc1);
        auto *data_ptr = args.get_data();
        builder_->CreateBr(header);

        builder_->SetInsertPoint(header);
        auto *argc = builder_->CreatePHI(get_int_ty(), 2);
        auto *index = builder_->CreatePHI(get_int_ty(), 2);
        argc->addIncoming(argc1, entry);
        index->addIncoming(index1, entry);

        auto *condition = builder_->CreateCmp(llvm::CmpInst::ICMP_SGT,
                argc, get_int(0));
        builder_->CreateCondBr(condition, body, exit);

        builder_->SetInsertPoint(body);
        auto *pelement = builder_->CreateInBoundsGEP(pstr_ty, data_ptr, {index});
        auto *cstr = builder_->CreateInBoundsGEP(pchar_ty, argv, {index});
        auto *str = emit_cstr_to_str(builder_->CreateLoad(pchar_ty, cstr));
        builder_->CreateStore(str, pelement); // write "str" element of "[str]"
        auto *index2 = builder_->CreateAdd(index, get_int(1), "index");
        auto *argc2 = builder_->CreateSub(argc, get_int(1), "argc");

        builder_->CreateBr(header);

        argc->addIncoming(argc2, body);
        index->addIncoming(index2, body);

        builder_->SetInsertPoint(exit);

        auto *ret = builder_->CreateCall(fn, {args.get_list()});
        builder_->CreateRet(builder_->CreateTrunc(ret, int_ty));
    }

    void enter_function(Mir &mir)
    {
        auto *f = fs_[typekey(mir.type)].get();
        auto const fn_type = f->mir.type->FnPtr_;
        f->outer = f_;
        f_ = f;

        if (std::string(mir.name->text) == "main")
            create_main_fn_wrapper(f->fn);

        for (int i = 0; i < mir.blocks->count; ++i)
            f->blocks[i] = llvm::BasicBlock::Create(*context_,
                    "bb" + std::to_string(i), f->fn);

        builder_->SetInsertPoint(f->blocks[0]);

        for (int i = 0; i < mir.kcache->data->count; ++i)
            f->constants[i] = new_constant(mir.kcache->data->data[i]);

        for (int i = 0; i < mir.captured->count; ++i) {
            MirCaptureInfo const capture = mir.captured->data[i];
            MirRegisterData const rdata = mir.registers->data[capture.r.value];
            f->captures[i] = builder_->CreateAlloca(get_ty(rdata.type),
                    nullptr, "capture" + std::to_string(i));
        }

        for (int i = 0; i < mir.upvalues->count; ++i)
            f->upvalues[i] = nullptr;

        for (int i = 0; i < fn_type.params->count; ++i) {
            auto *arg = f->get_arg(i);
            auto *param_type = fn_type.params->data[i];
            if (is_inline_aggregate(param_type)) {
                // move by-value aggregate to a stack slot
                auto *ptr = builder_->CreateAlloca(get_ty(param_type));
                builder_->CreateStore(arg, ptr);
                arg = ptr;
            }
            f->values[1 + i] = arg;
        }

        for (int i = 0; i < mir.children->count; ++i)
            f->closures[i] = std::make_unique<CgFn>(*this, *mir.children->data[i]);
    }

    void leave_function()
    {
        if (llvm::verifyFunction(*f_->fn, &llvm::errs()))
            llvm::errs() << "\nfunction verification failed: "
                << f_->fn->getName() << "\n";

        paw_assert(f_->current == nullptr);
        f_ = f_->outer;
    }

    void enter_basic_block(int b)
    {
        f_->current = get_block(b);
        f_->bb = *f_->mir.blocks->data[b];
        builder_->SetInsertPoint(f_->current);
    }

    void leave_basic_block()
    {
        f_->current = nullptr;
    }

    void finalize_basic_block(int b)
    {
        auto const *bb = f_->mir.blocks->data[b];
        for (auto iphi = 0; iphi < bb->joins->count; ++iphi) {
            auto const phi = bb->joins->data[iphi]->Phi_;
            for (auto iinput = 0; iinput < phi.inputs->count; ++iinput) {
                auto *pred = get_block(bb->predecessors->data[iinput].value);
                auto const input = phi.inputs->data[iinput];
                auto *node = operand(phi.output);
                auto *value = operand(input);
                llvm::cast<llvm::PHINode>(node)
                    ->addIncoming(value, pred);
            }
        }
    }

    void instruction(MirInstruction const &instr)
    {
        switch (MIR_KINDOF(&instr)) {
            case kMirNoop:
                return;
            case kMirPhi:
                emit_phi(instr.Phi_);
                break;
            case kMirMove:
                emit_move(instr.Move_);
                break;
            case kMirLoad:
                emit_load(instr.Load_);
                break;
            case kMirStore:
                emit_store(instr.Store_);
                break;
            case kMirInlineCopy:
                emit_inlinecopy(instr.InlineCopy_);
                break;
            case kMirUpvalue:
                emit_upvalue(instr.Upvalue_);
                break;
            case kMirGlobal:
                emit_global(instr.Global_);
                break;
            case kMirAllocLocal:
                emit_alloclocal(instr.AllocLocal_);
                break;
            case kMirSetUpvalue:
                emit_setupvalue(instr.SetUpvalue_);
                break;
            case kMirSetCapture:
                emit_setcapture(instr.SetCapture_);
                break;
            case kMirLoadConstant:
                emit_loadconstant(instr.LoadConstant_);
                break;
            case kMirAggregate:
                emit_aggregate(instr.Aggregate_);
                break;
            case kMirContainer:
                emit_container(instr.Container_);
                break;
            case kMirStructGEP:
                emit_structgep(instr.StructGEP_);
                break;
            case kMirStrGEP:
                emit_strgep(instr.StrGEP_);
                break;
            case kMirListGEP:
                emit_listgep(instr.ListGEP_);
                break;
            case kMirMapGEP:
                emit_mapgep(instr.MapGEP_);
                break;
            case kMirGetRange:
                emit_getrange(instr.GetRange_);
                break;
            case kMirSetRange:
                emit_setrange(instr.SetRange_);
                break;
            case kMirCall:
                emit_call(instr.Call_);
                break;
            case kMirCast:
                emit_cast(instr.Cast_);
                break;
            case kMirCapture:
                emit_capture(instr.Capture_);
                break;
            case kMirClose:
                emit_close(instr.Close_);
                break;
            case kMirClosure:
                emit_closure(instr.Closure_);
                break;
            case kMirUnaryOp:
                emit_unaryop(instr.UnaryOp_);
                break;
            case kMirBinaryOp:
                emit_binaryop(instr.BinaryOp_);
                break;
            case kMirConcat:
                emit_concat(instr.Concat_);
                break;
            case kMirUnreachable:
                emit_unreachable(instr.Unreachable_);
                break;
            case kMirReturn:
                emit_return(instr.Return_);
                break;
            case kMirBranch:
                emit_branch(instr.Branch_);
                break;
            case kMirSwitch:
                emit_switch(instr.Switch_);
                break;
            case kMirGoto:
                emit_goto(instr.Goto_);
                break;
        }
    }

    TypeKey typekey(IrType *type)
    {
        return TypeKey(compiler_, type);
    }

    std::unique_ptr<CgFn> new_function(Mir const &mir)
    {
        return std::make_unique<CgFn>(*this, mir);
    }

    CgFn *declare_function(Mir const &mir)
    {
        auto const [itr, inserted] = fs_.emplace(
                typekey(mir.type),
                new_function(mir));
        paw_assert(inserted); (void)inserted;

        auto *f = itr->second.get();
        auto const k = get_builtin_kind(mir.type);
        if (k != NOT_BUILTIN) builtins_[k] = f;
        return f;
    }

    void dump_mlir(std::string pathname) const
    {
        std::error_code ec;
        llvm::raw_fd_ostream os(pathname, ec, llvm::sys::fs::OF_Text);
        if (ec) {
            llvm::errs() << ec.message() << '\n';
        } else {
            module_->print(os, nullptr);
        }
    }

    void generate(std::string modname, CodegenOptions const &options) const
    {
        llvm::OptimizationLevel opt;
        switch (options.opt_suffix) {
            default:  opt = llvm::OptimizationLevel::O0; break;
            case '1': opt = llvm::OptimizationLevel::O1; break;
            case '2': opt = llvm::OptimizationLevel::O2; break;
            case '3': opt = llvm::OptimizationLevel::O3; break;
            case 's': opt = llvm::OptimizationLevel::Os; break;
            case 'z': opt = llvm::OptimizationLevel::Oz; break;
        }

        auto const ll_prefix = modname + "_" + opt_name(opt);
        if (options.verify_module &&
                llvm::verifyModule(*module_, &llvm::errs())) {
            llvm::errs() << "module verification failed\n";
            dump_mlir(ll_prefix + "_failure.ll");
            std::abort();
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

        auto mpm = pb.buildPerModuleDefaultPipeline(opt);
        if (options.enable_asan)
            mpm.addPass(llvm::AddressSanitizerPass({}));
        mpm.run(*module_, mam);

        if (options.print_mlir)
            dump_mlir(ll_prefix + ".ll");

        std::error_code ec;
        auto const pathname = modname + ".o";
        llvm::raw_fd_ostream os(pathname, ec, llvm::sys::fs::OF_None);
        if (ec) {
            llvm::errs() << "could not open file: " << ec.message();
            return;
        }

        llvm::legacy::PassManager pm;
        if (machine_->addPassesToEmitFile(pm, os, nullptr,
                    llvm::CodeGenFileType::ObjectFile)) {
            llvm::errs() << "TargetMachine cannot emit a file of this type\n";
            return;
        }

        pm.run(*module_);
        os.flush();

        llvm::outs() << "wrote " << pathname << '\n';
    }

    llvm::FunctionCallee get_puts()
    {
        return module_->getOrInsertFunction("puts",
            llvm::FunctionType::get(get_i32_ty(),
                {get_ptr_ty()}, false));
    }

    llvm::FunctionCallee get_strlen()
    {
        return module_->getOrInsertFunction("strlen",
            llvm::FunctionType::get(get_int_ty(),
                {get_ptr_ty()}, false));
    }

    // TODO: cast to "llvm::Function *" here and in other get_*()?
    llvm::FunctionCallee get_exit()
    {
        return module_->getOrInsertFunction("exit",
            llvm::FunctionType::get(get_void_ty(),
                {get_i32_ty()}, false));
    }

    llvm::Function *get_memcmp()
    {
        auto *fn = llvm::cast<llvm::Function>(
            module_->getOrInsertFunction("memcmp",
                llvm::FunctionType::get(get_i32_ty(), {
                        get_ptr_ty(),
                        get_ptr_ty(),
                        get_int_ty(),
                    }, false)).getCallee());

        fn->setDoesNotThrow();
        fn->setOnlyReadsMemory();
        return fn;
    }

    llvm::Value *call_memset(llvm::IRBuilder<> &b, llvm::Value *ptr, llvm::Value *value, llvm::Value *size, bool is_volatile) const
    {
        return b.CreateMemSet(ptr, value, size, llvm::MaybeAlign(1), is_volatile);
    }

    llvm::Value *call_memcpy(llvm::IRBuilder<> &b, llvm::Value *dest, llvm::Value *src, llvm::Value *size, bool is_volatile) const
    {
        return b.CreateMemCpy(dest, llvm::MaybeAlign(1), src,
                llvm::MaybeAlign(1), size, is_volatile);
    }

    llvm::Value *call_nonvolatile_memset(llvm::IRBuilder<> &b, llvm::Value *ptr, llvm::Value *value, llvm::Value *size) const
    {
        return call_memset(b, ptr, value, size, false);
    }

    llvm::Value *call_nonvolatile_memcpy(llvm::IRBuilder<> &b, llvm::Value *dest, llvm::Value *src, llvm::Value *size) const
    {
        return call_memcpy(b, dest, src, size, false);
    }

    // Create prelude function "fn panic(message: str) -> !"
    void create_panic()
    {
        auto *f = builtins_[PRELUDE_PANIC];

        auto *block = llvm::BasicBlock::Create(*context_, "", f->fn);
        builder_->SetInsertPoint(block);

        // write the "message" to the standard error stream
        auto *str = emit_str_append(f->get_arg(0), '\n');
        emit_print_to(STREAM_STDERR, str);

        // exit the process gracefully
        builder_->CreateCall(get_exit(), get_i32(1));
        builder_->CreateUnreachable();
    }

    void create_assert()
    {
        // TODO: pass source location of "assert" call and text of condition argument as hidden second parameter, use to create better message
        auto *f = builtins_[PRELUDE_ASSERT];

        auto *entry = llvm::BasicBlock::Create(*context_, "entry", f->fn);
        auto *success = llvm::BasicBlock::Create(*context_, "success", f->fn);
        auto *failure = llvm::BasicBlock::Create(*context_, "failure", f->fn);
        auto *condition = f->get_arg(0);

        builder_->SetInsertPoint(entry);
        builder_->CreateCondBr(condition, success, failure);

        builder_->SetInsertPoint(success);
        builder_->CreateRetVoid();

        builder_->SetInsertPoint(failure);
        PANIC_LITERAL("assertion failed");
    }

    void create_str_hash()
    {
        auto *f = builtins_[STR_HASH];

        auto *entry_block = llvm::BasicBlock::Create(*context_, "entry", f->fn);
        builder_->SetInsertPoint(entry_block);
        auto *hash32 = str_get_hash(f->get_arg(0));
        auto *hash64 = builder_->CreateZExt(hash32, get_int_ty()); // TODO: i* types in Paw?
        f->return_value(hash64);
    }

    llvm::FunctionCallee get_write() const
    {
        // TODO: what if "write" does not exist (Windows, etc.)? Should conditionally compile, using "WriteFile" (or maybe "_write") on Windows
        return module_->getOrInsertFunction("write",
            llvm::FunctionType::get(get_int_ty(), {
                get_i32_ty(), // fd
                get_ptr_ty(), // data
                get_int_ty(), // size
            }, false));
    }

    void emit_print_to(int stream, llvm::Value *message)
    {
        builder_->CreateCall(get_write(), {
                get_i32(stream),
                str_get_text(message),
                str_get_length(message),
            });
    }

    void create_print()
    {
        auto *f = builtins_[PRELUDE_PRINT];
        auto *block = llvm::BasicBlock::Create(*context_, "", f->fn);
        builder_->SetInsertPoint(block);
        emit_print_to(STREAM_STDOUT, f->get_arg(0));
        builder_->CreateRetVoid();
    }

    void create_println()
    {
        auto *f = builtins_[PRELUDE_PRINTLN];
        auto *block = llvm::BasicBlock::Create(*context_, "", f->fn);
        builder_->SetInsertPoint(block);
        auto *str = emit_str_append(f->get_arg(0), '\n');
        emit_print_to(STREAM_STDOUT, str);
        builder_->CreateRetVoid();
    }

    // Emit code to build a stack-allocated string out of a C-style string
    // The lifetime of the returned string is the same as that of the current
    // function (at most). Note that the ".hash" field is not filled out. The
    // returned "str" should only be used for printing error messages.
    llvm::Value *get_temp_str(StringLiteral lit) const
    {
        paw_assert(lit.length < 100); // short strings only
        auto *ty = llvm::StructType::create(*context_, {
                get_int_ty(), // length
                get_i32_ty(), // hash
                get_array_ty(get_char_ty(), lit.length + 1), // text
            });
        auto *array = llvm::ConstantDataArray::getString(
                *context_, lit.text, true);

        auto *str = builder_->CreateAlloca(ty);
        auto *length_ptr = builder_->CreateStructGEP(ty, str, 0);
        auto *text = builder_->CreateStructGEP(ty, str, 2);

        builder_->CreateStore(get_int(lit.length), length_ptr);
        builder_->CreateStore(array, text);
        return str;
    }

    void call_panic(StringLiteral lit) const
    {
        call_panic(get_temp_str(lit));
    }

    void call_panic(llvm::Value *message) const
    {
        auto *f = builtins_[PRELUDE_PANIC];
        builder_->CreateCall(f->fn, {message});
        builder_->CreateUnreachable();
    }

    void call_print(StringLiteral lit) const
    {
        call_print(get_temp_str(lit));
    }

    llvm::Value *call_print(llvm::Value *str) const
    {
        auto *f = builtins_[PRELUDE_PRINT];
        return builder_->CreateCall(f->fn, {str});
    }

    void call_println(StringLiteral lit) const
    {
        call_println(get_temp_str(lit));
    }

    llvm::Value *call_println(llvm::Value *str) const
    {
        auto *f = builtins_[PRELUDE_PRINTLN];
        return builder_->CreateCall(f->fn, {str});
    }

    llvm::Value *inbounds_siadd(llvm::Value *a, llvm::Value *b, llvm::Value *ilower, llvm::Value *iupper) const
    {
        // if ((b < 0 && a >= ilower - b)
        //         || (b > 0 && a <= iupper - b))
        //     return a + b
        // else
        //     panic()
        auto *entry_block = builder_->GetInsertBlock();
        auto *check_positive_block = llvm::BasicBlock::Create(*context_, "inbounds_siadd_check_positive", f_->fn);
        auto *check_ilower_block = llvm::BasicBlock::Create(*context_, "inbounds_siadd_check_ilower", f_->fn);
        auto *check_iupper_block = llvm::BasicBlock::Create(*context_, "inbounds_siadd_check_iupper", f_->fn);
        auto *ok_block = llvm::BasicBlock::Create(*context_, "inbounds_siadd_ok", f_->fn);
        auto *error_block = llvm::BasicBlock::Create(*context_, "inbounds_siadd_error", f_->fn);

        auto *negative_condition = builder_->CreateICmpSLT(b, get_int(0));
        builder_->CreateCondBr(negative_condition,
                check_ilower_block,
                check_positive_block);

        builder_->SetInsertPoint(check_ilower_block);
        auto *ilower_bound = builder_->CreateSub(ilower, b);
        auto *ilower_condition = builder_->CreateICmpSGE(a, ilower_bound);
        builder_->CreateCondBr(ilower_condition, ok_block, error_block);

        auto *positive_condition = builder_->CreateICmpSGT(b, get_int(0));
        builder_->CreateCondBr(positive_condition, check_iupper_block, ok_block);

        builder_->SetInsertPoint(check_iupper_block);
        auto *iupper_bound = builder_->CreateSub(iupper, b);
        auto *iupper_condition = builder_->CreateICmpSGE(a, iupper_bound);
        builder_->CreateCondBr(iupper_condition, ok_block, error_block);

        builder_->SetInsertPoint(error_block);
        PANIC_LITERAL("encountered signed integer boundary violation during operator \"+\"");

        builder_->SetInsertPoint(ok_block);
        auto *c = builder_->CreateSub(get_int(PAW_INT_MAX), b);
        return builder_->CreateAdd(a, b);
    }

    llvm::Value *ckd_siadd(llvm::Value *a, llvm::Value *b) const
    {
        auto *ok_block = llvm::BasicBlock::Create(*context_, "ckd_siadd_ok", f_->fn);
        auto *error_block = llvm::BasicBlock::Create(*context_, "ckd_siadd_err", f_->fn);

        auto *fn = llvm::Intrinsic::getDeclaration(module_.get(),
            llvm::Intrinsic::sadd_with_overflow, {get_int_ty()});
        auto *result = builder_->CreateCall(fn, {a, b});
        auto *overflow = builder_->CreateExtractValue(result, 1);
        builder_->CreateCondBr(overflow, error_block, ok_block);

        builder_->SetInsertPoint(error_block);
        PANIC_LITERAL("encountered signed integer overflow during operator \"+\"");

        builder_->SetInsertPoint(ok_block);
        return builder_->CreateExtractValue(result, 0);
    }

    llvm::Value *emit_alloc_str(llvm::Value *length)
    {
        // TODO: bounds check
        auto *extra_size = get_int(size_of(get_str_ty()) + 1);
        auto *str_size = builder_->CreateAdd(extra_size, length);

        auto *str = allocate(str_size);
        auto *length_ptr = str_get_length_ptr(str);
        builder_->CreateStore(length, length_ptr);

        // write the null terminator
        auto *end_ptr = builder_->CreateInBoundsGEP(
                get_char_ty(), str_get_text(str), {length});
        builder_->CreateStore(get_char('\0'), end_ptr);
        return str;
    }

    llvm::Value *str_get_length_ptr(llvm::Value *str)
    {
        return builder_->CreateStructGEP(get_str_ty(), str, 0);
    }

    llvm::Value *str_get_hash_ptr(llvm::Value *str)
    {
        return builder_->CreateStructGEP(get_str_ty(), str, 1);
    }

    llvm::Value *str_get_text(llvm::Value *str)
    {
        return builder_->CreateStructGEP(get_str_ty(), str, 2);
    }

    llvm::Value *str_get_length(llvm::Value *str)
    {
        return builder_->CreateLoad(get_int_ty(), str_get_length_ptr(str));
    }

    llvm::Value *str_get_hash(llvm::Value *str)
    {
        return builder_->CreateLoad(get_i32_ty(), str_get_hash_ptr(str));
    }

    // sdbm hash modified from http://www.cse.yorku.ca/~oz/hash.html
    llvm::Function *create_sdbm()
    {
        auto *fn = llvm::Function::Create(
                llvm::FunctionType::get(
                    get_i32_ty(), {
                        get_ptr_ty(),
                        get_int_ty(),
                        get_i32_ty(),
                    }, false),
                llvm::Function::PrivateLinkage,
                "sdbm", *module_);

        auto *entry_block = llvm::BasicBlock::Create(*context_, "entry", fn);
        auto *header_block = llvm::BasicBlock::Create(*context_, "header", fn);
        auto *body_block = llvm::BasicBlock::Create(*context_, "body", fn);
        auto *exit_block = llvm::BasicBlock::Create(*context_, "exit", fn);

        builder_->SetInsertPoint(entry_block);
        auto *data = fn->getArg(0);
        auto *size = fn->getArg(1);
        auto *hash0 = fn->getArg(2);
        auto *index0 = get_i64(0);
        builder_->CreateBr(header_block);

        // for (size_t i = 0; i < size; ++i)
        //     hash = data[i] + (hash << 6) + (hash << 16) - hash;
        builder_->SetInsertPoint(header_block);
        auto *hash = builder_->CreatePHI(get_i32_ty(), 2);
        auto *index = builder_->CreatePHI(get_i64_ty(), 2);
        hash->addIncoming(hash0, entry_block);
        index->addIncoming(index0, entry_block);
        auto *condition = builder_->CreateICmpSLT(index, size);
        builder_->CreateCondBr(condition, body_block, exit_block);

        builder_->SetInsertPoint(body_block);
        auto *i8_ptr = builder_->CreateInBoundsGEP(get_i8_ty(), data, {index});
        auto *i8 = builder_->CreateLoad(get_i8_ty(), i8_ptr);
        auto *i32 = builder_->CreateZExt(i8, get_i32_ty());
        auto *shift6 = builder_->CreateLShr(hash, get_i32(6));
        auto *shift16 = builder_->CreateLShr(hash, get_i32(16));
        auto *temp = builder_->CreateAdd(i32, builder_->CreateAdd(shift6, shift16));
        auto *hash1 = builder_->CreateSub(temp, hash);
        auto *index1 = builder_->CreateAdd(index, get_i64(1));
        hash->addIncoming(hash1, body_block);
        index->addIncoming(index1, body_block);
        builder_->CreateBr(header_block);

        builder_->SetInsertPoint(exit_block);
        builder_->CreateRet(hash);

        sdbm_fn_ = fn;
        return fn;
    }

    llvm::Value *call_sdbm_hash(llvm::Value *data, llvm::Value *size, llvm::Value *hash0)
    {
        return builder_->CreateCall(sdbm_fn_, {data, size, hash0});
    }

    llvm::Value *emit_finalize_str(llvm::Value *str)
    {
        auto *text = str_get_text(str);
        auto *length = str_get_length(str);
        auto *hash_ptr = str_get_hash_ptr(str);
        auto *hash = call_sdbm_hash(text, length, get_i32(0));
        builder_->CreateStore(hash, hash_ptr);
        return str;
    }

    llvm::Value *emit_new_str(llvm::Value *data, llvm::Value *length)
    {
        auto *str = emit_alloc_str(length);
        auto *text = str_get_text(str);
        call_nonvolatile_memcpy(*builder_, text, data, length);
        emit_finalize_str(str);
        return str;
    }

    llvm::Value *emit_cstr_to_str(llvm::Value *cstr)
    {
        auto *length = builder_->CreateCall(get_strlen(), {cstr});
        return emit_new_str(cstr, length);
    }

    llvm::Value *allocate(paw_Int size)
    {
        return allocate(get_int(size));
    }

    llvm::Value *allocate(llvm::Value *size)
    {
        return builder_->CreateCall(malloc_fn_, {size});
    }

    llvm::Value *allocate(llvm::Type *ty)
    {
        return allocate(stride_of(ty));
    }

    llvm::Value *emit_imin(llvm::Value *a, llvm::Value *b)
    {
        auto *a_lt_b = builder_->CreateICmpSLT(a, b);
        return builder_->CreateSelect(a_lt_b, a, b);
    }

    llvm::Value *emit_imax(llvm::Value *a, llvm::Value *b)
    {
        auto *a_gt_b = builder_->CreateICmpSGT(a, b);
        return builder_->CreateSelect(a_gt_b, a, b);
    }

    // Source: https://graphics.stanford.edu/~seander/bithacks.html#RoundUpPowerOf2
    // Modified to operate on 64-bit integers (requires 1 additional shift). Uses
    // the suggestion to fix the edge case at 0.
    llvm::Value *emit_next_pow2(llvm::Value *v)
    {
        v = builder_->CreateSub(v, get_int(1));
        v = builder_->CreateOr(v, builder_->CreateLShr(v, get_int(1)));
        v = builder_->CreateOr(v, builder_->CreateLShr(v, get_int(2)));
        v = builder_->CreateOr(v, builder_->CreateLShr(v, get_int(4)));
        v = builder_->CreateOr(v, builder_->CreateLShr(v, get_int(8)));
        v = builder_->CreateOr(v, builder_->CreateLShr(v, get_int(16)));
        v = builder_->CreateOr(v, builder_->CreateLShr(v, get_int(32)));
        v = builder_->CreateAdd(v, get_int(1));

        auto *eq0 = builder_->CreateCmp(llvm::CmpInst::ICMP_EQ, v, get_int(0));
        return builder_->CreateAdd(v, builder_->CreateZExt(eq0, get_int_ty()));
    }

    MirBlock get_predecessor(int p)
    {
        paw_assert(0 <= p && p < f_->bb.predecessors->count);
        return f_->bb.predecessors->data[p];
    }

    MirBlock get_successor(int s)
    {
        paw_assert(0 <= s && s < f_->bb.successors->count);
        return f_->bb.successors->data[s];
    }

    llvm::BasicBlock *get_block(int b)
    {
        return f_->blocks[b];
    }

    llvm::BasicBlock *get_predecessor_block(int p)
    {
        paw_assert(0 <= p && p < f_->bb.predecessors->count);
        return get_block(f_->bb.predecessors->data[p].value);
    }

    llvm::BasicBlock *get_successor_block(int s)
    {
        paw_assert(0 <= s && s < f_->bb.successors->count);
        return get_block(f_->bb.successors->data[s].value);
    }

    llvm::FunctionCallee get_callee(std::string const &name, llvm::Type *ty)
    {
        return module_->getOrInsertFunction(name, ty);
    }

    llvm::Value *new_constant(MirConstantData kdata)
    {
        switch (kdata.kind) {
            case BUILTIN_UNIT:
                return get_unit();
            case BUILTIN_BOOL:
                return get_bool(kdata.value.i);
            case BUILTIN_CHAR:
                return get_char(kdata.value.c);
            case BUILTIN_INT:
                return get_int(kdata.value.i);
            case BUILTIN_FLOAT:
                return get_float(kdata.value.f);
            default:
                paw_assert(kdata.kind == BUILTIN_STR);
                return get_kstr((Str *)kdata.value.p);
        }
    }

    void emit_phi(MirPhi const &x)
    {
        auto *phi = builder_->CreatePHI(get_ty(x.output.type), x.inputs->count);
        set_result(x.output, phi);
    }

    void emit_move(MirMove const &x)
    {
        PAW_UNREACHABLE();
    }

    void emit_inlinecopy(MirInlineCopy const &x)
    {
        auto *input_ptr = operand(x.input);
        auto *value_ty = get_aggregate_ty(x.output.type);
        auto *input = builder_->CreateLoad(value_ty, input_ptr);
        auto *output_ptr = builder_->CreateAlloca(value_ty);
        builder_->CreateStore(input, output_ptr);
        set_result(x.output, output_ptr);
    }

    void emit_load(MirLoad const &x)
    {
        auto *pointer = operand(x.pointer);
        auto *output_ty = get_ty(x.output.type);
        auto *output = builder_->CreateLoad(output_ty, pointer);
        if (is_inline_aggregate(x.output.type)) {
            auto *pointer = builder_->CreateAlloca(output_ty);
            builder_->CreateStore(output, pointer);
            set_result(x.output, pointer);
        } else {
            set_result(x.output, output);
        }
    }

    void emit_store(MirStore const &x)
    {
        auto *value = rvalue(x.value);
        auto *pointer = operand(x.pointer);
        builder_->CreateStore(value, pointer);
    }

    void emit_upvalue(MirUpvalue const &x)
    {
    }

    void emit_global(MirGlobal const &x)
    {
        auto const itr = fs_.find(typekey(x.output.type));
        paw_assert(itr != end(fs_));
        set_result(x.output, itr->second->fn);
    }

    void emit_alloclocal(MirAllocLocal const &x)
    {
    }

    // Value **upvalue = f_->upvalues[u];
    // **upvalue = value;
    void emit_setupvalue(MirSetUpvalue const &x)
    {
    }

    void emit_setcapture(MirSetCapture const &x)
    {
        // TODO: captured variables need to reside in alloca's
        builder_->CreateStore(rvalue(x.value), operand(x.target));
    }

    void emit_loadconstant(MirLoadConstant const &x)
    {
        PAW_UNREACHABLE();
    }

    void emit_aggregate(MirAggregate const &x)
    {
        auto *aggregate_ty = get_aggregate_ty(x.output.type);
        auto *result = x.is_boxed
            ? allocate(aggregate_ty)
            : builder_->CreateAlloca(aggregate_ty);

        for (auto i = 0; i < x.fields->count; ++i) {
            auto *field_ptr = builder_->CreateStructGEP(
                    aggregate_ty, result, i);
            auto *value = rvalue(x.fields->data[i]);
            builder_->CreateStore(value, field_ptr);
        }

        set_result(x.output, result);
    }

    llvm::Value *list_get_data_ptr(llvm::Value *list)
    {
        return builder_->CreateStructGEP(get_list_ty(), list, 0);
    }

    llvm::Value *list_get_length_ptr(llvm::Value *list)
    {
        return builder_->CreateStructGEP(get_list_ty(), list, 1);
    }

    llvm::Value *list_get_capacity_ptr(llvm::Value *list)
    {
        return builder_->CreateStructGEP(get_list_ty(), list, 2);
    }

    llvm::Value *list_get_data(llvm::Value *list)
    {
        return builder_->CreateLoad(get_ptr_ty(), list_get_data_ptr(list));
    }

    llvm::Value *list_get_length(llvm::Value *list)
    {
        return builder_->CreateLoad(get_int_ty(), list_get_length_ptr(list));
    }

    llvm::Value *list_get_capacity(llvm::Value *list)
    {
        return builder_->CreateLoad(get_int_ty(), list_get_capacity_ptr(list));
    }

    llvm::Value *rvalue(MirPlace place)
    {
        auto *value = operand(place);
        return is_inline_aggregate(place.type)
            ? builder_->CreateLoad(get_ty(place.type), value)
            : value;
    }

    llvm::Value *emit_literal_list(MirContainer const &x)
    {
        paw_assert(x.b_kind == BUILTIN_LIST);
        auto *element_ty = get_ty(ir_list_elem(x.output.type));
        auto cglist = CgList::construct(*this, element_ty, x.elems->count);

        auto *data = cglist.get_data();
        for (int i = 0; i < x.elems->count; ++i) {
            auto *value = rvalue(x.elems->data[i]);
            builder_->CreateStore(value, cglist.gep(i));
        }
        return cglist.get_list();
    }

    llvm::Value *emit_literal_map(MirContainer const &x)
    {
        paw_assert(x.b_kind == BUILTIN_MAP);
        auto *cgmap = get_cgmap(x.output.type);
        auto *map = cgmap->construct(*builder_, get_int(x.elems->count));
        for (int i = 0; i < x.elems->count; i += 2) {
            auto *key = rvalue(x.elems->data[i]);
            auto *value_ptr = cgmap->nep(*builder_, map, key);
            auto *value = rvalue(x.elems->data[i + 1]);
            builder_->CreateStore(value, value_ptr);
        }
        return map;
    }

    void emit_container(MirContainer const &x)
    {
        auto *result = x.b_kind == BUILTIN_LIST
            ? emit_literal_list(x)
            : emit_literal_map(x);
        set_result(x.output, result);
    }

    void emit_structgep(struct MirStructGEP const &x)
    {
        auto *object = operand(x.object);
        auto *object_ty = get_aggregate_ty(x.object.type);
        auto *field_ptr = builder_->CreateStructGEP(object_ty, object, x.field);
        set_result(x.output, field_ptr);
    }

    void emit_strgep(MirStrGEP const &x)
    {
        auto *text = str_get_text(operand(x.object));
        auto *element_ptr = builder_->CreateInBoundsGEP(get_char_ty(),
                text, {operand(x.index)});
        set_result(x.output, element_ptr);
    }

    void emit_listgep(MirListGEP const &x)
    {
        auto *element_type = x.object.type->Adt_.types->data[0];
        auto *elements = list_get_data(operand(x.object));
        auto *element_ptr = builder_->CreateInBoundsGEP(
                get_ty(element_type), elements, {operand(x.index)});
        set_result(x.output, element_ptr);
    }

    void emit_mapgep(MirMapGEP const &x)
    {
        auto *cgmap = get_cgmap(x.object.type);
        auto *value_ptr = cgmap->gep(*builder_,
                operand(x.object), operand(x.key));
        set_result(x.output, value_ptr);
    }

    llvm::Value *logical_and(llvm::Value *a, llvm::Value *b)
    {
        auto *lhs = builder_->GetInsertBlock();
        auto *rhs = llvm::BasicBlock::Create(*context_, "rhs", f_->fn);
        auto *merge = llvm::BasicBlock::Create(*context_, "merge", f_->fn);

        builder_->CreateCondBr(a, rhs, merge);
        builder_->SetInsertPoint(rhs);
        builder_->CreateBr(merge);

        builder_->SetInsertPoint(merge);
        auto *phi = builder_->CreatePHI(get_bool_ty(), 2);
        auto *k = get_bool(false); // already known
        phi->addIncoming(k, lhs);
        phi->addIncoming(b, rhs);
        return phi;
    }

    llvm::Value *array_gep(llvm::Type *element_ty, llvm::Value *array, llvm::Value *index)
    {
        return builder_->CreateInBoundsGEP(element_ty, array, {index});
    }

    llvm::Value *array_gep(llvm::Type *element_ty, llvm::Value *array, paw_Int index)
    {
        return array_gep(element_ty, array, get_int(index));
    }

    llvm::Value *array_get(llvm::Type *element_ty, llvm::Value *array, llvm::Value *index)
    {
        auto *element_ptr = array_gep(element_ty, array, index);
        return builder_->CreateLoad(element_ty, element_ptr);
    }

    llvm::Value *array_set(llvm::Type *element_ty, llvm::Value *array, llvm::Value *index, llvm::Value *element)
    {
        auto *element_ptr = array_gep(element_ty, array, index);
        builder_->CreateStore(element, element_ptr);
        return element_ptr;
    }

//    void emit_map_setelement(MirSetElement const &x)
//    {
//        paw_assert(x.b_kind == BUILTIN_MAP);
//        auto *cgmap = get_cgmap(x.object.type);
//        auto *value_ptr = cgmap->nep(*builder_, operand(x.object), operand(x.key));
//        builder_->CreateStore(operand(x.value), value_ptr);
//    }
//
//    // TODO: bounds checking
//    void emit_setelement(MirSetElement const &x)
//    {
//        if (x.b_kind == BUILTIN_LIST) {
//            emit_list_setelement(x);
//        } else {
//            emit_map_setelement(x);
//        }
//    }
//
//    llvm::Value *list_getelementptr(MirGetElementPtr const &x)
//    {
//        paw_assert(x.b_kind == BUILTIN_LIST);
//        auto const element_type = K_LIST_LAST(x.object.type->Adt_.types);
//        auto *begin_ptr = builder_->CreateStructGEP(
//                get_list_ty(), operand(x.object), 0);
//
//        auto *element_ty = get_ty(element_type);
//        llvm::Value *indices[] = {operand(x.key), get_int(0)};
//        return builder_->CreateInBoundsGEP(element_ty, begin_ptr, indices);
//    }
//
//    llvm::Value *map_getelementptr(MirGetElementPtr const &x)
//    {
//        paw_assert(x.b_kind == BUILTIN_MAP);
//        auto *cgmap = get_cgmap(x.object.type);
//        return cgmap->gep(*builder_, operand(x.object), operand(x.key));
//    }
//
//    void emit_getelementptr(MirGetElementPtr const &x)
//    {
//        auto *result = x.b_kind == BUILTIN_LIST
//            ? list_getelementptr(x)
//            : map_getelementptr(x);
//        set_result(x.output, result);
//    }

    void emit_setrange(MirSetRange const &x)
    {
    }

    // TODO: hidden by non-type decl.
    void emit_getrange(struct MirGetRange const &x)
    {
    }

    bool is_boxed_aggregate(IrType *type)
    {
        return mir_is_boxed_aggregate(compiler_, type);
    }

    bool is_inline_aggregate(IrType *type)
    {
        return mir_is_inline_aggregate(compiler_, type);
    }

    void emit_call(MirCall const &x)
    {
        auto *callee = operand(x.target);
        auto *fn_ty = get_fn_ty(x.target.type);
        auto *return_ty = get_ty(x.target.type->FnPtr_.result);

        std::vector<llvm::Value *> args(x.args->count);
        for (int i = 0; i < x.args->count; ++i)
            args[i] = rvalue(x.args->data[i]);

        if (fn_ty->getNumParams() != args.size()) {
            paw_assert(fn_ty->getNumParams() - 1 == args.size());
            auto *result = builder_->CreateAlloca(return_ty);
            args.insert(begin(args), result);
            builder_->CreateCall(fn_ty, callee, args);
            set_result(x.output, result);
        } else if (is_empty_ty(return_ty)) {
            builder_->CreateCall(fn_ty, callee, args);
            set_result(x.output, get_unit());
        } else {
            auto *result = builder_->CreateCall(fn_ty, callee, args);
            set_result(x.output, result);
        }
    }

    llvm::Value *create_cast(llvm::Value *target, BuiltinKind from, BuiltinKind to)
    {
        paw_assert(from != BUILTIN_UNIT && IS_SCALAR_TYPE(from));
        paw_assert(to != BUILTIN_UNIT && IS_SCALAR_TYPE(to));

        switch (from) {
            case BUILTIN_BOOL:
                if (to == BUILTIN_CHAR) {
                    return builder_->CreateZExt(target, get_char_ty());
                } else if (to == BUILTIN_INT) {
                    return builder_->CreateZExt(target, get_int_ty());
                } else { // to == BUILTIN_FLOAT
                    auto *temp = builder_->CreateZExt(target, get_int_ty());
                    return builder_->CreateSIToFP(temp, get_float_ty());
                }
            case BUILTIN_CHAR:
                if (to == BUILTIN_BOOL) {
                    return builder_->CreateCmp(llvm::CmpInst::ICMP_NE, target,
                            get_char(0), "char_to_bool");
                } else if (to == BUILTIN_INT) {
                    return builder_->CreateZExt(target, get_int_ty());
                } else { // to == BUILTIN_FLOAT
                    auto *temp = builder_->CreateZExt(target, get_int_ty());
                    return builder_->CreateSIToFP(temp, get_float_ty());
                }
            case BUILTIN_INT:
                if (to == BUILTIN_BOOL) {
                    return builder_->CreateCmp(llvm::CmpInst::ICMP_NE, target,
                            get_int(0), "int_to_bool");
                } else if (to == BUILTIN_CHAR) {
                    return builder_->CreateTrunc(target, get_char_ty());
                } else { // to == BUILTIN_FLOAT
                    return builder_->CreateSIToFP(target, get_float_ty());
                }
            default: // from == BUILTIN_FLOAT
                if (to == BUILTIN_BOOL) {
                    return builder_->CreateCmp(llvm::CmpInst::FCMP_ONE, target,
                            get_float(0.0), "float_to_bool");
                } else if (to == BUILTIN_CHAR) {
                    return builder_->CreateFPToSI(target, get_char_ty());
                } else { // to == BUILTIN_INT
                    return builder_->CreateFPToSI(target, get_int_ty());
                }
        }
    }

    void emit_cast(MirCast const &x)
    {
        auto *target = operand(x.target);
        auto *result = create_cast(target, x.from, x.to);
        set_result(x.output, result);
    }

    void emit_capture(MirCapture const &x)
    {
    }

    void emit_close(MirClose const &x)
    {
    }

    std::vector<llvm::Type *> get_tys(IrTypeList const *types)
    {
        std::vector<llvm::Type *> tys(types->count);
        for (int i = 0; i < types->count; ++i) {
            auto *type = types->data[i];
            tys[i] = is_boxed_aggregate(type)
                ? get_ptr_ty()
                : get_ty(type);
        }
        return tys;
    }

    void emit_closure(MirClosure const &x)
    {
        // upvalues: [Value **]

        Mir const child = *f_->mir.children->data[x.child_id];
        // TODO: sizeof function pointer + sizeof upvalues + ...
        auto *env = allocate(child.upvalues->count);
        set_result(x.output, env);

        auto const unpack_upvalues = [this](MirUpvalueList upvalues) {
            std::vector<llvm::Type *> tys(upvalues.count);
            for (int u = 0; u < upvalues.count; ++u)
                tys[u] = get_ty(upvalues.data[u].type);
            return tys;
        };

        auto *env_ty = llvm::StructType::create(*context_,
                unpack_upvalues(*child.upvalues), "env");
        auto const fn_type = x.output.type->FnPtr_;
        auto param_tys = get_tys(fn_type.params);
        param_tys.insert(begin(param_tys), env_ty);
        auto *result_ty = get_ty(fn_type.result);

        auto *fn_ty = llvm::FunctionType::get(result_ty, param_tys, false);
        auto *closure_ty = llvm::StructType::create(*context_,
                {get_ptr_ty(), env_ty}, "closure");

        auto *closure = allocate(closure_ty);
        auto *fn_ptr = builder_->CreateStructGEP(closure_ty, closure, 0);
        builder_->CreateStore(closure, fn_ptr);

        for (int u = 0; u < child.upvalues->count; ++u) {
            auto *upvalue_ptr = builder_->CreateStructGEP(closure_ty, closure, 1 + u);

            MirUpvalueInfo const up = child.upvalues->data[0];
            auto *upvalue = up.is_local
                ? f_->outer->captures[up.index]
                : f_->outer->upvalues[up.index];
            // TODO: probably get addr of upvalue location
            builder_->CreateStore(upvalue, upvalue_ptr);
        }
    }

    llvm::Value *new_unary_op(MirUnaryOpKind op, llvm::Value *value)
    {
        paw_assert(value != nullptr);

        switch (op) {
            case MIR_UNARY_STRLEN:
                value = builder_->CreateStructGEP(get_str_ty(), value, 0);
                return builder_->CreateLoad(get_int_ty(), value);
            case MIR_UNARY_LISTLEN:
                value = builder_->CreateStructGEP(get_list_ty(), value, 1);
                return builder_->CreateLoad(get_int_ty(), value);
            case MIR_UNARY_MAPLEN:
                value = builder_->CreateStructGEP(get_map_ty(), value, 1);
                return builder_->CreateLoad(get_int_ty(), value);
            case MIR_UNARY_IBITNOT:
                return builder_->CreateNot(value);
            case MIR_UNARY_INEG:
            case MIR_UNARY_FNEG:
                return builder_->CreateNeg(value);
            case MIR_UNARY_NOT:
                return builder_->CreateCmp(llvm::CmpInst::ICMP_EQ,
                        value, get_i1(0));
        }
    }

    void emit_unaryop(MirUnaryOp const &x)
    {
        auto *value = operand(x.val);
        auto *result = new_unary_op(x.op, value);
        set_result(x.output, result);
    }

    llvm::Value *call_rawcmp(llvm::Value *lhs, llvm::Value *lhs_length, llvm::Value *rhs, llvm::Value *rhs_length)
    {
        return builder_->CreateCall(rawcmp_fn_, {lhs, lhs_length, rhs, rhs_length});
    }

    void create_rawcmp()
    {
        auto *fn = llvm::Function::Create(
                llvm::FunctionType::get(
                    get_int_ty(), {
                        get_ptr_ty(), // lhs
                        get_int_ty(), // lhs_length
                        get_ptr_ty(), // rhs
                        get_int_ty(), // rhs_length
                    }, false),
                llvm::Function::PrivateLinkage,
                "rawcmp", *module_);
        fn->setDoesNotThrow();

        auto *entry_block = llvm::BasicBlock::Create(*context_, "entry", fn);
        auto *found_prefix_block = llvm::BasicBlock::Create(*context_, "found_prefix", fn);
        auto *emit_result_block = llvm::BasicBlock::Create(*context_, "emit_result", fn);

        auto *lhs = fn->getArg(0);
        auto *lhs_length = fn->getArg(1);
        auto *rhs = fn->getArg(2);
        auto *rhs_length = fn->getArg(3);

        builder_->SetInsertPoint(entry_block);
        auto *min_length = emit_imin(lhs_length, rhs_length);
        auto *comparison32 = builder_->CreateCall(get_memcmp(), {lhs, rhs, min_length});
        auto *comparison64 = builder_->CreateSExt(comparison32, get_int_ty());
        auto *is_zero = builder_->CreateICmpEQ(comparison64, get_int(0));
        builder_->CreateCondBr(is_zero, found_prefix_block, emit_result_block);

        builder_->SetInsertPoint(found_prefix_block);
        auto *difference = builder_->CreateSub(lhs_length, rhs_length);
        builder_->CreateBr(emit_result_block);

        builder_->SetInsertPoint(emit_result_block);
        auto *result = builder_->CreatePHI(get_int_ty(), 2);
        result->addIncoming(comparison64, entry_block);
        result->addIncoming(difference, found_prefix_block);
        builder_->CreateRet(result);

        rawcmp_fn_ = fn;
    }

    // NOTE: "strcmp" C stdlib function will not work here, since "str" might
    //       contain embedded null characters.
    llvm::Value *emit_strcmp(llvm::Value *lhs, llvm::Value *rhs)
    {
        return call_rawcmp(
                str_get_text(lhs), str_get_length(lhs),
                str_get_text(rhs), str_get_length(rhs));
    }

    llvm::Value *new_binary_op(MirBinaryOpKind op, llvm::Value *lhs, llvm::Value *rhs)
    {
        paw_assert(lhs != nullptr && rhs != nullptr);

        switch (op) {
            case MIR_BINARY_CEQ:
                return builder_->CreateCmp(llvm::CmpInst::ICMP_EQ, lhs, rhs);
            case MIR_BINARY_CNE:
                return builder_->CreateCmp(llvm::CmpInst::ICMP_NE, lhs, rhs);
            case MIR_BINARY_CLT:
                return builder_->CreateCmp(llvm::CmpInst::ICMP_ULT, lhs, rhs);
            case MIR_BINARY_CLE:
                return builder_->CreateCmp(llvm::CmpInst::ICMP_ULE, lhs, rhs);
            case MIR_BINARY_IEQ:
                return builder_->CreateCmp(llvm::CmpInst::ICMP_EQ, lhs, rhs);
            case MIR_BINARY_INE:
                return builder_->CreateCmp(llvm::CmpInst::ICMP_NE, lhs, rhs);
            case MIR_BINARY_ILT:
                return builder_->CreateCmp(llvm::CmpInst::ICMP_SLT, lhs, rhs);
            case MIR_BINARY_ILE:
                return builder_->CreateCmp(llvm::CmpInst::ICMP_SLE, lhs, rhs);
            case MIR_BINARY_FEQ:
                return builder_->CreateCmp(llvm::CmpInst::FCMP_OEQ, lhs, rhs);
            case MIR_BINARY_FNE:
                return builder_->CreateCmp(llvm::CmpInst::FCMP_ONE, lhs, rhs);
            case MIR_BINARY_FLT:
                return builder_->CreateCmp(llvm::CmpInst::FCMP_OLT, lhs, rhs);
            case MIR_BINARY_FLE:
                return builder_->CreateCmp(llvm::CmpInst::FCMP_OLE, lhs, rhs);
            case MIR_BINARY_STREQ:
                return builder_->CreateCmp(llvm::CmpInst::ICMP_EQ,
                        emit_strcmp(lhs, rhs), get_int(0));
            case MIR_BINARY_STRNE:
                return builder_->CreateCmp(llvm::CmpInst::ICMP_NE,
                        emit_strcmp(lhs, rhs), get_int(0));
            case MIR_BINARY_STRLT:
                return builder_->CreateCmp(llvm::CmpInst::ICMP_SLT,
                        emit_strcmp(lhs, rhs), get_int(0));
            case MIR_BINARY_STRLE:
                return builder_->CreateCmp(llvm::CmpInst::ICMP_SLE,
                        emit_strcmp(lhs, rhs), get_int(0));
            case MIR_BINARY_IADD:
                return builder_->CreateAdd(lhs, rhs);
            case MIR_BINARY_ISUB:
                return builder_->CreateSub(lhs, rhs);
            case MIR_BINARY_IMUL:
                return builder_->CreateMul(lhs, rhs);
            case MIR_BINARY_IDIV:
                return builder_->CreateSDiv(lhs, rhs);
            case MIR_BINARY_IMOD:
                return builder_->CreateSRem(lhs, rhs);
            case MIR_BINARY_FADD:
                return builder_->CreateFAdd(lhs, rhs);
            case MIR_BINARY_FSUB:
                return builder_->CreateFSub(lhs, rhs);
            case MIR_BINARY_FMUL:
                return builder_->CreateFMul(lhs, rhs);
            case MIR_BINARY_FDIV:
                return builder_->CreateFDiv(lhs, rhs);
            case MIR_BINARY_FMOD:
                return builder_->CreateFRem(lhs, rhs);
            case MIR_BINARY_IBITAND:
                return builder_->CreateAnd(lhs, rhs);
            case MIR_BINARY_IBITOR:
                return builder_->CreateOr(lhs, rhs);
            case MIR_BINARY_IBITXOR:
                return builder_->CreateXor(lhs, rhs);
            case MIR_BINARY_ISHL:
                return builder_->CreateShl(lhs, rhs);
            case MIR_BINARY_ISHR:
                return builder_->CreateLShr(lhs, rhs);
        }
    }

    void emit_binaryop(MirBinaryOp const &x)
    {
        auto *lhs = operand(x.lhs);
        auto *rhs = operand(x.rhs);
        auto *result = new_binary_op(x.op, lhs, rhs);
        set_result(x.output, result);
    }

    llvm::Value *emit_str_append(llvm::Value *lhs, char rhs)
    {
        return emit_str_append(lhs, get_char(rhs));
    }

    llvm::Value *emit_iadd1(llvm::Value *value)
    {
        return builder_->CreateAdd(value, get_int(1));
    }

    llvm::Value *emit_str_append(llvm::Value *str, llvm::Value *c)
    {
        auto *src = str_get_text(str);
        auto *src_size = str_get_length(str);

        // TODO: check for overflow
        // TODO: "finalize" string, hash "c" starting w/ hash from "str" to get final hash, add to internalization table
        auto *dest_size = emit_iadd1(src_size);
        auto *result = emit_alloc_str(dest_size);
        auto *dest = str_get_text(result);

        call_nonvolatile_memcpy(*builder_, dest, src, src_size);

        auto *last_ptr = builder_->CreateInBoundsGEP(
                get_char_ty(), dest, {src_size});
        builder_->CreateStore(c, last_ptr);
        return result;
    }

    llvm::Value *emit_strcat(llvm::Value *lhs, llvm::Value *rhs)
    {
        auto *lhs_text = str_get_text(lhs);
        auto *rhs_text = str_get_text(rhs);
        auto *lhs_length = str_get_length(lhs);
        auto *rhs_length = str_get_length(rhs);

        // TODO: check for overflow
        // TODO: "finalize" string, hash "rhs" starting w/ hash from "lhs" to get final hash, add to internalization table
        auto *length = builder_->CreateAdd(lhs_length, rhs_length);
        auto *result = emit_alloc_str(length);

        auto *length_ptr = str_get_length_ptr(result);
        auto *lhs_start = str_get_text(result);
        auto *rhs_start = builder_->CreateInBoundsGEP(
                get_char_ty(), lhs_start, {lhs_length});
        builder_->CreateStore(length, length_ptr);

        call_nonvolatile_memcpy(*builder_, lhs_start, lhs_text, lhs_length);
        call_nonvolatile_memcpy(*builder_, rhs_start, rhs_text, rhs_length);
        return result;
    }

    llvm::Value *emit_listcat(llvm::Value *lhs, llvm::Value *rhs)
    {
    }

    void emit_concat(MirConcat const &x)
    {
        auto *result = operand(x.inputs->data[0]);
        for (int i = 1; i < x.inputs->count; ++i) {
            auto *value = operand(x.inputs->data[i]);
            result = x.b_kind == BUILTIN_STR
                ? emit_strcat(result, value)
                : emit_listcat(result, value);
        }
        set_result(x.output, result);
    }

    void emit_unreachable(MirUnreachable const &x)
    {
        builder_->CreateUnreachable();
    }

    void emit_return(MirReturn const &x)
    {
        // TODO: use "rvalue" instead of "operand" and remove load inside "return_value"
        f_->return_value(operand(x.value));
    }

    void emit_branch(MirBranch const &x)
    {
        auto *condition = operand(x.cond);
        auto *then_block = get_successor_block(0);
        auto *else_block = get_successor_block(1);
        builder_->CreateCondBr(condition, then_block, else_block);
    }

    llvm::BasicBlock *create_unreachable_block() const
    {
        auto *before = builder_->GetInsertBlock();
        auto *block = llvm::BasicBlock::Create(*context_, "unreachable", f_->fn);
        builder_->SetInsertPoint(block);
        builder_->CreateUnreachable();
        builder_->SetInsertPoint(before);
        return block;
    }

    llvm::ConstantInt *get_constant_integral(MirConstant k)
    {
        auto const kdata = f_->mir.kcache->data->data[k.value];
        switch (kdata.kind) {
            case BUILTIN_BOOL:
                return get_bool(kdata.value.i);
            case BUILTIN_CHAR:
                return get_char(kdata.value.c);
            default:
                paw_assert(kdata.kind == BUILTIN_INT);
                return get_int(kdata.value.i);
        }
    }

    void emit_switch(MirSwitch const &x)
    {
        auto *node = builder_->CreateSwitch(
                operand(x.discr), x.has_otherwise
                    ? get_successor_block(x.arms->count)
                    : create_unreachable_block(),
                x.arms->count);
        for (int i = 0; i < x.arms->count; ++i) {
            auto *k = get_constant_integral(x.arms->data[i].k);
            auto *target = get_successor_block(i);
            node->addCase(k, target);
        }
    }

    void emit_goto(MirGoto const &x)
    {
        auto *target_block = get_successor_block(0);
        builder_->CreateBr(target_block);
    }

    void set_result(MirPlace place, llvm::Value *value)
    {
        paw_assert(place.kind == MIR_PLACE_LOCAL);
        f_->values[place.r.value] = value;
    }

    llvm::Value *get_unit() const
    {
        return llvm::ConstantStruct::get(get_unit_ty(), {});
    }

    llvm::ConstantInt *get_bool(paw_Bool value) const
    {
        return get_i1(value);
    }

    llvm::ConstantInt *get_char(paw_Char value) const
    {
        return get_i8(value);
    }

    llvm::ConstantInt *get_int(paw_Int value) const
    {
        return get_i64(value);
    }

    llvm::ConstantInt *get_i1(bool value) const
    {
        return builder_->getInt1(value);
    }

    llvm::ConstantInt *get_i8(int8_t value) const
    {
        return builder_->getInt8(value);
    }

    llvm::ConstantInt *get_i32(int32_t value) const
    {
        return builder_->getInt32(value);
    }

    llvm::ConstantInt *get_i64(int64_t value) const
    {
        return builder_->getInt64(value);
    }

    llvm::IntegerType *get_iptr_ty() const
    {
        return builder_->getIntPtrTy(module_->getDataLayout());
    }

    llvm::ConstantInt *get_iptr(uint64_t iptr) const
    {
        return llvm::ConstantInt::get(get_iptr_ty(), iptr);
    }

    llvm::Value *load_int(llvm::Value *int_ptr) const
    {
        return builder_->CreateLoad(get_int_ty(), int_ptr);
    }

    llvm::Value *load_ptr(llvm::Value *ptr_ptr) const
    {
        return builder_->CreateLoad(get_ptr_ty(), ptr_ptr);
    }

    llvm::IntegerType *get_index_ty() const
    {
        return builder_->getIndexTy(module_->getDataLayout(), 0);
    }

    llvm::ConstantInt *get_index(uint64_t index) const
    {
        return llvm::ConstantInt::get(get_index_ty(), index);
    }

    llvm::Constant *get_float(paw_Float value) const
    {
        return llvm::ConstantFP::get(get_float_ty(), value);
    }

    llvm::Value *operand(MirPlace const place)
    {
        switch (place.kind) {
            case MIR_PLACE_LOCAL:
                return f_->values[place.r.value];
            case MIR_PLACE_CONSTANT:
                return f_->constants[place.k.value];
            case MIR_PLACE_UPVALUE:
                return f_->upvalues[place.up];
        }
    }

    llvm::Type *get_ty(IrType *type)
    {
        switch (type->hdr.kind) {
            case kIrAdt:
                return get_adt_ty(type->Adt_);
            case kIrSignature:
            case kIrFnPtr:
                return get_fn_ty(type);
            case kIrTuple:
                return get_tuple_ty(type->Tuple_);
            case kIrNever:
                return get_void_ty();
            case kIrTraitObj:
            case kIrGeneric:
            case kIrInfer:
                // remaining types never appear at this phase of compilation
                PAW_UNREACHABLE();
        }
    }

    llvm::FunctionType *get_fn_ty(IrType *type)
    {
        return function_type_from_components(
                *type->FnPtr_.params,
                type->FnPtr_.result);
    }

    // Return the type of an ADT
    // Always returns the type "Ty" that needs to be passed to "alloca(Ty)" or
    // "malloc(sizeof(Ty))".
    llvm::Type *get_aggregate_ty(IrType *type)
    {
        if (type->hdr.kind == kIrTuple)
            return get_tuple_ty(type->Tuple_);
        return get_struct_ty(type->Adt_);
    }

    llvm::Type *get_struct_ty(IrAdt t)
    {
        IrAdtDef const *def = pawIr_get_adt_def(compiler_, t.did);
        if (def->is_struct) {
            IrTypeList *fields = pawP_instantiate_struct_fields(compiler_, &t);
            return struct_type_from_components(*fields);
        } else {
//            TODO(); // TODO: use "int + <opaque fields>" for enum?
            IrTypeList *fields = pawP_instantiate_variant_fields(compiler_, &t, 0);
            return struct_type_from_components(*fields);
        }
    }

    llvm::Type *get_adt_ty(IrAdt t)
    {
        auto const kind = pawP_type2code(compiler_, &(IrType &)t);
        switch (kind) {
            case BUILTIN_UNIT:
                return get_unit_ty();
            case BUILTIN_BOOL:
                return get_bool_ty();
            case BUILTIN_CHAR:
                return get_char_ty();
            case BUILTIN_INT:
                return get_int_ty();
            case BUILTIN_FLOAT:
                return get_float_ty();
            case BUILTIN_STR:
            case BUILTIN_LIST:
            case BUILTIN_MAP:
                return get_ptr_ty();
            default:
                break;
        }
        if (is_boxed_aggregate(&(IrType &)t))
            return get_ptr_ty();
        return get_struct_ty(t);
    }

    llvm::StructType *get_tuple_ty(IrTuple t)
    {
        return struct_type_from_components(*t.elems);
    }

    llvm::Value *get_kstr(Str const *key)
    {
        auto const itr = strings_.find(key);
        paw_assert(itr != strings_.end());
        return builder_->CreateLoad(get_ptr_ty(), itr->second);
    }

    bool is_unit_type(IrType *type)
    {
        return pawP_type2code(compiler_, type) == BUILTIN_UNIT;
    }

    llvm::FunctionType *function_type_from_components(IrTypeList param_types, IrType *return_type)
    {
        llvm::Type *return_ty = get_ty(return_type);
        auto param_tys = get_tys(&param_types);

        if (is_empty_ty(return_ty) || IrIsNever(return_type)) {
            return_ty = get_void_ty();
        } else if (!return_ty->isSingleValueType() || return_ty->isAggregateType()) {
            param_tys.insert(begin(param_tys), get_ptr_ty());
            return_ty = get_void_ty();
        }

        return llvm::FunctionType::get(return_ty, param_tys, false);
    }

    llvm::StructType *struct_type_from_components(IrTypeList fields)
    {
        return llvm::StructType::get(*context_, get_tys(&fields), false);
    }

    // Return the type of an opaque pointer
    llvm::PointerType *get_ptr_ty() const
    {
        return llvm::PointerType::getUnqual(*context_);
    }

    // Return the type of a statically-sized array
    llvm::ArrayType *get_array_ty(llvm::Type *type, uint64_t length) const
    {
        return llvm::ArrayType::get(type, length);
    }

    llvm::Type *get_void_ty() const
    {
        return builder_->getVoidTy();
    }

    llvm::StructType *get_unit_ty() const
    {
        return unit_ty_;
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
        return builder_->getDoubleTy();
    }

    llvm::IntegerType *get_int_ty() const
    {
        return get_i64_ty();
    }

    llvm::StructType *get_str_ty()
    {
        return str_ty_;
    }

    llvm::IntegerType *get_i1_ty() const
    {
        return builder_->getInt1Ty();
    }

    llvm::IntegerType *get_i8_ty() const
    {
        return builder_->getInt8Ty();
    }

    llvm::IntegerType *get_i32_ty() const
    {
        return builder_->getInt32Ty();
    }

    llvm::IntegerType *get_i64_ty() const
    {
        return builder_->getInt64Ty();
    }

    llvm::StructType *get_list_ty()
    {
        return llvm::StructType::get(*context_, {
                get_ptr_ty(),
                get_int_ty(),
                get_int_ty(),
            }, false);
    }

    llvm::StructType *get_map_ty()
    {
        return llvm::StructType::get(*context_, {
                get_ptr_ty(),
                get_int_ty(),
                get_int_ty(),
            }, false);
    }

//    // NOTE: It is the caller's responsibility to add an "unreachable" terminator after
//    //     calling a "noreturn" function.
//    llvm::Value *call_fn(llvm::FunctionType *ty, llvm::Value *fn, llvm::ArrayRef<llvm::Value *> args)
//    {
//        if (f_->ret != RETURN_WIDE)
//            return builder_->CreateCall(ty, fn, args);
//
//        // Call a function with a composite return type. The location where the
//        // return value should be written by the callee is passed as the first
//        // argument.
//
//        auto *return_ty = f_->fn->getReturnType();
//        auto *return_ptr = builder_->CreateAlloca(return_ty);
//
//        std::vector<llvm::Value *> ret_args(args);
//        ret_args.insert(begin(ret_args), return_ptr);
//        builder_->CreateCall(ty, fn, ret_args); // no return value
//        return builder_->CreateLoad(return_ty, return_ptr);
//    }

    CgMap *get_cgmap(IrType *type)
    {
        auto const itr = maps_.find(typekey(type));
        if (itr != end(maps_)) return itr->second.get();
        auto const e = maps_.emplace(typekey(type),
                std::make_unique<CgMap>(*this, type));
        paw_assert(e.second); // must be new element
        return e.first->second.get();
    }

    CgFn *get_cgfn(IrType *type)
    {
        auto const itr = fs_.find(typekey(type));
        paw_assert(itr != end(fs_));
        return itr->second.get();
    }

private:
    llvm::TargetMachine *machine_;
    std::unique_ptr<llvm::LLVMContext> context_;
    std::unique_ptr<llvm::Module> module_;
    std::unique_ptr<llvm::IRBuilder<>> builder_;
    std::map<Str const *, llvm::GlobalVariable *> strings_;
    std::vector<llvm::GlobalVariable *> globals_;
    std::unordered_map<TypeKey, std::unique_ptr<CgFn>> fs_;
    std::unordered_map<TypeKey, std::unique_ptr<CgMap>> maps_;
    std::unordered_map<TypeKey, std::unique_ptr<CgList>> lists_;
    CgFn *builtins_[NUM_BUILTINS];
    llvm::Function *malloc_fn_;
    llvm::Function *sdbm_fn_;
    llvm::Function *rawcmp_fn_;
    llvm::GlobalVariable *strtab_;
    llvm::StructType *unit_ty_;
    llvm::StructType *str_ty_;
    std::string modname_;
    Compiler *compiler_;
    CgFn *f_;
};

CgFn::CgFn(Context &ctx, llvm::Function *fn)
    : ctx(&ctx)
    , fn(fn)
    , return_ty(fn->getFunctionType()->getReturnType())
{
}

CgFn::CgFn(Context &ctx, Mir const mir)
    : ctx(&ctx)
    , blocks(mir.blocks->count)
    , constants(mir.kcache->data->count)
    , captures(mir.captured->count)
    , upvalues(mir.upvalues->count)
    , values(mir.registers->count)
    , closures(mir.children->count)
    , mir(mir)
{
    auto *b = ctx.get_builder();
    auto *m = ctx.get_module();
    auto *c = ctx.get_context();

    auto const linkage = pawIr_get_fn_def(mir.C, IR_TYPE_DID(mir.type))->is_pub
        ? llvm::Function::ExternalLinkage
        : llvm::Function::PrivateLinkage;

    auto *return_type = mir.type->FnPtr_.result;
    auto *param_types = mir.type->FnPtr_.params;
    auto *return_ty = ctx.get_ty(return_type);
    auto param_tys = ctx.get_tys(param_types);
    this->return_ty = return_ty;

    if (return_ty->isSingleValueType()
            && !return_ty->isAggregateType()) {
        ret = RETURN_SINGLE;
    } else if (is_empty_ty(return_ty)) {
        return_ty = ctx.get_void_ty();
        ret = RETURN_EMPTY;
    } else if (IrIsNever(return_type)) {
        return_ty = ctx.get_void_ty();
        ret = RETURN_NEVER;
    } else {
        param_tys.insert(begin(param_tys), ctx.get_ptr_ty());
        return_ty = ctx.get_void_ty();
        ret = RETURN_WIDE;
    }

    auto const mangled_name = mangle_fn_name(mir.C,
            mir.modname, mir.type, mir.self);
    fn = llvm::Function::Create(
            llvm::FunctionType::get(return_ty, param_tys, false),
            linkage, mangled_name, *m);
    fn->setDoesNotThrow();

    if (ret == RETURN_NEVER) {
        fn->setDoesNotReturn();
    } else if (ret == RETURN_WIDE) {
        fn->addParamAttr(0, llvm::Attribute::getWithStructRetType(*ctx.get_context(), this->return_ty));
    }
}

llvm::Value *CgFn::get_arg(unsigned index)
{
    auto const offset = ret == RETURN_WIDE;
    return fn->getArg(offset + index);
}

void CgFn::return_value(llvm::Value *value)
{
    auto *b = ctx->get_builder();
    switch (ret) {
        case RETURN_EMPTY:
            b->CreateRetVoid();
            break;
        case RETURN_SINGLE:
            b->CreateRet(value);
            break;
        case RETURN_WIDE:
            // write to the output argument
            value = b->CreateLoad(return_ty, value);
            b->CreateStore(value, fn->getArg(0));
            b->CreateRetVoid();
            break;
        case RETURN_NEVER:
            PAW_UNREACHABLE();
    }
}

CgList::CgList(Context &ctx, llvm::Value *list, llvm::Type *element_ty)
    : element_ty_(element_ty)
    , list_(list)
    , ctx_(&ctx)
{
    auto *b = ctx.get_builder();
    auto *list_ty = ctx.get_list_ty();
    L_.data_ptr = b->CreateStructGEP(list_ty, list, 0);
    L_.length_ptr = b->CreateStructGEP(list_ty, list, 1);
    L_.capacity_ptr = b->CreateStructGEP(list_ty, list, 2);
}

llvm::Value *CgList::get_data() const { return ctx_->load_ptr(get_data_ptr()); }
llvm::Value *CgList::get_length() const { return ctx_->load_int(get_length_ptr()); }
llvm::Value *CgList::get_capacity() const { return ctx_->load_int(get_capacity_ptr()); }

CgList CgList::construct(Context &ctx, llvm::Type *element_ty, llvm::Value *length)
{
    auto *b = ctx.get_builder();
    auto *list_ty = ctx.get_list_ty();
    auto *list = ctx.allocate(list_ty);

    CgList cglist(ctx, list, element_ty);

    // TODO: overflow check
    auto *capacity = ctx.emit_next_pow2(length);
    auto *data = cglist.new_buffer(capacity);

    b->CreateStore(data, cglist.get_data_ptr());
    b->CreateStore(length, cglist.get_length_ptr());
    b->CreateStore(capacity, cglist.get_capacity_ptr());

    return cglist;
}

CgList CgList::construct(Context &ctx, llvm::Type *element_ty, paw_Int length)
{
    return construct(ctx, element_ty, ctx.get_int(length));
}

llvm::Value *CgList::new_buffer(llvm::Value *capacity)
{
    // TODO: overflow check
    auto *b = ctx_->get_builder();
    auto const element_stride = ctx_->size_of(element_ty_);
    auto *buffer_size = b->CreateMul(capacity, ctx_->get_int(element_stride));
    return ctx_->allocate(buffer_size);
}

llvm::Value *CgList::gep(llvm::Value *index)
{
    auto *b = ctx_->get_builder();
    auto *data = ctx_->load_ptr(L_.data_ptr);
    return b->CreateInBoundsGEP(element_ty_, data, {index});
}

llvm::Value *CgList::gep(paw_Int index)
{
    return gep(ctx_->get_int(index));
}

// TODO: accept some components as arguments to avoid redundant loads
void CgList::grow()
{
    auto *x = ctx_;
    auto *c = x->get_context();
    auto *b = x->get_builder();

    // TODO: check for overflow
    // new_capacity = old_capacity * 1.5
    auto *old_capacity = x->load_int(L_.capacity_ptr);
    auto *half_capacity = b->CreateSDiv(old_capacity, x->get_int(2));
    auto *new_capacity = b->CreateAdd(old_capacity, half_capacity);

    auto *length = x->load_int(L_.length_ptr);
    auto *old_data = x->load_ptr(L_.data_ptr);
    auto *new_data = new_buffer(new_capacity);
    x->call_nonvolatile_memcpy(*b, new_data, old_data, length);

    b->CreateStore(new_capacity, L_.capacity_ptr);
    b->CreateStore(new_data, L_.data_ptr);
}


CgMap::CgMap(Context &ctx, IrType *type)
    : ctx_(&ctx)
    , builder_(std::make_unique<llvm::IRBuilder<>>(*ctx.get_context()))
    , map_type_(type)

{
    auto b = std::make_unique<llvm::IRBuilder<>>(*ctx.get_context());
    auto *key_type = K_LIST_AT(type->Adt_.types, 0);
    auto *value_type = K_LIST_AT(type->Adt_.types, 1);

    key_ty_ = ctx.get_ty(key_type);
    value_ty_ = ctx.get_ty(value_type);

    // look up "K::hash" and "K::equals" functions
    TraitOwnerList *const *powners = TraitOwners_get(ctx.compiler_,
            ctx.compiler_->trait_owners, key_type);
    auto *equals_type = K_LIST_FIRST((*powners)->data[TRAIT_EQUALS]);
    auto *hash_type = K_LIST_FIRST((*powners)->data[TRAIT_HASH]);
    equals_fn_ = ctx.fs_.find(ctx.typekey(equals_type))->second->fn;
    hash_fn_ = ctx.fs_.find(ctx.typekey(hash_type))->second->fn;
    equals_ty_ = ctx.get_fn_ty(equals_type);
    hash_ty_ = ctx.get_fn_ty(hash_type);

    create_getp(*b);
    create_newp(*b);
}

// "capacity" must be a power of 2 greater than 0
llvm::Value *CgMap::clamp_to_capacity(llvm::IRBuilder<> &b, llvm::Value *value, llvm::Value *capacity)
{
    auto *mask = b.CreateSub(capacity, ctx_->get_int(1));
    return b.CreateAnd(value, mask);
}

llvm::Value *CgMap::first_index(llvm::IRBuilder<> &b, llvm::Value *key, llvm::Value *capacity)
{
    auto *hash = b.CreateCall(hash_ty_, hash_fn_, key);
    return clamp_to_capacity(b, hash, capacity);
}

llvm::Value *CgMap::next_index(llvm::IRBuilder<> &b, llvm::Value *index, llvm::Value *capacity)
{
    auto *next = b.CreateAdd(index, ctx_->get_int(1));
    return clamp_to_capacity(b, next, capacity);
}

CgMap::Components CgMap::unpack(llvm::IRBuilder<> &b, llvm::Value *map)
{
    // determine map component addresses
    auto *data_ptr = b.CreateStructGEP(ctx_->get_map_ty(), map, 0);
    auto *length_ptr = b.CreateStructGEP(ctx_->get_map_ty(), map, 1);
    auto *capacity_ptr = b.CreateStructGEP(ctx_->get_map_ty(), map, 2);

    auto *length = b.CreateLoad(ctx_->get_int_ty(), length_ptr);
    auto *capacity = b.CreateLoad(ctx_->get_int_ty(), capacity_ptr);
    auto *flags = b.CreateLoad(ctx_->get_ptr_ty(), data_ptr);
    auto *keys = b.CreateInBoundsGEP(ctx_->get_char_ty(), flags, {capacity});
    auto *values = b.CreateInBoundsGEP(key_ty_, keys, {capacity});

    return {
        .length = length,
        .capacity = capacity,
        .data = flags,
        .flags = flags,
        .keys = keys,
        .values = values,
        .data_ptr = data_ptr,
        .length_ptr = length_ptr,
        .capacity_ptr = capacity_ptr,
    };
}

//pub fn lookup(m: Map_<str, int>, key: str) -> int
//{
//entry:
//    let index = first_index(m, key);
//
//loop_header:
//    match m.flags[index] {
//        MapFlag::Vacant => {
//found_vacant:
//            br emit_result
//        },
//        MapFlag::Erased => {
//found_erased:
//            br loop_footer
//        },
//        MapFlag::Exists => {
//found_exists:
//            if m.keys[index] == key {
//found_equals:
//                br emit_result
//            }
//        },
//    }
//loop_footer:
//    index = next_index(m, index)
//    br loop_header
//
//emit_result:
//    %result = phi nullptr found_vacant, &m.values[index] found_equals
//    -1
//}

// Determine where the "search_key" belongs inside a map object with the
// given components
// If the key is found, control is moved to the "target_block", and the
// index at which the key was found is returned. Otherwise, control is
// moved to
llvm::Value *CgMap::lookup(llvm::IRBuilder<> &b, llvm::Function *fn, CgMap::Components &m, llvm::Value *search_key)
{
    auto *x = ctx_;
    auto *c = x->get_context();

    auto *entry_block = b.GetInsertBlock();
    auto *loop_header_block = llvm::BasicBlock::Create(*c, "loop_header", fn);
    auto *loop_footer_block = llvm::BasicBlock::Create(*c, "loop_footer", fn);
    auto *found_exists_block = llvm::BasicBlock::Create(*c, "found_exists", fn);
    auto *found_equals_block = llvm::BasicBlock::Create(*c, "found_equals", fn);
    auto *emit_result_block = llvm::BasicBlock::Create(*c, "emit_result", fn);

    auto *index0 = first_index(b, search_key, m.capacity);
    auto *result0 = llvm::ConstantPointerNull::get(x->get_ptr_ty());

    b.CreateBr(loop_header_block);
    b.SetInsertPoint(loop_header_block);
    auto *index = b.CreatePHI(x->get_int_ty(), 2);
    index->addIncoming(index0, entry_block);
    // switch on the value of the flag
    auto *flag_ptr = b.CreateInBoundsGEP(x->get_i8_ty(), m.flags, {index});
    auto *flag = b.CreateLoad(x->get_i8_ty(), flag_ptr);
    auto *switch_flag = b.CreateSwitch(flag, found_exists_block, 2);
    switch_flag->addCase(x->get_i8(MAP_VACANT), emit_result_block);
    switch_flag->addCase(x->get_i8(MAP_ERASED), loop_footer_block);

    // check if the key at "index" matches the "search_key"
    b.SetInsertPoint(found_exists_block);
    auto *key_ptr = b.CreateInBoundsGEP(key_ty_, m.flags, {index});
    auto *key = b.CreateLoad(key_ty_, key_ptr);
    auto *key_equals = b.CreateCall(equals_ty_, equals_fn_, {search_key, key});
    b.CreateBr(loop_footer_block);

    b.SetInsertPoint(loop_footer_block);
    auto *key_found = b.CreatePHI(x->get_bool_ty(), 2);
    key_found->addIncoming(x->get_bool(false), loop_header_block);
    key_found->addIncoming(key_equals, found_exists_block);
    auto *index1 = next_index(b, index, m.capacity);
    index->addIncoming(index1, loop_footer_block);
    b.CreateCondBr(key_found, found_equals_block, loop_header_block);

    b.SetInsertPoint(found_equals_block);
    auto *result1 = b.CreateInBoundsGEP(value_ty_, m.values, {index});
    b.CreateBr(emit_result_block);

    b.SetInsertPoint(emit_result_block);
    auto *result = b.CreatePHI(x->get_ptr_ty(), 2);
    result->addIncoming(result0, loop_header_block);
    result->addIncoming(result1, found_equals_block);
    return result;
}

//pub fn insert(m: Map_<str, int>, key: str, value: int) -> int
//{
//    let VACANT = 0;
//    let ERASED = 1;
//    let EXISTS = 2;
//
//    let erased = -1;
//    let index = m.hash(key) & (m.capacity - 1);
//
//    loop {
//        match m.flags[index] {
//            0 => { // VACANT
//                break;
//            },
//            1 => { // ERASED
//                erased = if erased == -1 {
//                    index
//                } else {
//                    erased
//                };
//            },
//            _ => { // EXISTS
//                if m.keys[index] == key {
//                    return index;
//                }
//            },
//        }
//        index = (index + 1) & (m.capacity - 1);
//    }
//
//    return if erased == -1 {
//        index
//    } else {
//        erased
//    };
//}

llvm::Value *CgMap::access(llvm::IRBuilder<> &b, llvm::Function *fn, CgMap::Components &m, llvm::Value *search_key)
{
    auto *x = ctx_;
    auto *c = x->get_context();

    auto *entry_block = b.GetInsertBlock();
    auto *loop_header_block = llvm::BasicBlock::Create(*c, "loop_header", fn);
    auto *loop_footer_block = llvm::BasicBlock::Create(*c, "loop_footer", fn);
    auto *found_erased_block = llvm::BasicBlock::Create(*c, "found_erased", fn);
    auto *found_exists_block = llvm::BasicBlock::Create(*c, "found_exists", fn);
    auto *found_vacant_block = llvm::BasicBlock::Create(*c, "found_vacant", fn);
    auto *emit_result_block = llvm::BasicBlock::Create(*c, "emit_result", fn);

    auto *index0 = first_index(b, search_key, m.capacity);
    auto *erased0 = x->get_int(-1);

    b.CreateBr(loop_header_block);
    b.SetInsertPoint(loop_header_block);
    auto *index = b.CreatePHI(x->get_int_ty(), 2);
    auto *erased = b.CreatePHI(x->get_int_ty(), 2);
    index->addIncoming(index0, entry_block);
    erased->addIncoming(erased0, entry_block);
    // check if a key exists or was erased at the given "index"
    auto *flag_ptr = b.CreateInBoundsGEP(x->get_i8_ty(), m.flags, {index});
    auto *flag = b.CreateLoad(x->get_i8_ty(), flag_ptr);
    auto *switch_flag = b.CreateSwitch(flag, found_exists_block, 2);
    switch_flag->addCase(x->get_i8(MAP_VACANT), found_vacant_block);
    switch_flag->addCase(x->get_i8(MAP_ERASED), found_erased_block);

    b.SetInsertPoint(found_erased_block);
    auto *is_first_erased = b.CreateICmpEQ(erased, x->get_int(-1));
    auto *erased1 = b.CreateSelect(is_first_erased, index, erased);
    b.CreateBr(loop_footer_block);

    b.SetInsertPoint(found_exists_block);
    auto *key_ptr = b.CreateInBoundsGEP(key_ty_, m.flags, {index});
    auto *key = b.CreateLoad(key_ty_, key_ptr);
    auto *key_equals = b.CreateCall(equals_ty_, equals_fn_, {search_key, key});
    b.CreateCondBr(key_equals,
            emit_result_block,
            loop_footer_block);

    b.SetInsertPoint(loop_footer_block);
    auto *erased2 = b.CreatePHI(x->get_int_ty(), 2);
    erased2->addIncoming(erased, found_exists_block);
    erased2->addIncoming(erased1, found_erased_block);
    erased->addIncoming(erased2, loop_footer_block);
    auto *index1 = next_index(b, index, m.capacity);
    index->addIncoming(index1, loop_footer_block);
    b.CreateBr(loop_header_block);

    // add a new key-value pair to the map (falls through to "emit_result_block"
    // to handle emitting the pointer-to-value)
    b.SetInsertPoint(found_vacant_block);
    auto *not_erased = b.CreateICmpEQ(erased, x->get_int(-1));
    auto *insert_loc = b.CreateSelect(not_erased, index, erased);
    // m.flags[insert_loc] = MAP_EXISTS
    flag_ptr = b.CreateInBoundsGEP(x->get_i8_ty(), m.flags, {insert_loc});
    b.CreateStore(x->get_i8(MAP_EXISTS), flag_ptr);
    // m.keys[insert_loc] = search_key
    key_ptr = b.CreateInBoundsGEP(key_ty_, m.flags, {insert_loc});
    b.CreateStore(search_key, key_ptr);
    m.length = b.CreateAdd(m.length, x->get_int(1));
    b.CreateStore(m.length, m.length_ptr);
    b.CreateBr(emit_result_block);

    // set the value of an existing key-value pair
    b.SetInsertPoint(emit_result_block);
    // return &m.values[write_loc]
    auto *write_loc = b.CreatePHI(x->get_int_ty(), 2);
    write_loc->addIncoming(index, found_exists_block);
    write_loc->addIncoming(insert_loc, found_vacant_block);
    return b.CreateInBoundsGEP(value_ty_, m.values, {write_loc});
}

// TODO: call grow() after setelement if "length > capacity / fill_factor"
void CgMap::grow(llvm::IRBuilder<> &b, llvm::Function *fn, CgMap::Components &m)
{
    auto *x = ctx_;
    auto *c = x->get_context();

    // TODO: check for overflow
    auto *capacity = b.CreateMul(m.capacity, x->get_int(2));
    auto *buffer = new_buffer(b, capacity);

    Components rehash = m;
    rehash.data = rehash.flags = buffer;
    rehash.keys = b.CreateInBoundsGEP(x->get_char_ty(), rehash.flags, {capacity});
    rehash.values = b.CreateInBoundsGEP(key_ty_, rehash.keys, {capacity});
    rehash.capacity = capacity;

    auto *iteration_header = llvm::BasicBlock::Create(*c, "map_rehash_iteration_header", fn);
    auto *iteration_body = llvm::BasicBlock::Create(*c, "map_rehash_iteration_body", fn);
    auto *search_header = llvm::BasicBlock::Create(*c, "map_rehash_search_header", fn);
    auto *search_body = llvm::BasicBlock::Create(*c, "map_rehash_search_body", fn);
    auto *after = llvm::BasicBlock::Create(*c, "map_rehash_after", fn);

    auto *outer_index = x->get_int(0);
    auto *iterations = m.length;

    b.SetInsertPoint(iteration_header);
    auto *should_continue = b.CreateICmpSGT(iterations, x->get_int(0));
    b.CreateCondBr(should_continue, iteration_body, after);

    b.SetInsertPoint(iteration_body);
    {
        auto *key = x->array_get(key_ty_, m.keys, outer_index);
        auto *value = x->array_get(value_ty_, m.values, outer_index);

        auto *value_ptr = access(b, fn, m, key);
        b.CreateStore(value, value_ptr);
        b.CreateBr(search_body);
    }
// TODO    b.CreateCondBr(found, rehash_body, iteration_header);

//        // TODO: check can be performed before or after the search_body, since nonexistent keys are detected by inspecting the flags and the capacity is never 0
//        // TODO: consider moving the check to after the search_body, benchmark it both ways
//        b.CreateBr(search_header);
//        b.SetInsertPoint(search_header);
//        map_access_inner(data, keys, values, index, capacity,
//                key, create_if_missing);
//        auto *should_continue = b.CreateICmpSGT(count, get_int(0));
//        b.CreateCondBr(should_continue, search_body, rehash_header);

    b.SetInsertPoint(search_body);
    iterations = b.CreateSub(iterations, x->get_int(1));
    b.CreateBr(search_header);

    b.CreateStore(buffer, m.data_ptr);
    b.CreateStore(capacity, m.capacity_ptr);
}

llvm::Value *CgMap::new_buffer(llvm::IRBuilder<> &b, llvm::Value *capacity)
{
    auto *x = ctx_;
    auto *c = x->get_context();

    auto const key_stride = x->stride_of(key_ty_);
    auto const value_stride = x->stride_of(value_ty_);
    auto *item_size = x->get_int(1 + key_stride + value_stride);
    auto *buffer_size = b.CreateMul(capacity, item_size);
    auto *buffer = x->allocate(buffer_size);
    // clear the "flags" region of the buffer
    x->call_nonvolatile_memset(b, buffer, x->get_i8(0), capacity);
    return buffer;
}

llvm::Value *CgMap::construct(llvm::IRBuilder<> &b, llvm::Value *length)
{
    auto *x = ctx_;
    auto *c = x->get_context();

    auto *map_ty = x->get_map_ty();
    auto *map = x->allocate(map_ty);

    auto *data_ptr = b.CreateStructGEP(map_ty, map, 0);
    auto *length_ptr = b.CreateStructGEP(map_ty, map, 1);
    auto *capacity_ptr = b.CreateStructGEP(map_ty, map, 2);

    auto const max_align = std::max(
            x->align_of(key_ty_),
            x->align_of(value_ty_));

    // TODO: overflow check

    // "capacity" is a power of 2 greater than or equal to the maximum alignment
    // needed by a key or value. This makes it so that if the start of the map
    // is aligned properly (guaranteed by malloc), then both the start of the key
    // and value sections will also be aligned properly.
    auto *capacity = x->emit_imax(
            x->emit_next_pow2(length),
            x->get_int(max_align.value()));

    auto *data = new_buffer(b, capacity);

    b.CreateStore(data, data_ptr);
    b.CreateStore(length, length_ptr);
    b.CreateStore(capacity, capacity_ptr);

    return map;
}

llvm::Value *CgMap::gep(llvm::IRBuilder<> &b, llvm::Value *map, llvm::Value *key)
{
    return b.CreateCall(gep_fn_, {map, key});
}

llvm::Value *CgMap::nep(llvm::IRBuilder<> &b, llvm::Value *map, llvm::Value *key)
{
    return b.CreateCall(nep_fn_, {map, key});
}

void CgMap::create_getp(llvm::IRBuilder<> &b)
{
    auto *x = ctx_;
    auto *c = x->get_context();
    auto *m = x->get_module();

    auto const name = mangle_adt_name(
            x->compiler_,
            SCAN_STR(x->compiler_, "map"),
            map_type_) + "4getp";
    auto *ty = llvm::FunctionType::get(
            x->get_ptr_ty(), {
                x->get_ptr_ty(), // [K: V]
                key_ty_,         // K
            }, false);
    auto *fn = llvm::Function::Create(
            ty, llvm::Function::PrivateLinkage,
            name, *m);
    fn->setDoesNotThrow();

    auto *entry = llvm::BasicBlock::Create(*c, "entry", fn);
    b.SetInsertPoint(entry);

    auto *map = fn->getArg(0);
    auto *key = fn->getArg(1);
    auto com = unpack(b, map);

    auto *value = lookup(b, fn, com, key);
    b.CreateRet(value);

    gep_fn_ = fn;
}

void CgMap::create_newp(llvm::IRBuilder<> &b)
{
    auto *x = ctx_;
    auto *c = x->get_context();
    auto *m = x->get_module();

    auto const name = mangle_adt_name(
            x->compiler_,
            SCAN_STR(x->compiler_, "map"),
            map_type_) + "4newp";
    auto *ty = llvm::FunctionType::get(
            x->get_ptr_ty(), {
                x->get_ptr_ty(), // [K: V]
                key_ty_,         // K
            },
            false);
    auto *fn = llvm::Function::Create(
            ty, llvm::Function::PrivateLinkage,
            name, *m);
    fn->setDoesNotThrow();

    auto *entry = llvm::BasicBlock::Create(*c, "entry", fn);
    b.SetInsertPoint(entry);

    auto *map = fn->getArg(0);
    auto *key = fn->getArg(1);
    auto com = unpack(b, map);

    auto *value = access(b, fn, com, key);
    b.CreateRet(value);

    nep_fn_ = fn;
}

static void generate_function(Context &ctx, Mir &mir)
{
    if (mir.blocks->count == 0)
        // TODO: extern functions (compile as separate C library exposing functions/methods?)
        return;

    printf("generating function %s::%s...\n", mir.modname->text, mir.name->text);
    //printf("mir = %s\n", pawMir_dump((Mir *)&mir));

    auto const builtin_kind = ctx.get_builtin_kind(mir.type);
    if (builtin_kind != NOT_BUILTIN) return;

    ctx.enter_function(mir);
    for (auto i = 0; i < mir.blocks->count; ++i) {
        ctx.enter_basic_block(i);
        auto const &bb = *mir.blocks->data[i];
        for (auto i = 0; i < bb.joins->count; ++i)
            ctx.instruction(*bb.joins->data[i]);
        for (auto i = 0; i < bb.instructions->count; ++i)
            ctx.instruction(*bb.instructions->data[i]);
        ctx.leave_basic_block();
    }
    for (auto i = 0; i < mir.blocks->count; ++i)
        ctx.finalize_basic_block(i);
    for (int i = 0; i < mir.children->count; ++i) {
        Mir *child = mir.children->data[i];
        generate_function(ctx, *child);
    }
    ctx.leave_function();
}

void pawCodegen_generate(Compiler *C, TranslationUnit const *tu)
{
    // TODO: collect from commandline and store in "C->cgopt"
    CodegenOptions const C_cgopt = {
        .verify_module = true,
        .print_mlir = true,
        .enable_asan = false,
        .opt_suffix = '0',
    };

    Context ctx(C, tu->modname);
    for (int i = 0; i < tu->mir_count; ++i) {
        Mir const &mir = *tu->mirs[i];
        ctx.declare_function(mir);
    }

    ctx.startup_module();
    for (int i = 0; i < tu->mir_count; ++i)
        generate_function(ctx, *tu->mirs[i]);
    ctx.generate(tu->modname, C_cgopt);
    ctx.teardown_module();
}

