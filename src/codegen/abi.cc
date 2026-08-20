// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "context.h"

#include "abi.h"

namespace paw::cg {

namespace {

struct Field {
    llvm::Type *type;
    uint64_t offset;
};

void collect_fields(llvm::Type *ty, uint64_t base_offset, llvm::DataLayout const &DL, std::vector<Field> &out)
{
    if (auto *ST = llvm::dyn_cast<llvm::StructType>(ty)) {
        auto const *layout = DL.getStructLayout(ST);
        for (unsigned i = 0; i < ST->getNumElements(); ++i)
            collect_fields(ST->getElementType(i),
                base_offset + layout->getElementOffset(i),
                DL, out);
    } else if (auto *AT = llvm::dyn_cast<llvm::ArrayType>(ty)) {
        auto const element_size = DL.getTypeAllocSize(AT->getElementType()).getFixedValue();
        for (unsigned i = 0; i < AT->getNumElements(); ++i)
            collect_fields(AT->getElementType(),
                base_offset + i * element_size,
                DL, out);
    } else {
        out.push_back({ty, base_offset});
    }
}

AbiInfo create_abi_info(AbiInfo::Kind kind, llvm::Type *return_ty, llvm::Type *param_ty = nullptr)
{
    return AbiInfo{
        .kind = kind,
        .return_ty = return_ty,
        .param_ty = param_ty != nullptr ? param_ty : return_ty,
        .m = {0},
    };
}

AbiInfo create_value(llvm::Type *return_ty, llvm::Type *param_ty = nullptr)
{
    return create_abi_info(AbiInfo::Kind::VALUE, return_ty, param_ty);
}

#define CREATE_EMPTY(X_) create_abi_info(AbiInfo::Kind::EMPTY, \
        (X_).get_void_ty(), (X_).get_unit_ty())
#define CREATE_MEMORY(X_) create_abi_info(AbiInfo::Kind::MEMORY, \
        (X_).get_void_ty(), (X_).get_ptr_ty())

// Implementation of `abi_info` for the aarch64 platform
namespace aarch64 {

struct FieldCounter {
    unsigned floats;
    unsigned doubles;
    unsigned total;
};

FieldCounter count_fp_fields(llvm::Type *ty)
{
    if (ty->isFloatTy()) return FieldCounter{1, 0, 1};
    if (ty->isDoubleTy()) return FieldCounter{0, 1, 1};
    if (!ty->isStructTy()) return FieldCounter{0, 0, 1};

    struct FieldCounter counter = {};
    auto *st = llvm::cast<llvm::StructType>(ty);
    for (auto i = 0U; i < st->getNumElements(); ++i) {
        auto *field_ty = st->getElementType(i);
        auto const [floats, doubles, total] = count_fp_fields(field_ty);
        counter.floats += floats;
        counter.doubles += doubles;
        counter.total += total;
    }
    return counter;
}

struct HfaInfo {
    llvm::Type *ty;
    unsigned count;
};

// Determine if the given structure is a homogeneous floating-point aggregate (HFA)
// A structure is a HFA if it contains at least 1 and less than or equal to 4 fields of the
// same floating-point type. Such types are passed and returned by value.
bool check_hfa(Context &X, llvm::Type *ty, HfaInfo *out)
{
    paw_assert(ty->isStructTy() || ty->isArrayTy());
    auto const [floats, doubles, total] = count_fp_fields(ty);
    if (0 < total && total <= 4) {
        if (floats == total) {
            out->ty = X.get_f32_ty();
            out->count = total;
            return true;
        }
        if (doubles == total) {
            out->ty = X.get_f64_ty();
            out->count = total;
            return true;
        }
    }
    return false;
}

AbiInfo arg_info(Context &X, Type const &type)
{
    switch (type.get_kind()) {
        case Type::Kind::UNIT:
            return CREATE_EMPTY(X);
        case Type::Kind::BOOL:
            return create_value(X.get_i8_ty());
        case Type::Kind::CHAR:
        case Type::Kind::INT:
        case Type::Kind::FLOAT:
        case Type::Kind::PTR:
            return create_value(type.get_ty());
        case Type::Kind::STR:
        case Type::Kind::SLICE:
            return create_value(
                    // use `[i64 x 2]` since bitsize is 128
                    X.get_array_ty(X.get_i64_ty(), 2));
        case Type::Kind::ARRAY:
            if (type.get_bitsize() == 0)
                return CREATE_EMPTY(X);
            if (type.get_bitsize() > 128)
                return CREATE_MEMORY(X);
            return create_value(type.get_ty());
        case Type::Kind::OBJECT: {
            ObjectType const &t = (ObjectType &)type;
            if (t.get_bitsize() == 0)
                return CREATE_EMPTY(X);
            if (t.get_num_variants() > 1) {
                // NOTE: enums are represented as `[N x i8]`
                if (type.get_bitsize() > 128)
                    return CREATE_MEMORY(X);
                return create_value(type.get_ty());
            }
            auto *ty = t.get_variant_ty(Discriminant::base());
            if (HfaInfo hfa; check_hfa(X, ty, &hfa))
                return create_value(ty, X.get_array_ty(hfa.ty, hfa.count));
            if (type.get_bitsize() > 128)
                return CREATE_MEMORY(X);
            if (type.get_bitsize() > 64)
                return create_value(X.get_array_ty(X.get_i64_ty(), 2));
            auto const bitsize = PAW_ROUND_UP(t.get_bitsize(), 8U);
            return create_value(X.get_sized_int_ty(bitsize), X.get_i64_ty());
        }
        case Type::Kind::FN: {
            FnType const *fn = static_cast<FnType const *>(&type);
            if (fn->has_env())
                return arg_info(X, *fn->get_env_type());
            return create_value(X.get_ptr_ty());
        }
    }
}

AbiFnInfo abi_info(Context &X, FnType const &type)
{
    AbiFnInfo info;
    if ((info.has_env = type.has_env()))
        info.env_info = arg_info(X, *type.get_env_type());
    info.param_info.reserve(type.get_num_params());
    for (auto i = 0U; i < type.get_num_params(); ++i)
        info.param_info.push_back(arg_info(X, *type.get_param_type(i)));
    info.return_info = arg_info(X, *type.get_return_type());
    return info;
}

} // namespace aarch64

namespace x86_64 {

enum class Class {
    EMPTY,
    INTEGER,
    SSE,
    SSEUP,
    X87,
    X87UP,
    COMPLEX_X87,
    MEMORY,
};

Class combine_classes(Class a, Class b)
{
    // "If both classes are equal, this is the resulting class."
    if (a == b) return a;

    // "If one of the classes is NO_CLASS, the resulting class is the other class."
    if (a == Class::EMPTY) return b;
    if (b == Class::EMPTY) return a;

    // "If one of the classes is MEMORY, the result is the MEMORY class."
    if (a == Class::MEMORY || b == Class::MEMORY) return Class::MEMORY;

    // "If one of the classes is INTEGER, the result is the INTEGER."
    if (a == Class::INTEGER || b == Class::INTEGER) return Class::INTEGER;

    // "If one of the classes is X87, X87UP, COMPLEX_X87 class, MEMORY is used as class."
    if (a == Class::X87 || b == Class::X87
            || a == Class::X87UP || b == Class::X87UP
            || a == Class::COMPLEX_X87 || b == Class::COMPLEX_X87)
        return Class::MEMORY;

    // "Otherwise class SSE is used."
    return Class::SSE;
}

Class classify_field(llvm::Type *ty)
{
    if (ty->isPointerTy())
        return Class::INTEGER;
    if (ty->isIntegerTy())
        return Class::INTEGER;
    if (ty->isFloatingPointTy())
        return Class::SSE;
    PAW_UNREACHABLE();
}

llvm::Type *translate_field(Context &X, Class cls)
{
    switch (cls) {
        case Class::INTEGER:
            return X.get_i64_ty();
        case Class::SSE:
            return X.get_f64_ty();
        default:
            // TODO: Paw only needs INTEGER and SSE right now
            PAW_UNREACHABLE();
    }
}

struct RegisterInfo {
    unsigned num_int_regs;
    unsigned num_sse_regs;
};

#define DECREMENT_IF_NONZERO(Reg_) ((Reg_) -= (Reg_) > 0)

AbiInfo composite_info(Context &X, llvm::Type *ty, RegisterInfo &regs)
{
    if (X.bitsize_of(ty) == 0)
        return CREATE_EMPTY(X);
    if (X.bitsize_of(ty) > 128) {
        auto info = CREATE_MEMORY(X);
        info.m.requires_byval = true;
        info.m.alignment = X.align_of(ty);
        return info;
    }

    auto const DL = X.get_module()->get_module()->getDataLayout();
    auto *c = X.get_context();

    std::vector<Field> fields;
    collect_fields(ty, 0, DL, fields);

    Class info[] = {
        Class::EMPTY,
        Class::EMPTY,
    };
    for (auto const [ty, offset]: fields) {
        paw_assert(offset < 16);
        auto &cls = info[offset / 8];
        // classify each of the 2 eightbytes separately
        cls = combine_classes(cls,
                classify_field(ty));
    }

#define DECREMENT_BY_CLASS(Regs_, Class_) do { \
            if ((Class_) == Class::INTEGER) { \
                (Regs_).num_int_regs -= 1; \
            } else { \
                (Regs_).num_sse_regs -= 1; \
            } \
        } while (0)

    if (info[1] == Class::EMPTY) {
        paw_assert(X.bitsize_of(ty) <= 64 // upper eightbyte unused
                && (info[0] == Class::INTEGER || info[0] == Class::SSE));
        auto *abi_ty = info[0] == Class::INTEGER
            ? X.get_sized_int_ty(PAW_ROUND_UP(X.bitsize_of(ty), 8U))
            : X.bitsize_of(ty) == 32 ? X.get_f32_ty() : X.get_f64_ty();
        DECREMENT_BY_CLASS(regs, info[0]);
        return create_abi_info(AbiInfo::Kind::VALUE, abi_ty);
    }

#undef DECREMENT_BY_CLASS

    std::vector const field_tys = {
        translate_field(X, info[0]),
        translate_field(X, info[1]),
    };
    unsigned const int_regs_needed = (info[0] == Class::INTEGER) + (info[1] == Class::INTEGER);
    unsigned const sse_regs_needed = (info[0] == Class::SSE) + (info[1] == Class::SSE);
    if (regs.num_int_regs >= int_regs_needed && regs.num_sse_regs >= sse_regs_needed) {
        regs.num_int_regs -= int_regs_needed;
        regs.num_sse_regs -= sse_regs_needed;
        auto *abi_ty = llvm::StructType::get(*c, field_tys);
        return create_abi_info(AbiInfo::Kind::EXPAND, abi_ty);
    }

    // not enough registers for this argument: pass it on the stack
    auto m = CREATE_MEMORY(X);
    m.m.requires_byval = true;
    m.m.alignment = X.align_of(ty);
    return m;
}

AbiInfo arg_info(Context &X, Type const &type, RegisterInfo &regs)
{
    auto *ty = type.get_ty();
    switch (type.get_kind()) {
        case Type::Kind::UNIT:
            return CREATE_EMPTY(X);
        case Type::Kind::BOOL:
            DECREMENT_IF_NONZERO(regs.num_int_regs);
            return create_value(X.get_i8_ty());
        case Type::Kind::CHAR:
        case Type::Kind::INT:
        case Type::Kind::PTR:
            DECREMENT_IF_NONZERO(regs.num_int_regs);
            return create_value(ty);
        case Type::Kind::FLOAT:
            DECREMENT_IF_NONZERO(regs.num_sse_regs);
            return create_value(ty);
        case Type::Kind::STR:
        case Type::Kind::SLICE:
        case Type::Kind::ARRAY:
        case Type::Kind::OBJECT:
            if (type.get_bitsize() == 0)
                return CREATE_EMPTY(X);
            return composite_info(X, type.get_ty(), regs);
        case Type::Kind::FN: {
            FnType const *fn = static_cast<FnType const *>(&type);
            if (fn->has_env())
                return arg_info(X, *fn->get_env_type(), regs);
            DECREMENT_IF_NONZERO(regs.num_int_regs);
            return create_value(X.get_ptr_ty());
        }
    }
}

AbiFnInfo abi_info(Context &X, FnType const &type)
{
    AbiFnInfo info;
    RegisterInfo regs;

    // `regs` not relevant for return value
    regs.num_int_regs = regs.num_sse_regs = UINT_MAX;
    info.return_info = arg_info(X, *type.get_return_type(), regs);

    regs.num_int_regs = 6; // %rdi, %rsi, %rdx, %rcx, %r8, %r9
    regs.num_sse_regs = 8; // %xmm0, %xmm1, ..., %xmm7

    // `sret` pointer is passed in `%rdi`
    if (info.return_info.is_memory())
        --regs.num_int_regs;

    if ((info.has_env = type.has_env()))
        info.env_info = arg_info(X, *type.get_env_type(), regs);
    info.param_info.reserve(type.get_num_params());
    for (auto i = 0U; i < type.get_num_params(); ++i)
        info.param_info.push_back(arg_info(X, *type.get_param_type(i), regs));
    return info;
}

} // namespace x86_64

namespace unknown {

AbiInfo arg_info(Context &X, Type const &type)
{
    if (type.get_bitsize() == 0)
        return CREATE_EMPTY(X);
    auto const two_pointers = X.bitsize_of(X.get_ptr_ty()) * 2;
    if (type.get_bitsize() > two_pointers)
        return CREATE_MEMORY(X);
    return create_value(type.get_ty());
}

AbiFnInfo abi_info(Context &X, FnType const &type)
{
    AbiFnInfo info;
    if ((info.has_env = type.has_env()))
        info.env_info = arg_info(X, *type.get_env_type());
    info.param_info.reserve(type.get_num_params());
    for (auto i = 0U; i < type.get_num_params(); ++i)
        info.param_info.push_back(arg_info(X, *type.get_param_type(i)));
    info.return_info = arg_info(X, *type.get_return_type());
    return info;
}

} // namespace unknown

} // namespace

AbiFnInfo get_abi_info_(Context &X, FnType const &type)
{
    auto *m = X.get_module()->get_module();
    llvm::Triple const triple(m->getTargetTriple());
    switch (triple.getArch()) {
        case llvm::Triple::aarch64:
            return aarch64::abi_info(X, type);
        case llvm::Triple::x86_64:
            return x86_64::abi_info(X, type);
        default:
            return unknown::abi_info(X, type);
    }
}

} // namespace paw::cg

