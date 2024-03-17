// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#include "prefix.h"

#include "array.h"
#include "aux.h"
#include "bigint.h"
#include "call.h"
#include "env.h"
#include "error.h"
#include "gc.h"
#include "io.h"
#include "lex.h"
#include "lib.h"
#include "map.h"
#include "mem.h"
#include "opcode.h"
#include "paw.h"
#include "rt.h"
#include "str.h"
#include "util.h"
#include "value.h"
#include <assert.h>
#include <math.h>
#include <stdarg.h>

// Helpers for the VM:
#define vm_switch(x) switch (x)
#define vm_case(x) \
    break;         \
    case OP_##x
#define vm_default \
    break;         \
    default
#define vm_continue continue
#define vm_shift(n) paw_shift(P, n)
#define vm_push(v) (*pawC_stkinc(P, 1) = (v))
#define vm_pop(n) pawC_stkdec(P, n)
#define vm_peek(n) (&P->top[-(n)-1])
#define vm_save() (cf->pc = pc, cf->top = P->top)
#define vm_local() (&cf->base[Iw()])
#define vm_upvalue() (fn->up[Iw()]->p)
#define vm_const() (fn->p->k[Iw()])
#define vm_jmp() decode_jump(Iw())

#define vm_push0() pawC_stkinc(P, 1)
#define vm_pushi(i) vm_seti(pawC_stkinc(P, 1), i)
#define vm_pushf(f) vm_setf(pawC_stkinc(P, 1), f)
#define vm_pushb(b) vm_setb(pawC_stkinc(P, 1), b)
#define vm_set0(sp) pawV_set_null(sp)
#define vm_seti(sp, i) pawV_set_int(sp, i)
#define vm_setf(sp, f) pawV_set_float(sp, f)
#define vm_setb(sp, b) pawV_set_bool(sp, b)

// Slot 0 (the callable or 'self') is an implicit parameter
#define vm_argc() (paw_get_count(P) - 1)

static Value vnull(void)
{
    Value v;
    pawV_set_null(&v);
    return v;
}

static Value vint(paw_Int i)
{
    Value v;
    pawV_set_int(&v, i);
    return v;
}

const char *pawR_opcode_name(Op op)
{
    switch (op) {
        case OP_PUSHNULL:
            return "PUSHNULL";
        case OP_PUSHTRUE:
            return "PUSHTRUE";
        case OP_PUSHFALSE:
            return "PUSHFALSE";
        case OP_PUSHCONST:
            return "PUSHCONST";
        case OP_POP:
            return "POP";
        case OP_CLOSE:
            return "CLOSE";
        case OP_RETURN:
            return "RETURN";
        case OP_CLOSURE:
            return "CLOSURE";
        case OP_GETSUPER:
            return "GETSUPER";
        case OP_INVOKESUPER:
            return "INVOKESUPER";
        case OP_CALL:
            return "CALL";
        case OP_INHERIT:
            return "INHERIT";
        case OP_INVOKE:
            return "INVOKE";
        case OP_JUMP:
            return "JUMP";
        case OP_JUMPFALSE:
            return "JUMPFALSE";
        case OP_JUMPNULL:
            return "JUMPNULL";
        case OP_GLOBAL:
            return "GLOBAL";
        case OP_GETGLOBAL:
            return "GETGLOBAL";
        case OP_SETGLOBAL:
            return "SETGLOBAL";
        case OP_GETLOCAL:
            return "GETLOCAL";
        case OP_SETLOCAL:
            return "SETLOCAL";
        case OP_UPVALUE:
            return "UPVALUE";
        case OP_GETUPVALUE:
            return "GETUPVALUE";
        case OP_SETUPVALUE:
            return "SETUPVALUE";
        case OP_NEWCLASS:
            return "NEWCLASS";
        case OP_NEWMETHOD:
            return "NEWMETHOD";
        case OP_NEWARRAY:
            return "NEWARRAY";
        case OP_NEWMAP:
            return "NEWMAP";
        case OP_VARARG:
            return "VARARG";
        case OP_UNPACK:
            return "UNPACK";
        case OP_UNPACKEX:
            return "UNPACKEX";
        case OP_FORNUM0:
            return "FORNUM0";
        case OP_FORNUM:
            return "FORNUM";
        case OP_FORIN0:
            return "FORIN0";
        case OP_FORIN:
            return "FORIN";
        case OP_LEN:
            return "LEN";
        case OP_NEG:
            return "NEG";
        case OP_NOT:
            return "NOT";
        case OP_BNOT:
            return "BNOT";
        case OP_ADD:
            return "ADD";
        case OP_SUB:
            return "SUB";
        case OP_MUL:
            return "MUL";
        case OP_DIV:
            return "DIV";
        case OP_IDIV:
            return "IDIV";
        case OP_MOD:
            return "MOD";
        case OP_POW:
            return "POW";
        case OP_CONCAT:
            return "CONCAT";
        case OP_BXOR:
            return "BXOR";
        case OP_BAND:
            return "BAND";
        case OP_BOR:
            return "BOR";
        case OP_SHL:
            return "SHL";
        case OP_SHR:
            return "SHR";
        case OP_EQ:
            return "EQ";
        case OP_LT:
            return "LT";
        case OP_LE:
            return "LE";
        case OP_IN:
            return "IN";
        case OP_GETATTR:
            return "GETATTR";
        case OP_SETATTR:
            return "SETATTR";
        case OP_GETITEM:
            return "GETITEM";
        case OP_SETITEM:
            return "SETITEM";
        case OP_RADD:
            return "RADD";
        case OP_RSUB:
            return "RSUB";
        case OP_RMUL:
            return "RMUL";
        case OP_RDIV:
            return "RDIV";
        case OP_RIDIV:
            return "RIDIV";
        case OP_RMOD:
            return "RMOD";
        case OP_RPOW:
            return "RPOW";
        case OP_RCONCAT:
            return "RCONCAT";
        case OP_RBXOR:
            return "RBXOR";
        case OP_RBAND:
            return "RBAND";
        case OP_RBOR:
            return "RBOR";
        case OP_RSHL:
            return "RSHL";
        case OP_RSHR:
            return "RSHR";
        case OP_STR:
            return "STR";
        case OP_INT:
            return "INT";
        case OP_FLOAT:
            return "FLOAT";
        case OP_BOOL:
            return "BOOL";
        case OP_ARRAY:
            return "ARRAY";
        case OP_MAP:
            return "MAP";
        default:
            return "???";
    }
}

static paw_Bool meta_call(paw_Env *P, Op op, Value x, int argc)
{
    const Value *meta = pawT_get_meta(P, op, x);
    if (meta) {
        // Expect 'x', followed by 'argc' args, on top of the stack.
        pawC_call(P, *meta, argc);
        return PAW_BTRUE;
    }
    return PAW_BFALSE;
}

static paw_Bool meta_unop(paw_Env *P, Op op, Value x)
{
    const Value *meta = pawT_get_meta(P, op, x);
    if (meta) {
        vm_push(x);
        pawC_call(P, *meta, 0);
        return PAW_BTRUE;
    }
    return PAW_BFALSE;
}

static void maybe_meta_unop(paw_Env *P, Op op, Value x)
{
    if (meta_unop(P, op, x)) {
        vm_shift(1);
    }
}

static paw_Bool meta_binop_aux(paw_Env *P, Op op, Value x, Value y)
{
    paw_Bool swap = PAW_BFALSE;
    const Value *meta = pawT_get_meta(P, op, x);
    if (!meta && mm_has_r(op)) {
        // Check the reverse metamethod (i.e. y.__<binop>r(x)).
        meta = pawT_get_meta(P, mm_get_r(op), y);
        swap = PAW_BTRUE;
    }
    if (meta) {
        StackPtr sp = pawC_stkinc(P, 2);
        sp[0] = swap ? y : x;
        sp[1] = swap ? x : y;
        pawC_call(P, *meta, 1);
        return PAW_BTRUE;
    }
    return PAW_BFALSE;
}

static paw_Bool meta_binop(paw_Env *P, Op op, Value x, Value y)
{
    return meta_binop_aux(P, op, x, y);
}

static paw_Bool meta_contains(paw_Env *P, Op op, Value obj, Value key)
{
    // Operands will be swapped in meta_binop_aux. OP_IN does not have a
    // reverse metamethod, so only obj.__contains(key) is attempted.
    return meta_binop_aux(P, op, obj, key);
}

static inline paw_Bool meta_getter(paw_Env *P, Op op, Value obj, Value key)
{
    const Value *meta = pawT_get_meta(P, op, obj);
    if (meta) {
        StackPtr sp = pawC_stkinc(P, 2);
        sp[0] = obj;
        sp[1] = key;
        pawC_call(P, *meta, 1);
        return PAW_BTRUE;
    }
    return PAW_BFALSE;
}

static inline paw_Bool meta_setter(paw_Env *P, Op op, Value obj, Value key, Value val)
{
    const Value *meta = pawT_get_meta(P, op, obj);
    if (meta) {
        StackPtr sp = pawC_stkinc(P, 3);
        sp[0] = obj;
        sp[1] = key;
        sp[2] = val;
        pawC_call(P, *meta, 2);
        vm_pop(1); // unused return value
        return PAW_BTRUE;
    }
    return PAW_BFALSE;
}

static void float2integer(paw_Env *P, paw_Float f)
{
    if (pawV_float_fits_int(f)) {
        vm_pushi(f);
    } else {
        pawB_from_float(P, f);
    }
}

// Consume at most a single '-' at the start of the number
static paw_Bool consume_prefix(const char **str)
{
    while (ISSPACE(**str)) {
        ++*str;
    }
    if (**str == '-') {
        ++*str;
        return PAW_BTRUE;
    }
    return PAW_BFALSE;
}

void pawR_to_integer(paw_Env *P)
{
    StackPtr sp = vm_peek(0);
    if (pawV_is_int(*sp) || pawV_is_bigint(*sp)) {
        // NOOP
    } else if (pawV_is_float(*sp)) {
        const paw_Float f = pawV_get_float(*sp);
        float2integer(P, f);
        vm_shift(1);
    } else if (pawV_is_bool(*sp)) {
        pawV_set_int(sp, pawV_get_bool(*sp));
    } else if (pawV_is_string(*sp)) {
        // Copy into null-terminated buffer.
        Buffer buf;
        pawL_init_buffer(&buf);
        pawL_add_value(P, &buf, *sp);
        pawL_add_char(P, &buf, '\0');

        const char *str = buf.data;
        paw_Bool neg = consume_prefix(&str);
        // Use the stack slot containing the string. If the buffer needed a box,
        // it will be above this slot.
        if (pawV_parse_integer(P, str, sp)) {
            pawE_error(P, PAW_ESYNTAX, "invalid integer '%s'", buf.data);
        }
        pawL_discard_result(P, &buf);
        if (neg) {
            // Call the main negation routine, since the parsed integer may be
            // a bigint.
            pawR_neg(P);
        }
    } else if (meta_unop(P, OP_INT, *sp)) {
        vm_shift(1);
    } else {
        pawV_type_error(P, *sp);
    }
}

void pawR_to_float(paw_Env *P)
{
    StackPtr sp = vm_peek(0);
    if (pawV_is_float(*sp)) {
    } else if (pawV_is_int(*sp)) {
        pawV_set_float(sp, (paw_Float)pawV_get_int(*sp));
    } else if (pawV_is_bigint(*sp)) {
        pawV_set_float(sp, pawB_get_float(pawV_get_bigint(*sp)));
    } else if (pawV_is_bool(*sp)) {
        pawV_set_float(sp, pawV_get_bool(*sp));
    } else if (pawV_is_string(*sp)) {
        // Copy into null-terminated buffer.
        Buffer buf;
        pawL_init_buffer(&buf);
        pawL_add_value(P, &buf, *sp);
        pawL_add_char(P, &buf, '\0');

        const char *str = buf.data;
        paw_Bool neg = consume_prefix(&str);
        if (pawV_parse_float(str, sp)) {
            pawE_error(P, PAW_ESYNTAX, "invalid float '%s'", buf.data);
        }
        pawL_discard_result(P, &buf);
        if (neg) {
            const paw_Float f = pawV_get_float(*sp);
            pawV_set_float(sp, -f);
        }
    } else if (meta_unop(P, OP_FLOAT, *sp)) {
        vm_shift(1);
    } else {
        pawV_type_error(P, *sp);
    }
}

void pawR_to_string(paw_Env *P)
{
    const Value v = *vm_peek(0);
    if (!meta_unop(P, OP_STR, v)) {
        Buffer buf;
        pawL_init_buffer(&buf);
        pawL_add_value(P, &buf, *vm_peek(0));
        pawL_push_result(P, &buf);
    }
    vm_shift(1);
}

static void try_int(paw_Env *P, Value *pv)
{
    if (pawV_num2int(pv)) {
        pawV_type_error(P, *pv);
    }
}

static void try_float2(paw_Env *P, Value *pv, Value *pv2)
{
    if (pawV_num2float(pv) || pawV_num2float(pv2)) {
        pawV_type_error2(P, *pv, *pv2);
    }
}

static Map *attributes(paw_Env *P, const Value v)
{
    Map *attr = NULL;
    if (pawV_is_instance(v)) {
        attr = pawV_get_instance(v)->attr;
    } else if (pawV_is_userdata(v)) {
        attr = pawV_get_userdata(v)->attr;
    } else if (pawV_is_object(v)) {
        attr = P->attr[VOBJINDEX(pawV_get_type(v))];
    } else {
        pawV_type_error(P, v);
    }
    return attr;
}

// Return true if the multiplication will overflow, false otherwise
// See CERT C rule INT32-C.
static paw_Bool check_mul_overflow(paw_Int x, paw_Int y)
{
    if (x > 0) {
        if (y > 0) { // x > 0 && y > 0
            return x > VINT_MAX / y;
        } else { // x > 0 && y <= 0
            return y < VINT_MIN / x;
        }
    } else if (x < 0) {
        if (y > 0) { // x < 0 && y > 0
            return x < VINT_MIN / y;
        } else { // x < 0 && y <= 0
            return y < VINT_MAX / x;
        }
    }
    return PAW_BFALSE;
}

int pawR_read_global(paw_Env *P, Value name)
{
    assert(pawV_get_type(name) == VSTRING);
    Value *global = pawH_get(P, P->globals, name);
    if (global) {
        vm_push(*global);
        return 0;
    }
    return -1;
}

void pawR_write_global(paw_Env *P, Value name, paw_Bool create)
{
    assert(pawV_get_type(name) == VSTRING);
    const MapAction action = create ? MAP_ACTION_CREATE : MAP_ACTION_NONE;
    Value *global = pawH_action(P, P->globals, name, action);
    if (!global) {
        paw_assert(!create);               // MAP_ACTION_CREATE never returns NULL
        pawE_name(P, pawV_get_text(name)); // no return
    }
    *global = *vm_peek(0);
    vm_pop(1);
}

static UpValue *capture_upvalue(paw_Env *P, StackPtr local)
{
    UpValue *prev = NULL;
    UpValue *next = P->up_list;
    while (next && UPV_LEVEL(next) > local) {
        assert(UPV_IS_OPEN(next));
        prev = next;
        next = next->open.next;
    }

    if (next && UPV_LEVEL(next) == local) {
        return next;
    }

    UpValue *new_up = pawV_new_upvalue(P);
    pawV_link_upvalue(P, new_up, prev, next);
    new_up->p = local;
    return new_up;
}

void pawR_close_upvalues(paw_Env *P, const StackPtr top)
{
    while (P->up_list && UPV_LEVEL(P->up_list) >= top) {
        UpValue *up = P->up_list;
        assert(UPV_IS_OPEN(up));
        // Save before switching active union member (open -> closed).
        UpValue *next = up->open.next;
        up->closed = *up->p;
        up->p = &up->closed;
        P->up_list = next;
    }
}

int pawR_getitem_raw(paw_Env *P, paw_Bool has_fallback)
{
    const Value key = *vm_peek(0);
    const Value obj = *vm_peek(1);
    Value val = vnull();
    if (pawV_is_array(obj)) {
        const paw_Int idx = pawE_check_int(P, key);
        val = *pawA_get(P, pawV_get_array(obj), idx);
    } else if (pawV_is_map(obj)) {
        const Value *pval = pawH_get(P, pawV_get_map(obj), key);
        if (pval) {
            val = *pval;
        } else if (has_fallback) {
            val = *vm_peek(2);
        } else {
            return -1;
        }
    } else if (pawV_is_string(obj)) {
        paw_Int idx = pawE_check_int(P, key);
        String *str = pawV_get_string(obj);
        if (idx < 0) {
            idx += paw_cast_int(str->length);
        }
        if (idx >= paw_cast_int(str->length)) {
            pawE_range(P, "string index");
        }
        const char c = str->text[idx];
        String *res = pawS_new_nstr(P, &c, 1);
        pawV_set_string(&val, res);
    } else {
        pawV_type_error(P, obj);
    }
    vm_push(val);
    vm_shift(2 + has_fallback);
    return 0;
}

int pawR_getattr_raw(paw_Env *P, paw_Bool has_fallback)
{
    const Value obj = *vm_peek(1);
    const Value key = *vm_peek(0);
    paw_assert(pawV_is_string(key));

    Map *attr = attributes(P, obj);
    const Value *pval = pawH_get(P, attr, key);
    if (pval) {
        // found attribute in map
    } else if (has_fallback) {
        pval = vm_peek(2);
    } else {
        return -1;
    }
    vm_push(*pval); // anchor
    if (paw_is_function(P, -1)) {
        // bind method to context ('self') variable
        Method *mtd = pawV_new_method(P, obj, *pval);
        pawV_set_method(&P->top[-1], mtd); // replace
    }
    vm_shift(2 + has_fallback);
    return 0;
}

void pawR_setattr_raw(paw_Env *P)
{
    const Value val = *vm_peek(0);
    const Value key = *vm_peek(1);
    const Value obj = *vm_peek(2);
    paw_assert(pawV_get_type(key) == VSTRING);

    Map *attr = NULL;
    if (pawV_is_instance(obj)) {
        attr = pawV_get_instance(obj)->attr;
    } else if (pawV_is_userdata(obj)) {
        attr = pawV_get_userdata(obj)->attr;
    } else {
        pawV_type_error(P, obj);
    }
    pawH_insert(P, attr, key, val);
    vm_pop(3);
}

void pawR_setattr(paw_Env *P)
{
    const Value val = *vm_peek(0);
    const Value key = *vm_peek(1);
    const Value obj = *vm_peek(2);
    paw_assert(pawV_get_type(key) == VSTRING);

    if (!meta_setter(P, OP_SETATTR, obj, key, val)) {
        // Don't call pawR_setattr_raw(), since that function is allowed to set
        // attributes on values of type 'userdata': something that should only
        // happen from C, not paw. pawR_setattr_raw is called by C API functions
        // and the 'setattr' builtin function, while this function is called by
        // the paw runtime.
        if (pawV_is_instance(obj)) {
            Instance *ins = pawV_get_instance(obj);
            pawH_insert(P, ins->attr, key, val);
        } else {
            pawV_type_error(P, obj);
        }
    }
    vm_pop(3);
}

void pawR_setitem_raw(paw_Env *P)
{
    const Value val = *vm_peek(0);
    const Value key = *vm_peek(1);
    const Value obj = *vm_peek(2);
    if (pawV_is_array(obj)) {
        const paw_Int idx = pawE_check_int(P, key);
        Value *slot = pawA_get(P, pawV_get_array(obj), idx);
        *slot = val;
    } else if (pawV_is_map(obj)) {
        pawH_insert(P, pawV_get_map(obj), key, val);
    } else {
        pawV_type_error(P, obj);
    }
    vm_pop(3);
}

void pawR_setitem(paw_Env *P)
{
    const Value val = *vm_peek(0);
    const Value key = *vm_peek(1);
    const Value obj = *vm_peek(2);
    if (meta_setter(P, OP_SETITEM, obj, key, val)) {
        vm_pop(3);
    } else {
        pawR_setitem_raw(P);
    }
}

static CallFrame *invoke_from_class(paw_Env *P, Class *cls, Value name, int argc)
{
    Value *base = vm_peek(argc);
    const Value *method = pawH_get(P, cls->attr, name);
    if (!method) {
        pawE_attr(P, pawV_get_text(name));
    }
    // The receiver instance + parameters are on top of the stack.
    return pawC_precall(P, base, *method, argc);
}

static CallFrame *invoke(paw_Env *P, Value name, int argc)
{
    Value *base = vm_peek(argc);
    Map *attr = attributes(P, *base);
    const Value *method = pawH_get(P, attr, name);
    if (method) {
        return pawC_precall(P, base, *method, argc);
    } else if (!pawV_is_instance(*base)) {
        pawE_attr(P, pawV_get_text(name));
    }
    Instance *obj = pawV_get_instance(*base);
    return invoke_from_class(P, obj->self, name, argc);
}

#define stop_loop(i, i2, d) (((d) < 0 && (i) <= (i2)) || \
                             ((d) > 0 && (i) >= (i2)))

static paw_Bool fornum_init_aux(paw_Env *P, Value a, Value b, Value c)
{
    try_int(P, &a);
    try_int(P, &b);
    try_int(P, &c);
    const paw_Int begin = pawV_get_int(a);
    const paw_Int end = pawV_get_int(b);
    const paw_Int step = pawV_get_int(c);
    if (step == 0) {
        pawE_error(P, PAW_ERUNTIME, "loop step equals 0");
    }
    const paw_Bool skip = stop_loop(begin, end, step);
    if (!skip) {
        pawV_set_int(vm_peek(2), begin);
        pawV_set_int(vm_peek(1), end);
        pawV_set_int(vm_peek(0), step);
        vm_pushi(begin);
    }
    return skip;
}

static paw_Bool fornum_init(paw_Env *P)
{
    const Value step = *vm_peek(0);
    const Value end = *vm_peek(1);
    const Value begin = *vm_peek(2);
    // only VINT is supported
    if (!pawV_is_int(begin) || !pawV_is_int(end) || !pawV_is_int(step)) {
        pawE_error(P, PAW_ETYPE, "expected integer loop bounds");
    }
    return fornum_init_aux(P, begin, end, step);
}

static paw_Bool fornum(paw_Env *P)
{
    const paw_Int step = pawV_get_int(*vm_peek(0));
    const paw_Int end = pawV_get_int(*vm_peek(1));
    const paw_Int itr = pawV_get_int(*vm_peek(2));
    const paw_Int next = itr + step;
    if (stop_loop(next, end, step)) {
        return PAW_BFALSE;
    }
    *vm_peek(2) = vint(next);
    vm_pushi(next);
    return PAW_BTRUE;
}

static paw_Bool meta_forin_init(paw_Env *P, Value v)
{
    ptrdiff_t save = pawC_stksave(P, P->top);
    if (meta_unop(P, OP_LEN, v)) {
        if (paw_int(P, -1) > 0) {
            vm_pop(1); // pop length
            // Attempt to get the first item. If 'v' has __getitem, then
            // we can iterate over it.
            if (meta_getter(P, OP_GETITEM, v, vint(0))) {
                // Set up the stack to look like '..., 0, item'.
                vm_pushi(0);
                paw_rotate(P, -2, 1);
                return PAW_BFALSE;
            }
        }
    }
    P->top = pawC_stkload(P, save);
    return PAW_BTRUE; // skip the loop
}

static paw_Bool forin_init(paw_Env *P)
{
    const Value v = *vm_peek(0);
    paw_Int itr = PAW_ITER_INIT;
    if (pawV_is_array(v)) {
        Array *arr = pawV_get_array(v);
        if (pawA_iter(arr, &itr)) {
            vm_pushi(itr);
            vm_push(arr->begin[itr]);
            return PAW_BFALSE;
        }
    } else if (pawV_is_map(v)) {
        Map *map = pawV_get_map(v);
        if (pawH_iter(map, &itr)) {
            vm_pushi(itr);
            vm_push(map->keys[itr]);
            return PAW_BFALSE;
        }
    } else if (has_meta(v)) {
        return meta_forin_init(P, v);
    } else {
        pawV_type_error(P, v);
    }
    return PAW_BTRUE;
}

static paw_Bool meta_forin(paw_Env *P, Value v, paw_Int itr)
{
    // Push the length
    meta_unop(P, OP_LEN, v);

    ++itr;
    const paw_Int len = paw_int(P, -1);
    vm_pop(1);

    if (itr < len) {
        // Write the next iterator value and set up the stack for __getitem. 'v'
        // replaces the length, and the iterator value gets pushed.
        Value *pitr = vm_peek(0);
        pawV_set_int(pitr, itr);

        meta_getter(P, OP_GETITEM, v, vint(itr));
        return PAW_BTRUE;
    }
    return PAW_BFALSE; // Finish the loop
}

static paw_Bool forin(paw_Env *P)
{
    const Value obj = *vm_peek(1);
    const Value itr = *vm_peek(0);
    if (pawV_is_array(obj)) {
        Array *arr = pawV_get_array(obj);
        paw_Int i = pawV_get_int(itr);
        if (pawA_iter(arr, &i)) {
            *vm_peek(0) = vint(i);
            vm_push(arr->begin[i]);
            return PAW_BTRUE;
        }
    } else if (pawV_is_map(obj)) {
        Map *map = pawV_get_map(obj);
        paw_Int i = pawV_get_int(itr);
        if (pawH_iter(map, &i)) {
            *vm_peek(0) = vint(i);
            vm_push(map->keys[i]);
            return PAW_BTRUE;
        }
    } else if (has_meta(obj)) {
        return meta_forin(P, obj, pawV_get_int(itr));
    } else {
        pawV_type_error(P, obj);
    }
    return PAW_BFALSE; // Stop the loop
}

static void int_arith(paw_Env *P, Op op, Value lhs, Value rhs);

static void float_arith(paw_Env *P, Op op, Value lhs, Value rhs)
{
    const paw_Float x = pawV_get_float(lhs);
    const paw_Float y = pawV_get_float(rhs);
    paw_Float z = 0.0; // Result
    switch (op) {
        case OP_ADD:
            z = x + y;
            break;
        case OP_SUB:
            z = x - y;
            break;
        case OP_MUL:
            z = x * y;
            break;
        case OP_DIV:
        case OP_MOD:
            if (y == 0) {
                pawE_error(P, PAW_ERUNTIME, "divide by 0");
            } else if (op == OP_DIV) {
                z = x / y;
            } else {
                z = fmod(x, y);
            }
            break;
        default: {
            paw_assert(op == OP_IDIV);
            if (y == 0) {
                pawE_error(P, PAW_ERUNTIME, "divide by 0");
            }
            const paw_Float f = x / y;
            float2integer(P, f);
            return;
        }
    }
    vm_pushf(z);
}

static paw_Bool mul_overflow(paw_Env *P, paw_Int x, paw_Int y)
{
    if (check_mul_overflow(x, y)) {
        // Add a copy of 'y' as a BigInt above 'x' on the stack. OP_MUL is
        // commutative, so just do 'y * x' and fix the stack after.
        StackPtr sp = pawC_stkinc(P, 1);
        pawB_from_int(P, sp, y);
        pawB_arith(P, OP_MUL, *sp, vint(x));
        vm_shift(1);
        return PAW_BTRUE;
    }
    return PAW_BFALSE;
}

static void int_arith(paw_Env *P, Op op, Value lhs, Value rhs)
{
    paw_Int x = pawV_get_int(lhs);
    paw_Int y = pawV_get_int(rhs);
    paw_Int z = 0; // Result
    switch (op) {
        // Addition and subtraction will never overflow the native type used
        // for a paw_Int, but may result in a value smaller than VINT_MIN
        // or larger than VINT_MAX. Out-of-range values are converted to
        // BigInt below.
        case OP_ADD:
            z = x + y;
            break;
        case OP_SUB:
            z = x - y;
            break;
        case OP_MUL:
            if (mul_overflow(P, x, y)) {
                return;
            } else {
                z = x * y;
            }
            break;
        case OP_IDIV:
        case OP_MOD:
            if (y == 0) {
                pawE_error(P, PAW_ERUNTIME, "divide by 0");
            } else if (op == OP_IDIV) {
                z = x / y;
            } else {
                z = x % y;
            }
            break;
        default:
            paw_assert(op == OP_DIV);
            vm_setf(&lhs, (paw_Float)x);
            vm_setf(&rhs, (paw_Float)y);
            float_arith(P, op, lhs, rhs);
            return;
    }
    StackPtr sp = pawC_stkinc(P, 1);
    if (pawV_int_fits_int(z)) {
        pawV_set_int(sp, z);
    } else {
        pawB_from_int(P, sp, z);
    }
}

static void string_arith(paw_Env *P, Op op, Value lhs, Value rhs)
{
    if (op == OP_ADD) {
        if (pawV_is_string(lhs) && pawV_is_string(rhs)) {
            Buffer print;
            pawL_init_buffer(&print);
            String *x = pawV_get_string(lhs);
            String *y = pawV_get_string(rhs);
            pawL_add_nstring(P, &print, x->text, x->length);
            pawL_add_nstring(P, &print, y->text, y->length);
            pawL_push_result(P, &print); // Push
            return;
        }
    } else if (op == OP_MUL) {
        String *s;
        paw_Int n;
        if (pawV_is_string(lhs)) {
            s = pawV_get_string(lhs);
            n = pawE_check_int(P, rhs);
        } else {
            n = pawE_check_int(P, lhs);
            s = pawV_get_string(rhs);
        }
        Buffer print;
        pawL_init_buffer(&print);
        for (paw_Int i = 0; i < n; ++i) {
            pawL_add_nstring(P, &print, s->text, s->length);
        }
        pawL_push_result(P, &print); // Push
        return;
    }
    pawV_type_error2(P, lhs, rhs);
}

static void int_rel(paw_Env *P, Op op, paw_Int x, paw_Int y)
{
    if (op == OP_LT) {
        vm_pushb(x < y);
    } else { // op == OP_LE
        vm_pushb(x <= y);
    }
}

static void float_rel(paw_Env *P, Op op, paw_Float x, paw_Float y)
{
    if (op == OP_LT) {
        vm_pushb(x < y);
    } else { // op == OP_LE
        vm_pushb(x <= y);
    }
}

static void string_rel(paw_Env *P, Op op, String *x, String *y)
{
    if (op == OP_LT) {
        vm_pushb(pawS_cmp(x, y) < 0);
    } else { // op == OP_LE
        vm_pushb(pawS_cmp(x, y) <= 0);
    }
}

static Value fetch(paw_Env *P, int i)
{
    Value *pv = vm_peek(i);
    if (pawV_is_bool(*pv)) {
        pawV_set_int(pv, pawV_get_bool(*pv));
    }
    return *pv;
}

static void rel_aux(paw_Env *P, Op op)
{
    Value y = fetch(P, 0);
    Value x = fetch(P, 1);
    if (pawV_is_int(x) && pawV_is_int(y)) {
        int_rel(P, op, pawV_get_int(x), pawV_get_int(y));
    } else if (pawV_is_float(x) && pawV_is_float(y)) {
        float_rel(P, op, pawV_get_float(x), pawV_get_float(y));
    } else if (pawV_is_string(x) && pawV_is_string(y)) {
        string_rel(P, op, pawV_get_string(x), pawV_get_string(y));
    } else if (meta_binop(P, op, x, y)) {
        // called metamethod on either 'x' or 'y'
    } else if (pawV_is_float(x) || pawV_is_float(y)) {
        try_float2(P, &x, &y);
        float_rel(P, op, pawV_get_float(x), pawV_get_float(y));
    } else if (pawV_is_bigint(x) || pawV_is_bigint(y)) {
        pawB_rel(P, op, x, y);
    } else {
        pawV_type_error2(P, x, y);
    }
    vm_shift(2);
}

static void int_unop(paw_Env *P, Op op, paw_Int i)
{
    switch (op) {
        case OP_NEG:
            if (i == VINT_MIN) {
                // The expression '-VINT_MIN' will overflow. Convert to VBIGINT.
                StackPtr sp = pawC_stkinc(P, 1);
                BigInt *bi = pawB_from_int(P, sp, i);
                pawB_unop(P, OP_NEG, obj2v(bi));
                vm_shift(1);
                break;
            }
            vm_pushi(-i);
            break;
        case OP_NOT:
            vm_pushi(!i);
            break;
        default:
            paw_assert(op == OP_BNOT);
            vm_pushi(~i);
    }
}

static void float_unop(paw_Env *P, Op op, paw_Float f)
{
    switch (op) {
        case OP_NEG:
            vm_pushf(-f);
            break;
        case OP_NOT:
            vm_pushf(!f);
            break;
        default:
            paw_assert(op == OP_BNOT);
            pawE_error(P, PAW_ETYPE, "'~' on 'float'");
    }
}

static void unop_aux(paw_Env *P, Op op)
{
    Value x = fetch(P, 0);
    if (pawV_is_int(x)) {
        const paw_Int i = pawV_get_int(x);
        int_unop(P, op, i);
    } else if (pawV_is_float(x)) {
        const paw_Float f = pawV_get_float(x);
        float_unop(P, op, f);
    } else if (pawV_is_bigint(x)) {
        pawB_unop(P, op, x);
    } else if (meta_unop(P, op, x)) {
        // called metamethod on 'x'
    } else if (op == OP_NOT) {
        // allows expressions like '!str'
        vm_pushb(!pawV_truthy(x));
    } else {
        pawV_type_error(P, x);
    }
    vm_shift(1);
}

static void arith_aux(paw_Env *P, Op op)
{
    Value y = fetch(P, 0);
    Value x = fetch(P, 1);
    if (pawV_is_int(x) && pawV_is_int(y)) {
        int_arith(P, op, x, y);
    } else if (pawV_is_float(x) && pawV_is_float(y)) {
        float_arith(P, op, x, y);
    } else if (meta_binop(P, op, x, y)) {
        // called metamethod on either 'x' or 'y'
    } else if (pawV_is_string(x) || pawV_is_string(y)) {
        string_arith(P, op, x, y);
    } else if (pawV_is_float(x) || pawV_is_float(y)) {
        try_float2(P, &x, &y);
        float_arith(P, op, x, y);
    } else if (pawV_is_bigint(x) || pawV_is_bigint(y)) {
        pawB_arith(P, op, x, y);
    } else {
        pawV_type_error2(P, x, y);
    }
    vm_shift(2);
}

static void int_bitwise(paw_Env *P, Op op, Value lhs, Value rhs)
{
    paw_Int x = pawV_get_int(lhs);
    paw_Int y = pawV_get_int(rhs);
    paw_Int z = 0;

    switch (op) {
        case OP_BAND:
            z = x & y;
            break;
        case OP_BOR:
            z = x | y;
            break;
        case OP_BXOR:
            z = x ^ y;
            break;
        case OP_SHL:
            if (y < 0) {
                pawE_range(P, "negative shift count");
            } else if (y == 0) {
                z = x; // NOOP
            } else if (cast_size(y) >= VINT_WIDTH ||
                       x > (VINT_MAX >> y) ||
                       x < (VINT_MIN >> y)) {
                // Shift left will overflow. Use a BigInt.
                pawB_bitwise(P, OP_SHL, lhs, rhs);
                return;
            } else {
                z = paw_cast_int((uint64_t)x << y);
            }
            break;
        default:
            paw_assert(op == OP_SHR);
            if (y < 0) {
                pawE_range(P, "negative shift count");
            } else if (y == 0) {
                z = x; // NOOP
            } else {
                // Right shift by >= width of 'x' is UB in C. Clamp the
                // shift count. If 'x' < 0, then the results of the
                // shift are implementation-defined (may or may not
                // preserve the sign).
                y = paw_min(y, (int)(sizeof(x) * 8 - 1));
                z = x >> y;
            }
            break;
    }
    vm_pushi(z);
}

static void bitwise_aux(paw_Env *P, Op op)
{
    Value y = fetch(P, 0);
    Value x = fetch(P, 1);
    if (meta_binop(P, op, x, y)) {
        // called metamethod on either 'x' or 'y'
    } else if (pawV_is_int(x) && pawV_is_int(y)) {
        int_bitwise(P, op, x, y);
    } else if ((pawV_is_bigint(x) || pawV_is_int(x)) &&
               (pawV_is_bigint(y) || pawV_is_int(y))) {
        pawB_bitwise(P, op, x, y);
    } else {
        pawV_type_error2(P, x, y);
    }
    vm_shift(2);
}

void pawR_len(paw_Env *P)
{
    size_t len = 0;
    Value *pv = vm_peek(0);
    if (pawV_is_string(*pv)) {
        len = pawS_length(pawV_get_string(*pv));
    } else if (pawV_is_array(*pv)) {
        len = pawA_length(pawV_get_array(*pv));
    } else if (pawV_is_map(*pv)) {
        len = pawH_length(pawV_get_map(*pv));
    } else if (meta_unop(P, OP_LEN, *pv)) {
        // Special case: __len may not return an integer.
        vm_shift(1);
        return;
    } else if (pawV_is_userdata(*pv)) {
        len = pawV_get_userdata(*pv)->size;
    } else {
        pawV_type_error(P, *pv);
    }
    // Replace the container with its length.
    paw_assert(len <= VINT_MAX);
    pawV_set_int(pv, (paw_Int)len);
}

#define GEN_UNOP(a, b)        \
    void pawR_##a(paw_Env *P) \
    {                         \
        unop_aux(P, OP_##b);  \
    }
GEN_UNOP(neg, NEG)
GEN_UNOP(not, NOT)
GEN_UNOP(bnot, BNOT)

#define GEN_ARITH(a, b)       \
    void pawR_##a(paw_Env *P) \
    {                         \
        arith_aux(P, OP_##b); \
    }
GEN_ARITH(add, ADD)
GEN_ARITH(sub, SUB)
GEN_ARITH(mul, MUL)
GEN_ARITH(div, DIV)
GEN_ARITH(idiv, IDIV)
GEN_ARITH(mod, MOD)

#define GEN_BITWISE(a, b)       \
    void pawR_##a(paw_Env *P)   \
    {                           \
        bitwise_aux(P, OP_##b); \
    }
GEN_BITWISE(shl, SHL)
GEN_BITWISE(shr, SHR)
GEN_BITWISE(band, BAND)
GEN_BITWISE(bor, BOR)
GEN_BITWISE(bxor, BXOR)

#define GEN_REL(a, b)         \
    void pawR_##a(paw_Env *P) \
    {                         \
        rel_aux(P, OP_##b);   \
    }
GEN_REL(lt, LT)
GEN_REL(le, LE)

static int ensure_str(paw_Env *P, Value *pv)
{
    if (!pawV_is_string(*pv)) {
        vm_push(*pv);
        pawR_to_string(P);
        *pv = *vm_peek(0);
        vm_pop(1);
    }
    return 0;
}

void pawR_concat(paw_Env *P)
{
    Value y = *vm_peek(0);
    Value x = *vm_peek(1);
    if (!meta_binop(P, OP_CONCAT, x, y)) {
        if (ensure_str(P, &x) || ensure_str(P, &y)) {
            // Either 'x' or 'y' is not a string and has no '__str' metamethod.
            pawV_type_error2(P, x, y);
        }
        String *s = pawV_get_string(x);
        String *t = pawV_get_string(y);

        Buffer print;
        pawL_init_buffer(&print);
        pawL_add_nstring(P, &print, s->text, s->length);
        pawL_add_nstring(P, &print, t->text, t->length);
        pawL_push_result(P, &print); // Push
    }
    vm_shift(2);
}

void pawR_in(paw_Env *P)
{
    const Value cnt = *vm_peek(0);
    const Value key = *vm_peek(1);
    if (pawV_is_map(cnt)) {
        vm_pushb(pawH_contains(P, pawV_get_map(cnt), key));
    } else if (pawV_is_array(cnt)) {
        vm_pushb(pawA_contains(P, pawV_get_array(cnt), key));
    } else if (meta_contains(P, OP_IN, cnt, key)) {
        // called 'cnt.__contains(key)'
    } else {
        pawV_type_error(P, cnt);
    }
    vm_shift(2);
}

void pawR_eq(paw_Env *P)
{
    const Value y = *vm_peek(0);
    const Value x = *vm_peek(1);
    if (!meta_binop(P, OP_EQ, x, y)) {
        paw_Bool eq;
        if (pawV_is_array(x) && pawV_is_array(y)) {
            eq = pawA_equals(P, pawV_get_array(x), pawV_get_array(y));
        } else if (pawV_is_map(x) && pawV_is_map(y)) {
            eq = pawH_equals(P, pawV_get_map(x), pawV_get_map(y));
        } else {
            eq = pawV_equal(x, y);
        }
        vm_pushb(eq);
    }
    vm_shift(2);
}

void pawR_ne(paw_Env *P)
{
    pawR_eq(P);
    pawR_not(P);
}

void pawR_gt(paw_Env *P)
{
    pawR_le(P);
    pawR_not(P);
}

void pawR_ge(paw_Env *P)
{
    pawR_lt(P);
    pawR_not(P);
}

static int getattr_aux(paw_Env *P)
{
    const Value obj = *vm_peek(1);
    const Value key = *vm_peek(0);
    if (meta_getter(P, OP_GETATTR, obj, key)) {
        vm_shift(2);
    } else if (pawR_getattr_raw(P, PAW_BFALSE)) {
        return -1;
    }
    return 0;
}

static int getitem_aux(paw_Env *P)
{
    const Value obj = *vm_peek(1);
    const Value key = *vm_peek(0);
    if (meta_getter(P, OP_GETITEM, obj, key)) {
        vm_shift(2);
    } else if (pawR_getitem_raw(P, PAW_BFALSE)) {
        return -1;
    }
    return 0;
}

void pawR_getattr(paw_Env *P)
{
    if (getattr_aux(P)) {
        pawE_attr(P, paw_string(P, -1));
    }
}

void pawR_getitem(paw_Env *P)
{
    if (getitem_aux(P)) {
        pawE_key(P, paw_string(P, -1));
    }
}

void pawR_literal_array(paw_Env *P, int n)
{
    StackPtr sp = pawC_stkinc(P, 1);
    Array *a = pawA_new(P);
    pawV_set_array(sp, a);
    if (n > 0) {
        pawA_resize(P, a, cast_size(n));
        for (int i = 1; i <= n; ++i) {
            *pawA_get(P, a, -i) = *--sp;
        }
        // Replace contents with array itself.
        vm_shift(n);
    }
}

void pawR_literal_map(paw_Env *P, int n)
{
    StackPtr sp = pawC_stkinc(P, 1);
    Map *m = pawH_new(P);
    pawV_set_map(sp, m);
    if (n > 0) {
        for (int i = 0; i < n; ++i) {
            const Value value = *--sp;
            pawH_insert(P, m, *--sp, value);
        }
        // Replace contents with map itself.
        vm_shift(2 * n);
    }
}

void pawR_execute(paw_Env *P, CallFrame *cf)
{
    const OpCode *pc;
    Closure *fn;

top:
    pc = cf->pc;
    fn = cf->fn;

    for (;;) {
        vm_switch(*pc++)
        {
            vm_case(POP) :
            {
                vm_pop(1);
            }

            vm_case(PUSHNULL) :
            {
                vm_push0();
            }

            vm_case(PUSHTRUE) :
            {
                vm_pushb(PAW_BTRUE);
            }

            vm_case(PUSHFALSE) :
            {
                vm_pushb(PAW_BFALSE);
            }

            vm_case(PUSHCONST) :
            {
                vm_push(vm_const());
            }

            vm_case(LEN) :
            {
                pawR_len(P);
            }

            vm_case(NEG) :
            {
                pawR_neg(P);
            }

            vm_case(NOT) :
            {
                pawR_not(P);
            }

            vm_case(BNOT) :
            {
                pawR_bnot(P);
            }

            vm_case(SHL) :
            {
                pawR_shl(P);
            }

            vm_case(SHR) :
            {
                pawR_shr(P);
            }

            vm_case(BAND) :
            {
                pawR_band(P);
            }

            vm_case(BOR) :
            {
                pawR_bor(P);
            }

            vm_case(BXOR) :
            {
                pawR_bxor(P);
            }

            vm_case(ADD) :
            {
                pawR_add(P);
            }

            vm_case(SUB) :
            {
                pawR_sub(P);
            }

            vm_case(MUL) :
            {
                pawR_mul(P);
            }

            vm_case(DIV) :
            {
                pawR_div(P);
            }

            vm_case(IDIV) :
            {
                pawR_idiv(P);
            }

            vm_case(MOD) :
            {
                pawR_mod(P);
            }

            vm_case(CONCAT) :
            {
                pawR_concat(P);
            }

            vm_case(EQ) :
            {
                pawR_eq(P);
            }

            vm_case(LT) :
            {
                pawR_lt(P);
            }

            vm_case(LE) :
            {
                pawR_le(P);
            }

            vm_case(IN) :
            {
                pawR_in(P);
            }

            vm_case(NEWARRAY) :
            {
                const int n = Iw();
                pawR_literal_array(P, n);
            }

            vm_case(NEWMAP) :
            {
                const int n = Iw();
                pawR_literal_map(P, n);
            }

            vm_case(NEWCLASS) :
            {
                const Value name = vm_const();
                Class *cls = pawV_push_class(P);
                cls->name = pawV_get_string(name);
            }

            vm_case(NEWMETHOD) :
            {
                const Value name = vm_const();
                const Value method = *vm_peek(0);
                const Value object = *vm_peek(1);
                Class *cls = pawV_get_class(object);
                paw_assert(pawV_is_string(name));
                pawH_insert(P, cls->attr, name, method);
                vm_pop(1); // Pop closure, leave class
            }

            vm_case(INHERIT) :
            {
                const Value parent = *vm_peek(1);
                if (!pawV_is_class(parent)) {
                    pawE_error(P, PAW_ETYPE, "superclass is not of 'class' type");
                }
                Class *sub = pawV_get_class(*vm_peek(0));
                Class *super = pawV_get_class(parent);
                if (sub == super) {
                    pawE_error(P, PAW_EVALUE, "inherit from self");
                }
                pawH_extend(P, sub->attr, super->attr);

                // Ensure that __init is not inherited. Note that 'sub' has not had any
                // of its own attributes added to it yet.
                const Value key = pawE_cstr(P, CSTR_INIT);
                pawH_action(P, sub->attr, key, MAP_ACTION_REMOVE);
                vm_pop(1); // Pop 'sub'
            }

            vm_case(GETSUPER) :
            {
                // Attributes on 'super' can only refer to methods, not data fields
                const Value super = *vm_peek(0);
                const Value self = *vm_peek(1);
                const Value name = vm_const();
                vm_pop(1); // Pop 'parent'

                Map *attr = attributes(P, super);
                Value *value = pawH_get(P, attr, name);
                if (!value) {
                    pawE_attr(P, pawV_get_text(name));
                }

                StackPtr sp = pawC_stkinc(P, 1);
                Method *mtd = pawV_new_method(P, self, *value);
                pawV_set_method(sp, mtd);
            }

            vm_case(INVOKESUPER) :
            {
                const Value parent = *vm_peek(0);
                const Value name = vm_const();
                const int argc = Ib();
                vm_pop(1); // pop 'parent'
                vm_save();

                Class *super = pawV_get_class(parent);
                CallFrame *callee = invoke_from_class(P, super, name, argc);
                if (callee) {
                    cf = callee;
                    goto top;
                }
            }

            vm_case(GETLOCAL) :
            {
                const Value local = *vm_local();
                vm_push(local);
            }

            vm_case(SETLOCAL) :
            {
                Value *plocal = vm_local();
                *plocal = *vm_peek(0);
                vm_pop(1);
            }

            vm_case(GETUPVALUE) :
            {
                const Value upval = *vm_upvalue();
                vm_push(upval);
            }

            vm_case(SETUPVALUE) :
            {
                Value *pupval = vm_upvalue();
                *pupval = *vm_peek(0);
                vm_pop(1);
            }

            vm_case(GETGLOBAL) :
            {
                const Value name = vm_const();
                if (pawR_read_global(P, name)) {
                    pawE_name(P, pawV_get_text(name));
                }
            }

            vm_case(SETGLOBAL) :
            {
                const Value name = vm_const();
                // Error if 'name' does not exist
                pawR_write_global(P, name, PAW_BFALSE);
            }

            vm_case(GLOBAL) :
            {
                const Value name = vm_const();
                pawR_write_global(P, name, PAW_BTRUE);
            }

            vm_case(GETATTR) :
            {
                pawR_getattr(P);
            }

            vm_case(SETATTR) :
            {
                pawR_setattr(P);
            }

            vm_case(GETITEM) :
            {
                pawR_getitem(P);
            }

            vm_case(SETITEM) :
            {
                pawR_setitem(P);
            }

            vm_case(CLOSE) :
            {
                pawR_close_upvalues(P, vm_peek(1));
                vm_pop(1);
            }

            vm_case(CLOSURE) :
            {
                StackPtr sp = pawC_stkinc(P, 1);
                Proto *proto = fn->p->p[Iw()];
                Closure *closure = pawV_new_closure(P, proto->nup);
                pawV_set_closure(sp, closure);
                closure->p = proto;

                // open upvalues
                for (int i = 0; i < closure->nup; ++i) {
                    const int tag = Iw();
                    const int index = tag & UPVALUE_MAX;
                    closure->up[i] = tag & UPVALUE_LOCAL
                                         ? capture_upvalue(P, cf->base + index)
                                         : fn->up[index];
                }
            }

            vm_case(INVOKE) :
            {
                const Value name = vm_const();
                const uint8_t argc = Ib();
                vm_save();

                CallFrame *callee = invoke(P, name, argc);
                if (callee) {
                    cf = callee;
                    goto top;
                }
            }

            vm_case(CALL) :
            {
                const uint8_t argc = Ib();
                StackPtr ptr = vm_peek(argc);
                if (pawV_is_class(*ptr)) {
                    // No metamethods on Class
                } else if (meta_call(P, OP_CALL, *ptr, argc)) {
                    vm_continue; // Called __call metamethod
                }
                vm_save();

                CallFrame *callee = pawC_precall(P, ptr, *ptr, argc);
                if (callee) {
                    cf = callee;
                    goto top;
                }
            }

            vm_case(RETURN) :
            {
                const Value result = *vm_peek(0);
                vm_pop(1);

                P->top = CF_STACK_RETURN(cf);
                vm_save();

                pawR_close_upvalues(P, vm_peek(0));
                vm_push(result);
                P->cf = cf->prev;
                if (IS_ENTRY(cf)) {
                    return;
                }
                cf = P->cf;
                goto top;
            }

            vm_case(VARARG) :
            {
                // Must be run immediately after OP_CALL
                const int nexpect = Ib();
                const int nactual = vm_argc();
                const int nextra = nactual - nexpect;
                StackPtr sp = pawC_stkinc(P, 1);
                Array *argv = pawA_new(P);
                pawV_set_array(sp, argv);
                if (nextra) {
                    pawA_resize(P, argv, cast_size(nextra));
                    StackPtr base = cf->base + 1 + nexpect;
                    for (int i = 0; i < nextra; ++i) {
                        argv->begin[i] = base[i];
                    }
                    // Replace first variadic parameter with 'argv' array
                    vm_shift(nextra);
                }
            }

            vm_case(JUMP) :
            {
                const int offset = vm_jmp();
                pc += offset;
            }

            vm_case(JUMPNULL) :
            {
                const int offset = vm_jmp();
                const StackPtr sp = vm_peek(0);
                maybe_meta_unop(P, OP_NULL, *sp); // result in 'sp'
                if (pawV_is_null(*sp)) {
                    pc += offset;
                }
            }

            vm_case(JUMPFALSE) :
            {
                const int offset = vm_jmp();
                const StackPtr sp = vm_peek(0);
                maybe_meta_unop(P, OP_BOOL, *sp); // result in 'sp'
                if (!pawV_truthy(*sp)) {
                    pc += offset;
                }
            }

            vm_case(FORNUM0) :
            {
                const int offset = vm_jmp();
                if (fornum_init(P)) {
                    pc += offset; // skip
                }
            }

            vm_case(FORNUM) :
            {
                const int offset = vm_jmp();
                if (fornum(P)) {
                    pc += offset; // continue
                }
            }

            vm_case(FORIN0) :
            {
                const int offset = vm_jmp();
                if (forin_init(P)) {
                    // Skip the loop. We need to add a dummy value to the stack,
                    // since there was an 'OP_POP' generated to pop it. See
                    // forin() in parse.c for details.
                    vm_push0();
                    pc += offset;
                }
            }

            vm_case(FORIN) :
            {
                const int offset = vm_jmp();
                if (forin(P)) {
                    // Jump to start
                    pc += offset;
                }
            }

        vm_default : {
            paw_assert(PAW_BFALSE);
        }
        }
    }
}
