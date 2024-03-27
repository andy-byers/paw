// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#include "prefix.h"

#include "array.h"
#include "auxlib.h"
#include "bigint.h"
#include "call.h"
#include "env.h"
#include "gc.h"
#include "lex.h"
#include "lib.h"
#include "map.h"
#include "mem.h"
#include "opcode.h"
#include "os.h"
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
#define vm_pop(n) pawC_stkdec(P, n)
#define vm_peek(n) (&P->top.p[-(n) - 1])
#define vm_save() (cf->pc = pc, cf->top = P->top)
#define vm_local() (&cf->base.p[Iw()])
#define vm_upvalue() (fn->up[Iw()]->p.p)
#define vm_const() (fn->p->k[Iw()])
#define vm_jmp() decode_jump(Iw())
#define vm_pushv(v) pawC_pushv(P, v)
#define vm_push0() pawC_push0(P)
#define vm_pushi(i) pawC_pushi(P, i)
#define vm_pushf(f) pawC_pushf(P, f)
#define vm_pushb(b) pawC_pushb(P, b)

// Slot 0 (the callable or 'self') is an implicit parameter
#define vm_argc() (paw_get_count(P) - 1)

// Generate code for creating common builtin objects
// Requires a placeholder slot (the vm_push0() pushes null) so the GC
// doesn't get confused. Both the vm_push0(), and the pawA_new calls
// might fail and cause an error to be thrown, so we have to be careful
// not to leave a junk value on top of the stack.
#define vm_array_init(pa, pv) \
    pv = vm_push0();          \
    pa = pawA_new(P);         \
    pawV_set_array(pv, pa);

#define vm_map_init(pm, pv) \
    pv = vm_push0();        \
    pm = pawH_new(P);       \
    pawV_set_map(pv, pm);

static void vm_unop(paw_Env *P, Op op);

static Value vint(paw_Int i)
{
    Value v;
    pawV_set_int(&v, i);
    return v;
}

static int current_line(const CallFrame *cf)
{
    Proto *p = cf->fn->p;
    const int pc = cf->pc - p->source;

    int i = 0;
    for (; i < p->nlines - 1; ++i) {
        if (p->lines[i].pc >= pc) {
            break;
        }
    }
    return p->lines[i].line;
}

static void add_location(paw_Env *P, Buffer *buf)
{
    const CallFrame *cf = P->cf;
    for (; cf != &P->main; cf = cf->prev) {
        if (cf_is_paw(cf)) {
            const Proto *p = cf->fn->p;
            const int line = current_line(cf);
            const char *name = p->modname->text;
            pawL_add_fstring(P, buf, "%s:%d: ", name, line);
            break;
        } else if (cf_is_entry(cf)) {
            pawL_add_literal(P, buf, "[C]: ");
            break;
        }
    }
}

static void add_3_parts(paw_Env *P, const char *before, const char *value, const char *after)
{
    Buffer buf;
    pawL_init_buffer(P, &buf);
    add_location(P, &buf);
    pawL_add_fstring(P, &buf, "%s%s%s", before, value, after);
    pawL_push_result(P, &buf);
}

void pawR_name_error(paw_Env *P, Value name)
{
    paw_assert(pawV_is_string(name));
    add_3_parts(P, "name '", pawV_get_text(name), "' is not defined");
    pawC_throw(P, PAW_ENAME);
}

void pawR_attr_error(paw_Env *P, Value name)
{
    paw_assert(pawV_is_string(name));
    add_3_parts(P, "attribute '", pawV_get_text(name), "' does not exist");
    pawC_throw(P, PAW_EATTR);
}

void pawR_int_error(paw_Env *P)
{
    Buffer buf;
    pawL_init_buffer(P, &buf);
    pawL_add_string(P, &buf, "integer ");
    pawL_add_value(P, &buf); // value on stack
    pawL_add_string(P, &buf, " is too large");
    pawL_push_result(P, &buf);
    pawC_throw(P, PAW_EOVERFLOW);
}

void pawR_type_error(paw_Env *P, const char *what)
{
    pawR_error(P, PAW_ETYPE, "unsupported operand type for '%s': '%s'",
               what, paw_typename(P, -1));
}

void pawR_type_error2(paw_Env *P, const char *what)
{
    pawR_error(P, PAW_ETYPE, "unsupported operand types for '%s': '%s' and '%s'",
               what, paw_typename(P, -2), paw_typename(P, -1));
}

void pawR_error(paw_Env *P, int error, const char *fmt, ...)
{
    Buffer buf;
    pawL_init_buffer(P, &buf);
    add_location(P, &buf);

    va_list arg;
    va_start(arg, fmt);
    pawL_add_vfstring(P, &buf, fmt, arg);
    va_end(arg);

    pawL_push_result(P, &buf);
    pawC_throw(P, error);
}

static paw_Bool meta_call(paw_Env *P, Op op, Value x, int argc)
{
    const Value *meta = pawT_get_meta(P, op, x);
    if (meta) {
        // Expect 'x', followed by 'argc' args, on top of the stack.
        pawC_call(P, *meta, argc);
        return PAW_TRUE;
    }
    return PAW_FALSE;
}

static paw_Bool meta_unop(paw_Env *P, Op op, Value x)
{
    const Value *meta = pawT_get_meta(P, op, x);
    if (meta) {
        vm_pushv(x);
        pawC_call(P, *meta, 0);
        return PAW_TRUE;
    }
    return PAW_FALSE;
}

static void maybe_meta_unop(paw_Env *P, Op op, Value x)
{
    if (meta_unop(P, op, x)) {
        vm_shift(1);
    }
}

static paw_Bool meta_binop_aux(paw_Env *P, Op op, Value x, Value y)
{
    paw_Bool swap = PAW_FALSE;
    const Value *meta = pawT_get_meta(P, op, x);
    if (!meta && mm_has_r(op)) {
        // Check the reverse metamethod (i.e. y.__<binop>r(x)).
        meta = pawT_get_meta(P, mm_get_r(op), y);
        swap = PAW_TRUE;
    }
    if (meta) {
        StackPtr sp = pawC_stkinc(P, 2);
        sp[0] = swap ? y : x;
        sp[1] = swap ? x : y;
        pawC_call(P, *meta, 1);
        return PAW_TRUE;
    }
    return PAW_FALSE;
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
        return PAW_TRUE;
    }
    return PAW_FALSE;
}

static inline paw_Bool meta_getslice(paw_Env *P, Value obj, Value begin, Value end)
{
    const Value *meta = pawT_get_meta(P, OP_GETSLICE, obj);
    if (meta) {
        StackPtr sp = pawC_stkinc(P, 4);
        sp[0] = *meta;
        sp[1] = obj;
        sp[2] = begin;
        sp[3] = end;
        pawC_call(P, *meta, 3);
        return PAW_TRUE;
    }
    return PAW_FALSE;
}

static inline paw_Bool meta_setslice(paw_Env *P, Value obj, Value begin, Value end, Value val)
{
    const Value *meta = pawT_get_meta(P, OP_GETSLICE, obj);
    if (meta) {
        StackPtr sp = pawC_stkinc(P, 5);
        sp[0] = *meta;
        sp[1] = obj;
        sp[2] = begin;
        sp[3] = end;
        sp[4] = val;
        pawC_call(P, *meta, 4);
        return PAW_TRUE;
    }
    return PAW_FALSE;
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
        return PAW_TRUE;
    }
    return PAW_FALSE;
}

static void float2integer(paw_Env *P, paw_Float f)
{
    if (pawV_float_fits_int(f)) {
        vm_pushi(paw_cast_int(f));
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
        return PAW_TRUE;
    }
    return PAW_FALSE;
}

void pawR_to_integer(paw_Env *P)
{
    StackPtr sp = vm_peek(0);
    if (pawV_is_int(*sp) || pawV_is_bigint(*sp)) {
        return; // already an integer
    } else if (pawV_is_float(*sp)) {
        const paw_Float f = pawV_get_float(*sp);
        float2integer(P, f);
    } else if (pawV_is_bool(*sp)) {
        pawV_set_int(sp, pawV_get_bool(*sp));
        return;
    } else if (pawV_is_string(*sp)) {
        const char *begin = pawV_get_text(*sp);
        const char *str = begin;
        const paw_Bool neg = consume_prefix(&str);
        if (pawV_parse_integer(P, str)) {
            pawR_error(P, PAW_ESYNTAX, "invalid integer '%s'", str);
        }
        if (neg) {
            // Call the main negation routine, since the parsed integer may be
            // a bigint.
            vm_unop(P, OP_NEG);
        }
    } else if (meta_unop(P, OP_INT, *sp)) {
        // called sp->__int()
    } else {
        pawR_type_error(P, "int");
    }
    vm_shift(1);
}

void pawR_to_float(paw_Env *P)
{
    StackPtr sp = vm_peek(0);
    if (pawV_is_float(*sp)) {
        // already a float
    } else if (pawV_is_int(*sp)) {
        pawV_set_float(sp, (paw_Float)pawV_get_int(*sp));
    } else if (pawV_is_bigint(*sp)) {
        pawV_set_float(sp, pawB_get_float(pawV_get_bigint(*sp)));
    } else if (pawV_is_bool(*sp)) {
        pawV_set_float(sp, pawV_get_bool(*sp));
    } else if (pawV_is_string(*sp)) {
        const char *begin = pawV_get_text(*sp);
        const char *str = begin;
        paw_Bool neg = consume_prefix(&str);
        if (pawV_parse_float(P, str)) {
            pawR_error(P, PAW_ESYNTAX, "invalid float '%s'", begin);
        }
        if (neg) {
            sp = vm_peek(0); // new top
            const paw_Float f = pawV_get_float(*sp);
            pawV_set_float(sp, -f);
        }
        vm_shift(1);
    } else if (meta_unop(P, OP_FLOAT, *sp)) {
        vm_shift(1);
    } else {
        pawR_type_error(P, "float");
    }
}

const char *pawR_to_string(paw_Env *P, size_t *plen)
{
    const char *out;
    Value v = *vm_peek(0);
    if (meta_unop(P, OP_STR, v)) {
        const String *str = pawV_get_string(*vm_peek(0));
        *plen = str->length;
        out = str->text;
    } else if (!(out = pawV_to_string(P, v, plen))) {
        pawR_type_error(P, "str");
    }
    vm_shift(1);
    return out;
}

static void try_float2(paw_Env *P, Value *pv, Value *pv2, const char *what)
{
    if (pawV_num2float(pv) || pawV_num2float(pv2)) {
        pawR_type_error2(P, what);
    }
}

static Value *find_attr(paw_Env *P, Value obj, Value name)
{
    Map *attr = NULL;
    if (pawV_is_instance(obj)) {
        attr = pawV_get_instance(obj)->attr;
    } else if (pawV_is_foreign(obj)) {
        attr = pawV_get_foreign(obj)->attr;
    } else {
        return NULL;
    }
    return pawH_get(P, attr, name);
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
    return PAW_FALSE;
}

int pawR_read_global(paw_Env *P, Value name)
{
    assert(pawV_get_type(name) == VSTRING);
    Value *global = pawH_get(P, P->globals, name);
    if (global) {
        vm_pushv(*global);
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
        paw_assert(!create);      // MAP_ACTION_CREATE never returns NULL
        pawR_name_error(P, name); // no return
    }
    *global = *vm_peek(0);
    vm_pop(1);
}

static UpValue *capture_upvalue(paw_Env *P, StackPtr local)
{
    UpValue *prev = NULL;
    UpValue *next = P->up_list;
    while (next && upv_level(next) > local) {
        assert(upv_is_open(next));
        prev = next;
        next = next->open.next;
    }

    if (next && upv_level(next) == local) {
        return next;
    }

    UpValue *new_up = pawV_new_upvalue(P);
    pawV_link_upvalue(P, new_up, prev, next);
    new_up->p.p = local;
    return new_up;
}

void pawR_close_upvalues(paw_Env *P, const StackPtr top)
{
    while (P->up_list && upv_level(P->up_list) >= top) {
        UpValue *up = P->up_list;
        assert(upv_is_open(up));
        // Save before switching active union member (open -> closed).
        UpValue *next = up->open.next;
        up->closed = *up->p.p;
        up->p.p = &up->closed;
        P->up_list = next;
    }
}

int pawR_getattr_raw(paw_Env *P, paw_Bool has_fallback)
{
    const Value obj = *vm_peek(1);
    const Value key = *vm_peek(0);
    paw_assert(pawV_is_string(key));

    // Look for data attribute or non-binding function first. These attributes
    // (added after instantiation) can shadow bound functions.
    const Value *pval = find_attr(P, obj, key);
    if (pval) {
        *vm_peek(0) = *pval;
    } else if ((pval = pawV_find_binding(P, obj, key))) {
        // found a binding: bind to context ('self') variable
        Method *mtd = pawV_new_method(P, obj, *pval);
        pawV_set_method(vm_peek(0), mtd);
    } else if (has_fallback) {
        *vm_peek(0) = *vm_peek(2);
    } else {
        return -1;
    }
    vm_shift(1 + has_fallback);
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
    } else if (pawV_is_foreign(obj)) {
        attr = pawV_get_foreign(obj)->attr;
    } else {
        vm_pop(1); // pop 'val'
        pawR_type_error2(P, "setattr");
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
        // attributes on values of type 'foreign': something that should only
        // happen from C, not paw. pawR_setattr_raw is called by C API functions
        // and the 'setattr' builtin function, while this function is called by
        // the paw runtime.
        if (pawV_is_instance(obj)) {
            Instance *ins = pawV_get_instance(obj);
            pawH_insert(P, ins->attr, key, val);
        } else {
            vm_pop(1); // pop 'val'
            pawR_type_error2(P, "setattr");
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
        const paw_Int idx = pawR_check_int(P, key);
        Value *slot = pawA_get(P, pawV_get_array(obj), idx);
        *slot = val;
    } else if (pawV_is_map(obj)) {
        pawH_insert(P, pawV_get_map(obj), key, val);
    } else {
        vm_pop(1); // pop 'val'
        pawR_type_error2(P, "setitem");
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

#define BASE_INSERT 0
#define BASE_PUSH 1
#define BASE_POP 2
#define BASE_ERASE 3
#define BASE_CLONE 4
#define BASE_STARTS_WITH 5
#define BASE_ENDS_WITH 6
#define iflag(i) (-(int)(i) - 1)

void pawR_init(paw_Env *P)
{
    static const char *kBaseMethods[] = {
        "insert",
        "push",
        "pop",
        //"erase",
        //"clone",
    };
    for (size_t i = 0; i < paw_countof(kBaseMethods); ++i) {
        String *s = pawS_new_str(P, kBaseMethods[i]);
        pawG_fix_object(P, cast_object(s));
        s->flag = iflag(i);
    }
}

static void check_argc(paw_Env *P, int argc, int expect)
{
    if (argc != expect) {
        pawR_error(P, PAW_ERUNTIME, "expected %d arguments but found %d", expect, argc);
    }
}

static void check_varargc(paw_Env *P, int argc, int least, int most)
{
    if (argc < least) {
        pawR_error(P, PAW_ERUNTIME, "expected at least %d arguments but found %d", least, argc);
    } else if (argc > most) {
        pawR_error(P, PAW_ERUNTIME, "expected at most %d arguments but found %d", most, argc);
    }
}

static void array_insert(paw_Env *P, Array *a, int argc)
{
    check_argc(P, argc, 2);
    const paw_Int i = pawR_check_int(P, *vm_peek(1));
    pawA_insert(P, a, i, *vm_peek(0));
    vm_pop(2);
}

static void array_push(paw_Env *P, Array *a, int argc)
{
    check_varargc(P, argc, 1, UINT8_MAX);
    for (int i = argc; i > 0; --i) {
        pawA_push(P, a, *vm_peek(i - 1));
    }
    vm_pop(argc);
}

static void array_pop(paw_Env *P, Array *a, int argc)
{
    check_varargc(P, argc, 0, 1);
    // Argument, if present, indicates the index at which to remove an
    // element. Default to -1: the last element.
    const paw_Int i = argc ? pawV_get_int(*vm_peek(0)) : -1;

    *vm_peek(argc) = *pawA_get(P, a, i); // checks bounds
    pawA_pop(P, a, i);
    vm_pop(argc);
}

static void array_invoke(paw_Env *P, Array *a, Value name, int argc)
{
    const String *key = pawV_get_string(name);
    switch (iflag(key->flag)) {
        case BASE_PUSH: 
            array_push(P, a, argc);
            break;
        case BASE_POP: 
            array_pop(P, a, argc);
            break;
        case BASE_INSERT: 
            array_insert(P, a, argc);
            break;
        default:
            pawR_attr_error(P, name);
    }
}

static CallFrame *super_invoke(paw_Env *P, Class *super, Value name, int argc)
{
    Value *base = vm_peek(argc);
    const Value *method = pawH_get(P, super->attr, name);
    if (!method) {
        pawR_attr_error(P, name);
    }
    // The receiver (subclass) instance + parameters are on top of the stack.
    return pawC_precall(P, base, *method, argc);
}

static paw_Bool bound_call(paw_Env *P, CallFrame **pcf, Value obj, StackPtr base, Value name, int argc)
{
    if (pawV_is_array(obj)) {
        Array *a = pawV_get_array(obj);
        array_invoke(P, a, name, argc);
        return PAW_TRUE;
    }
    Value *bound = pawV_find_binding(P, obj, name);
    if (bound) {
        *pcf = pawC_precall(P, base, *bound, argc);
        return PAW_TRUE;
    }
    return PAW_FALSE;
}

static CallFrame *invoke(paw_Env *P, Value name, int argc)
{
    CallFrame *cf = NULL;
    Value *base = vm_peek(argc);
    if (bound_call(P, &cf, *base, base, name, argc)) {
        // found a bound function with the given 'name'
        return cf;
    }
    // attempt to call a non-bound function
    const Value *method = find_attr(P, *base, name);
    if (!method) {
        pawR_attr_error(P, name);
    }
    *base = *method; // replace object with callable
    return pawC_precall(P, base, *method, argc);
}

static void inherit(paw_Env *P, Class *cls, const Class *super)
{
    // 'copy-down' inheritance
    pawH_extend(P, cls->attr, super->attr);
    // Ensure that __init is not inherited. Note that 'sub' has not had any
    // of its own attributes added to it yet, so this will not remove the
    // actual __init attribute belonging to 'sub'.
    const Value key = pawE_cstr(P, CSTR_INIT);
    pawH_action(P, cls->attr, key, MAP_ACTION_REMOVE);
}

#define stop_loop(i, i2, d) (((d) < 0 && (i) <= (i2)) || \
                             ((d) > 0 && (i) >= (i2)))

static paw_Bool fornum_init(paw_Env *P)
{
    Value c = *vm_peek(0);
    Value b = *vm_peek(1);
    Value a = *vm_peek(2);
    // FIXME: Only small integers are supported right now, but we should also support
    //        big integers. Floats are not really necessary, as float loop bounds are
    //        seldom a good idea IMO.
    if (!pawV_is_int(a) || !pawV_is_int(b) || !pawV_is_int(c)) {
        pawR_error(P, PAW_ETYPE, "for loop bounds must be integer");
    }
    const paw_Int begin = pawV_get_int(a);
    const paw_Int end = pawV_get_int(b);
    const paw_Int step = pawV_get_int(c);
    if (step == 0) {
        pawR_error(P, PAW_ERUNTIME, "loop step equals 0");
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

static paw_Bool fornum(paw_Env *P)
{
    const paw_Int step = pawV_get_int(*vm_peek(0));
    const paw_Int end = pawV_get_int(*vm_peek(1));
    const paw_Int itr = pawV_get_int(*vm_peek(2));
    const paw_Int next = itr + step;
    if (stop_loop(next, end, step)) {
        return PAW_FALSE;
    }
    *vm_peek(2) = vint(next);
    vm_pushi(next);
    return PAW_TRUE;
}

static paw_Bool meta_forin_init(paw_Env *P, Value v)
{
    ptrdiff_t save = save_offset(P, P->top.p);
    if (meta_unop(P, OP_LEN, v)) {
        if (paw_int(P, -1) > 0) {
            vm_pop(1); // pop length
            // Attempt to get the first item. If 'v' has __getitem, then
            // we can iterate over it.
            if (meta_getter(P, OP_GETITEM, v, vint(0))) {
                // Set up the stack to look like '..., 0, item'.
                vm_pushi(0);
                paw_rotate(P, -2, 1);
                return PAW_FALSE;
            }
        }
    }
    P->top.p = restore_pointer(P, save);
    return PAW_TRUE; // skip the loop
}

static paw_Bool forin_init(paw_Env *P)
{
    const Value v = *vm_peek(0);
    paw_Int itr = PAW_ITER_INIT;
    if (pawV_is_array(v)) {
        Array *arr = pawV_get_array(v);
        if (pawA_iter(arr, &itr)) {
            vm_pushi(itr);
            vm_pushv(arr->begin[itr]);
            return PAW_FALSE;
        }
    } else if (pawV_is_map(v)) {
        Map *map = pawV_get_map(v);
        if (pawH_iter(map, &itr)) {
            vm_pushi(itr);
            vm_pushv(map->keys[itr]);
            return PAW_FALSE;
        }
    } else if (has_meta(v)) {
        return meta_forin_init(P, v);
    } else {
        pawR_type_error(P, "iterator for loop");
    }
    return PAW_TRUE;
}

static paw_Bool meta_forin(paw_Env *P, Value v, paw_Int itr)
{
    // push the length
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
        return PAW_TRUE;
    }
    return PAW_FALSE; // finish the loop
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
            vm_pushv(arr->begin[i]);
            return PAW_TRUE;
        }
    } else if (pawV_is_map(obj)) {
        Map *map = pawV_get_map(obj);
        paw_Int i = pawV_get_int(itr);
        if (pawH_iter(map, &i)) {
            *vm_peek(0) = vint(i);
            vm_pushv(map->keys[i]);
            return PAW_TRUE;
        }
    } else {
        paw_assert(has_meta(obj)); // checked by forin_init()
        return meta_forin(P, obj, pawV_get_int(itr));
    }
    return PAW_FALSE; // stop the loop
}

static void int_arith(paw_Env *P, Op op, Value lhs, Value rhs);

static void float_arith(paw_Env *P, Op op, Value lhs, Value rhs)
{
    const paw_Float x = pawV_get_float(lhs);
    const paw_Float y = pawV_get_float(rhs);
    paw_Float z = 0.0; // result
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
                pawR_error(P, PAW_ERUNTIME, "divide by 0");
            } else if (op == OP_DIV) {
                z = x / y;
            } else {
                z = fmod(x, y);
            }
            break;
        default: {
            paw_assert(op == OP_IDIV);
            if (y == 0) {
                pawR_error(P, PAW_ERUNTIME, "divide by 0");
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
        StackPtr sp = vm_push0(); // placeholder
        pawB_from_int(P, sp, y);
        pawB_arith(P, OP_MUL, *sp, vint(x));
        vm_shift(1);
        return PAW_TRUE;
    }
    return PAW_FALSE;
}

static void int_arith(paw_Env *P, Op op, Value lhs, Value rhs)
{
    paw_Int x = pawV_get_int(lhs);
    paw_Int y = pawV_get_int(rhs);
    paw_Int z = 0; // result
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
                pawR_error(P, PAW_ERUNTIME, "divide by 0");
            } else if (op == OP_IDIV) {
                z = x / y;
            } else {
                z = x % y;
            }
            break;
        default:
            paw_assert(op == OP_DIV);
            pawV_set_float(&lhs, (paw_Float)x);
            pawV_set_float(&rhs, (paw_Float)y);
            float_arith(P, op, lhs, rhs);
            return;
    }
    if (pawV_int_fits_int(z)) {
        vm_pushi(z);
    } else {
        Value *pv = vm_push0();
        pawB_from_int(P, pv, z);
    }
}

static void string_arith(paw_Env *P, Op op, Value lhs, Value rhs)
{
    if (op == OP_ADD) {
        if (pawV_is_string(lhs) && pawV_is_string(rhs)) {
            Buffer print;
            pawL_init_buffer(P, &print);
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
            n = pawR_check_int(P, rhs);
        } else {
            n = pawR_check_int(P, lhs);
            s = pawV_get_string(rhs);
        }
        Buffer print;
        pawL_init_buffer(P, &print);
        for (paw_Int i = 0; i < n; ++i) {
            pawL_add_nstring(P, &print, s->text, s->length);
        }
        pawL_push_result(P, &print); // push
        return;
    }
    pawR_type_error2(P, "arithmetic operator");
}

Array *concat_arrays(paw_Env *P, const Array *x, const Array *y)
{
    // Both 'nx' and 'ny' are in [0,PAW_SIZE_MAX].
    const size_t nx = pawA_length(x);
    const size_t ny = pawA_length(y);
    if (nx > PAW_SIZE_MAX - ny) {
        pawM_error(P);
    }
    Value *pv = vm_push0();
    Array *cat = pawA_clone(P, pv, x);
    pawA_reserve(P, cat, nx + ny);
    for (size_t i = 0; i < ny; ++i) {
        pawA_push(P, cat, y->begin[i]);
    }
    return cat;
}

static void array_arith(paw_Env *P, Op op, Value lhs, Value rhs)
{
    if (op == OP_ADD) {
        if (pawV_is_array(lhs) && pawV_is_array(rhs)) {
            Array *x = pawV_get_array(lhs);
            Array *y = pawV_get_array(rhs);
            concat_arrays(P, x, y);
            return;
        }
    } else if (op == OP_MUL) {
        Array *a;
        paw_Int n;
        if (pawV_is_array(lhs)) {
            a = pawV_get_array(lhs);
            n = pawR_check_int(P, rhs);
        } else {
            n = pawR_check_int(P, lhs);
            a = pawV_get_array(rhs);
        }
        Value *pv;
        Array *cat;
        vm_array_init(cat, pv);
        for (paw_Int i = 0; i < n; ++i) {
            cat = concat_arrays(P, cat, a);
            vm_shift(1); // copy over old 'cat'
        }
        return;
    }
    pawR_type_error2(P, "arithmetic operator");
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

static void vm_compare(paw_Env *P, Op op)
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
        try_float2(P, &x, &y, "relational comparison");
        float_rel(P, op, pawV_get_float(x), pawV_get_float(y));
    } else if (pawV_is_bigint(x) || pawV_is_bigint(y)) {
        vm_pushb(pawB_compare(P, op, x, y));
    } else {
        pawR_type_error2(P, "relational comparison");
    }
    vm_shift(2);
}

static void int_unop(paw_Env *P, Op op, paw_Int i)
{
    switch (op) {
        case OP_NEG:
            if (i == VINT_MIN) {
                // The expression '-VINT_MIN' will overflow. Convert to VBIGINT.
                Value *pv = vm_push0();
                BigInt *bi = pawB_from_int(P, pv, i);
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
            pawR_type_error(P, "bitwise operator");
    }
}

static void vm_unop(paw_Env *P, Op op)
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
        pawR_type_error(P, "unary operator");
    }
    vm_shift(1);
}

static void vm_arith(paw_Env *P, Op op)
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
    } else if (pawV_is_array(x) || pawV_is_array(y)) {
        array_arith(P, op, x, y);
    } else if (pawV_is_float(x) || pawV_is_float(y)) {
        try_float2(P, &x, &y, "arithmetic operator");
        float_arith(P, op, x, y);
    } else if (pawV_is_bigint(x) || pawV_is_bigint(y)) {
        pawB_arith(P, op, x, y);
    } else {
        pawR_type_error2(P, "arithmetic operator");
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
                pawR_error(P, PAW_ERUNTIME, "negative shift count");
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
                pawR_error(P, PAW_ERUNTIME, "negative shift count");
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

static void vm_bitwise(paw_Env *P, Op op)
{
    Value y = fetch(P, 0);
    Value x = fetch(P, 1);
    if (pawV_is_int(x) && pawV_is_int(y)) {
        int_bitwise(P, op, x, y);
    } else if ((pawV_is_bigint(x) || pawV_is_int(x)) &&
               (pawV_is_bigint(y) || pawV_is_int(y))) {
        pawB_bitwise(P, op, x, y);
    } else if (meta_binop(P, op, x, y)) {
        // called metamethod on either 'x' or 'y'
    } else {
        pawR_type_error2(P, "bitwise operator");
    }
    vm_shift(2);
}

static const char *ensure_str(paw_Env *P, Value v, int offset, size_t *plen)
{
    if (pawV_is_string(v)) {
        const String *str = pawV_get_string(v);
        *plen = str->length;
        return str->text;
    }
    vm_pushv(v); // convert to string
    const char *str = pawR_to_string(P, plen);
    *vm_peek(offset + 1) = *vm_peek(0);
    vm_pop(1);
    return str;
}

static void vm_concat(paw_Env *P)
{
    Value y = *vm_peek(0);
    Value x = *vm_peek(1);
    if (!meta_binop(P, OP_CONCAT, x, y)) {
        size_t ns, nt;
        const char *s = ensure_str(P, x, 1, &ns);
        const char *t = ensure_str(P, y, 0, &nt);

        Buffer print;
        pawL_init_buffer(P, &print);
        pawL_add_nstring(P, &print, s, ns);
        pawL_add_nstring(P, &print, t, nt);
        pawL_push_result(P, &print);
    }
    vm_shift(2);
}

static void vm_in(paw_Env *P)
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
        pawR_type_error2(P, "in");
    }
    vm_shift(2);
}

void pawR_length(paw_Env *P)
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
    } else if (pawV_is_foreign(*pv)) {
        len = pawV_get_foreign(*pv)->size;
    } else {
        pawR_type_error(P, "length operator ('#')");
    }
    // Replace the container with its length.
    paw_assert(len <= VINT_MAX);
    pawV_set_int(pv, (paw_Int)len);
}

void pawR_arith(paw_Env *P, Op op)
{
    switch (op) {
        case OP_NEG:
        case OP_NOT:
        case OP_BNOT:
            vm_unop(P, op);
            break;
        case OP_ADD:
        case OP_SUB:
        case OP_MUL:
        case OP_DIV:
        case OP_IDIV:
        case OP_MOD:
        case OP_POW:
            vm_arith(P, op);
            break;
        case OP_CONCAT:
            vm_concat(P);
            break;
        case OP_BXOR:
        case OP_BAND:
        case OP_BOR:
        case OP_SHL:
        case OP_SHR:
            vm_bitwise(P, op);
            break;
        default:
            pawR_error(P, PAW_ERUNTIME, "unsupported operator %u", op);
    }
}

void pawR_compare(paw_Env *P, Op op)
{
    if (op == OP_EQ) {
        pawR_equals(P);
    } else {
        vm_compare(P, op);
    }
}

void pawR_equals(paw_Env *P)
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

void pawR_getattr(paw_Env *P)
{
    const Value obj = *vm_peek(1);
    const Value key = *vm_peek(0);
    if (meta_getter(P, OP_GETATTR, obj, key)) {
        vm_shift(2);
    } else if (pawR_getattr_raw(P, PAW_FALSE)) {
        pawR_attr_error(P, *vm_peek(0));
    }
}

static void getitem_list(paw_Env *P, Value obj, Value key)
{
    Array *a = pawV_get_array(obj);
    const paw_Int i = pawR_check_int(P, key);
    *vm_peek(1) = *pawA_get(P, a, i);
    vm_pop(1);
}

static int getitem_map(paw_Env *P, Value obj, Value key)
{
    const Value *pv = pawH_get(P, pawV_get_map(obj), key);
    if (pv) {
        *vm_peek(1) = *pv;
        vm_pop(1);
        return 0;
    }
    return -1;
}

static void getitem_string(paw_Env *P, Value obj, Value key)
{
    paw_Int idx = pawR_check_int(P, key);
    String *str = pawV_get_string(obj);
    pawA_check_abs(P, idx, str->length);
    const char c = str->text[idx];
    String *res = pawS_new_nstr(P, &c, 1);
    pawV_set_string(vm_peek(1), res);
    vm_pop(1);
}

int pawR_getitem(paw_Env *P)
{
    const Value obj = *vm_peek(1);
    const Value key = *vm_peek(0);
    if (pawV_is_array(obj)) {
        getitem_list(P, obj, key);
    } else if (pawV_is_map(obj)) {
        if (getitem_map(P, obj, key)) {
            return -1;
        }
    } else if (pawV_is_string(obj)) {
        getitem_string(P, obj, key);
    } else if (meta_getter(P, OP_GETITEM, obj, key)) {
        // called obj.__getitem(key)
        vm_shift(2);
    } else {
        pawR_type_error(P, "getitem");
    }
    return 0;
}

static void cannonicalize_slice(paw_Env *P, size_t len, Value begin, Value end, paw_Int *bout, paw_Int *eout, paw_Int *nout)
{
    const paw_Int ibegin = pawV_is_null(begin) // null acts like 0
                               ? 0
                               : pawA_abs_index(pawR_check_int(P, begin), len);
    const paw_Int iend = pawV_is_null(end) // null acts like #a
                             ? paw_cast_int(len)
                             : pawA_abs_index(pawR_check_int(P, end), len);
    // clamp to sequence bounds
    *bout = paw_min(paw_max(ibegin, 0), paw_cast_int(len));
    *eout = paw_min(paw_max(iend, 0), paw_cast_int(len));
    *nout = paw_max(0, *eout - *bout);
}

void pawR_getslice(paw_Env *P)
{
    const Value obj = *vm_peek(2);
    const Value begin = *vm_peek(1);
    const Value end = *vm_peek(0);

    if (pawV_is_array(obj)) {
        paw_Int i1, i2, n;
        const Array *src = pawV_get_array(obj);
        cannonicalize_slice(P, pawA_length(src), begin, end, &i1, &i2, &n);

        Value *pv;
        Array *dst;
        vm_array_init(dst, pv);
        pawA_resize(P, dst, cast_size(n));
        for (paw_Int i = i1; i < i2; ++i) {
            dst->begin[i - i1] = src->begin[i];
        }
    } else if (pawV_is_string(obj)) {
        paw_Int i1, i2, n;
        const String *src = pawV_get_string(obj);
        cannonicalize_slice(P, src->length, begin, end, &i1, &i2, &n);

        Value *pv = vm_push0(); // placeholder
        String *dst = pawS_new_nstr(P, src->text + i1, cast_size(n));
        pawV_set_string(pv, dst);
    } else if (meta_getslice(P, obj, begin, end)) {
        // called obj.__getslice(begin, end)
    } else {
        pawR_type_error(P, "getslice");
    }
    vm_shift(3);
}

void pawR_setslice(paw_Env *P)
{
    const Value obj = *vm_peek(3);
    const Value begin = *vm_peek(2);
    const Value end = *vm_peek(1);
    const Value val = *vm_peek(0);

    if (pawV_is_array(obj) && pawV_is_array(val)) {
        paw_Int i1, i2, n;
        Array *a = pawV_get_array(obj);
        const Array *b = pawV_get_array(val);
        if (a == b) {
            //Value *pv = vm_push0();
            //b = pawA_clone(P, pv, a);
            //vm_shift(1);
        }
        const size_t alen = pawA_length(a);
        const size_t blen = pawA_length(b);
        cannonicalize_slice(P, alen, begin, end, &i1, &i2, &n);

        // Resize 'a' to the final length, preserving items after the region
        // of items being replaced.
        if (cast_size(n) > blen) {
            memmove(a->begin + i1 + blen, a->begin + i1 + n,
                    (alen - cast_size(i1 + n)) * sizeof(a->begin[0]));
        }
        const paw_Int length = i1 + paw_cast_int(alen + blen) - i2;
        pawA_resize(P, a, cast_size(length)); // a[:i1] + b + a[i2:]
        if (cast_size(n) < blen) {
            memmove(a->begin + i1 + blen, a->begin + i2,
                    (alen - cast_size(i2)) * sizeof(a->begin[0]));
        }
        memmove(a->begin + i1, b->begin, blen * sizeof(a->begin[0]));
    } else if (meta_setslice(P, obj, begin, end, val)) {
        // called obj.__setslice(begin, end)
    } else {
        pawR_type_error(P, "setslice");
    }
    vm_pop(4);
}

void pawR_literal_array(paw_Env *P, int n)
{
    Array *a;
    StackPtr sp;
    vm_array_init(a, sp);
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
    Map *m;
    StackPtr sp;
    vm_map_init(m, sp);
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
                vm_pushb(PAW_TRUE);
            }

            vm_case(PUSHFALSE) :
            {
                vm_pushb(PAW_FALSE);
            }

            vm_case(PUSHCONST) :
            {
                vm_pushv(vm_const());
            }

            vm_case(LEN) :
            {
                pawR_length(P);
            }

            vm_case(NEG) :
            {
                vm_unop(P, OP_NEG);
            }

            vm_case(NOT) :
            {
                vm_unop(P, OP_NOT);
            }

            vm_case(BNOT) :
            {
                vm_unop(P, OP_BNOT);
            }

            vm_case(SHL) :
            {
                vm_bitwise(P, OP_SHL);
            }

            vm_case(SHR) :
            {
                vm_bitwise(P, OP_SHR);
            }

            vm_case(BAND) :
            {
                vm_bitwise(P, OP_BAND);
            }

            vm_case(BOR) :
            {
                vm_bitwise(P, OP_BOR);
            }

            vm_case(BXOR) :
            {
                vm_bitwise(P, OP_BXOR);
            }

            vm_case(ADD) :
            {
                vm_arith(P, OP_ADD);
            }

            vm_case(SUB) :
            {
                vm_arith(P, OP_SUB);
            }

            vm_case(MUL) :
            {
                vm_arith(P, OP_MUL);
            }

            vm_case(DIV) :
            {
                vm_arith(P, OP_DIV);
            }

            vm_case(IDIV) :
            {
                vm_arith(P, OP_IDIV);
            }

            vm_case(MOD) :
            {
                vm_arith(P, OP_MOD);
            }

            vm_case(CONCAT) :
            {
                vm_concat(P);
            }

            vm_case(EQ) :
            {
                pawR_equals(P);
            }

            vm_case(LT) :
            {
                vm_compare(P, OP_LT);
            }

            vm_case(LE) :
            {
                vm_compare(P, OP_LE);
            }

            vm_case(IN) :
            {
                vm_in(P);
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
                const paw_Bool has_super = Ib();
                Class *cls = pawV_push_class(P);
                cls->name = pawV_get_string(name);
                if (has_super) {
                    const Value parent = *vm_peek(1);
                    if (!pawV_is_class(parent)) {
                        pawR_error(P, PAW_ETYPE, "superclass is not of 'class' type");
                    }
                    const Class *super = pawV_get_class(parent);
                    inherit(P, cls, super);
                    // Swap the superclass and subclass. Leave the superclass on the stack.
                    // The compiler assigns it a local slot, named 'super'. It can be
                    // captured as an upvalue in methods as needed.
                    const Value tmp = P->top.p[-1];
                    P->top.p[-1] = P->top.p[-2];
                    P->top.p[-2] = tmp;
                }
            }

            vm_case(NEWMETHOD) :
            {
                const Value name = vm_const();
                const Value method = *vm_peek(0);
                const Value object = *vm_peek(1);
                Class *cls = pawV_get_class(object);
                paw_assert(pawV_is_string(name));
                pawH_insert(P, cls->attr, name, method);
                vm_pop(1); // pop closure, leave class
            }

            vm_case(GETSUPER) :
            {
                // Attributes on 'super' can only refer to methods, not data fields.
                const Value parent = *vm_peek(0);
                const Value self = *vm_peek(1);
                const Value name = vm_const();
                vm_pop(1); // pop 'parent'

                Class *super = pawV_get_class(parent);
                Value *value = pawH_get(P, super->attr, name);
                if (!value) {
                    pawR_attr_error(P, name);
                }

                Value *pv = vm_push0();
                Method *mtd = pawV_new_method(P, self, *value);
                pawV_set_method(pv, mtd);
            }

            vm_case(INVOKESUPER) :
            {
                const Value parent = *vm_peek(0);
                const Value name = vm_const();
                const int argc = Ib();
                vm_pop(1); // pop 'parent'
                vm_save();

                Class *super = pawV_get_class(parent);
                CallFrame *callee = super_invoke(P, super, name, argc);
                if (callee) {
                    cf = callee;
                    goto top;
                }
            }

            vm_case(GETLOCAL) :
            {
                const Value local = *vm_local();
                vm_pushv(local);
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
                vm_pushv(upval);
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
                    pawR_name_error(P, name);
                }
            }

            vm_case(SETGLOBAL) :
            {
                const Value name = vm_const();
                // Error if 'name' does not exist
                pawR_write_global(P, name, PAW_FALSE);
            }

            vm_case(GLOBAL) :
            {
                const Value name = vm_const();
                pawR_write_global(P, name, PAW_TRUE);
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
                if (pawR_getitem(P)) {
                    pawH_key_error(P, *vm_peek(0));
                }
            }

            vm_case(SETITEM) :
            {
                pawR_setitem(P);
            }

            vm_case(GETSLICE) :
            {
                pawR_getslice(P);
            }

            vm_case(SETSLICE) :
            {
                pawR_setslice(P);
            }

            vm_case(CLOSE) :
            {
                pawR_close_upvalues(P, vm_peek(1));
                vm_pop(1);
            }

            vm_case(CLOSURE) :
            {
                Value *pv = vm_push0();
                Proto *proto = fn->p->p[Iw()];
                Closure *closure = pawV_new_closure(P, proto->nup);
                pawV_set_closure(pv, closure);
                closure->p = proto;

                // open upvalues
                for (int i = 0; i < closure->nup; ++i) {
                    const int tag = Iw();
                    const int index = tag & UPVALUE_MAX;
                    closure->up[i] = tag & UPVALUE_LOCAL
                                         ? capture_upvalue(P, cf->base.p + index)
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
                    // no metamethods on class
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

                P->top.p = cf_stack_return(cf);
                vm_save();

                pawR_close_upvalues(P, vm_peek(0));
                vm_pushv(result);
                P->cf = cf->prev;
                if (cf_is_entry(cf)) {
                    return;
                }
                cf = P->cf;
                goto top;
            }

            vm_case(VARARG) :
            {
                // must be run immediately after OP_CALL
                const int nexpect = Ib();
                const int nactual = vm_argc();
                const int nextra = nactual - nexpect;
                Value *pv;
                Array *argv;
                vm_array_init(argv, pv);
                if (nextra) {
                    pawA_resize(P, argv, cast_size(nextra));
                    StackPtr argv0 = cf->base.p + 1 + nexpect;
                    for (int i = 0; i < nextra; ++i) {
                        argv->begin[i] = argv0[i];
                    }
                    // replace first variadic parameter with 'argv' array
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
                maybe_meta_unop(P, OP_NULL, *vm_peek(0));
                if (pawV_is_null(*vm_peek(0))) {
                    pc += offset;
                }
            }

            vm_case(JUMPFALSE) :
            {
                const int offset = vm_jmp();
                maybe_meta_unop(P, OP_BOOL, *vm_peek(0));
                if (!pawV_truthy(*vm_peek(0))) {
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
                    pc += offset; // continue
                }
            }

        vm_default:
            paw_assert(PAW_FALSE);
        }
    }
}
