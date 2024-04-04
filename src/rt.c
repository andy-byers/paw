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
#define vm_shift(n) (*vm_peek(n) = *vm_peek(0), vm_pop(n))
#define vm_pop(n) pawC_stkdec(P, n)
#define vm_peek(n) (&P->top.p[-(n) - 1])
#define vm_save() (vm_protect(), cf->top = P->top)
#define vm_protect() (cf->pc = pc)
#define vm_upvalue(o) (fn->up[get_U(o)]->p.p)
#define vm_pushv(v) pawC_pushv(P, v)
#define vm_push0() pawC_push0(P)
#define vm_pushi(i) pawC_pushi(P, i)
#define vm_pushf(f) pawC_pushf(P, f)
#define vm_pushb(b) pawC_pushb(P, b)

// Slot 0 (the callable or 'self') is an implicit parameter that doesn't
// contribute to argc.
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

Value *get_meta(paw_Env *P, Metamethod mm, Value obj)
{
    if (has_meta(obj)) {
        const Value key = P->meta_keys[mm];
        return pawV_find_binding(P, obj, key);
    }
    return NULL;
}

static paw_Bool meta_call(paw_Env *P, Value x, int argc)
{
    const Value *meta = get_meta(P, MM_CALL, x);
    if (meta) {
        // Expect 'x', followed by 'argc' args, on top of the stack.
        pawC_call(P, *meta, argc);
        return PAW_TRUE;
    }
    return PAW_FALSE;
}

static paw_Bool meta_single(paw_Env *P, Metamethod mm, Value x)
{
    const Value *meta = get_meta(P, mm, x);
    if (meta) {
        vm_pushv(x);
        pawC_call(P, *meta, 0);
        return PAW_TRUE;
    }
    return PAW_FALSE;
}

static paw_Bool meta_unop(paw_Env *P, UnaryOp op, Value x)
{
    return meta_single(P, unop2meta(op), x);
}

static paw_Bool meta_binop_aux(paw_Env *P, BinaryOp binop, Value x, Value y)
{
    const Value *meta = get_meta(P, binop2meta(binop), x);
    if (meta) {
        StackPtr sp = pawC_stkinc(P, 2);
        sp[0] = x;
        sp[1] = y;
        pawC_call(P, *meta, 1);
        return PAW_TRUE;
    }
    return PAW_FALSE;
}

static paw_Bool meta_eq_ne(paw_Env *P, BinaryOp binop, Value x, Value y)
{
    paw_assert(binop == BINARY_EQ || binop == BINARY_NE);
    return meta_binop_aux(P, binop, x, y) ||
           meta_binop_aux(P, binop, y, x);
}

static paw_Bool meta_binop(paw_Env *P, BinaryOp binop, Value x, Value y)
{
    switch (binop) {
        case BINARY_LT:
            return meta_binop_aux(P, BINARY_LT, x, y) ||
                   meta_binop_aux(P, BINARY_GT, y, x);
        case BINARY_LE:
            return meta_binop_aux(P, BINARY_LE, x, y) ||
                   meta_binop_aux(P, BINARY_GE, y, x);
        case BINARY_GT:
            return meta_binop_aux(P, BINARY_GT, x, y) ||
                   meta_binop_aux(P, BINARY_LT, y, x);
        case BINARY_GE:
            return meta_binop_aux(P, BINARY_GE, x, y) ||
                   meta_binop_aux(P, BINARY_LE, y, x);
        case BINARY_IN:
            return meta_binop_aux(P, BINARY_IN, y, x);
        default:
            break;
    }

    paw_Bool swap = PAW_FALSE;
    const Value *meta = get_meta(P, binop2meta(binop), x);
    if (!meta && binop_has_r(binop)) {
        // Check the reverse metamethod (i.e. y.__<binop>r(x)).
        meta = get_meta(P, binop_r(binop), y);
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

static inline paw_Bool meta_getter(paw_Env *P, Op op, Value obj, Value key)
{
    const Value *meta = get_meta(P, op - METAMETHOD0, obj);
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
    const Value *meta = get_meta(P, MM_GETSLICE, obj);
    if (meta) {
        StackPtr sp = pawC_stkinc(P, 3);
        sp[0] = obj;
        sp[1] = begin;
        sp[2] = end;
        pawC_call(P, *meta, 2);
        return PAW_TRUE;
    }
    return PAW_FALSE;
}

static inline paw_Bool meta_setslice(paw_Env *P, Value obj, Value begin, Value end, Value val)
{
    const Value *meta = get_meta(P, MM_SETSLICE, obj);
    if (meta) {
        StackPtr sp = pawC_stkinc(P, 4);
        sp[0] = obj;
        sp[1] = begin;
        sp[2] = end;
        sp[3] = val;
        pawC_call(P, *meta, 3);
        vm_pop(1); // unused return value
        return PAW_TRUE;
    }
    return PAW_FALSE;
}

static inline paw_Bool meta_setter(paw_Env *P, Op op, Value obj, Value key, Value val)
{
    const Value *meta = get_meta(P, op - METAMETHOD0, obj);
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
            pawR_unop(P, UNARY_NEG);
        }
    } else if (meta_single(P, MM_INT, *sp)) {
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
    } else if (meta_single(P, MM_FLOAT, *sp)) {
        vm_shift(1);
    } else {
        pawR_type_error(P, "float");
    }
}

const char *pawR_to_string(paw_Env *P, size_t *plen)
{
    const char *out;
    Value v = *vm_peek(0);
    if (meta_single(P, MM_STR, v)) {
        const String *str = pawV_get_string(*vm_peek(0));
        *plen = str->length;
        out = str->text;
    } else if (!(out = pawV_to_string(P, v, plen))) {
        pawR_type_error(P, "str");
    }
    vm_shift(1);
    return out;
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

void pawR_init(paw_Env *P)
{
    String *errmsg = pawS_new_str(P, "not enough memory");
    pawG_fix_object(P, cast_object(errmsg));
    pawV_set_string(&P->mem_errmsg, errmsg);
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
    const Value c = *vm_peek(0);
    const Value b = *vm_peek(1);
    const Value a = *vm_peek(2);
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
    if (meta_unop(P, UNARY_LEN, v)) {
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
    meta_unop(P, UNARY_LEN, v);

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

static paw_Bool mul_overflow(paw_Env *P, paw_Int x, paw_Int y)
{
    if (check_mul_overflow(x, y)) {
        // Add a copy of 'y' as a BigInt above 'x' on the stack. OP_MUL is
        // commutative, so just do 'y * x' and fix the stack after.
        StackPtr sp = vm_push0(); // placeholder
        pawB_from_int(P, sp, y);
        pawB_binop(P, BINARY_MUL, *sp, vint(x));
        vm_shift(1);
        return PAW_TRUE;
    }
    return PAW_FALSE;
}

#define finish_strcmp(x, y, op) (pawS_cmp(x, y) op 0)

static int string_binop(paw_Env *P, BinaryOp binop, Value lhs, Value rhs)
{
    if (binop == BINARY_MUL) {
        // 's * n' and 'n * s' repeat string 's' 'n' times
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
        vm_shift(2);
        return 0;
    } else if (!pawV_is_string(lhs) || !pawV_is_string(rhs)) {
        return -1; // all other operators require 2 string operands
    }

    const String *x = pawV_get_string(lhs);
    const String *y = pawV_get_string(rhs);
    switch (binop) {
        case BINARY_ADD: {
            // 's + t' concatenates strings 's' and 't'
            Buffer print;
            pawL_init_buffer(P, &print);
            pawL_add_nstring(P, &print, x->text, x->length);
            pawL_add_nstring(P, &print, y->text, y->length);
            pawL_push_result(P, &print);
            break;
        }
        case BINARY_LT:
            vm_pushb(finish_strcmp(x, y, <));
            break;
        case BINARY_LE:
            vm_pushb(finish_strcmp(x, y, <=));
            break;
        case BINARY_GT:
            vm_pushb(finish_strcmp(x, y, >));
            break;
        case BINARY_GE:
            vm_pushb(finish_strcmp(x, y, >=));
            break;
        default:
            return -1;
    }
    vm_shift(2);
    return 0;
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

static int array_binop(paw_Env *P, BinaryOp binop, Value lhs, Value rhs)
{
    if (binop == BINARY_ADD) {
        if (pawV_is_array(lhs) && pawV_is_array(rhs)) {
            Array *x = pawV_get_array(lhs);
            Array *y = pawV_get_array(rhs);
            concat_arrays(P, x, y);
            vm_shift(2);
            return 0;
        }
    } else if (binop == BINARY_MUL) {
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
        vm_shift(2);
        return 0;
    }
    return -1;
}

static paw_Bool eq_different(Value x, Value y)
{
    if (pawV_is_float(x) || pawV_is_float(y)) {
        const paw_Float fx = pawV_to_float(x);
        const paw_Float fy = pawV_to_float(y);
        return fx == fy;
    } else if (pawV_is_bool(x) || pawV_is_bool(y)) {
        return pawV_truthy(x) == pawV_truthy(y);
    } else if (pawV_is_bigint(x) || pawV_is_bigint(y)) {
        return pawB_equals(x, y);
    }
    return PAW_FALSE;
}

static void eq_ne(paw_Env *P, BinaryOp binop, Value x, Value y)
{
    paw_Bool result;
    const paw_Bool bt = binop == BINARY_EQ;
    const paw_Bool bf = binop != BINARY_EQ;
    if (pawV_get_type(x) != pawV_get_type(y)) {
        if (!has_meta(x) && !has_meta(y)) {
            result = eq_different(x, y);
            pawV_set_bool(vm_peek(1), result ? bt : bf);
            vm_pop(1);
            return;
        }
    } else if (pawV_is_object(x)) {
        if (pawV_is_bigint(x)) {
            result = pawB_equals(x, y);
        } else if (pawV_is_array(x)) {
            const Array *lhs = pawV_get_array(x);
            const Array *rhs = pawV_get_array(y);
            result = pawA_equals(P, lhs, rhs);
        } else if (pawV_is_map(x)) {
            Map *lhs = pawV_get_map(x);
            Map *rhs = pawV_get_map(y);
            result = pawH_equals(P, lhs, rhs);
        } else {
            goto compare_values;
        }
        pawV_set_bool(vm_peek(1), result ? bt : bf);
        vm_pop(1);
        return;
    } else {
        goto compare_values;
    }
    if (meta_eq_ne(P, binop, x, y)) {
        vm_shift(2);
        return;
    }
compare_values:
    // Fall back to comparing the value representation.
    result = x.u == y.u ? bt : bf;
    pawV_set_bool(vm_peek(1), result);
    vm_pop(1);
}

#define finish_cmp(x, y, op) (pawV_set_bool(vm_peek(1), (x)op(y)), vm_pop(1))

static int float_binop(paw_Env *P, BinaryOp binop, paw_Float x, paw_Float y);

static int int_binop(paw_Env *P, BinaryOp binop, paw_Int x, paw_Int y)
{
    paw_Int z = 0;
    switch (binop) {
        case BINARY_LT:
            finish_cmp(x, y, <);
            return 0;
        case BINARY_LE:
            finish_cmp(x, y, <=);
            return 0;
        case BINARY_GT:
            finish_cmp(x, y, >);
            return 0;
        case BINARY_GE:
            finish_cmp(x, y, >=);
            return 0;
        // Addition and subtraction will never overflow the native type used
        // for a paw_Int, but may result in a value smaller than VINT_MIN
        // or larger than VINT_MAX. Out-of-range values are converted to
        // BigInt below.
        case BINARY_ADD:
            z = x + y;
            break;
        case BINARY_SUB:
            z = x - y;
            break;
        case BINARY_MUL:
            if (mul_overflow(P, x, y)) {
                vm_shift(2);
                return 0;
            } else {
                z = x * y;
            }
            break;
        case BINARY_IDIV:
        case BINARY_MOD:
            if (y == 0) {
                pawR_error(P, PAW_ERUNTIME, "divide by 0");
            } else if (binop == BINARY_IDIV) {
                z = x / y;
            } else {
                z = x % y;
            }
            break;
        case BINARY_DIV:
            paw_assert(binop == BINARY_DIV);
            float_binop(P, binop, (paw_Float)x, (paw_Float)y);
            return 0;
        case BINARY_BAND:
            z = x & y;
            break;
        case BINARY_BOR:
            z = x | y;
            break;
        case BINARY_BXOR:
            z = x ^ y;
            break;
        case BINARY_SHL:
            if (y < 0) {
                pawR_error(P, PAW_ERUNTIME, "negative shift count");
            } else if (y == 0) {
                z = x; // NOOP
            } else if (cast_size(y) >= VINT_WIDTH ||
                       x > (VINT_MAX >> y) ||
                       x < (VINT_MIN >> y)) {
                // Shift left will overflow. Use a BigInt.
                pawB_binop(P, BINARY_SHL, *vm_peek(1), *vm_peek(0));
                vm_shift(2);
                return 0;
            } else {
                z = paw_cast_int((uint64_t)x << y);
            }
            break;
        case BINARY_SHR:
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
        default:
            return -1;
    }
    if (pawV_int_fits_int(z)) {
        vm_pushi(z);
    } else {
        Value *pv = vm_push0();
        pawB_from_int(P, pv, z);
    }
    vm_shift(2);
    return 0;
}

static int float_binop(paw_Env *P, BinaryOp binop, paw_Float x, paw_Float y)
{
    Value *pv = vm_peek(1);
    switch (binop) {
        case BINARY_LT:
            finish_cmp(x, y, <);
            return 0;
        case BINARY_LE:
            finish_cmp(x, y, <=);
            return 0;
        case BINARY_GT:
            finish_cmp(x, y, >);
            return 0;
        case BINARY_GE:
            finish_cmp(x, y, >=);
            return 0;
        case BINARY_ADD:
            pawV_set_float(pv, x + y);
            break;
        case BINARY_SUB:
            pawV_set_float(pv, x - y);
            break;
        case BINARY_MUL:
            pawV_set_float(pv, x * y);
            break;
        case BINARY_IDIV:
        case BINARY_DIV:
        case BINARY_MOD:
            if (y == 0) {
                pawR_error(P, PAW_ERUNTIME, "divide by 0");
            } else if (binop == BINARY_DIV) {
                pawV_set_float(pv, x / y);
            } else if (binop == BINARY_MOD) {
                pawV_set_float(pv, fmod(x, y));
            } else { // BINARY_IDIV
                const paw_Float f = x / y;
                float2integer(P, f);
                vm_shift(2);
                return 0;
            }
            break;
        default: {
            return -1;
        }
    }
    vm_pop(1);
    return 0;
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

static int other_binop(paw_Env *P, BinaryOp binop, Value x, Value y)
{
    if (meta_binop(P, binop, x, y)) {
        vm_shift(2);
        return 0;
    } else if (binop == BINARY_IN) {
        if (pawV_is_map(y)) {
            pawV_set_bool(vm_peek(1), pawH_contains(P, pawV_get_map(y), x));
        } else if (pawV_is_array(y)) {
            pawV_set_bool(vm_peek(1), pawA_contains(P, pawV_get_array(y), x));
        } else {
            return -1;
        }
        vm_pop(1);
        return 0;
    } else if (binop == BINARY_CONCAT) {
        size_t ns, nt;
        const char *s = ensure_str(P, x, 1, &ns);
        const char *t = ensure_str(P, y, 0, &nt);

        Buffer print;
        pawL_init_buffer(P, &print);
        pawL_add_nstring(P, &print, s, ns);
        pawL_add_nstring(P, &print, t, nt);
        pawL_push_result(P, &print);
        vm_shift(2);
        return 0;
    } else if (pawV_is_bigint(x) || pawV_is_bigint(y)) {
        pawB_binop(P, binop, x, y);
        vm_shift(2);
        return 0;
    } else if (pawV_is_string(x) || pawV_is_string(y)) {
        return string_binop(P, binop, x, y);
    } else if (pawV_is_array(x) || pawV_is_array(y)) {
        return array_binop(P, binop, x, y);
    }
    return -1;
}

static int binop_aux(paw_Env *P, BinaryOp binop, Value x, Value y)
{
    if (binop == BINARY_EQ || binop == BINARY_NE) {
        eq_ne(P, binop, x, y);
        return 0;
    }

    // Handle `int <binop> float` and `float <binop> int`.
    if (pawV_is_int(x)) {
        const paw_Int ix = pawV_get_int(x);
        if (pawV_is_int(y)) {
            const paw_Int iy = pawV_get_int(y);
            return int_binop(P, binop, ix, iy);
        } else if (pawV_is_float(y)) {
            const paw_Float fy = pawV_get_float(y);
            return float_binop(P, binop, (paw_Float)ix, fy);
        }
    } else if (pawV_is_float(x)) {
        const paw_Float fx = pawV_get_float(x);
        if (pawV_is_int(y)) {
            const paw_Int iy = pawV_get_int(y);
            return float_binop(P, binop, fx, (paw_Float)iy);
        } else if (pawV_is_float(y)) {
            const paw_Float fy = pawV_get_float(y);
            return float_binop(P, binop, fx, fy);
        }
    }
    return other_binop(P, binop, x, y);
}

void pawR_binop(paw_Env *P, BinaryOp binop)
{
    const Value y = *vm_peek(0);
    const Value x = *vm_peek(1);
    if (binop_aux(P, binop, x, y)) {
        pawR_type_error2(P, "binary operator");
    }
}

static int int_unop(paw_Env *P, UnaryOp unop, paw_Int i)
{
    Value *pv = vm_peek(0);
    switch (unop) {
        case UNARY_NEG:
            if (i == VINT_MIN) {
                // The expression '-VINT_MIN' will overflow. Convert to VBIGINT.
                pv = vm_push0();
                BigInt *bi = pawB_from_int(P, pv, i);
                pawB_unop(P, UNARY_NEG, obj2v(bi));
                vm_shift(1);
                return 0;
            }
            pawV_set_int(pv, -i);
            break;
        case UNARY_NOT:
            pawV_set_int(pv, !i);
            break;
        case UNARY_BNOT:
            pawV_set_int(pv, ~i);
            break;
        default:
            return -1;
    }
    return 0;
}

static int float_unop(paw_Env *P, UnaryOp unop, paw_Float f)
{
    Value *pv = vm_peek(0);
    switch (unop) {
        case UNARY_NEG:
            pawV_set_float(pv, -f);
            break;
        case UNARY_NOT:
            pawV_set_bool(pv, !f);
            break;
        default:
            return -1;
    }
    return 0;
}

static int unop_aux(paw_Env *P, UnaryOp unop, Value x)
{
    if (pawV_is_int(x)) {
        const paw_Int i = pawV_get_int(x);
        return int_unop(P, unop, i);
    } else if (pawV_is_float(x)) {
        const paw_Float f = pawV_get_float(x);
        return float_unop(P, unop, f);
    } else if (pawV_is_bigint(x)) {
        pawB_unop(P, unop, x);
    } else if (meta_unop(P, unop, x)) {
        // called metamethod on 'x'
    } else if (unop == UNARY_LEN) {
        const paw_Int len = pawV_length(x);
        if (len >= 0) {
            // Replace the container with its length.
            paw_assert(len <= VINT_MAX);
            pawV_set_int(vm_peek(0), len);
            return 0;
        }
        return -1;
    } else if (unop == UNARY_NOT) {
        // allows expressions like '!str'
        vm_pushb(!pawV_truthy(x));
    } else {
        return -1;
    }
    vm_shift(1);
    return 0;
}

void pawR_unop(paw_Env *P, UnaryOp unop)
{
    const Value x = *vm_peek(0);
    if (unop_aux(P, unop, x)) {
        pawR_type_error(P, "unary operator");
    }
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
        vm_pop(1); // pop 'key'
        pawR_type_error(P, "getitem");
    }
    return 0;
}

static void cannonicalize_slice(paw_Env *P, size_t len, Value begin, Value end, paw_Int *bout, paw_Int *eout, paw_Int *nout)
{
    const paw_Int ibegin = pawV_is_null(begin)
                               ? 0 // null acts like 0
                               : pawA_abs_index(pawR_check_int(P, begin), len);
    const paw_Int iend = pawV_is_null(end)
                             ? paw_cast_int(len) // null acts like #a
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
        paw_Int i1, i2, replace;
        // If 'a == b', then we must be executing something like 'a[i:j] = a'.
        // This will work, as long as memmove is used (making room for '#a' items,
        // so the first memmove will never overwrite items we still need).
        Array *a = pawV_get_array(obj);
        const Array *b = pawV_get_array(val);
        const size_t alen = pawA_length(a);
        const size_t blen = pawA_length(b);
        cannonicalize_slice(P, alen, begin, end, &i1, &i2, &replace);

        // Resize 'a' to the final length, preserving items after the region
        // of items being replaced.
        if (cast_size(replace) > blen) {
            memmove(a->begin + i1 + blen, a->begin + i1 + replace,
                    (alen - cast_size(i1 + replace)) * sizeof(a->begin[0]));
        }
        const paw_Int length = i1 + paw_cast_int(alen + blen) - i2;
        pawA_resize(P, a, cast_size(length)); // a[:i1] + b + a[i2:]
        if (cast_size(replace) < blen) {
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
        Value *pv = a->end;
        do {
            *--pv = *--sp;
        } while (pv != a->begin);
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

static paw_Bool should_jump_null(paw_Env *P)
{
    const Value *pv = vm_peek(0);
    if (meta_single(P, MM_NULL, *pv)) {
        if (pawV_is_null(*vm_peek(0))) {
            vm_pop(1);
            return PAW_TRUE;
        }
        vm_shift(1);
        return PAW_FALSE;
    }
    return pawV_is_null(*pv);
}

static paw_Bool should_jump_false(paw_Env *P)
{
    const Value *pv = vm_peek(0);
    if (meta_single(P, MM_BOOL, *pv)) {
        const paw_Bool jump = !pawV_truthy(*vm_peek(0));
        vm_pop(1);
        return jump;
    }
    return !pawV_truthy(*pv);
}

void pawR_execute(paw_Env *P, CallFrame *cf)
{
    const OpCode *pc;
    const Value *K;
    Closure *fn;

top:
    pc = cf->pc;
    fn = cf->fn;
    K = fn->p->k;

    for (;;) {
        const OpCode opcode = *pc++;
        vm_switch(get_OP(opcode))
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
                vm_pushv(K[get_U(opcode)]);
            }

            vm_case(UNOP) :
            {
                vm_protect();
                pawR_unop(P, get_U(opcode));
            }

            vm_case(BINOP) :
            {
                vm_protect();
                pawR_binop(P, get_U(opcode));
            }

            vm_case(NEWARRAY) :
            {
                vm_protect();
                pawR_literal_array(P, get_U(opcode));
                check_gc(P);
            }

            vm_case(NEWMAP) :
            {
                vm_protect();
                pawR_literal_map(P, get_U(opcode));
                check_gc(P);
            }

            vm_case(NEWCLASS) :
            {
                vm_protect();
                Class *cls = pawV_push_class(P);
                cls->name = pawV_get_string(*vm_peek(1));
                vm_shift(1);

                if (get_U(opcode)) {
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
                check_gc(P);
            }

            vm_case(NEWMETHOD) :
            {
                vm_protect();
                const Value name = K[get_U(opcode)];
                const Value method = *vm_peek(0);
                const Value object = *vm_peek(1);
                Class *cls = pawV_get_class(object);
                paw_assert(pawV_is_string(name));
                pawH_insert(P, cls->attr, name, method);
                vm_pop(1); // pop closure, leave class
                check_gc(P);
            }

            vm_case(GETSUPER) :
            {
                vm_protect();
                // Attributes on 'super' can only refer to methods, not data fields.
                const Value parent = *vm_peek(0);
                const Value self = *vm_peek(1);
                const Value name = K[get_U(opcode)];
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
                vm_protect();
                const Value parent = *vm_peek(0);
                const Value name = K[get_A(opcode)];
                const int argc = get_B(opcode);
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
                const Value local = cf->base.p[get_U(opcode)];
                vm_pushv(local);
            }

            vm_case(SETLOCAL) :
            {
                Value *plocal = &cf->base.p[get_U(opcode)];
                *plocal = *vm_peek(0);
                vm_pop(1);
            }

            vm_case(GETUPVALUE) :
            {
                const Value upval = *vm_upvalue(opcode);
                vm_pushv(upval);
            }

            vm_case(SETUPVALUE) :
            {
                Value *pupval = vm_upvalue(opcode);
                *pupval = *vm_peek(0);
                vm_pop(1);
            }

            vm_case(GETGLOBAL) :
            {
                vm_protect();
                const Value name = K[get_U(opcode)];
                if (pawR_read_global(P, name)) {
                    pawR_name_error(P, name);
                }
            }

            vm_case(SETGLOBAL) :
            {
                vm_protect();
                const Value name = K[get_U(opcode)];
                // Error if 'name' does not exist
                pawR_write_global(P, name, PAW_FALSE);
            }

            vm_case(GLOBAL) :
            {
                const Value name = K[get_U(opcode)];
                pawR_write_global(P, name, PAW_TRUE);
            }

            vm_case(GETATTR) :
            {
                vm_protect();
                pawR_getattr(P);
            }

            vm_case(SETATTR) :
            {
                vm_protect();
                pawR_setattr(P);
            }

            vm_case(GETITEM) :
            {
                vm_protect();
                if (pawR_getitem(P)) {
                    pawH_key_error(P, *vm_peek(0));
                }
            }

            vm_case(SETITEM) :
            {
                vm_protect();
                pawR_setitem(P);
            }

            vm_case(GETSLICE) :
            {
                vm_protect();
                pawR_getslice(P);
            }

            vm_case(SETSLICE) :
            {
                vm_protect();
                pawR_setslice(P);
            }

            vm_case(CLOSE) :
            {
                const int n = get_A(opcode);
                if (get_B(opcode)) {
                    pawR_close_upvalues(P, vm_peek(n));
                }
                vm_pop(n);
            }

            vm_case(CLOSURE) :
            {
                vm_protect();
                Value *pv = vm_push0();
                Proto *proto = fn->p->p[get_U(opcode)];
                Closure *closure = pawV_new_closure(P, proto->nup);
                pawV_set_closure(pv, closure);
                closure->p = proto;

                // open upvalues
                StackPtr base = cf->base.p;
                for (int i = 0; i < closure->nup; ++i) {
                    const struct UpValueInfo u = proto->u[i];
                    closure->up[i] = u.is_local
                                         ? capture_upvalue(P, base + u.index)
                                         : fn->up[u.index];
                }
                check_gc(P);
            }

            vm_case(INVOKE) :
            {
                const Value name = K[get_A(opcode)];
                const uint8_t argc = get_B(opcode);
                vm_save();

                CallFrame *callee = invoke(P, name, argc);
                if (callee) {
                    cf = callee;
                    goto top;
                }
            }

            vm_case(CALL) :
            {
                const uint8_t argc = get_U(opcode);
                StackPtr ptr = vm_peek(argc);
                if (meta_call(P, *ptr, argc)) {
                    vm_continue; // called ptr->__call(...)
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
                vm_protect();
                // must be run immediately after OP_CALL
                const int nexpect = get_U(opcode);
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
                check_gc(P);
            }

            vm_case(JUMP) :
            {
                pc += get_S(opcode);
            }

            vm_case(JUMPNULL) :
            {
                if (should_jump_null(P)) {
                    pc += get_S(opcode);
                }
            }

            vm_case(JUMPFALSE) :
            {
                if (should_jump_false(P)) {
                    pc += get_S(opcode);
                }
            }

            vm_case(JUMPFALSEPOP) :
            {
                if (should_jump_false(P)) {
                    pc += get_S(opcode);
                }
                vm_pop(1);
            }

            vm_case(FORNUM0) :
            {
                vm_protect();
                if (fornum_init(P)) {
                    pc += get_S(opcode); // skip
                }
            }

            vm_case(FORNUM) :
            {
                if (fornum(P)) {
                    pc += get_S(opcode); // continue
                }
            }

            vm_case(FORIN0) :
            {
                vm_protect();
                if (forin_init(P)) {
                    // Skip the loop. We need to add a dummy value to the stack,
                    // since there was an 'OP_POP' generated to pop it. See
                    // forin() in parse.c for details.
                    vm_push0();
                    pc += get_S(opcode);
                }
            }

            vm_case(FORIN) :
            {
                vm_protect(); // metamethod may throw an error
                if (forin(P)) {
                    pc += get_S(opcode); // continue
                }
            }

        vm_default:
            paw_assert(PAW_FALSE);
        }
    }
}
