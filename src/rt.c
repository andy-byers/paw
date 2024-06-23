// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#include "prefix.h"

#include "vector.h"
#include "auxlib.h"
#include "call.h"
#include "env.h"
#include "gc_aux.h"
#include "lex.h"
#include "lib.h"
#include "map.h"
#include "mem.h"
#include "opcode.h"
#include "os.h"
#include "paw.h"
#include "rt.h"
#include "str.h"
#include "type.h"
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
#define vm_pusho(o) pawC_pusho(P, cast_object(o))

// Slot 0 (the callable) is an implicit parameter.
#define vm_argc() (paw_get_count(P) - 1)

// Generate code for creating common builtin objects
// Requires a placeholder slot (the vm_push0() pushes an empty slot) so 
// the GC doesn't get confused. Both the vm_push0(), and the pawA_new calls
// might fail and cause an error to be thrown, so we have to be careful not
// to leave a junk value on top of the stack.
#define vm_vector_init(pa, pv) \
    pv = vm_push0();           \
    pa = pawA_new(P);          \
    v_set_object(pv, pa);

#define vm_map_init(pm, pv) \
    pv = vm_push0();        \
    pm = pawH_new(P);       \
    v_set_object(pv, pm);

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
    add_3_parts(P, "name '", v_text(name), "' is not defined");
    pawC_throw(P, PAW_ENAME);
}

void pawR_attr_error(paw_Env *P, Value name)
{
    add_3_parts(P, "attribute '", v_text(name), "' does not exist");
    pawC_throw(P, PAW_EATTR);
}

void pawR_int_error(paw_Env *P)
{
    Buffer buf;
    pawL_init_buffer(P, &buf);
    pawL_add_string(P, &buf, "integer ");
    pawL_add_value(P, &buf, PAW_TINT); // value on stack
    pawL_add_string(P, &buf, " is too large");
    pawL_push_result(P, &buf);
    pawC_throw(P, PAW_EOVERFLOW);
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

// Convert a paw_Float to a paw_Int (from Lua)
// Assumes 2's complement, which means PAW_INT_MIN is a power-of-2 with
// an exact paw_Float representation. 
#define float2int_aux(f, pv) \
     ((f) >= (paw_Float)(PAW_INT_MIN) && \
      (f) < -(paw_Float)(PAW_INT_MIN) && \
      (v_set_int(pv, paw_cast_int(f)), 1))

static void float2int(paw_Env *P, paw_Float f, Value *pv)
{
    if (!float2int_aux(f, pv)) {
        pawR_error(P, PAW_EOVERFLOW, "float %f is too large", f);
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

void pawR_cast_bool(paw_Env *P, paw_Type type)
{
    paw_assert(type < PAW_TSTRING);
    StackPtr sp = vm_peek(0);
    if (type == PAW_TFLOAT) {
        const paw_Float f = v_float(*sp);
        v_set_bool(sp, f != 0.0);
    } else if (type == PAW_TINT) {
        const paw_Int i = v_int(*sp);
        v_set_bool(sp, i != 0);
    }
}

void pawR_cast_int(paw_Env *P, paw_Type type)
{
    paw_assert(type < PAW_TSTRING);
    StackPtr sp = vm_peek(0);
    if (type == PAW_TFLOAT) {
        const paw_Float f = v_float(*sp);
        float2int(P, f, sp);
    }
}

void pawR_cast_float(paw_Env *P, paw_Type type)
{
    paw_assert(type < PAW_TSTRING);
    StackPtr sp = vm_peek(0);
    if (type != PAW_TFLOAT) {
        const paw_Int i = v_int(*sp);
        v_set_float(sp, (paw_Float)i);
    }
}

void pawR_to_bool(paw_Env *P, paw_Type type)
{
    Value *pv = vm_peek(0);
    v_set_bool(pv, pawV_truthy(*pv, type));
}

void pawR_to_int(paw_Env *P, paw_Type type)
{
    StackPtr sp = vm_peek(0);
    if (type == PAW_TSTRING) {
        const char *begin = v_text(*sp);
        const char *str = begin;
        const paw_Bool neg = consume_prefix(&str);
        if (pawV_parse_integer(P, str)) {
            pawR_error(P, PAW_ESYNTAX, "invalid integer '%s'", str);
        }
        if (neg) {
            sp = vm_peek(0); // new top
            const paw_Int i = v_int (*sp);
            if (i == PAW_INT_MIN) {
                pawR_error(P, PAW_EOVERFLOW, "%I has no positive representation", i);
            }
            v_set_int(sp, -i);
        }
        vm_shift(1);
    } else {
        pawR_cast_int(P, type);
    }
}

void pawR_to_float(paw_Env *P, paw_Type type)
{
    StackPtr sp = vm_peek(0);
    if (type == PAW_TSTRING) {
        const char *begin = v_text(*sp);
        const char *str = begin;
        paw_Bool neg = consume_prefix(&str);
        if (pawV_parse_float(P, str)) {
            pawR_error(P, PAW_ESYNTAX, "invalid float '%s'", begin);
        }
        if (neg) {
            sp = vm_peek(0); // new top
            const paw_Float f = v_float(*sp);
            v_set_float(sp, -f);
        }
        vm_shift(1);
    } else {
        pawR_cast_float(P, type);
    }
}

const char *pawR_to_string(paw_Env *P, paw_Type type, size_t *plen)
{
    Value v = *vm_peek(0);
    const char *out = pawV_to_string(P, v, type, plen);
    vm_shift(1);
    return out;
}

//static Value *find_attr(paw_Env *P, Value obj, Value name)
//{
//    Map *attr = NULL;
//    if (t_is_instance(obj)) {
//        attr = v_instance(obj)->attr;
//    } else if (t_is_foreign(obj)) {
//        attr = v_foreign(obj)->attr;
//    } else {
//        return NULL;
//    }
//    return pawH_get(P, attr, name);
//}

void pawR_read_global(paw_Env *P, int g)
{
    paw_assert(g < P->gv.size);
    const GlobalVar *global = &P->gv.data[g];
    vm_pushv(global->value);
}

void pawR_write_global(paw_Env *P, int g)
{
    GlobalVar *global = &P->gv.data[g];
    global->value = *vm_peek(0);
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

void pawR_setattr(paw_Env *P, int index)
{
    const Value val = *vm_peek(0);
    const Value obj = *vm_peek(1);

    Instance *ins = v_instance(obj);
    ins->attrs[1 + index] = val;
    vm_pop(2);
}

void pawR_setitem(paw_Env *P, paw_Type ttarget)
{
    const Value val = *vm_peek(0);
    const Value key = *vm_peek(1);
    const Value obj = *vm_peek(2);
    if (ttarget == PAW_TVECTOR) {
        const paw_Int idx = v_int(key);
        Value *slot = pawA_get(P, v_vector(obj), idx);
        *slot = val;
    } else {
        paw_assert(ttarget == PAW_TMAP);
        pawH_insert(P, v_map(obj), key, val);
    }
    vm_pop(3);
}

void pawR_init(paw_Env *P)
{
    String *errmsg = pawS_new_str(P, "not enough memory");
    pawG_fix_object(P, cast_object(errmsg));
    v_set_object(&P->mem_errmsg, errmsg);
}

#define stop_loop(i, i2, d) (((d) < 0 && (i) <= (i2)) || \
                             ((d) > 0 && (i) >= (i2)))

static paw_Bool fornum_init(paw_Env *P)
{
    const paw_Int begin = v_int(*vm_peek(2));
    const paw_Int end = v_int(*vm_peek(1));
    const paw_Int step = v_int(*vm_peek(0));
    if (step == 0) {
        pawR_error(P, PAW_ERUNTIME, "loop step equals 0");
    }
    const paw_Bool skip = stop_loop(begin, end, step);
    if (!skip) {
        v_set_int(vm_peek(2), begin);
        v_set_int(vm_peek(1), end);
        v_set_int(vm_peek(0), step);
        vm_pushi(begin);
    }
    return skip;
}

static paw_Bool fornum(paw_Env *P)
{
    const paw_Int itr = v_int(*vm_peek(2));
    const paw_Int step = v_int(*vm_peek(0));
    const paw_Int end = v_int(*vm_peek(1));
    const paw_Int next = itr + step;
    if (stop_loop(next, end, step)) {
        return PAW_FALSE;
    }
    v_set_int(vm_peek(2), next);
    vm_pushi(next);
    return PAW_TRUE;
}

static paw_Bool forin_init(paw_Env *P, paw_Type t)
{
//    const Value v = *vm_peek(0);
//    paw_Int itr = PAW_ITER_INIT;
//    if (t == PAW_TVECTOR) {
//        Vector *arr = v_vector(v);
//        if (pawA_iter(arr, &itr)) {
//            vm_pushi(itr);
//            vm_pushv(arr->begin[itr]);
//            return PAW_FALSE;
//        }
//    } else {
////        paw_assert(t == PAW_TMAP);
////        Map *map = v_map(v);
////        if (pawH_iter(map, &itr)) {
////            vm_pushi(itr);
////            vm_pushv(map->keys[itr]);
////            return PAW_FALSE;
////        }
//    }
//    return PAW_TRUE;
}

static paw_Bool forin(paw_Env *P, paw_Type t)
{
//    const Value obj = *vm_peek(1);
//    const Value itr = *vm_peek(0);
//    if (t == PAW_TVECTOR) {
//        Vector *arr = v_vector(obj);
//        paw_Int i = v_int(itr);
//        if (pawA_iter(arr, &i)) {
//            v_set_int(vm_peek(0), i);
//            vm_pushv(arr->begin[i]);
//            return PAW_TRUE;
//        }
//    } else {
////        paw_assert(t == PAW_TMAP);
////        Map *map = v_map(obj);
////        paw_Int i = v_int(itr);
////        if (pawH_iter(map, &i)) {
////            v_set_int(vm_peek(0), i);
////            vm_pushv(map->keys[i]);
////            return PAW_TRUE;
////        }
//    }
//    return PAW_FALSE; // stop the loop
}

#define finish_strcmp(x, y, op) (pawS_cmp(x, y) op 0)

static void string_binop(paw_Env *P, BinaryOp binop, Value lhs, Value rhs)
{
    const String *x = v_string(lhs);
    const String *y = v_string(rhs);
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
        default:
            paw_assert(binop == BINARY_GE);
            vm_pushb(finish_strcmp(x, y, >=));
    }
    vm_shift(2);
}

static void eq_ne(paw_Env *P, BinaryOp binop, paw_Type t, Value x, Value y)
{
    paw_Bool result;
    const paw_Bool bt = binop == BINARY_EQ;
    const paw_Bool bf = binop != BINARY_EQ;
    if (t == PAW_TVECTOR) {
        const Vector *lhs = v_vector(x);
        const Vector *rhs = v_vector(y);
        result = pawA_equals(P, lhs, rhs);
    } else if (t == PAW_TMAP) {
        Map *lhs = v_map(x);
        Map *rhs = v_map(y);
        result = pawH_equals(P, lhs, rhs);
    } else {
        // Fall back to comparing the value representation.
        result = x.u == y.u;
    }
    v_set_bool(vm_peek(1), result ? bt : bf);
    vm_pop(1);
}

#define i2u(i) ((uint64_t)(i))
#define u2i(u) paw_cast_int(u)

// Generate code for int operators
// Casts to unsigned to avoid UB (signed integer overflow). Negative
// numbers 'just work' on processors using 2's complement integers. 
#define i_unop(a, op) u2i(op i2u(a))
#define i_binop(a, b, op) u2i(i2u(a) op i2u(b))

static void int_binop(paw_Env *P, BinaryOp binop, paw_Int x, paw_Int y)
{
    paw_Int z = 0;
    switch (binop) {
        case BINARY_LT:
            z = x < y;
            break;
        case BINARY_LE:
            z = x <= y;
            break;
        case BINARY_GT:
            z = x > y;
            break;
        case BINARY_GE:
            z = x >= y;
            break;
        case BINARY_ADD:
            z = i_binop(x, y, +);
            break;
        case BINARY_SUB:
            z = i_binop(x, y, -);
            break;
        case BINARY_MUL:
            z = i_binop(x, y, *);
            break;
        case BINARY_DIV:
        case BINARY_MOD:
            if (y == 0) {
                pawR_error(P, PAW_ERUNTIME, "divide by 0");
            } else if (binop == BINARY_DIV) {
                z = i_binop(x, y, /);
            } else {
                z = i_binop(x, y, %);
            }
            break;
        case BINARY_BAND:
            z = i_binop(x, y, &);
            break;
        case BINARY_BOR:
            z = i_binop(x, y, |);
            break;
        case BINARY_BXOR:
            z = i_binop(x, y, ^);
            break;
        case BINARY_SHL:
            if (y < 0) {
                pawR_error(P, PAW_ERUNTIME, "negative shift count");
            } else if (y == 0) {
                z = x; // NOOP
            } else {
                y = paw_min(y, (int)(sizeof(x) * 8 - 1));
                z = paw_cast_int(i2u(x) << y);
            }
            break;
        default:
            paw_assert(binop == BINARY_SHR);
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
    }
    v_set_int(vm_peek(1), z);
    vm_pop(1);
}

#define finish_cmp(x, y, op) (v_set_bool(vm_peek(1), (x)op(y)), vm_pop(1))

static void float_binop(paw_Env *P, BinaryOp binop, paw_Float x, paw_Float y)
{
    Value *pv = vm_peek(1);
    switch (binop) {
        case BINARY_LT:
            finish_cmp(x, y, <);
            return;
        case BINARY_LE:
            finish_cmp(x, y, <=);
            return;
        case BINARY_GT:
            finish_cmp(x, y, >);
            return;
        case BINARY_GE:
            finish_cmp(x, y, >=);
            return;
        case BINARY_ADD:
            v_set_float(pv, x + y);
            break;
        case BINARY_SUB:
            v_set_float(pv, x - y);
            break;
        case BINARY_MUL:
            v_set_float(pv, x * y);
            break;
        default:
            if (y == 0) {
                pawR_error(P, PAW_ERUNTIME, "divide by 0");
            } else if (binop == BINARY_DIV) {
                v_set_float(pv, x / y);
            } else {
                paw_assert(binop == BINARY_MOD);
                v_set_float(pv, fmod(x, y));
            }
    }
    vm_pop(1);
}

static void other_binop(paw_Env *P, BinaryOp binop, paw_Type t, Value x, Value y)
{
    if (binop == BINARY_IN) {
        if (t == PAW_TVECTOR) {
            v_set_bool(vm_peek(1), pawA_contains(P, v_vector(y), x));
        } else {
            paw_assert(t == PAW_TMAP);
            v_set_bool(vm_peek(1), pawH_contains(P, v_map(y), x));
        }
        vm_pop(1);
    } else {
        paw_assert(t == PAW_TSTRING);
        string_binop(P, binop, x, y);
    }
}

static int binop_aux(paw_Env *P, BinaryOp binop, paw_Type t, Value x, Value y)
{
    if (binop == BINARY_EQ || binop == BINARY_NE) {
        eq_ne(P, binop, t, x, y);
    } else if (t == PAW_TINT) {
        int_binop(P, binop, v_int(x), v_int(y));
    } else if (t == PAW_TFLOAT) {
        float_binop(P, binop, v_float(x), v_float(y));
    } else {
        other_binop(P, binop, t, x, y);
    }
    return 0;
}

void pawR_binop(paw_Env *P, BinaryOp binop, paw_Type t)
{
    const Value x = *vm_peek(1);
    const Value y = *vm_peek(0);
    binop_aux(P, binop, t, x, y);
}

static void int_unop(paw_Env *P, UnaryOp unop, paw_Int i)
{
    Value *pv = vm_peek(0);
    switch (unop) {
        case UNARY_NEG:
            v_set_int(pv, i_unop(i, -));
            break;
        case UNARY_NOT:
            v_set_bool(pv, i_unop(i, !));
            break;
        default:
            paw_assert(unop == UNARY_BNOT);
            v_set_int(pv, i_unop(i, ~));
    }
}

static void float_unop(paw_Env *P, UnaryOp unop, paw_Float f)
{
    Value *pv = vm_peek(0);
    switch (unop) {
        case UNARY_NEG:
            v_set_float(pv, -f);
            break;
        default:
            paw_assert(unop == UNARY_NOT);
            v_set_bool(pv, !f);
    }
}

static void other_unop(paw_Env *P, UnaryOp unop, paw_Type t, Value x)
{
    if (unop == UNARY_LEN) {
        // Replace the container with its length.
        v_set_int(vm_peek(0), pawV_length(x, t));
    } else {
        paw_assert(unop == UNARY_NOT);
        // allows expressions like '!str'
        v_set_bool(vm_peek(0), !pawV_truthy(x, t));
    }
}

static void unop_aux(paw_Env *P, UnaryOp unop, paw_Type t, Value x)
{
    if (t == PAW_TINT) {
        int_unop(P, unop, v_int(x)); 
    } else if (t == PAW_TFLOAT) {
        float_unop(P, unop, v_float(x));
    } else {
        other_unop(P, unop, t, x);
    }
}

void pawR_unop(paw_Env *P, UnaryOp unop, paw_Type t)
{
    const Value x = *vm_peek(0);
    unop_aux(P, unop, t, x);
}

void pawR_getattr(paw_Env *P, int index)
{
    const Value obj = *vm_peek(0);

    Instance *ins = v_instance(obj);
    *vm_peek(0) = ins->attrs[1 + index];
}

static void getitem_vector(paw_Env *P, Value obj, Value key)
{
    Vector *a = v_vector(obj);
    const paw_Int i = v_int(key);
    *vm_peek(1) = *pawA_get(P, a, i);
    vm_pop(1);
}

static int getitem_map(paw_Env *P, Value obj, Value key)
{
    const Value *pv = pawH_get(P, v_map(obj), key);
    if (pv) {
        *vm_peek(1) = *pv;
        vm_pop(1);
        return 0;
    }
    return -1;
}

static void getitem_string(paw_Env *P, Value obj, Value key)
{
    paw_Int idx = v_int(key);
    String *str = v_string(obj);
    pawA_check_abs(P, idx, str->length);
    const char c = str->text[idx];
    String *res = pawS_new_nstr(P, &c, 1);
    v_set_object(vm_peek(1), res);
    vm_pop(1);
}

int pawR_getitem(paw_Env *P, paw_Type ttarget)
{
    const Value obj = *vm_peek(1);
    const Value key = *vm_peek(0);
    if (ttarget == PAW_TVECTOR) {
        getitem_vector(P, obj, key);
    } else if (ttarget == PAW_TMAP) {
        if (getitem_map(P, obj, key)) {
            return -1;
        }
    } else if (ttarget == PAW_TSTRING) {
        getitem_string(P, obj, key);
    }
    return 0;
}

void pawR_literal_vector(paw_Env *P, int n)
{
    Vector *v;
    StackPtr sp;
    vm_vector_init(v, sp);
    if (n > 0) {
        pawA_resize(P, v, cast_size(n));
        Value *pv = v->end;
        do {
            *--pv = *--sp;
        } while (pv != v->begin);
        // Replace contents with vector itself.
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

// TODO: 'null' -> Option[T]::None
//static paw_Bool should_jump_null(paw_Env *P)
//{
//    const Value *pv = vm_peek(0);
////    if (meta_single(P, MM_NULL, *pv)) {
////        if (v_is_null(*vm_peek(0))) {
////            vm_pop(1);
////            return PAW_TRUE;
////        }
////        vm_shift(1);
////        return PAW_FALSE;
////    }
//    return v_is_null(*pv);
//}

static paw_Bool should_jump_false(paw_Env *P)
{
    return !v_true(*vm_peek(0));
}

void pawR_execute(paw_Env *P, CallFrame *cf)
{
    Closure *fn;
    const OpCode *pc;
    const Value *K;
    Struct **C;

top:
    pc = cf->pc;
    fn = cf->fn;
    K = fn->p->k;
    C = fn->p->c;

    for (;;) {
        const OpCode opcode = *pc++;
        vm_switch(get_OP(opcode))
        {
            vm_case(POP) :
            {
                vm_pop(1);
            }

            vm_case(PUSHUNIT) :
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

            vm_case(PUSHSTRUCT) :
            {
                vm_pusho(C[get_U(opcode)]);
            }

            vm_case(UNOP) :
            {
                vm_protect();
                pawR_unop(P, get_A(opcode), get_B(opcode));
            }

            vm_case(BINOP) :
            {
                vm_protect();
                pawR_binop(P, get_A(opcode), get_B(opcode));
            }

            vm_case(NEWVECTOR) :
            {
                vm_protect();
                pawR_literal_vector(P, get_U(opcode));
                check_gc(P);
            }

            vm_case(NEWMAP) :
            {
                vm_protect();
                pawR_literal_map(P, get_U(opcode));
                check_gc(P);
            }

            vm_case(CASTBOOL) :
            {
                pawR_cast_bool(P, get_U(opcode));
            }

            vm_case(CASTINT) :
            {
                pawR_cast_int(P, get_U(opcode));
            }

            vm_case(CASTFLOAT) :
            {
                pawR_cast_float(P, get_U(opcode));
            }

            vm_case(NEWINSTANCE) :
            {
                vm_protect();
                const int nfields = get_U(opcode);
                Value *pv = vm_peek(0);
                Struct *struct_ = v_struct(*pv);
                Instance *ins = pawV_new_instance(P, 1 + nfields);
                v_set_object(ins->attrs, struct_);
                v_set_object(pv, ins); // replace Struct
                check_gc(P);
            }

            vm_case(INITFIELD) :
            {
                vm_protect();
                const int u = get_U(opcode);
                Instance *ins = v_instance(*vm_peek(1));
                ins->attrs[1 + u] = *vm_peek(0);
                vm_pop(1);
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
                const int u = get_U(opcode);
                pawR_read_global(P, u);
            }

            vm_case(SETGLOBAL) :
            {
                const int u = get_U(opcode);
                pawR_write_global(P, u);
            }

            vm_case(GETATTR) :
            {
                vm_protect();
                const int u = get_U(opcode);
                pawR_getattr(P, u);
            }

            vm_case(SETATTR) :
            {
                vm_protect();
                const int u = get_U(opcode);
                pawR_setattr(P, u);
            }

            vm_case(GETITEM) :
            {
                vm_protect();
                if (pawR_getitem(P, get_A(opcode))) {
                    pawH_key_error(P, *vm_peek(0), PAW_TSTRING); // TODO: lookup key type
                }
            }

            vm_case(SETITEM) :
            {
                vm_protect();
                pawR_setitem(P, get_U(opcode));
            }

            vm_case(CLOSE) :
            {
                pawR_close_upvalues(P, vm_peek(1));
                vm_pop(1);
            }

            vm_case(CLOSURE) :
            {
                vm_protect();
                Value *pv = vm_push0();
                Proto *proto = fn->p->p[get_U(opcode)];
                Closure *closure = pawV_new_closure(P, proto->nup);
                v_set_object(pv, closure);
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

            vm_case(CALL) :
            {
                const uint8_t argc = get_U(opcode);
                StackPtr ptr = vm_peek(argc);
                vm_save();

                CallFrame *callee = pawC_precall(P, ptr, v_object(*ptr), argc);
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

//            vm_case(VARARG) :
//            {
//                vm_protect();
//                // must be run immediately after OP_CALL
//                const int nexpect = get_U(opcode);
//                const int nactual = vm_argc();
//                const int nextra = nactual - nexpect;
//                Value *pv;
//                Vector *argv;
//                vm_vector_init(argv, pv);
//                if (nextra) {
//                    pawA_resize(P, argv, cast_size(nextra));
//                    StackPtr argv0 = cf->base.p + 1 + nexpect;
//                    for (int i = 0; i < nextra; ++i) {
//                        argv->begin[i] = argv0[i];
//                    }
//                    // replace first variadic parameter with 'argv' vector
//                    vm_shift(nextra);
//                }
//                check_gc(P);
//            }
//
            vm_case(JUMP) :
            {
                pc += get_S(opcode);
            }

    //        vm_case(JUMPNULL) :
    //        {
    //            if (should_jump_null(P)) {
    //                pc += get_S(opcode);
    //            }
    //        }

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
                if (forin_init(P, get_U(opcode))) {
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
                if (forin(P, get_U(opcode))) {
                    pc += get_S(opcode); // continue
                }
            }

        vm_default:
            paw_assert(PAW_FALSE);
        }
    }
}
