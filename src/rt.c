// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#include "prefix.h"

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
#define vm_shift(n) (*vm_top((n) + 1) = *vm_top(1), vm_pop(n))
#define vm_pop(n) pawC_stkdec(P, n)
#define vm_top(n) (&P->top.p[-(n)])
#define vm_save() (vm_protect(), cf->top = P->top)
#define vm_protect() (cf->pc = pc)
#define vm_upvalue(u) (fn->up[u]->p.p)
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
// the GC doesn't get confused. Both the vm_push0(), and the pawV_vec_new calls
// might fail and cause an error to be thrown, so we have to be careful not
// to leave a junk value on top of the stack.
#define vm_vector_init(pa, pv) \
    pv = vm_push0();           \
    pa = pawV_vec_new(P);      \
    v_set_object(pv, pa);

#define vm_map_init(dm, pv) \
    pv = vm_push0();        \
    dm = pawH_new(P);       \
    v_set_object(pv, dm);

static void add_zeros(paw_Env *P, int n)
{
    for (int i = 0; i < n; ++i) {
        vm_push0();
    }
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

static void add_3_parts(paw_Env *P, const char *before, const char *value,
                        const char *after)
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
#define float2int_aux(f, pv)                                               \
    ((f) >= (paw_Float)(PAW_INT_MIN) && (f) < -(paw_Float)(PAW_INT_MIN) && \
     (v_set_int(pv, paw_cast_int(f)), 1))

static void float2int(paw_Env *P, paw_Float f, Value *pv)
{
    if (!float2int_aux(f, pv)) {
        pawR_error(P, PAW_EOVERFLOW, "float %f is too large", f);
    }
}

void pawR_cast_bool(paw_Env *P, paw_Type type)
{
    paw_assert(type < PAW_TSTRING);
    paw_unused(type);

    Value *pv = vm_top(1);
    v_set_bool(pv, pv->u != 0);
}

void pawR_cast_int(paw_Env *P, paw_Type type)
{
    paw_assert(type < PAW_TSTRING);
    if (type == PAW_TFLOAT) {
        // NOTE: Other primitives have a value representation compatible with
        //       that of the 'int' type.
        Value *pv = vm_top(1);
        const paw_Float f = v_float(*pv);
        float2int(P, f, pv);
    }
}

void pawR_cast_float(paw_Env *P, paw_Type type)
{
    paw_assert(type < PAW_TSTRING);
    if (type != PAW_TFLOAT) {
        Value *pv = vm_top(1);
        const paw_Int i = v_int(*pv);
        v_set_float(pv, cast(i, paw_Float));
    }
}

void pawR_to_bool(paw_Env *P, paw_Type type)
{
    Value *pv = vm_top(1);
    v_set_bool(pv, pawV_truthy(*pv, type));
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

void pawR_to_int(paw_Env *P, paw_Type type)
{
    StackPtr sp = vm_top(1);
    if (type == PAW_TSTRING) {
        const char *begin = v_text(*sp);
        const char *str = begin;
        const paw_Bool neg = consume_prefix(&str);
        const int rc = pawV_parse_uint64(P, str);
        if (rc == PAW_ESYNTAX) {
            pawR_error(P, PAW_ESYNTAX, "invalid integer '%s'", str);
        } else if (rc == PAW_EOVERFLOW) {
            goto handle_overflow;
        }
        sp = vm_top(1); // new top
        const uint64_t u = sp->u;
        if (u > PAW_INT_MAX) {
            if (!neg || u - 1 > PAW_INT_MAX) {
handle_overflow:
                pawR_error(P, PAW_EOVERFLOW, "integer '%s' is out of range", str);
            } 
            v_set_int(sp, PAW_INT_MIN);
        } else if (neg) {
            v_set_int(sp, -paw_cast_int(u));
        }
        vm_shift(1);
    } else {
        pawR_cast_int(P, type);
    }
}

void pawR_to_float(paw_Env *P, paw_Type type)
{
    StackPtr sp = vm_top(1);
    if (type == PAW_TSTRING) {
        const char *begin = v_text(*sp);
        const char *str = begin;
        paw_Bool neg = consume_prefix(&str);
        if (pawV_parse_float(P, str)) {
            pawR_error(P, PAW_ESYNTAX, "invalid float '%s'", begin);
        }
        if (neg) {
            sp = vm_top(1); // new top
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
    Value v = *vm_top(1);
    const char *out = pawV_to_string(P, v, type, plen);
    vm_shift(1);
    return out;
}

void pawR_read_global(paw_Env *P, int g)
{
    paw_assert(g < P->gv.size);
    const GlobalVar *global = &P->gv.data[g];
    vm_pushv(global->value);
}

void pawR_write_global(paw_Env *P, int g)
{
    GlobalVar *global = &P->gv.data[g];
    global->value = *vm_top(1);
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

void pawR_settuple(paw_Env *P, int index)
{
    const Value val = *vm_top(1);
    const Value obj = *vm_top(2);
    v_tuple(obj)->elems[index] = val;
    vm_pop(2);
}

void pawR_setattr(paw_Env *P, int index)
{
    const Value val = *vm_top(1);
    const Value obj = *vm_top(2);
    Instance *ins = v_instance(obj);
    ins->attrs[index] = val;
    vm_pop(2);
}

void pawR_setitem(paw_Env *P, paw_Type t)
{
    const Value val = *vm_top(1);
    const Value key = *vm_top(2);
    const Value obj = *vm_top(3);
    if (t == PAW_TVECTOR) {
        const paw_Int index = v_int(key);
        Value *pval = pawV_vec_get(P, v_vector(obj), index);
        *pval = val;
    } else {
        paw_assert(t == PAW_TMAP);
        pawH_insert(P, v_map(obj), key, val);
    }
    vm_pop(3);
}

static size_t check_index(paw_Env *P, paw_Int index, size_t length,
                          const char *what)
{
    const paw_Int n = paw_cast_int(length);
    index = pawV_abs_index(index, length);
    if (index < 0 || index > n) {
        pawE_error(P, PAW_ERUNTIME, -1,
                   "index %I is out of bounds for %s of length %I", index, what,
                   paw_cast_int(length));
    }
    return cast_size(index);
}

static void setslice_vector(paw_Env *P, Vector *va, paw_Int i, paw_Int j,
                            const Vector *vb)
{
    const size_t na = pawV_vec_length(va);
    const size_t nb = pawV_vec_length(vb);
    const size_t zi = check_index(P, i, na, "vector");
    const size_t zj = check_index(P, j, na, "vector");

    if (va == vb) {
        Value *pv = vm_top(1);
        paw_assert(pv->p == vb);
        vb = pawV_vec_clone(P, pv, vb);
    }

    const size_t szelem = sizeof(va->begin[0]);
    const size_t nelems = na - zj + zi + nb;
    pawV_vec_reserve(P, va, nelems);

    Value *gap = va->begin + zi;
    memmove(gap + nb, va->begin + zj, (na - zj) * szelem);
    memcpy(gap, vb->begin, nb * szelem);
    pawV_vec_resize(P, va, nelems);
}

// setslice([a1..an], i, j, [b1..bn]) => [a1..ai b1..bn aj..an]
void pawR_setslice(paw_Env *P, paw_Type t)
{
    const Value va = *vm_top(4);
    const paw_Int i = v_int(*vm_top(3));
    const paw_Int j = v_int(*vm_top(2));
    const Value vb = *vm_top(1);

    paw_assert(t == PAW_TVECTOR);
    setslice_vector(P, v_vector(va), i, j, v_vector(vb));

    vm_pop(4);
}

void pawR_init(paw_Env *P)
{
    String *errmsg = pawS_new_str(P, "not enough memory");
    pawG_fix_object(P, cast_object(errmsg));
    v_set_object(&P->mem_errmsg, errmsg);
}

#define stop_loop(i, i2, d) \
    (((d) < 0 && (i) <= (i2)) || ((d) > 0 && (i) >= (i2)))

static paw_Bool fornum_init(paw_Env *P)
{
    const paw_Int begin = v_int(*vm_top(3));
    const paw_Int end = v_int(*vm_top(2));
    const paw_Int step = v_int(*vm_top(1));
    if (step == 0) {
        pawR_error(P, PAW_ERUNTIME, "loop step equals 0");
    }
    const paw_Bool skip = stop_loop(begin, end, step);
    if (!skip) {
        v_set_int(vm_top(3), begin);
        v_set_int(vm_top(2), end);
        v_set_int(vm_top(1), step);
        vm_pushi(begin);
    }
    return skip;
}

static paw_Bool fornum(paw_Env *P)
{
    const paw_Int itr = v_int(*vm_top(3));
    const paw_Int step = v_int(*vm_top(1));
    const paw_Int end = v_int(*vm_top(2));
    const paw_Int next = itr + step;
    if (stop_loop(next, end, step)) {
        return PAW_FALSE;
    }
    v_set_int(vm_top(3), next);
    vm_pushi(next);
    return PAW_TRUE;
}

static paw_Bool forvector_init(paw_Env *P)
{
    const Value v = *vm_top(1);
    paw_Int itr = PAW_ITER_INIT;
    Vector *arr = v_vector(v);
    if (pawV_vec_iter(arr, &itr)) {
        vm_pushi(itr);
        vm_pushv(arr->begin[itr]);
        return PAW_FALSE;
    }
    return PAW_TRUE;
}

static paw_Bool forvector(paw_Env *P)
{
    const Value obj = *vm_top(2);
    const Value itr = *vm_top(1);
    Vector *arr = v_vector(obj);
    paw_Int i = v_int(itr);
    if (pawV_vec_iter(arr, &i)) {
        v_set_int(vm_top(1), i);
        vm_pushv(arr->begin[i]);
        return PAW_TRUE;
    }
    return PAW_FALSE;
}

static paw_Bool formap_init(paw_Env *P)
{
    const Value v = *vm_top(1);
    paw_Int itr = PAW_ITER_INIT;
    Map *map = v_map(v);
    if (pawH_iter(map, &itr)) {
        const Value v = *pawH_key(map, cast_size(itr));
        vm_pushi(itr);
        vm_pushv(v);
        return PAW_FALSE;
    }
    return PAW_TRUE;
}

static paw_Bool formap(paw_Env *P)
{
    const Value obj = *vm_top(2);
    const Value itr = *vm_top(1);
    Map *map = v_map(obj);
    paw_Int i = v_int(itr);
    if (pawH_iter(map, &i)) {
        const Value v = *pawH_key(map, cast_size(i));
        v_set_int(vm_top(1), i);
        vm_pushv(v);
        return PAW_TRUE;
    }
    return PAW_FALSE; // stop the loop
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

static void vector_binop(paw_Env *P, BinaryOp binop, Value lhs, Value rhs)
{
    const Vector *x = v_vector(lhs);
    const Vector *y = v_vector(rhs);
    paw_assert(binop == BINARY_ADD);

    Value *pv;
    Vector *z;
    vm_vector_init(z, pv);

    const size_t zelem = sizeof(z->begin[0]);
    const size_t nx = pawV_vec_length(x);
    const size_t ny = pawV_vec_length(y);
    pawV_vec_resize(P, z, nx + ny);
    if (nx > 0) {
        memcpy(z->begin, x->begin, nx * zelem);
    }
    if (ny > 0) {
        memcpy(z->begin + nx, y->begin, ny * zelem);
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
        result = pawV_vec_equals(P, lhs, rhs);
    } else if (t == PAW_TMAP) {
        Map *lhs = v_map(x);
        Map *rhs = v_map(y);
        result = pawH_equals(P, lhs, rhs);
    } else {
        // Fall back to comparing the value representation.
        result = x.u == y.u;
    }
    v_set_bool(vm_top(2), result ? bt : bf);
    vm_pop(1);
}

#define i2u(i) (cast(i, uint64_t))
#define u2i(u) paw_cast_int(u)

// Generate code for int operators
// Casts to unsigned to avoid UB (signed integer overflow). Requires
// 2's complement integer representation to work properly.
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
                y = paw_min(y, cast(sizeof(x) * 8 - 1, int));
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
                y = paw_min(y, cast(sizeof(x) * 8 - 1, int));
                z = x >> y;
            }
    }
    v_set_int(vm_top(2), z);
    vm_pop(1);
}

#define finish_cmp(x, y, op) (v_set_bool(vm_top(2), (x)op(y)), vm_pop(1))

static void float_binop(paw_Env *P, BinaryOp binop, paw_Float x, paw_Float y)
{
    Value *pv = vm_top(2);
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

static void other_binop(paw_Env *P, BinaryOp binop, paw_Type t, Value x,
                        Value y)
{
    if (binop == BINARY_IN) {
        Value *pv = vm_top(2);
        if (t == PAW_TVECTOR) {
            v_set_bool(pv, pawV_vec_contains(P, v_vector(y), x));
        } else {
            paw_assert(t == PAW_TMAP);
            v_set_bool(pv, pawH_contains(P, v_map(y), x));
        }
        vm_pop(1);
    } else if (t == PAW_TSTRING) {
        string_binop(P, binop, x, y);
    } else {
        paw_assert(t == PAW_TVECTOR);
        vector_binop(P, binop, x, y);
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
    const Value x = *vm_top(2);
    const Value y = *vm_top(1);
    binop_aux(P, binop, t, x, y);
}

static void int_unop(paw_Env *P, UnaryOp unop, paw_Int i)
{
    Value *pv = vm_top(1);
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
    Value *pv = vm_top(1);
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
        v_set_int(vm_top(1), pawV_length(x, t));
    } else {
        paw_assert(unop == UNARY_NOT);
        v_set_bool(vm_top(1), !pawV_truthy(x, t));
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
    const Value x = *vm_top(1);
    unop_aux(P, unop, t, x);
}

void pawR_gettuple(paw_Env *P, int index)
{
    Tuple *tup = v_tuple(*vm_top(1));
    *vm_top(1) = tup->elems[index];
}

void pawR_getattr(paw_Env *P, int index)
{
    Instance *ins = v_instance(*vm_top(1));
    *vm_top(1) = ins->attrs[index];
}

static void getitem_vector(paw_Env *P, Vector *vector, paw_Int index)
{
    *vm_top(2) = *pawV_vec_get(P, vector, index);
    vm_pop(1);
}

static int getitem_map(paw_Env *P, Map *map, Value key)
{
    const Value *pv = pawH_get(P, map, key);
    if (pv) {
        *vm_top(2) = *pv;
        vm_pop(1);
        return 0;
    }
    return -1;
}

static void getitem_string(paw_Env *P, const String *string, paw_Int index)
{
    pawV_check_abs(P, index, string->length);
    const char c = string->text[index];
    String *res = pawS_new_nstr(P, &c, 1);
    v_set_object(vm_top(2), res);
    vm_pop(1);
}

int pawR_getitem(paw_Env *P, paw_Type t)
{
    const Value obj = *vm_top(2);
    const Value key = *vm_top(1);
    if (t == PAW_TMAP) {
        return getitem_map(P, v_map(obj), key);
    }
    if (t == PAW_TVECTOR) {
        getitem_vector(P, v_vector(obj), v_int(key));
    } else if (t == PAW_TSTRING) {
        getitem_string(P, v_string(obj), v_int(key));
    }
    return 0;
}

static void getslice_vector(paw_Env *P, Vector *vec, paw_Int i, paw_Int j)
{
    const size_t n = pawV_vec_length(vec);
    const size_t zi = check_index(P, i, n, "vector");
    const size_t zj = check_index(P, j, n, "vector");

    Value *pv;
    Vector *slice;
    vm_vector_init(slice, pv);

    const size_t nelems = zj - zi;
    pawV_vec_resize(P, slice, nelems);
    memcpy(slice->begin, vec->begin + zi, nelems * sizeof(vec->begin[0]));
}

static void getslice_string(paw_Env *P, String *str, paw_Int i, paw_Int j)
{
    const size_t n = pawS_length(str);
    const size_t zi = check_index(P, i, n, "string");
    const size_t zj = check_index(P, j, n, "string");

    Value *pv = vm_push0();
    const size_t nbytes = zj - zi;
    String *slice = pawS_new_nstr(P, str->text + zi, nbytes);
    v_set_object(pv, slice);
}

void pawR_getslice(paw_Env *P, paw_Type t)
{
    const Value obj = *vm_top(3);
    const paw_Int i = v_int(*vm_top(2));
    const paw_Int j = v_int(*vm_top(1));
    if (t == PAW_TVECTOR) {
        getslice_vector(P, v_vector(obj), i, j);
    } else {
        paw_assert(t == PAW_TSTRING);
        getslice_string(P, v_string(obj), i, j);
    }
    vm_shift(3);
}

void pawR_literal_tuple(paw_Env *P, int n)
{
    Value *pv = vm_push0();
    Tuple *tuple = pawV_new_tuple(P, n);
    v_set_object(pv, tuple);

    Value *dst = tuple->elems + n;
    const Value *src = vm_top(1);
    for (int i = 0; i < n; ++i) {
        *--dst = *--src;
    }
    vm_shift(n);
}

void pawR_literal_vector(paw_Env *P, int n)
{
    Vector *v;
    StackPtr sp;
    vm_vector_init(v, sp);
    if (n > 0) {
        pawV_vec_resize(P, v, cast_size(n));
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

static void new_variant(paw_Env *P, int k, int nfields)
{
    Value *pv = vm_push0();
    Variant *var = pawV_new_variant(P, k, nfields);
    v_set_object(pv, var);
    for (int i = 0; i < nfields; ++i) {
        var->fields[i] = P->top.p[i - nfields - 1];
    }
    vm_shift(nfields);
}

static void unpack_variant(paw_Env *P, int n)
{
    const Variant *v = v_variant(*vm_top(1));
    add_zeros(P, n - 1);
    for (int i = 0; i < n; ++i) {
        *vm_top(n - i + 1) = v->fields[i];
    }
}

static void unpack_instance(paw_Env *P, int n)
{
    const Instance *s = v_instance(*vm_top(1));
    add_zeros(P, n - 1);
    for (int i = 0; i < n; ++i) {
        *vm_top(n - i + 1) = s->attrs[i];
    }
}

static void unpack_tuple(paw_Env *P, int n)
{
    const Tuple *t = v_tuple(*vm_top(1));
    add_zeros(P, n - 1);
    for (int i = 0; i < n; ++i) {
        *vm_top(n - i + 1) = t->elems[i];
    }
}

void pawR_execute(paw_Env *P, CallFrame *cf)
{
    const Value *K;
    const OpCode *pc;
    Closure *fn;

top:
    pc = cf->pc;
    fn = cf->fn;
    K = fn->p->k;

    for (;;) {
        const OpCode opcode = *pc++;

        // clang-format off
        vm_switch(get_OP(opcode))
        {
            vm_case(POP) :
            {
                vm_pop(get_U(opcode));
            }

            vm_case(CLOSE) :
            {
                const int u = get_U(opcode);
                pawR_close_upvalues(P, vm_top(u));
                vm_pop(u);
            }

            vm_case(COPY) :
            {
                const int u = get_U(opcode);
                vm_pushv(*vm_top(u + 1));
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

            vm_case(NEWTUPLE) :
            {
                vm_protect();
                pawR_literal_tuple(P, get_U(opcode));
                check_gc(P);
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

            vm_case(MATCHVARIANT) :
            {
                const Variant *var = v_variant(*vm_top(1));
                vm_pushb(var->k == get_U(opcode));
            }

            vm_case(UNPACKVARIANT) :
            {
                vm_protect();
                unpack_variant(P, get_U(opcode));
            }

            vm_case(UNPACKINSTANCE) :
            {
                vm_protect();
                unpack_instance(P, get_U(opcode));
            }

            vm_case(UNPACKTUPLE) :
            {
                vm_protect();
                unpack_tuple(P, get_U(opcode));
            }

            vm_case(NEWVARIANT) :
            {
                vm_protect();
                new_variant(P, get_A(opcode), get_B(opcode));
                check_gc(P);
            }

            vm_case(NEWINSTANCE) :
            {
                vm_protect();
                Value *pv = vm_push0();
                Instance *ins = pawV_new_instance(P, get_U(opcode));
                v_set_object(pv, ins);
                check_gc(P);
            }

            vm_case(INITFIELD) :
            {
                vm_protect();
                const int u = get_U(opcode);
                Instance *ins = v_instance(*vm_top(2));
                ins->attrs[u] = *vm_top(1);
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
                *plocal = *vm_top(1);
                vm_pop(1);
            }

            vm_case(GETUPVALUE) :
            {
                const int u = get_U(opcode);
                const Value upval = *vm_upvalue(u);
                vm_pushv(upval);
            }

            vm_case(SETUPVALUE) :
            {
                const int u = get_U(opcode);
                Value *pupval = vm_upvalue(u);
                *pupval = *vm_top(1);
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

            vm_case(GETTUPLE) :
            {
                vm_protect();
                pawR_gettuple(P, get_U(opcode));
            }

            vm_case(GETATTR) :
            {
                vm_protect();
                pawR_getattr(P, get_U(opcode));
            }

            vm_case(SETTUPLE) :
            {
                vm_protect();
                pawR_settuple(P, get_U(opcode));
            }

            vm_case(SETATTR) :
            {
                vm_protect();
                pawR_setattr(P, get_U(opcode));
            }

            vm_case(GETITEM) :
            {
                vm_protect();
                if (pawR_getitem(P, get_U(opcode))) {
                    pawH_key_error(P, *vm_top(1), PAW_TSTRING); // TODO: lookup key type
                }
            }

            vm_case(SETITEM) :
            {
                vm_protect();
                pawR_setitem(P, get_U(opcode));
            }

            vm_case(GETSLICE) :
            {
                vm_protect();
                pawR_getslice(P, get_U(opcode));
            }

            vm_case(SETSLICE) :
            {
                vm_protect();
                pawR_setslice(P, get_U(opcode));
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
                StackPtr ptr = vm_top(argc + 1);
                vm_save();

                CallFrame *callee = pawC_precall(P, ptr, v_object(*ptr), argc);
                if (callee) {
                    cf = callee;
                    goto top;
                }
            }

            vm_case(TRANSIT) :
            {
                const int u = get_U(opcode);
                const Value v = *vm_top(1);
                vm_pop(u - 1);

                pawR_close_upvalues(P, vm_top(1));
                *vm_top(1) = v;
            }

            vm_case(RETURN) :
            {
                const Value result = *vm_top(1);
                vm_pop(1);

                P->top.p = cf_stack_return(cf);
                vm_save();

                pawR_close_upvalues(P, vm_top(1));
                vm_pushv(result);
                P->cf = cf->prev;
                if (cf_is_entry(cf)) {
                    return;
                }
                cf = P->cf;
                goto top;
            }

            vm_case(JUMP) :
            {
                pc += get_S(opcode);
            }

            vm_case(JUMPNULL) :
            {
                const Variant *var = v_variant(*vm_top(1));
                if (var->k == 0) {
                    // jump over the OP_RETURN and unpack the value
                    *vm_top(1) = var->fields[0];
                    pc += get_S(opcode);
                }
            }

            vm_case(JUMPFALSE) :
            {
                if (!v_true(*vm_top(1))) {
                    pc += get_S(opcode);
                }
            }

            vm_case(JUMPFALSEPOP) :
            {
                if (!v_true(*vm_top(1))) {
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

// clang-format off
#define vm_forin0(t, T) \
            vm_case(FOR##T##0) : \
            { \
                vm_protect(); \
                if (for##t##_init(P)) { \
                    vm_push0(); \
                    pc += get_S(opcode); \
                } \
            }
#define vm_forin(t, T) \
            vm_case(FOR##T) : \
            { \
                if (for##t(P)) { \
                    pc += get_S(opcode); \
                } \
            }
            vm_forin0(vector, VECTOR)
            vm_forin0(map, MAP)
            vm_forin(vector, VECTOR)
            vm_forin(map, MAP)
#undef vm_forin0
#undef vm_forin
                // clang-format on

                vm_default : paw_assert(PAW_FALSE);
        }
        // clang-format on
    }
}
