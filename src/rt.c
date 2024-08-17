// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#include "prefix.h"

#include "env.h"
#include "call.h"
#include "gc.h"
#include "map.h"
#include "value.h"
#include <math.h>

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
#define vm_pusho(o) pawC_pusho(P, CAST_OBJECT(o))

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
    V_SET_OBJECT(pv, pa);

#define vm_map_init(dm, pv) \
    pv = vm_push0();        \
    dm = pawH_new(P);       \
    V_SET_OBJECT(pv, dm);

static void add_zeros(paw_Env *P, int n)
{
    for (int i = 0; i < n; ++i) {
        vm_push0();
    }
}

static int current_line(const CallFrame *cf)
{
    Proto *p = cf->fn->p;
    const int pc = CAST(cf->pc - p->source - 1, int);

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
    add_3_parts(P, "name '", V_TEXT(name), "' is not defined");
    pawC_throw(P, PAW_ENAME);
}

void pawR_attr_error(paw_Env *P, Value name)
{
    add_3_parts(P, "attribute '", V_TEXT(name), "' does not exist");
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
#define FLOAT2INT_AUX(f, pv) \
    ((f) >= CAST(PAW_INT_MIN, paw_Float) && \
     (f) < -CAST(PAW_INT_MIN, paw_Float) && \
     (V_SET_INT(pv, PAW_CAST_INT(f)), 1))

static void float2int(paw_Env *P, paw_Float f, Value *pv)
{
    if (!FLOAT2INT_AUX(f, pv)) {
        pawR_error(P, PAW_EOVERFLOW, "float %f is too large", f);
    }
}

void pawR_cast_bool(paw_Env *P, paw_Type type)
{
    paw_assert(type < PAW_TSTRING);
    paw_unused(type);

    Value *pv = vm_top(1);
    V_SET_BOOL(pv, pv->u != 0);
}

void pawR_cast_int(paw_Env *P, paw_Type type)
{
    paw_assert(type < PAW_TSTRING);
    if (type == PAW_TFLOAT) {
        // NOTE: Other primitives have a value representation compatible with
        //       that of the 'int' type.
        Value *pv = vm_top(1);
        const paw_Float f = V_FLOAT(*pv);
        float2int(P, f, pv);
    }
}

void pawR_cast_float(paw_Env *P, paw_Type type)
{
    paw_assert(type < PAW_TSTRING);
    if (type != PAW_TFLOAT) {
        Value *pv = vm_top(1);
        const paw_Int i = V_INT(*pv);
        V_SET_FLOAT(pv, CAST(i, paw_Float));
    }
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
    while (next != NULL && upv_level(next) > local) {
        assert(upv_is_open(next));
        prev = next;
        next = next->open.next;
    }

    if (next != NULL && upv_level(next) == local) {
        return next;
    }

    UpValue *new_up = pawV_new_upvalue(P);
    pawV_link_upvalue(P, new_up, prev, next);
    new_up->p.p = local;
    return new_up;
}

void pawR_close_upvalues(paw_Env *P, const StackPtr top)
{
    while (P->up_list != NULL && upv_level(P->up_list) >= top) {
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
    V_TUPLE(obj)->elems[index] = val;
    vm_pop(2);
}

void pawR_setattr(paw_Env *P, int index)
{
    const Value val = *vm_top(1);
    const Value obj = *vm_top(2);
    Instance *ins = V_INSTANCE(obj);
    ins->attrs[index] = val;
    vm_pop(2);
}

void pawR_setitem(paw_Env *P, paw_Type t)
{
    const Value val = *vm_top(1);
    const Value key = *vm_top(2);
    const Value obj = *vm_top(3);
    if (t == PAW_TVECTOR) {
        const paw_Int index = V_INT(key);
        Value *pval = pawV_vec_get(P, V_VECTOR(obj), index);
        *pval = val;
    } else {
        paw_assert(t == PAW_TMAP);
        pawH_insert(P, V_MAP(obj), key, val);
    }
    vm_pop(3);
}

static size_t check_slice_bound(paw_Env *P, paw_Int index, size_t length, const char *what, const char *cont)
{
    const paw_Int n = PAW_CAST_INT(length);
    index = pawV_abs_index(index, length);
    if (index < 0 || index > n) {
        pawE_error(P, PAW_ERUNTIME, -1,
                   "slice %s index %I is out of bounds for %s of length %I", 
                   what, index, cont, PAW_CAST_INT(length));
    }
    return CAST_SIZE(index);
}

static void setslice_vector(paw_Env *P, Vector *va, paw_Int i, paw_Int j,
                            const Vector *vb)
{
    const size_t na = pawV_vec_length(va);
    const size_t nb = pawV_vec_length(vb);
    const size_t zi = check_slice_bound(P, i, na, "start", "vector");
    const size_t zj = check_slice_bound(P, j, na, "end", "vector");

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
    const paw_Int i = V_INT(*vm_top(3));
    const paw_Int j = V_INT(*vm_top(2));
    const Value vb = *vm_top(1);

    paw_assert(t == PAW_TVECTOR);
    setslice_vector(P, V_VECTOR(va), i, j, V_VECTOR(vb));

    vm_pop(4);
}

void pawR_init(paw_Env *P)
{
    String *errmsg = pawS_new_str(P, "not enough memory");
    pawG_fix_object(P, CAST_OBJECT(errmsg));
    V_SET_OBJECT(&P->mem_errmsg, errmsg);
}

#define stop_loop(i, i2, d) \
    (((d) < 0 && (i) <= (i2)) || ((d) > 0 && (i) >= (i2)))

static paw_Bool fornum_init(paw_Env *P)
{
    const paw_Int begin = V_INT(*vm_top(3));
    const paw_Int end = V_INT(*vm_top(2));
    const paw_Int step = V_INT(*vm_top(1));
    if (step == 0) {
        pawR_error(P, PAW_ERUNTIME, "loop step equals 0");
    }
    const paw_Bool skip = stop_loop(begin, end, step);
    if (!skip) {
        V_SET_INT(vm_top(3), begin);
        V_SET_INT(vm_top(2), end);
        V_SET_INT(vm_top(1), step);
        vm_pushi(begin);
    }
    return skip;
}

static paw_Bool fornum(paw_Env *P)
{
    const paw_Int itr = V_INT(*vm_top(3));
    const paw_Int step = V_INT(*vm_top(1));
    const paw_Int end = V_INT(*vm_top(2));
    const paw_Int next = itr + step;
    if (stop_loop(next, end, step)) {
        return PAW_FALSE;
    }
    V_SET_INT(vm_top(3), next);
    vm_pushi(next);
    return PAW_TRUE;
}

static paw_Bool forvector_init(paw_Env *P)
{
    const Value v = *vm_top(1);
    paw_Int itr = PAW_ITER_INIT;
    Vector *arr = V_VECTOR(v);
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
    Vector *arr = V_VECTOR(obj);
    paw_Int i = V_INT(itr);
    if (pawV_vec_iter(arr, &i)) {
        V_SET_INT(vm_top(1), i);
        vm_pushv(arr->begin[i]);
        return PAW_TRUE;
    }
    return PAW_FALSE;
}

static paw_Bool formap_init(paw_Env *P)
{
    const Value v = *vm_top(1);
    paw_Int itr = PAW_ITER_INIT;
    Map *map = V_MAP(v);
    if (pawH_iter(map, &itr)) {
        const Value v = *pawH_key(map, CAST_SIZE(itr));
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
    Map *map = V_MAP(obj);
    paw_Int i = V_INT(itr);
    if (pawH_iter(map, &i)) {
        const Value v = *pawH_key(map, CAST_SIZE(i));
        V_SET_INT(vm_top(1), i);
        vm_pushv(v);
        return PAW_TRUE;
    }
    return PAW_FALSE; // stop the loop
}

#define finish_strcmp(x, y, op) (pawS_cmp(x, y) op 0)

static void string_binop(paw_Env *P, BinaryOp binop, Value lhs, Value rhs)
{
    const String *x = V_STRING(lhs);
    const String *y = V_STRING(rhs);
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
    const Vector *x = V_VECTOR(lhs);
    const Vector *y = V_VECTOR(rhs);
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
        const Vector *lhs = V_VECTOR(x);
        const Vector *rhs = V_VECTOR(y);
        result = pawV_vec_equals(P, lhs, rhs);
    } else if (t == PAW_TMAP) {
        Map *lhs = V_MAP(x);
        Map *rhs = V_MAP(y);
        result = pawH_equals(P, lhs, rhs);
    } else {
        // Fall back to comparing the value representation.
        result = x.u == y.u;
    }
    V_SET_BOOL(vm_top(2), result ? bt : bf);
    vm_pop(1);
}

#define I2U(i) (CAST(i, uint64_t))
#define U2I(u) PAW_CAST_INT(u)

// Generate code for int operators
// Casts to unsigned to avoid UB (signed integer overflow). Requires
// 2's complement integer representation to work properly.
#define I_UNOP(a, op) U2I(op I2U(a))
#define I_BINOP(a, b, op) U2I(I2U(a) op I2U(b))

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
            z = I_BINOP(x, y, +);
            break;
        case BINARY_SUB:
            z = I_BINOP(x, y, -);
            break;
        case BINARY_MUL:
            z = I_BINOP(x, y, *);
            break;
        case BINARY_DIV:
        case BINARY_MOD:
            if (y == 0) {
                pawR_error(P, PAW_ERUNTIME, "divide by 0");
            } else if (x == PAW_INT_MIN && y == -1) {
                // If x / y is undefined, then so too is x % y (see C11 section 6.5.5,
                // item 6). Both cases equal 0 in Paw (x / y wraps).
                z = 0; 
            } else if (binop == BINARY_DIV) {
                z = x / y; 
            } else {
                z = x % y; 
            }
            break;
        case BINARY_BAND:
            z = I_BINOP(x, y, &);
            break;
        case BINARY_BOR:
            z = I_BINOP(x, y, |);
            break;
        case BINARY_BXOR:
            z = I_BINOP(x, y, ^);
            break;
        case BINARY_SHL:
            if (y < 0) {
                pawR_error(P, PAW_ERUNTIME, "negative shift count");
            } else if (y == 0) {
                z = x; // NOOP
            } else {
                y = PAW_MIN(y, U2I(sizeof(x) * 8 - 1));
                z = U2I(I2U(x) << y);
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
                y = PAW_MIN(y, U2I(sizeof(x) * 8 - 1));
                z = x >> y;
            }
    }
    V_SET_INT(vm_top(2), z);
    vm_pop(1);
}

#define finish_cmp(x, y, op) (V_SET_BOOL(vm_top(2), (x)op(y)), vm_pop(1))

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
            V_SET_FLOAT(pv, x + y);
            break;
        case BINARY_SUB:
            V_SET_FLOAT(pv, x - y);
            break;
        case BINARY_MUL:
            V_SET_FLOAT(pv, x * y);
            break;
        default:
            if (y == 0) {
                pawR_error(P, PAW_ERUNTIME, "divide by 0");
            } else if (binop == BINARY_DIV) {
                V_SET_FLOAT(pv, x / y);
            } else {
                paw_assert(binop == BINARY_MOD);
                V_SET_FLOAT(pv, fmod(x, y));
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
            V_SET_BOOL(pv, pawV_vec_contains(P, V_VECTOR(y), x));
        } else {
            paw_assert(t == PAW_TMAP);
            V_SET_BOOL(pv, pawH_contains(V_MAP(y), x));
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
        int_binop(P, binop, V_INT(x), V_INT(y));
    } else if (t == PAW_TFLOAT) {
        float_binop(P, binop, V_FLOAT(x), V_FLOAT(y));
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
            V_SET_INT(pv, I_UNOP(i, -));
            break;
        case UNARY_NOT:
            V_SET_BOOL(pv, I_UNOP(i, !));
            break;
        default:
            paw_assert(unop == UNARY_BNOT);
            V_SET_INT(pv, I_UNOP(i, ~));
    }
}

static void float_unop(paw_Env *P, UnaryOp unop, paw_Float f)
{
    Value *pv = vm_top(1);
    switch (unop) {
        case UNARY_NEG:
            V_SET_FLOAT(pv, -f);
            break;
        default:
            paw_assert(unop == UNARY_NOT);
            V_SET_BOOL(pv, !f);
    }
}

static void other_unop(paw_Env *P, UnaryOp unop, paw_Type t, Value x)
{
    if (unop == UNARY_LEN) {
        V_SET_INT(vm_top(1), pawV_length(x, t));
    } else {
        paw_assert(unop == UNARY_NOT);
        V_SET_BOOL(vm_top(1), !pawV_truthy(x, t));
    }
}

static void unop_aux(paw_Env *P, UnaryOp unop, paw_Type t, Value x)
{
    if (t == PAW_TINT) {
        int_unop(P, unop, V_INT(x));
    } else if (t == PAW_TFLOAT) {
        float_unop(P, unop, V_FLOAT(x));
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
    Tuple *tup = V_TUPLE(*vm_top(1));
    *vm_top(1) = tup->elems[index];
}

void pawR_getattr(paw_Env *P, int index)
{
    Instance *ins = V_INSTANCE(*vm_top(1));
    *vm_top(1) = ins->attrs[index];
}

static void getitem_vector(paw_Env *P, Vector *vector, paw_Int index)
{
    *vm_top(2) = *pawV_vec_get(P, vector, index);
    vm_pop(1);
}

static int getitem_map(paw_Env *P, Map *map, Value key)
{
    const Value *pv = pawH_get(map, key);
    if (pv) {
        *vm_top(2) = *pv;
        vm_pop(1);
        return 0;
    }
    return -1;
}

static void getitem_string(paw_Env *P, const String *string, paw_Int index)
{
    pawV_check_abs(P, index, string->length, "str");
    const char c = string->text[index];
    String *res = pawS_new_nstr(P, &c, 1);
    V_SET_OBJECT(vm_top(2), res);
    vm_pop(1);
}

int pawR_getitem(paw_Env *P, paw_Type t)
{
    const Value obj = *vm_top(2);
    const Value key = *vm_top(1);
    if (t == PAW_TMAP) {
        return getitem_map(P, V_MAP(obj), key);
    }
    if (t == PAW_TVECTOR) {
        getitem_vector(P, V_VECTOR(obj), V_INT(key));
    } else if (t == PAW_TSTRING) {
        getitem_string(P, V_STRING(obj), V_INT(key));
    }
    return 0;
}

static void getslice_vector(paw_Env *P, Vector *vec, paw_Int i, paw_Int j)
{
    const size_t n = pawV_vec_length(vec);
    const size_t zi = check_slice_bound(P, i, n, "start", "vector");
    const size_t zj = check_slice_bound(P, j, n, "end", "vector");

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
    const size_t zi = check_slice_bound(P, i, n, "start", "string");
    const size_t zj = check_slice_bound(P, j, n, "end", "string");

    Value *pv = vm_push0();
    const size_t nbytes = zj - zi;
    String *slice = pawS_new_nstr(P, str->text + zi, nbytes);
    V_SET_OBJECT(pv, slice);
}

void pawR_getslice(paw_Env *P, paw_Type t)
{
    const Value obj = *vm_top(3);
    const paw_Int i = V_INT(*vm_top(2));
    const paw_Int j = V_INT(*vm_top(1));
    if (t == PAW_TVECTOR) {
        getslice_vector(P, V_VECTOR(obj), i, j);
    } else {
        paw_assert(t == PAW_TSTRING);
        getslice_string(P, V_STRING(obj), i, j);
    }
    vm_shift(3);
}

void pawR_literal_tuple(paw_Env *P, int n)
{
    Value *pv = vm_push0();
    Tuple *tuple = pawV_new_tuple(P, n);
    V_SET_OBJECT(pv, tuple);

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
        pawV_vec_resize(P, v, CAST_SIZE(n));
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
    V_SET_OBJECT(pv, var);
    for (int i = 0; i < nfields; ++i) {
        var->fields[i] = P->top.p[i - nfields - 1];
    }
    vm_shift(nfields);
}

static void unpack_variant(paw_Env *P, int n)
{
    const Variant *v = V_VARIANT(*vm_top(1));
    add_zeros(P, n - 1);
    for (int i = 0; i < n; ++i) {
        *vm_top(n - i + 1) = v->fields[i];
    }
}

static void unpack_instance(paw_Env *P, int n)
{
    const Instance *s = V_INSTANCE(*vm_top(1));
    add_zeros(P, n - 1);
    for (int i = 0; i < n; ++i) {
        *vm_top(n - i + 1) = s->attrs[i];
    }
}

static void unpack_tuple(paw_Env *P, int n)
{
    const Tuple *t = V_TUPLE(*vm_top(1));
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
                CHECK_GC(P);
            }

            vm_case(NEWVECTOR) :
            {
                vm_protect();
                pawR_literal_vector(P, get_U(opcode));
                CHECK_GC(P);
            }

            vm_case(NEWMAP) :
            {
                vm_protect();
                pawR_literal_map(P, get_U(opcode));
                CHECK_GC(P);
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
                const Variant *var = V_VARIANT(*vm_top(1));
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
                CHECK_GC(P);
            }

            vm_case(NEWINSTANCE) :
            {
                vm_protect();
                Value *pv = vm_push0();
                Instance *ins = pawV_new_instance(P, get_U(opcode));
                V_SET_OBJECT(pv, ins);
                CHECK_GC(P);
            }

            vm_case(INITFIELD) :
            {
                vm_protect();
                const int u = get_U(opcode);
                Instance *ins = V_INSTANCE(*vm_top(2));
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
                V_SET_OBJECT(pv, closure);
                closure->p = proto;

                // open upvalues
                StackPtr base = cf->base.p;
                for (int i = 0; i < closure->nup; ++i) {
                    const struct UpValueInfo u = proto->u[i];
                    closure->up[i] = u.is_local
                                         ? capture_upvalue(P, base + u.index)
                                         : fn->up[u.index];
                }
                CHECK_GC(P);
            }

            vm_case(CALL) :
            {
                const uint8_t argc = get_U(opcode);
                StackPtr ptr = vm_top(argc + 1);
                vm_save();

                CallFrame *callee = pawC_precall(P, ptr, V_OBJECT(*ptr), argc);
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
                const Variant *var = V_VARIANT(*vm_top(1));
                if (var->k == 0) {
                    // jump over the OP_RETURN and unpack the value
                    *vm_top(1) = var->fields[0];
                    pc += get_S(opcode);
                }
            }

            vm_case(JUMPFALSE) :
            {
                if (!V_TRUE(*vm_top(1))) {
                    pc += get_S(opcode);
                }
            }

            vm_case(JUMPFALSEPOP) :
            {
                if (!V_TRUE(*vm_top(1))) {
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
