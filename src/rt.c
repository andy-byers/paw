// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#include "prefix.h"
#include <math.h>

#include "rt.h"
#include "auxlib.h"
#include "debug.h"
#include "env.h"
#include "call.h"
#include "gc.h"
#include "map.h"
#include "type.h"
#include "value.h"

// Helpers for the VM:

#define vm_switch(x) switch (x)
#define vm_case(x) break; case OP_##x
#define vm_default break; default

#define VM_FETCH(P) do { \
        if (PAW_UNLIKELY(trap)) { \
            trap = pawD_trace_exec(P, pc); \
            updatebase(ci); \
        } \
        i = *pc++; \
    } while (0)

#define VM_SHIFT(n) (*VM_TOP((n) + 1) = *VM_TOP(1), VM_POP(n))
#define VM_POP(n) pawC_stkdec(P, n)
#define VM_TOP(i) (&P->top.p[-(i)])
#define VM_SAVE_PC() (cf->pc = pc)
#define VM_UPVALUE(u) (fn->up[u]->p.p)

#define VM_PUSH(v) CHECK_EXP((P)->bound.p - (P)->top.p > 0, \
        (*P->top.p++ = (v), &P->top.p[-1]))
#define VM_PUSH_0() VM_PUSH((Value){.u = 0})
#define VM_PUSH_BOOL(B) VM_PUSH((Value){.b = B})
#define VM_PUSH_INT(I) VM_PUSH((Value){.i = I})
#define VM_PUSH_FLOAT(F) VM_PUSH((Value){.f = F})
#define VM_PUSH_OBJECT(O) VM_PUSH((Value){.o = O})

#define VM_SET_0(top) V_SET_0(VM_TOP(top))
#define VM_SET_BOOL(top, b) V_SET_BOOL(VM_TOP(top), b)
#define VM_SET_INT(top, i) V_SET_INT(VM_TOP(top), i)
#define VM_SET_FLOAT(top, f) V_SET_FLOAT(VM_TOP(top), f)
#define VM_SET_OBJECT(top, o) V_SET_OBJECT(VM_TOP(top), o)

#define VM_INT(top) V_INT(*VM_TOP(top))
#define VM_BOOL(top) V_BOOL(*VM_TOP(top))
#define VM_FLOAT(top) V_FLOAT(*VM_TOP(top))
#define VM_STR(top) V_STRING(*VM_TOP(top))
#define VM_LIST(top) V_LIST(*VM_TOP(top))
#define VM_MAP(top) V_MAP(*VM_TOP(top))

// Generate code for creating common builtin objects
// Requires a placeholder slot (the VM_PUSH_0() pushes an empty slot) so
// the GC doesn't get confused. Both the VM_PUSH_0(), and the pawV_list_new calls
// might fail and cause an error to be thrown, so we have to be careful not
// to leave a junk value on top of the stack.
#define VM_LIST_INIT(plist, pv) \
    pv = VM_PUSH_0(); \
    plist = pawV_list_new(P); \
    V_SET_OBJECT(pv, plist);
#define VM_MAP_INIT(pmap, pv) \
    pv = VM_PUSH_0(); \
    pmap = pawH_new(P); \
    V_SET_OBJECT(pv, pmap);

static void add_zeros(paw_Env *P, int n)
{
    for (int i = 0; i < n; ++i) {
        VM_PUSH_0();
    }
}

static void add_location(paw_Env *P, Buffer *buf)
{
    const CallFrame *cf = P->cf;
    for (; cf != &P->main; cf = cf->prev) {
        const int line = pawD_line_number(cf, cf->pc);
        if (line >= 0) {
            const Proto *p = cf->fn->p;
            const char *name = p->modname->text;
            pawL_add_fstring(P, buf, "%s:%d: ", name, line);
            break;
        } else if (CF_IS_ENTRY(cf)) {
            L_ADD_LITERAL(P, buf, "[C]: ");
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

void pawR_field_error(paw_Env *P, Value name)
{
    add_3_parts(P, "field '", V_TEXT(name), "' does not exist");
    pawC_throw(P, PAW_EATTR);
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
    ((f) >= CAST(paw_Float, PAW_INT_MIN) && \
     (f) < -CAST(paw_Float, PAW_INT_MIN) && \
     (V_SET_INT(pv, PAW_CAST_INT(f)), 1))

static void float2int(paw_Env *P, paw_Float f, Value *pv)
{
    if (!FLOAT2INT_AUX(f, pv)) {
        pawR_error(P, PAW_EOVERFLOW, "float %f is too large", f);
    }
}

void pawR_cast_bool(paw_Env *P, paw_Type type)
{
    paw_assert(type < PAW_TSTR);
    PAW_UNUSED(type);

    Value *pv = VM_TOP(1);
    V_SET_BOOL(pv, pv->u != 0);
}

void pawR_cast_int(paw_Env *P, paw_Type type)
{
    paw_assert(type < PAW_TSTR);
    if (type == PAW_TFLOAT) {
        // NOTE: Other primitives have a value representation compatible with
        //       that of the 'int' type.
        Value *pv = VM_TOP(1);
        const paw_Float f = V_FLOAT(*pv);
        float2int(P, f, pv);
    }
}

void pawR_cast_float(paw_Env *P, paw_Type type)
{
    paw_assert(type < PAW_TSTR);
    if (type != PAW_TFLOAT) {
        Value *pv = VM_TOP(1);
        const paw_Int i = V_INT(*pv);
        V_SET_FLOAT(pv, CAST(paw_Float, i));
    }
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

void pawR_setfield(paw_Env *P, int index)
{
    const Value val = *VM_TOP(1);
    const Value obj = *VM_TOP(2);
    V_TUPLE(obj)->elems[index] = val;
    VM_SHIFT(1);
}

void pawR_init(paw_Env *P)
{
    String *errmsg = pawS_new_str(P, "not enough memory");
    pawG_fix_object(P, CAST_OBJECT(errmsg));
    V_SET_OBJECT(&P->mem_errmsg, errmsg);
}

#define STOP_LOOP(i, i2, d) \
    (((d) < 0 && (i) <= (i2)) || ((d) > 0 && (i) >= (i2)))

static paw_Bool fornum_init(paw_Env *P)
{
    const paw_Int begin = VM_INT(3);
    const paw_Int end = VM_INT(2);
    const paw_Int step = VM_INT(1);
    if (step == 0) {
        pawR_error(P, PAW_ERUNTIME, "loop step equals 0");
    }
    const paw_Bool skip = STOP_LOOP(begin, end, step);
    if (!skip) {
        VM_SET_INT(3, begin);
        VM_SET_INT(2, end);
        VM_SET_INT(1, step);
        VM_PUSH_INT(begin);
    }
    return skip;
}

static paw_Bool fornum(paw_Env *P)
{
    const paw_Int itr = VM_INT(3);
    const paw_Int step = VM_INT(1);
    const paw_Int end = VM_INT(2);
    const paw_Int next = itr + step;
    if (STOP_LOOP(next, end, step)) {
        return PAW_FALSE;
    }
    VM_SET_INT(3, next);
    VM_PUSH_INT(next);
    return PAW_TRUE;
}

static paw_Bool forlist_init(paw_Env *P)
{
    const Value v = *VM_TOP(1);
    paw_Int itr = PAW_ITER_INIT;
    List *arr = V_LIST(v);
    if (pawV_list_iter(arr, &itr)) {
        VM_PUSH_INT(itr);
        VM_PUSH(arr->begin[itr]);
        return PAW_FALSE;
    }
    return PAW_TRUE;
}

static paw_Bool forlist(paw_Env *P)
{
    const Value obj = *VM_TOP(2);
    const Value itr = *VM_TOP(1);
    List *arr = V_LIST(obj);
    paw_Int i = V_INT(itr);
    if (pawV_list_iter(arr, &i)) {
        VM_SET_INT(1, i);
        VM_PUSH(arr->begin[i]);
        return PAW_TRUE;
    }
    return PAW_FALSE;
}

static paw_Bool formap_init(paw_Env *P)
{
    const Value v = *VM_TOP(1);
    paw_Int itr = PAW_ITER_INIT;
    Map *map = V_MAP(v);
    if (pawH_iter(map, &itr)) {
        const Value v = *pawH_key(map, CAST_SIZE(itr));
        VM_PUSH_INT(itr);
        VM_PUSH(v);
        return PAW_FALSE;
    }
    return PAW_TRUE;
}

static paw_Bool formap(paw_Env *P)
{
    const Value obj = *VM_TOP(2);
    const Value itr = *VM_TOP(1);
    Map *map = V_MAP(obj);
    paw_Int i = V_INT(itr);
    if (pawH_iter(map, &i)) {
        const Value v = *pawH_key(map, CAST_SIZE(i));
        VM_SET_INT(1, i);
        VM_PUSH(v);
        return PAW_TRUE;
    }
    return PAW_FALSE; // stop the loop
}

#define I2U(i) (CAST(uint64_t, i))
#define U2I(u) PAW_CAST_INT(u)

// Generate code for int operators
// Casts to unsigned to avoid UB (signed integer overflow). Requires
// 2's complement integer representation to work properly.
#define I_UNOP(a, op) U2I(op I2U(a))
#define I_BINOP(a, b, op) U2I(I2U(a) op I2U(b))

#define CMP_CASES(x, y, z) \
    case CMP_EQ: \
        (z) = (x) == (y); \
        break; \
    case CMP_NE: \
        (z) = (x) != (y); \
        break; \
    case CMP_LT: \
        (z) = (x) < (y); \
        break; \
    case CMP_LE: \
        (z) = (x) <= (y); \
        break; \
    case CMP_GT: \
        (z) = (x) > (y); \
        break; \
    case CMP_GE: \
        (z) = (x) >= (y);

#define DIVIDE_BY_0(P) pawR_error(P, PAW_ERUNTIME, "divide by 0")

static void int_cmp(paw_Env *P, enum CmpOp op, paw_Int x, paw_Int y)
{
    paw_Bool z;
    switch (op) {
        CMP_CASES(x, y, z)
    }
    VM_SET_BOOL(2, z);
    VM_POP(1);
}

void pawR_cmpi(paw_Env *P, enum CmpOp op)
{
    int_cmp(P, op, VM_INT(2), VM_INT(1));
}

void pawR_arithi1(paw_Env *P, enum ArithOp1 op)
{
    paw_Int x = VM_INT(1);
    switch (op) {
        case ARITH1_NEG:
            x = I_UNOP(x, -);
    }
    VM_SET_INT(1, x);
}

void pawR_arithi2(paw_Env *P, enum ArithOp2 op)
{
    paw_Int x = VM_INT(2);
    const paw_Int y = VM_INT(1);
    switch (op) {
        case ARITH2_ADD:
            x = I_BINOP(x, y, +);
            break;
        case ARITH2_SUB:
            x = I_BINOP(x, y, -);
            break;
        case ARITH2_MUL:
            x = I_BINOP(x, y, *);
            break;
        case ARITH2_DIV:
        case ARITH2_MOD:
            if (y == 0) {
                DIVIDE_BY_0(P);
            } else if (x == PAW_INT_MIN && y == -1) {
                // If x / y is undefined, then so too is x % y (see C11 section 6.5.5,
                // item 6). Both cases equal 0 in Paw (x / y wraps).
                x = 0;
            } else if (op == ARITH2_DIV) {
                x = x / y;
            } else {
                x = x % y;
            }
    }
    VM_SET_INT(2, x);
    VM_POP(1);
}

void pawR_bitw1(paw_Env *P, enum BitwOp1 op)
{
    paw_Int x = VM_INT(1);
    switch (op) {
        case BITW1_NOT:
            x = I_UNOP(x, ~);
    }
    VM_SET_INT(1, x);
}

void pawR_bitw2(paw_Env *P, enum BitwOp2 op)
{
    paw_Int x = VM_INT(2);
    paw_Int y = VM_INT(1);
    switch (op) {
        case BITW2_AND:
            x = I_BINOP(x, y, &);
            break;
        case BITW2_OR:
            x = I_BINOP(x, y, |);
            break;
        case BITW2_XOR:
            x = I_BINOP(x, y, ^);
            break;
        case BITW2_SHL:
            if (y < 0) {
                pawR_error(P, PAW_ERUNTIME, "negative shift count");
            } else if (y > 0) {
                y = PAW_MIN(y, U2I(sizeof(x) * 8 - 1));
                x = U2I(I2U(x) << y);
            }
            break;
        case BITW2_SHR:
            if (y < 0) {
                pawR_error(P, PAW_ERUNTIME, "negative shift count");
            } else if (y > 0) {
                // Right shift by >= width of 'x' is UB in C. Clamp the
                // shift count. If 'x' < 0, then the results of the
                // shift are implementation-defined (may or may not
                // preserve the sign).
                y = PAW_MIN(y, U2I(sizeof(x) * 8 - 1));
                x = x >> y;
            }
    }
    VM_SET_INT(2, x);
    VM_POP(1);
}

void pawR_cmpf(paw_Env *P, enum CmpOp op)
{
    const paw_Float x = VM_FLOAT(2);
    const paw_Float y = VM_FLOAT(1);
    paw_Bool z;
    switch (op) {
        CMP_CASES(x, y, z)
    }
    VM_SET_BOOL(2, z);
    VM_POP(1);
}

void pawR_arithf1(paw_Env *P, enum ArithOp1 op)
{
    const paw_Float x = VM_FLOAT(1);
    paw_Float y;
    switch (op) {
        case ARITH1_NEG:
            y = -x;
    }
    VM_SET_FLOAT(1, y);
}

void pawR_arithf2(paw_Env *P, enum ArithOp2 op)
{
    paw_Float x = VM_FLOAT(2);
    const paw_Float y = VM_FLOAT(1);
    switch (op) {
        case ARITH2_ADD:
            x = x + y;
            break;
        case ARITH2_SUB:
            x = x - y;
            break;
        case ARITH2_MUL:
            x = x * y;
            break;
        case ARITH2_DIV:
            if (y == 0.0) DIVIDE_BY_0(P);
            x = x / y;
            break;
        case ARITH2_MOD:
            if (y == 0.0) DIVIDE_BY_0(P);
            x = fmod(x, y);
    }
    VM_SET_FLOAT(2, x);
    VM_POP(1);
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

static size_t get_length(Value v, enum paw_AdtKind kind)
{
    switch (kind) {
        case PAW_ADT_STR:
            return pawS_length(V_STRING(v));
        case PAW_ADT_LIST:
            return pawV_list_length(V_LIST(v));
        case PAW_ADT_MAP:
            return pawH_length(V_MAP(v));
    }
}

void pawR_length(paw_Env *P, enum paw_AdtKind kind)
{
    Value *pv = VM_TOP(1);
    const size_t length = get_length(*pv, kind);
    V_SET_INT(pv, length);
}

static void str_concat(paw_Env *P)
{
    const String *x = VM_STR(2);
    const String *y = VM_STR(1);

    paw_assert(x->length < PAW_SIZE_MAX);
    paw_assert(y->length < PAW_SIZE_MAX);
    if (x->length > PAW_SIZE_MAX - y->length) {
        pawR_error(P, PAW_EMEMORY, "string is too large");
    }
    String *z = pawS_new_uninit(P, x->length + y->length);
    memcpy(z->text, x->text, x->length);
    memcpy(z->text + x->length, y->text, y->length);
    pawS_register(P, &z);

    VM_SET_OBJECT(2, z);
    VM_POP(1);
}

static void list_concat(paw_Env *P)
{
    const List *x = VM_LIST(2);
    const List *y = VM_LIST(1);

    List *z;
    Value *pv;
    VM_LIST_INIT(z, pv);

    const size_t nx = pawV_list_length(x);
    const size_t ny = pawV_list_length(y);
    pawV_list_resize(P, z, nx + ny);
    if (nx > 0) memcpy(z->begin, x->begin, nx * sizeof(z->begin[0]));
    if (ny > 0) memcpy(z->begin + nx, y->begin, ny * sizeof(z->begin[0]));
    VM_SHIFT(2);
}

void pawR_concat(paw_Env *P, enum paw_AdtKind kind)
{
    switch (kind) {
        case PAW_ADT_STR:
            str_concat(P);
            break;
        case PAW_ADT_LIST:
            list_concat(P);
            break;
        case PAW_ADT_MAP:
            PAW_UNREACHABLE();
    }
}

static void str_get(paw_Env *P)
{
    const String *str = VM_STR(2);
    const paw_Int idx = VM_INT(1);
    pawV_check_abs(P, idx, str->length, "str");
    const char c = str->text[idx];
    String *res = pawS_new_nstr(P, &c, 1);
    V_SET_OBJECT(VM_TOP(2), res);
    VM_POP(1);
}

static void list_get(paw_Env *P)
{
    List *list = VM_LIST(2);
    const paw_Int key = VM_INT(1);
    *VM_TOP(2) = *pawV_list_get(P, list, key);
    VM_POP(1);
}

static void map_get(paw_Env *P)
{
    Map *map = VM_MAP(2);
    const Value key = *VM_TOP(1);
    const Value *pv = pawH_get(map, key);
    if (pv == NULL) pawR_error(P, PAW_EKEY, "key does not exist");
    *VM_TOP(2) = *pv;
    VM_POP(1);
}

void pawR_getelem(paw_Env *P, enum paw_AdtKind kind)
{
    switch (kind) {
        case PAW_ADT_STR:
            str_get(P);
            break;
        case PAW_ADT_LIST:
            list_get(P);
            break;
        case PAW_ADT_MAP:
            map_get(P);
    }
}

static void list_set(paw_Env *P)
{
    const Value val = *VM_TOP(1);
    const paw_Int index = VM_INT(2);
    List *list = VM_LIST(3);
    Value *pval = pawV_list_get(P, list, index);
    *pval = val;
    VM_SHIFT(2);
}

static void map_set(paw_Env *P)
{
    const Value val = *VM_TOP(1);
    const Value key = *VM_TOP(2);
    Map *map = VM_MAP(3);
    pawH_insert(P, map, key, val);
    VM_SHIFT(2);
}

void pawR_setelem(paw_Env *P, enum paw_AdtKind kind)
{
    switch (kind) {
        case PAW_ADT_LIST:
            list_set(P);
            break;
        case PAW_ADT_MAP:
            map_set(P);
            break;
        case PAW_ADT_STR:
            PAW_UNREACHABLE();
    }
}

static void str_getn(paw_Env *P)
{
    const String *str = VM_STR(3);
    const paw_Int i = VM_INT(2);
    const paw_Int j = VM_INT(1);

    const size_t n = pawS_length(str);
    const size_t zi = check_slice_bound(P, i, n, "start", "string");
    const size_t zj = check_slice_bound(P, j, n, "end", "string");

    Value *pv = VM_PUSH_0();
    const size_t nbytes = zj - zi;
    String *slice = pawS_new_nstr(P, str->text + zi, nbytes);
    V_SET_OBJECT(pv, slice);

    VM_SHIFT(3);
}

static void list_getn(paw_Env *P)
{
    const List *list = VM_LIST(3);
    const paw_Int i = VM_INT(2);
    const paw_Int j = VM_INT(1);

    const size_t n = pawV_list_length(list);
    const size_t zi = check_slice_bound(P, i, n, "start", "list");
    const size_t zj = check_slice_bound(P, j, n, "end", "list");

    Value *pv;
    List *slice;
    VM_LIST_INIT(slice, pv);

    const size_t nelems = zj - zi;
    pawV_list_resize(P, slice, nelems);
    memcpy(slice->begin, list->begin + zi,
            nelems * sizeof(list->begin[0]));

    VM_SHIFT(3);
}

void pawR_getrange(paw_Env *P, enum paw_AdtKind kind)
{
    switch (kind) {
        case PAW_ADT_LIST:
            list_getn(P);
            break;
        case PAW_ADT_STR:
            str_getn(P);
            break;
        case PAW_ADT_MAP:
            PAW_UNREACHABLE();
    }
}

static List *list_copy(paw_Env *P, Value *pv, const List *list)
{
    List *copy = pawV_list_new(P);
    V_SET_OBJECT(pv, copy); // anchor
    if (pawV_list_length(list)) {
        pawV_list_resize(P, copy, pawV_list_length(list));
        memcpy(copy->begin, list->begin, sizeof(list->begin[0]) * pawV_list_length(list));
    }
    return copy;
}

// setn([a1..an], i, j, [b1..bn]) => [a1..ai b1..bn aj..an]
static void list_setn(paw_Env *P)
{
    List *va = VM_LIST(4);
    const paw_Int i = VM_INT(3);
    const paw_Int j = VM_INT(2);
    const List *vb = VM_LIST(1);

    const size_t na = pawV_list_length(va);
    const size_t nb = pawV_list_length(vb);
    const size_t zi = check_slice_bound(P, i, na, "start", "list");
    const size_t zj = check_slice_bound(P, j, na, "end", "list");

    if (va == vb) {
        Value *pv = VM_TOP(1);
        paw_assert(pv->p == vb);
        vb = list_copy(P, pv, vb);
    }

    const size_t zelem = sizeof(va->begin[0]);
    const size_t nelems = na - zj + zi + nb;
    pawV_list_reserve(P, va, nelems);

    Value *gap = va->begin + zi;
    memmove(gap + nb, va->begin + zj, (na - zj) * zelem);
    memcpy(gap, vb->begin, nb * zelem);
    pawV_list_resize(P, va, nelems);

    VM_SHIFT(3);
}

void pawR_setrange(paw_Env *P, enum paw_AdtKind kind)
{
    switch (kind) {
        case PAW_ADT_LIST:
            list_setn(P);
            break;
        case PAW_ADT_STR:
        case PAW_ADT_MAP:
            PAW_UNREACHABLE();
    }
}

void pawR_cmps(paw_Env *P, enum CmpOp op)
{
    const String *x = VM_STR(2);
    const String *y = VM_STR(1);
    const int cmp = pawS_cmp(x, y);
    paw_Bool z;
    switch (op) {
        CMP_CASES(cmp, 0, z);
    }
    VM_SET_BOOL(2, z);
    VM_POP(1);
}

void pawR_getfield(paw_Env *P, int index)
{
    Tuple *tup = V_TUPLE(*VM_TOP(1));
    *VM_TOP(1) = tup->elems[index];
}

void pawR_literal_tuple(paw_Env *P, int n)
{
    Value *pv = VM_PUSH_0();
    Tuple *tuple = pawV_new_tuple(P, n);
    V_SET_OBJECT(pv, tuple);

    Value *dst = tuple->elems + n;
    const Value *src = VM_TOP(1);
    for (int i = 0; i < n; ++i) {
        *--dst = *--src;
    }
    VM_SHIFT(n);
}

void pawR_literal_list(paw_Env *P, int n)
{
    List *v;
    StackPtr sp;
    VM_LIST_INIT(v, sp);
    if (n > 0) {
        pawV_list_resize(P, v, CAST_SIZE(n));
        Value *pv = v->end;
        do {
            *--pv = *--sp;
        } while (pv != v->begin);
        // Replace contents with list itself.
        VM_SHIFT(n);
    }
}

void pawR_literal_map(paw_Env *P, int n)
{
    Map *m;
    StackPtr sp;
    VM_MAP_INIT(m, sp);
    if (n > 0) {
        for (int i = 0; i < n; ++i) {
            const Value value = *--sp;
            pawH_insert(P, m, *--sp, value);
        }
        // Replace contents with map itself.
        VM_SHIFT(2 * n);
    }
}

static void new_variant(paw_Env *P, int k, int nfields)
{
    Value *pv = VM_PUSH_0();
    Variant *var = pawV_new_variant(P, k, nfields);
    V_SET_OBJECT(pv, var);
    for (int i = 0; i < nfields; ++i) {
        var->fields[i] = P->top.p[i - nfields - 1];
    }
    VM_SHIFT(nfields);
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
        vm_switch(GET_OP(opcode))
        {
            vm_case(NOOP) :
            {
                // do nothing
            }

            vm_case(POP) :
            {
                VM_POP(GET_U(opcode));
            }

            vm_case(CLOSE) :
            {
                const int u = GET_U(opcode);
                pawR_close_upvalues(P, VM_TOP(u));
                VM_POP(u);
            }

            vm_case(COPY) :
            {
                const int u = GET_U(opcode);
                const Value v = *VM_TOP(u + 1);
                VM_PUSH(v);
            }

            vm_case(PUSHZERO) :
            {
                VM_PUSH_0();
            }

            vm_case(PUSHONE) :
            {
                VM_PUSH_INT(1);
            }

            vm_case(PUSHSMI) :
            {
                VM_PUSH_INT(GET_S(opcode));
            }

            vm_case(PUSHCONST) :
            {
                VM_PUSH(K[GET_U(opcode)]);
            }

            vm_case(CMPI) :
            {
                pawR_cmpi(P, GET_U(opcode));
            }

            vm_case(CMPF) :
            {
                pawR_cmpf(P, GET_U(opcode));
            }

            vm_case(CMPS) :
            {
                pawR_cmps(P, GET_U(opcode));
            }

            vm_case(ARITHI1) :
            {
                pawR_arithi1(P, GET_U(opcode));
            }

            vm_case(ARITHI2) :
            {
                pawR_arithi2(P, GET_U(opcode));
            }

            vm_case(ARITHF1) :
            {
                pawR_arithf1(P, GET_U(opcode));
            }

            vm_case(ARITHF2) :
            {
                pawR_arithf2(P, GET_U(opcode));
            }

            vm_case(BITW1) :
            {
                pawR_bitw1(P, GET_U(opcode));
            }

            vm_case(BITW2) :
            {
                pawR_bitw2(P, GET_U(opcode));
            }

            vm_case(NOT) :
            {
                const paw_Bool b = VM_INT(1);
                VM_SET_INT(1, !b);
            }

            vm_case(LENGTH) :
            {
                const int u = GET_U(opcode);
                pawR_length(P, u);
            }

            vm_case(CONCAT) :
            {
                VM_SAVE_PC();
                const int u = GET_U(opcode);
                pawR_concat(P, u);
            }

            vm_case(GETELEM) :
            {
                VM_SAVE_PC();
                const int u = GET_U(opcode);
                pawR_getelem(P, u);
            }

            vm_case(SETELEM) :
            {
                VM_SAVE_PC();
                const int u = GET_U(opcode);
                pawR_setelem(P, u);
            }

            vm_case(GETRANGE) :
            {
                VM_SAVE_PC();
                const int u = GET_U(opcode);
                pawR_getrange(P, u);
            }

            vm_case(SETRANGE) :
            {
                VM_SAVE_PC();
                const int u = GET_U(opcode);
                pawR_setrange(P, u);
            }

            vm_case(NEWTUPLE) :
            {
                VM_SAVE_PC();
                pawR_literal_tuple(P, GET_U(opcode));
                CHECK_GC(P);
            }

            vm_case(NEWLIST) :
            {
                VM_SAVE_PC();
                pawR_literal_list(P, GET_U(opcode));
                CHECK_GC(P);
            }

            vm_case(NEWMAP) :
            {
                VM_SAVE_PC();
                pawR_literal_map(P, GET_U(opcode));
                CHECK_GC(P);
            }

            vm_case(CASTBOOL) :
            {
                pawR_cast_bool(P, GET_U(opcode));
            }

            vm_case(CASTINT) :
            {
                pawR_cast_int(P, GET_U(opcode));
            }

            vm_case(CASTFLOAT) :
            {
                pawR_cast_float(P, GET_U(opcode));
            }

            vm_case(NEWVARIANT) :
            {
                VM_SAVE_PC();
                new_variant(P, GET_A(opcode), GET_B(opcode));
                CHECK_GC(P);
            }

            vm_case(NEWINSTANCE) :
            {
                VM_SAVE_PC();
                Value *pv = VM_PUSH_0();
                Tuple *tuple = pawV_new_tuple(P, GET_U(opcode));
                V_SET_OBJECT(pv, tuple);
                CHECK_GC(P);
            }

            vm_case(INITFIELD) :
            {
                VM_SAVE_PC();
                const int u = GET_U(opcode);
                Tuple *tuple = V_TUPLE(*VM_TOP(2));
                tuple->elems[u] = *VM_TOP(1);
                VM_POP(1);
            }

            vm_case(GETLOCAL) :
            {
                const Value local = cf->base.p[GET_U(opcode)];
                VM_PUSH(local);
            }

            vm_case(SETLOCAL) :
            {
                Value *plocal = &cf->base.p[GET_U(opcode)];
                *plocal = *VM_TOP(1);
            }

            vm_case(GETUPVALUE) :
            {
                const int u = GET_U(opcode);
                const Value upval = *VM_UPVALUE(u);
                VM_PUSH(upval);
            }

            vm_case(SETUPVALUE) :
            {
                const int u = GET_U(opcode);
                Value *pupval = VM_UPVALUE(u);
                *pupval = *VM_TOP(1);
            }

            vm_case(GETGLOBAL) :
            {
                const int u = GET_U(opcode);
                VM_PUSH(*Y_PVAL(P, u));
            }

            vm_case(GETFIELD) :
            {
                VM_SAVE_PC();
                pawR_getfield(P, GET_U(opcode));
            }

            vm_case(SETFIELD) :
            {
                VM_SAVE_PC();
                pawR_setfield(P, GET_U(opcode));
            }

            vm_case(CLOSURE) :
            {
                VM_SAVE_PC();
                Value *pv = VM_PUSH_0();
                Proto *proto = fn->p->p[GET_U(opcode)];
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
                const uint8_t argc = GET_U(opcode);
                StackPtr ptr = VM_TOP(argc + 1);
                VM_SAVE_PC();

                CallFrame *callee = pawC_precall(P, ptr, V_OBJECT(*ptr), argc);
                if (callee) {
                    cf = callee;
                    goto top;
                }
            }

            vm_case(SHIFT) :
            {
                const int u = GET_U(opcode);
                pawR_close_upvalues(P, VM_TOP(u));
                VM_SHIFT(u);
            }

            vm_case(RETURN) :
            {
                const Value result = *VM_TOP(1);
                VM_POP(1);

                P->top.p = CF_STACK_RETURN(cf);
                VM_SAVE_PC();

                pawR_close_upvalues(P, VM_TOP(1));
                VM_PUSH(result);
                P->cf = cf->prev;
                if (CF_IS_ENTRY(cf)) {
                    return;
                }
                cf = P->cf;
                goto top;
            }

            vm_case(JUMP) :
            {
                pc += GET_S(opcode);
            }

            vm_case(JUMPNULL) :
            {
                const Variant *var = V_VARIANT(*VM_TOP(1));
                if (var->k == 0) {
                    // jump over the OP_RETURN and unpack the value
                    *VM_TOP(1) = var->fields[0];
                    pc += GET_S(opcode);
                }
            }

            vm_case(JUMPFALSE) :
            {
                if (!V_TRUE(*VM_TOP(1))) {
                    pc += GET_S(opcode);
                }
            }

            vm_case(JUMPFALSEPOP) :
            {
                if (!V_TRUE(*VM_TOP(1))) {
                    pc += GET_S(opcode);
                }
                VM_POP(1);
            }

            vm_case(FORNUM0) :
            {
                VM_SAVE_PC();
                if (fornum_init(P)) {
                    pc += GET_S(opcode); // skip
                }
            }

            vm_case(FORNUM) :
            {
                if (fornum(P)) {
                    pc += GET_S(opcode); // continue
                }
            }

#define VM_FORIN0(t, T) \
            vm_case(FOR##T##0) : \
            { \
                VM_SAVE_PC(); \
                if (for##t##_init(P)) { \
                    VM_PUSH_0(); \
                    pc += GET_S(opcode); \
                } \
            }
#define VM_FORIN(t, T) \
            vm_case(FOR##T) : \
            { \
                if (for##t(P)) { \
                    pc += GET_S(opcode); \
                } \
            }
            VM_FORIN0(list, LIST)
            VM_FORIN0(map, MAP)
            VM_FORIN(list, LIST)
            VM_FORIN(map, MAP)
#undef VM_FORIN0
#undef VM_FORIN

            vm_default:
                PAW_UNREACHABLE();
        }
    }
}
