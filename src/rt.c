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

#define VM_SAVE_PC() (cf->pc = pc)
#define VM_UPVALUE(u) (fn->up[u]->p.p)

#define VM_RA(opcode) (&cf->base.p[GET_A(opcode)])
#define VM_RB(opcode) (&cf->base.p[GET_B(opcode)])
#define VM_RC(opcode) (&cf->base.p[GET_C(opcode)])

#define VM_REG(r) (&cf->base.p[r])

// Generate code for creating common builtin objects
#define VM_LIST_INIT(r) \
    (List *)V_SET_OBJECT(r, pawV_list_new(P));
#define VM_MAP_INIT(r) \
    (Map *)V_SET_OBJECT(r, pawH_new(P));

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

void pawR_tuple_get(CallFrame *cf, Value *ra, const Value *rb, int index)
{
    *ra = V_TUPLE(*rb)->elems[index];
}

void pawR_tuple_set(CallFrame *cf, Value *ra, int index, const Value *rb)
{
    V_TUPLE(*ra)->elems[index] = *rb;
}

void pawR_init(paw_Env *P)
{
    String *errmsg = pawS_new_str(P, "not enough memory");
    pawG_fix_object(P, CAST_OBJECT(errmsg));
    V_SET_OBJECT(&P->mem_errmsg, errmsg);
}

#define I2U(i) (CAST(uint64_t, i))
#define U2I(u) PAW_CAST_INT(u)

// Generate code for int operators
// Casts to unsigned to avoid UB (signed integer overflow). Requires
// 2's complement integer representation to work properly.
#define I_UNOP(a, op) U2I(op I2U(a))
#define I_BINOP(a, b, op) U2I(I2U(a) op I2U(b))

#define DIVIDE_BY_0(P) pawR_error(P, PAW_ERUNTIME, "divide by 0")

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

void pawR_str_concat(paw_Env *P, CallFrame *cf, int n)
{
    StackPtr ra = P->top.p - n;
    paw_assert(n >= 1);
    if (n <= 1) return;

    size_t total_size = 0;
    for (int i = 0; i < n; ++i) {
        total_size += pawS_length(V_STRING(ra[i]));
    }

    // caution: unanchored allocation
    String *r = pawS_new_uninit(P, total_size);
    char *data = r->text;

    paw_assert(data != NULL);
    for (int i = 0; i < n; ++i) {
        const String *s = V_STRING(ra[i]);
        memcpy(data, s->text, s->length);
        data += s->length;
    }
    pawS_register(P, &r);

    V_SET_OBJECT(ra, r);
    P->top.p = ra + 1;
}

void pawR_str_length(paw_Env *P, CallFrame *cf, Value *ra, const Value *rb)
{
    const size_t length = pawS_length(V_STRING(*rb));
    V_SET_INT(ra, PAW_CAST_INT(length));
}

void pawR_list_length(paw_Env *P, CallFrame *cf, Value *ra, const Value *rb)
{
    const size_t length = pawV_list_length(V_LIST(*rb));
    V_SET_INT(ra, PAW_CAST_INT(length));
}

// TODO: operation needs an extra register since lists need 2 allocations...
//       the compiler needs to make sure this register exists
void pawR_list_concat(paw_Env *P, CallFrame *cf, int n)
{
    StackPtr ra = P->top.p - n;
    paw_assert(n >= 1);
    if (n <= 1) return;

    size_t total_size = 0;
    for (int i = 0; i < n; ++i) {
        total_size += pawV_list_length(V_LIST(ra[i]));
    }

    StackPtr temp = P->top.p++;
    List *r = VM_LIST_INIT(temp);
    if (total_size > 0) {
        pawV_list_reserve(P, r, total_size);

        for (int i = 0; i < n; ++i) {
            const List *list = V_LIST(ra[i]);
            const size_t length = pawV_list_length(list);
            memcpy(r->end, list->begin, length * sizeof(list->begin[0]));
            r->end += length;
        }
    }

    V_SET_OBJECT(ra, r);
    P->top.p = ra + 1;
}

void pawR_str_get(paw_Env *P, CallFrame *cf, Value *ra, const Value *rb, const Value *rc)
{
    const String *str = V_STRING(*rb);
    const paw_Int idx = V_INT(*rc);
    pawV_check_abs(P, idx, str->length, "str");
    const char ch = str->text[idx];
    String *res = pawS_new_nstr(P, &ch, 1);
    V_SET_OBJECT(ra, res);
}

void pawR_list_get(paw_Env *P, CallFrame *cf, Value *ra, const Value *rb, const Value *rc)
{
    List *list = V_LIST(*rb);
    const paw_Int idx = V_INT(*rc);
    *ra = *pawV_list_get(P, list, idx);
}

void pawR_list_set(paw_Env *P, CallFrame *cf, Value *ra, const Value *rb, const Value *rc)
{
    List *list = V_LIST(*ra);
    const paw_Int idx = V_INT(*rb);
    Value *pval = pawV_list_get(P, list, idx);
    *pval = *rc;
}

void pawR_map_length(paw_Env *P, CallFrame *cf, Value *ra, const Value *rb)
{
    const size_t length = pawH_length(V_MAP(*rb));
    V_SET_INT(ra, PAW_CAST_INT(length));
}

int pawR_map_get(paw_Env *P, CallFrame *cf, Value *ra, const Value *rb, const Value *rc)
{
    Map *map = V_MAP(*rb);
    const Value *pval = pawH_get(map, *rc);
    if (pval == NULL) return -1;
    *ra = *pval;
    return 0;
}

void pawR_map_set(paw_Env *P, CallFrame *cf, Value *ra, const Value *rb, const Value *rc)
{
    pawH_insert(P, V_MAP(*ra), *rb, *rc);
}

void pawR_str_getn(paw_Env *P, CallFrame *cf, Value *ra, const Value *rb, const Value *rc)
{
    const String *str = V_STRING(*rb);
    const paw_Int i = V_INT(rc[0]);
    const paw_Int j = V_INT(rc[1]);

    const size_t n = pawS_length(str);
    const size_t zi = check_slice_bound(P, i, n, "start", "string");
    const size_t zj = check_slice_bound(P, j, n, "end", "string");

    const size_t nbytes = zi < zj ? zj - zi : 0;
    String *slice = pawS_new_nstr(P, str->text + zi, nbytes);
    V_SET_OBJECT(ra, slice);
}

void pawR_list_getn(paw_Env *P, CallFrame *cf, Value *ra, const Value *rb, const Value *rc)
{
    const List *list = V_LIST(*rb);
    const paw_Int i = V_INT(rc[0]);
    const paw_Int j = V_INT(rc[1]);

    const size_t n = pawV_list_length(list);
    const size_t zi = check_slice_bound(P, i, n, "start", "list");
    const size_t zj = check_slice_bound(P, j, n, "end", "list");
    List *slice = VM_LIST_INIT(ra);

    const size_t nelems = zi < zj ? zj - zi : 0;
    pawV_list_resize(P, slice, nelems);
    memcpy(slice->begin, list->begin + zi,
            nelems * sizeof(list->begin[0]));
}

static List *list_copy(paw_Env *P, CallFrame *cf, Value *pv, const List *list)
{
    List *copy = pawV_list_new(P);
    V_SET_OBJECT(pv, copy); // anchor
    if (pawV_list_length(list) > 0) {
        pawV_list_resize(P, copy, pawV_list_length(list));
        memcpy(copy->begin, list->begin, sizeof(list->begin[0]) * pawV_list_length(list));
    }
    return copy;
}

// setn([a1..an], i, j, [b1..bn]) => [a1..ai b1..bn aj..an]
void pawR_list_setn(paw_Env *P, CallFrame *cf, Value *ra, const Value *rb, const Value *rc)
{
    List *lhs = V_LIST(*ra);
    const paw_Int i = V_INT(rb[0]);
    const paw_Int j = V_INT(rb[1]);
    const List *rhs = V_LIST(*rc);

    const size_t na = pawV_list_length(lhs);
    const size_t nb = pawV_list_length(rhs);
    const size_t zi = check_slice_bound(P, i, na, "start", "list");
    const size_t zj = check_slice_bound(P, j, na, "end", "list");

    if (lhs == rhs) {
        // handles "list[i:j] = list"
        rhs = list_copy(P, cf, (Value *)rc, rhs);
    }

    const size_t nelems = na - zj + zi + nb;
    pawV_list_reserve(P, lhs, nelems);

    Value *gap = lhs->begin + zi;
    memmove(gap + nb, lhs->begin + zj, (na - zj) * sizeof(lhs->begin[0]));
    memcpy(gap, rhs->begin, nb * sizeof(lhs->begin[0]));
    pawV_list_resize(P, lhs, nelems);
}

Tuple *pawR_new_tuple(paw_Env *P, CallFrame *cf, Value *ra, int b)
{
    Tuple *tuple = pawV_new_tuple(P, b);
    V_SET_OBJECT(ra, tuple);
    return tuple;
}

List *pawR_new_list(paw_Env *P, CallFrame *cf, Value *ra, int b)
{
    List *list = VM_LIST_INIT(ra);
    pawV_list_resize(P, list, CAST_SIZE(b));
    return list;
}

Map *pawR_new_map(paw_Env *P, CallFrame *cf, Value *ra, int b)
{
    Map *map = VM_MAP_INIT(ra);
    pawH_reserve(P, map, CAST_SIZE(b));
    return map;
}


// Macros for generating arithmetic and bitwise operations

#define INT_UNARY_OP(op) { \
        const Value *rb = VM_RB(opcode); \
        V_SET_INT(ra, I_UNOP(V_INT(*rb), op)); \
    }

#define FLOAT_UNARY_OP(op) { \
        const Value *rb = VM_RB(opcode); \
        V_SET_FLOAT(ra, op V_FLOAT(*rb)); \
    }

#define INT_COMPARISON(op) { \
        const Value *rb = VM_RB(opcode); \
        const Value *rc = VM_RC(opcode); \
        V_SET_BOOL(ra, V_INT(*rb) op V_INT(*rc)); \
    }

#define INT_BINARY_OP(op) { \
        const Value *rb = VM_RB(opcode); \
        const Value *rc = VM_RC(opcode); \
        V_SET_INT(ra, I_BINOP(V_INT(*rb), V_INT(*rc), op)); \
    }

#define FLOAT_COMPARISON(op) { \
        const Value *rb = VM_RB(opcode); \
        const Value *rc = VM_RC(opcode); \
        V_SET_BOOL(ra, V_FLOAT(*rb) op V_FLOAT(*rc)); \
    }

#define FLOAT_BINARY_OP(op) { \
        const Value *rb = VM_RB(opcode); \
        const Value *rc = VM_RC(opcode); \
        V_SET_FLOAT(ra, V_FLOAT(*rb) op V_FLOAT(*rc)); \
    }

#define STR_COMPARISON(op) { \
        const Value *rb = VM_RB(opcode); \
        const Value *rc = VM_RC(opcode); \
        V_SET_BOOL(ra, pawS_cmp(V_STRING(*rb), V_STRING(*rc)) op 0); \
    }

// If x / y is undefined, then so too is x % y (see C11 section 6.5.5,
// item 6). Both cases equal 0 in Paw (x / y wraps).
#define DIVMOD_OVERFLOWS(x, y) ((x) == PAW_INT_MIN && (y) == -1)

#define STOP_LOOP(a, b, c) \
    (((c) < 0 && (a) <= (b)) || ((c) > 0 && (a) >= (b)))


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
        const Op op = GET_OP(opcode);
        Value *ra = VM_RA(opcode);

        vm_switch(op)
        {
            vm_case(NOOP):
            {
                // do nothing
            }

            vm_case(CLOSE):
            {
                pawR_close_upvalues(P, ra);
            }

            vm_case(MOVE):
            {
                *ra = *VM_RB(opcode);
            }

            vm_case(LOADSMI):
            {
                V_SET_INT(ra, GET_sBx(opcode));
            }

            vm_case(LOADK):
            {
                *ra = K[GET_Bx(opcode)];
            }

            vm_case(IEQ): INT_COMPARISON(==)
            vm_case(INE): INT_COMPARISON(!=)
            vm_case(ILT): INT_COMPARISON(<)
            vm_case(ILE): INT_COMPARISON(<=)
            vm_case(IGT): INT_COMPARISON(>)
            vm_case(IGE): INT_COMPARISON(>=)

            vm_case(NOT):  INT_UNARY_OP(!)
            vm_case(INEG): INT_UNARY_OP(-)
            vm_case(IADD): INT_BINARY_OP(+)
            vm_case(ISUB): INT_BINARY_OP(-)
            vm_case(IMUL): INT_BINARY_OP(*)

            vm_case(IDIV):
            {
                const paw_Int x = V_INT(*VM_RB(opcode));
                const paw_Int y = V_INT(*VM_RC(opcode));
                if (y == 0) DIVIDE_BY_0(P);
                if (DIVMOD_OVERFLOWS(x, y)) {
                    V_SET_INT(ra, 0);
                } else {
                    V_SET_INT(ra, x / y);
                }
            }

            vm_case(IMOD):
            {
                const paw_Int x = V_INT(*VM_RB(opcode));
                const paw_Int y = V_INT(*VM_RC(opcode));
                if (y == 0) DIVIDE_BY_0(P);
                if (DIVMOD_OVERFLOWS(x, y)) {
                    V_SET_INT(ra, 0);
                } else {
                    V_SET_INT(ra, x % y);
                }
            }

            vm_case(BNOT): INT_UNARY_OP(~)
            vm_case(BAND): INT_BINARY_OP(&)
            vm_case(BOR):  INT_BINARY_OP(|)
            vm_case(BXOR): INT_BINARY_OP(^)

            vm_case(SHL):
            {
                paw_Int x = V_INT(*VM_RB(opcode));
                paw_Int y = V_INT(*VM_RC(opcode));
                if (y < 0) {
                    pawR_error(P, PAW_ERUNTIME, "negative shift count");
                } else if (y > 0) {
                    y = PAW_MIN(y, U2I(sizeof(x) * 8 - 1));
                    x = U2I(I2U(x) << y);
                }
                V_SET_INT(ra, x);
            }

            vm_case(SHR):
            {
                paw_Int x = V_INT(*VM_RB(opcode));
                paw_Int y = V_INT(*VM_RC(opcode));
                if (y < 0) {
                    pawR_error(P, PAW_ERUNTIME, "negative shift count");
                } else if (y > 0) {
                    // Right shift by >= width of 'x' is UB in C. Clamp the
                    // shift count. If 'x' < 0, then the results of the
                    // shift are implementation-defined (may or may not
                    // preserve the sign).
                    y = PAW_MIN(y, U2I(sizeof(x) * 8 - 1));
                    x >>= y;
                }
                V_SET_INT(ra, x);
            }

            vm_case(FEQ):  FLOAT_COMPARISON(==)
            vm_case(FNE):  FLOAT_COMPARISON(!=)
            vm_case(FLT):  FLOAT_COMPARISON(<)
            vm_case(FLE):  FLOAT_COMPARISON(<=)
            vm_case(FGT):  FLOAT_COMPARISON(>)
            vm_case(FGE):  FLOAT_COMPARISON(>=)

            vm_case(FNEG): FLOAT_UNARY_OP(-)
            vm_case(FADD): FLOAT_BINARY_OP(+)
            vm_case(FSUB): FLOAT_BINARY_OP(-)
            vm_case(FMUL): FLOAT_BINARY_OP(*)

            vm_case(FDIV):
            {
                const paw_Float x = V_FLOAT(*VM_RB(opcode));
                const paw_Float y = V_FLOAT(*VM_RC(opcode));
                if (y == 0.0) DIVIDE_BY_0(P);
                V_SET_FLOAT(ra, x / y);
            }

            vm_case(FMOD):
            {
                const paw_Float x = V_FLOAT(*VM_RB(opcode));
                const paw_Float y = V_FLOAT(*VM_RC(opcode));
                if (y == 0.0) DIVIDE_BY_0(P);
                V_SET_FLOAT(ra, fmod(x, y));
            }

            vm_case(SLENGTH):
            {
                const Value *rb = VM_RB(opcode);
                pawR_str_length(P, cf, ra, rb);
            }

            vm_case(SEQ): STR_COMPARISON(==)
            vm_case(SNE): STR_COMPARISON(!=)
            vm_case(SLT): STR_COMPARISON(<)
            vm_case(SLE): STR_COMPARISON(<=)
            vm_case(SGT): STR_COMPARISON(>)
            vm_case(SGE): STR_COMPARISON(>=)

            vm_case(SCONCAT):
            {
                VM_SAVE_PC();
                const int b = GET_B(opcode);
                P->top.p = ra + b;
                pawR_str_concat(P, cf, b);
                CHECK_GC(P);
            }

            vm_case(LLENGTH):
            {
                const Value *rb = VM_RB(opcode);
                pawR_list_length(P, cf, ra, rb);
            }

            vm_case(LCONCAT):
            {
                VM_SAVE_PC();
                const int b = GET_B(opcode);
                P->top.p = ra + b;
                pawR_list_concat(P, cf, b);
                CHECK_GC(P);
            }

            vm_case(SGET):
            {
                VM_SAVE_PC();
                const Value *rb = VM_RB(opcode);
                const Value *rc = VM_RC(opcode);
                pawR_str_get(P, cf, ra, rb, rc);
            }

            vm_case(SGETN):
            {
                VM_SAVE_PC();
                const Value *rb = VM_RB(opcode);
                Value *rc = VM_RC(opcode);
                P->top.p = rc + 1;
                pawR_str_getn(P, cf, ra, rb, rc);
            }

            vm_case(LGET):
            {
                VM_SAVE_PC();
                const Value *rb = VM_RB(opcode);
                const Value *rc = VM_RC(opcode);
                pawR_list_get(P, cf, ra, rb, rc);
            }

            vm_case(LSET):
            {
                VM_SAVE_PC();
                const Value *rb = VM_RB(opcode);
                const Value *rc = VM_RC(opcode);
                pawR_list_set(P, cf, ra, rb, rc);
            }

            vm_case(LGETN):
            {
                VM_SAVE_PC();
                const Value *rb = VM_RB(opcode);
                Value *rc = VM_RC(opcode);
                P->top.p = rc + 1;
                pawR_list_getn(P, cf, ra, rb, rc);
            }

            vm_case(LSETN):
            {
                VM_SAVE_PC();
                const Value *rb = VM_RB(opcode);
                Value *rc = VM_RC(opcode);
                P->top.p = rc + 1;
                pawR_list_setn(P, cf, ra, rb, rc);
            }

            vm_case(MLENGTH):
            {
                const Value *rb = VM_RB(opcode);
                pawR_map_length(P, cf, ra, rb);
            }

            vm_case(MGET):
            {
                VM_SAVE_PC();
                const Value *rb = VM_RB(opcode);
                const Value *rc = VM_RC(opcode);
                if (pawR_map_get(P, cf, ra, rb, rc)) {
                    pawR_error(P, PAW_EKEY, "key does not exist");
                }
            }

            vm_case(MSET):
            {
                VM_SAVE_PC();
                const Value *rb = VM_RB(opcode);
                const Value *rc = VM_RC(opcode);
                pawR_map_set(P, cf, ra, rb, rc);
            }

            vm_case(NEWTUPLE):
            {
                VM_SAVE_PC();
                P->top.p = ra + 1;
                const int b = GET_B(opcode);
                pawR_new_tuple(P, cf, ra, b);
                CHECK_GC(P);
            }

            vm_case(NEWLIST):
            {
                VM_SAVE_PC();
                P->top.p = ra + 1;
                const int b = GET_B(opcode);
                pawR_new_list(P, cf, ra, b);
                CHECK_GC(P);
            }

            vm_case(NEWMAP):
            {
                VM_SAVE_PC();
                P->top.p = ra + 1;
                const int b = GET_B(opcode);
                pawR_new_map(P, cf, ra, b);
                CHECK_GC(P);
            }

            vm_case(TESTK):
            {
                const Value *ra = VM_RA(opcode);
                const Value k = K[GET_B(opcode)];
                const int c = GET_C(opcode);
                if (ra->u != k.u) ++pc;
            }

            vm_case(SWITCHINT):
            {
                const Value *ra = VM_RA(opcode);
                const paw_Int b = GET_B(opcode);
                if (V_INT(*ra) != b) ++pc;
            }

            vm_case(XCASTB):
            {
                const Value *rb = VM_RB(opcode);
                V_SET_BOOL(ra, rb->u != 0);
            }

            vm_case(ICASTF):
            {
                const Value *rb = VM_RB(opcode);
                const paw_Int i = V_INT(*rb);
                V_SET_FLOAT(ra, CAST(paw_Float, i));
            }

            vm_case(FCASTI):
            {
                const Value *rb = VM_RB(opcode);
                const paw_Float f = V_FLOAT(*rb);
                float2int(P, f, ra);
            }

            vm_case(GETUPVALUE):
            {
                const int b = GET_B(opcode);
                *ra = *VM_UPVALUE(b);
            }

            vm_case(SETUPVALUE):
            {
                const Value *rb = VM_RB(opcode);
                *VM_UPVALUE(GET_A(opcode)) = *rb;
            }

            vm_case(GETGLOBAL):
            {
                const int bc = GET_Bx(opcode);
                *ra = *Y_PVAL(P, bc);
            }

            vm_case(GETFIELD):
            {
                VM_SAVE_PC();
                const Value *rb = VM_RB(opcode);
                const int c = GET_C(opcode);
                pawR_tuple_get(cf, ra, rb, c);
            }

            vm_case(SETFIELD):
            {
                VM_SAVE_PC();
                const int b = GET_B(opcode);
                const Value *rc = VM_RC(opcode);
                pawR_tuple_set(cf, ra, b, rc);
            }

            vm_case(CLOSURE):
            {
                const int bx = GET_Bx(opcode);

                VM_SAVE_PC();
                Proto *proto = fn->p->p[bx];
                Closure *closure = pawV_new_closure(P, proto->nup);
                V_SET_OBJECT(ra, closure);
                closure->p = proto;
                P->top.p = ra + 1;

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

            vm_case(CALL):
            {
                const uint8_t b = GET_B(opcode);
                VM_SAVE_PC();

                P->top.p = ra + b + 1;
                CallFrame *callee = pawC_precall(P, ra, V_OBJECT(*ra), b);
                if (callee) {
                    cf = callee;
                    goto top;
                }
            }

            vm_case(RETURN):
            {
                P->top.p = CF_STACK_RETURN(cf);
                VM_SAVE_PC();

                pawR_close_upvalues(P, P->top.p);
                ++P->top.p;

                P->cf = cf->prev;
                if (CF_IS_ENTRY(cf)) {
                    // return from entrypoint function
                    return;
                }
                cf = P->cf;
                goto top;
            }

            vm_case(JUMP):
            {
                pc += GET_sBx(opcode);
            }

            vm_case(JUMPT):
            {
                if (V_TRUE(*ra)) pc += GET_sBx(opcode);
            }

            vm_case(JUMPF):
            {
                if (!V_TRUE(*ra)) pc += GET_sBx(opcode);
            }

            vm_case(FORPREP):
            {
                const paw_Int step = V_INT(ra[0]);
                const paw_Int end = V_INT(ra[1]);
                const paw_Int iter = V_INT(ra[2]);
                paw_Int *pvar = &V_INT(ra[3]);
                if (step == 0) {
                    pawR_error(P, PAW_ERUNTIME, "loop step equals 0");
                }
                if (STOP_LOOP(iter, end, step)) {
                    // skip loop body
                    pc += GET_sBx(opcode);
                } else {
                    *pvar = iter;
                }
            }

            vm_case(FORLOOP):
            {
                const paw_Int step = V_INT(ra[0]);
                const paw_Int end = V_INT(ra[1]);
                paw_Int *piter = &V_INT(ra[2]);
                paw_Int *pvar = &V_INT(ra[3]);
                const paw_Int next = I_BINOP(*piter, step, +);
                if (!STOP_LOOP(next, end, step)) {
                    // update state and jump to loop body
                    *pvar = *piter = next;
                    pc += GET_sBx(opcode);
                }
            }

            vm_default:
                PAW_UNREACHABLE();
        }
    }
}
