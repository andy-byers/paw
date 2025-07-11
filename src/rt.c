// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#include "prefix.h"
#include <math.h>

#include "auxlib.h"
#include "call.h"
#include "debug.h"
#include "env.h"
#include "gc.h"
#include "list.h"
#include "map.h"
#include "rt.h"
#include "rtti.h"
#include "value.h"

// Helpers for the VM:

#define vm_switch(x) switch (x)
#define vm_case(x) \
    break;         \
    case OP_##x
#define vm_default \
    break;         \
    default

#define VM_FETCH(P)                        \
    do {                                   \
        if (PAW_UNLIKELY(trap)) {          \
            trap = pawD_trace_exec(P, pc); \
            updatebase(ci);                \
        }                                  \
        i = *pc++;                         \
    } while (0)

#define VM_SAVE_PC() (cf->pc = pc)
#define VM_UPVALUE(u) (fn->up[u]->p.p)

#define VM_RA(opcode) (&cf->base.p[GET_A(opcode)])
#define VM_RB(opcode) (&cf->base.p[GET_B(opcode)])
#define VM_RC(opcode) (&cf->base.p[GET_C(opcode)])

#define VM_REG(r) (&cf->base.p[r])

// Generate code for creating common builtin objects
#define VM_LIST_INIT(z, r, n) \
    pawList_new(P, z, n, r);
#define VM_MAP_INIT(p, r, n) \
    pawMap_new(P, p, n, r);

static void add_location(paw_Env *P, Buffer *buf)
{
    CallFrame const *cf = P->cf;
    for (; cf != &P->main; cf = cf->prev) {
        int const line = pawD_line_number(cf, cf->pc);
        if (line >= 0) {
            Proto const *p = cf->fn->p;
            char const *name = p->modname->text;
            pawL_add_fstring(P, buf, "%s:%d: ", name, line);
            break;
        } else if (CF_IS_ENTRY(cf)) {
            L_ADD_LITERAL(P, buf, "[C]: ");
            break;
        }
    }
}

static void add_3_parts(paw_Env *P, char const *before, char const *value,
                        char const *after)
{
    Buffer buf;
    pawL_init_buffer(P, &buf);
    add_location(P, &buf);
    pawL_add_fstring(P, &buf, "%s%s%s", before, value, after);
    pawL_push_result(P, &buf);
}

void pawR_error(paw_Env *P, int error, char const *fmt, ...)
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
    ((f) >= CAST(paw_Float, PAW_INT_MIN) && (f) < -CAST(paw_Float, PAW_INT_MIN) && (V_SET_INT(pv, PAW_CAST_INT(f)), 1))

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

void pawR_close_upvalues(paw_Env *P, StackPtr const top)
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

void pawR_tuple_get(CallFrame *cf, Value *ra, Value const *rb, int index)
{
    Tuple const *t = V_TUPLE(*rb);
    paw_assert(0 <= index && index < t->nelems);
    *ra = t->elems[index];
}

void pawR_tuple_set(CallFrame *cf, Value *ra, int index, Value const *rb)
{
    Tuple *t = V_TUPLE(*ra);
    paw_assert(0 <= index && index < t->nelems);
    t->elems[index] = *rb;
}

void pawR_init(paw_Env *P)
{
    Str *errmsg = pawS_new_str(P, "not enough memory");
    pawG_fix_object(P, CAST_OBJECT(errmsg));
    V_SET_OBJECT(&P->mem_errmsg, errmsg);
}

#define DIVIDE_BY_0(P) pawR_error(P, PAW_ERUNTIME, "divide by 0")

static size_t check_slice_bound(paw_Env *P, paw_Int index, size_t length, char const *what, char const *cont)
{
    paw_Int const n = PAW_CAST_INT(length);
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
    if (n <= 1)
        return;

    size_t total_size = 0;
    for (int i = 0; i < n; ++i) {
        total_size += pawS_length(V_STR(ra[i]));
    }

    // caution: unanchored allocation
    Str *r = pawS_new_uninit(P, total_size);
    char *data = r->text;

    paw_assert(data != NULL);
    for (int i = 0; i < n; ++i) {
        Str const *s = V_STR(ra[i]);
        memcpy(data, s->text, s->length);
        data += s->length;
    }
    pawS_register(P, &r);

    V_SET_OBJECT(ra, r);
    P->top.p = ra + 1;
}

void pawR_str_length(paw_Env *P, CallFrame *cf, Value *ra, Value const *rb)
{
    size_t const length = pawS_length(V_STR(*rb));
    V_SET_INT(ra, PAW_CAST_INT(length));
}

void pawR_list_length(paw_Env *P, CallFrame *cf, Value *ra, Value const *rb)
{
    size_t const length = pawList_length(P, V_TUPLE(*rb));
    V_SET_INT(ra, PAW_CAST_INT(length));
}

void pawR_list_concat(paw_Env *P, CallFrame *cf, int n)
{
    // TODO: convert to binary operation in the compiler, maybe do the "concatenate n things on top of the stack" later

    StackPtr ra = P->top.p - n;
    Tuple const *a = V_TUPLE(ra[0]);
    Tuple const *b = V_TUPLE(ra[1]);
    Value *rout = P->top.p++;
    pawList_concat(P, a, b, rout);

    P->top.p = ra + 1;
    *ra = *rout;
}

void pawR_str_get(paw_Env *P, CallFrame *cf, Value *ra, Value const *rb, Value const *rc)
{
    Str const *str = V_STR(*rb);
    size_t const index = pawV_check_abs(P, V_INT(*rc), str->length, "str");
    V_SET_CHAR(ra, (paw_Char)str->text[index]);
}

static void list_out_of_bounds(paw_Env *P, Tuple const *list, paw_Int index)
{
    pawV_index_error(P, index, pawList_length(P, list), "list");
}

void pawR_list_getp(paw_Env *P, CallFrame *cf, Value *ra, Value const *rb, Value const *rc)
{
    Tuple *list = V_TUPLE(*rb);
    paw_Int const index = V_INT(*rc);
    Value *pv = pawList_get(P, list, index);
    if (pv == NULL) list_out_of_bounds(P, list, index);
    ra->p = pv;
}

void pawR_list_get(paw_Env *P, CallFrame *cf, Value *ra, Value const *rb, Value const *rc)
{
    Tuple *list = V_TUPLE(*rb);
    paw_Int const index = V_INT(*rc);
    Value const *pv = pawList_get(P, list, index);
    if (pv == NULL) list_out_of_bounds(P, list, index);
    *ra = *pv;
}

void pawR_list_set(paw_Env *P, CallFrame *cf, Value *ra, Value const *rb, Value const *rc)
{
    Tuple *list = V_TUPLE(*ra);
    paw_Int const index = V_INT(*rb);
    Value *pv = pawList_get(P, list, index);
    if (pv == NULL) list_out_of_bounds(P, list, index);
    *pv = *rc;
}

void pawR_map_length(paw_Env *P, CallFrame *cf, Value *ra, Value const *rb)
{
    size_t const length = pawMap_length(V_TUPLE(*rb));
    V_SET_INT(ra, PAW_CAST_INT(length));
}

static void emit_ptr(paw_Env *P, ptrdiff_t offset, Value *ptr)
{
    RESTORE_POINTER(P, offset)->p = ptr;
}

int pawR_map_getp(paw_Env *P, CallFrame *cf, Value *ra, Value const *rb, Value const *rc)
{
    Tuple *map = V_TUPLE(*rb);
    ptrdiff_t out = SAVE_OFFSET(P, ra);
    Value *ptr = pawMap_get(P, map, rc);
    if (ptr == NULL)
        return -1;
    emit_ptr(P, out, ptr);
    return 0;
}

void pawR_map_newp(paw_Env *P, CallFrame *cf, Value *ra, Value const *rb, Value const *rc)
{
    Tuple *map = V_TUPLE(*rb);
    ptrdiff_t out = SAVE_OFFSET(P, ra);
    Value *ptr = pawMap_create(P, map, rc);
    emit_ptr(P, out, ptr);
}

int pawR_map_get(paw_Env *P, CallFrame *cf, Value *ra, Value const *rb, Value const *rc)
{
    Tuple *map = V_TUPLE(*rb);
    ptrdiff_t out = SAVE_OFFSET(P, ra);
    Value const *pval = pawMap_get(P, map, rc);
    if (pval == NULL)
        return -1;
    ra = RESTORE_POINTER(P, out);
    *ra = *pval;
    return 0;
}

void pawR_map_set(paw_Env *P, CallFrame *cf, Value *ra, Value const *rb, Value const *rc)
{
    pawMap_insert(P, V_TUPLE(*ra), rb, rc);
}

void pawR_str_getn(paw_Env *P, CallFrame *cf, Value *ra, Value const *rb, Value const *rc)
{
    Str const *str = V_STR(*rb);
    paw_Int const i = V_INT(rc[0]);
    paw_Int const j = V_INT(rc[1]);

    size_t const n = pawS_length(str);
    size_t const zi = check_slice_bound(P, i, n, "start", "string");
    size_t const zj = check_slice_bound(P, j, n, "end", "string");
    if (zi > zj)
        pawE_error(P, PAW_ERUNTIME, -1, "string slice \"start\" is greater than \"end\"");

    size_t const nbytes = zi < zj ? zj - zi : 0;
    Str *slice = pawS_new_nstr(P, str->text + zi, nbytes);
    V_SET_OBJECT(ra, slice);
}

void pawR_list_getn(paw_Env *P, CallFrame *cf, Value *ra, Value const *rb, Value const *rc, Value *rout)
{
    Tuple const *t = V_TUPLE(*rb);
    paw_Int const i = V_INT(rc[0]);
    paw_Int const j = V_INT(rc[1]);
    Tuple *out = VM_LIST_INIT(LIST_ZELEMENT(t), rout, 0);
    pawList_get_range(P, t, i, j, out);
    V_SET_OBJECT(ra, out);
}

// setn([a1..an], i, j, [b1..bn]) => [a1..ai b1..bn aj..an]
void pawR_list_setn(paw_Env *P, CallFrame *cf, Value *ra, Value const *rb, Value const *rc, Value *rtemp)
{
    Tuple *a = V_TUPLE(*ra);
    paw_Int const i = V_INT(rb[0]);
    paw_Int const j = V_INT(rb[1]);
    Tuple const *b = V_TUPLE(*rc);
    pawList_set_range(P, a, i, j, b, rtemp);
}

Tuple *pawR_new_tuple(paw_Env *P, CallFrame *cf, Value *ra, int b)
{
    Tuple *tuple = pawV_new_tuple(P, b);
    V_SET_OBJECT(ra, tuple);
    return tuple;
}

Tuple *pawR_new_list(paw_Env *P, CallFrame *cf, Value *ra, int b, int c)
{
    Tuple *list = VM_LIST_INIT(c, ra, b);
    pawList_resize(P, list, CAST_SIZE(b));
    return list;
}

Tuple *pawR_new_map(paw_Env *P, CallFrame *cf, Value *ra, int b, int c)
{
    return VM_MAP_INIT(c, ra, b);
}

// Macros for generating scalar operations

#define VM_DIVMOD_OVERFLOWS(Kind_, X_, Y_) (V_##Kind_(X_) == PAW_##Kind_##_MIN \
                                            && V_##Kind_(Y_) == -1)

#define VM_COMPARISON(Kind_, Op_)              \
    {                                          \
        const Value *rb = VM_RB(opcode);       \
        const Value *rc = VM_RC(opcode);       \
        Kind_##_COMPARISON(ra, *rb, *rc, Op_); \
    }

#define VM_UNARY_OP(Kind_, Op_)          \
    {                                    \
        const Value *rb = VM_RB(opcode); \
        Kind_##_UNARY_OP(ra, *rb, Op_);  \
    }

#define VM_BINARY_OP(Kind_, Op_)              \
    {                                         \
        const Value *rb = VM_RB(opcode);      \
        const Value *rc = VM_RC(opcode);      \
        Kind_##_BINARY_OP(ra, *rb, *rc, Op_); \
    }

#define VM_INTEGRAL_DIVMOD(Kind_, Op_)                        \
    {                                                         \
        Value const x = *VM_RB(opcode);                       \
        Value const y = *VM_RC(opcode);                       \
        if (V_##Kind_(y) == 0)                                \
            DIVIDE_BY_0(P);                                   \
        if (VM_DIVMOD_OVERFLOWS(Kind_, x, y)) {               \
            V_SET_##Kind_(ra, PAW_##Kind_##_C(0));            \
        } else {                                              \
            V_SET_##Kind_(ra, V_##Kind_(x) Op_ V_##Kind_(y)); \
        }                                                     \
    }

// NOTE: Shifting by >= width of 'x' is UB in C. The following macros clamp the shift count
//       to avoid this situation. If 'x' < 0, then the results of the shift are implementation-
//       defined (may or may not preserve the sign).

#define VM_INTEGRAL_SHL(Kind_)                                                             \
    {                                                                                      \
        Value x = *VM_RB(opcode);                                                          \
        Value y = *VM_RC(opcode);                                                          \
        if (V_##Kind_(y) < 0) {                                                            \
            pawR_error(P, PAW_ERUNTIME, "negative shift count");                           \
        } else if (V_##Kind_(y) > 0) {                                                     \
            V_##Kind_(y) = PAW_MIN(V_##Kind_(y), U2##Kind_(sizeof(V_##Kind_(x)) * 8 - 1)); \
            V_##Kind_(x) = U2##Kind_(Kind_##2U(V_##Kind_(x)) << V_##Kind_(y));             \
        }                                                                                  \
        *ra = x;                                                                           \
    }

#define VM_INTEGRAL_SHR(Kind_)                                                             \
    {                                                                                      \
        Value x = *VM_RB(opcode);                                                          \
        Value y = *VM_RC(opcode);                                                          \
        if (V_##Kind_(y) < 0) {                                                            \
            pawR_error(P, PAW_ERUNTIME, "negative shift count");                           \
        } else if (V_##Kind_(y) > 0) {                                                     \
            V_##Kind_(y) = PAW_MIN(V_##Kind_(y), U2##Kind_(sizeof(V_##Kind_(x)) * 8 - 1)); \
            V_##Kind_(x) >>= V_##Kind_(y);                                                 \
        }                                                                                  \
        *ra = x;                                                                           \
    }

// If x / y is undefined, then so too is x % y (see C11 section 6.5.5,
// item 6). Both cases equal 0 in Paw (x / y wraps).
#define DIVMOD_OVERFLOWS(x, y) ((x) == PAW_INT_MIN && (y) == -1)

static void call_hook(paw_Env *P)
{
#define CHECK_FLAG(P_, Kind_) ((P_)->hook_mask & (1 << ((Kind_) - 1)))

    if (P->hook_count <= 0) {
        P->hook_mask = 0;
        return;
    }

    CallFrame *cf = P->cf;
    paw_Debug const d = {
        .modname = P->modname->text,
        .line = pawD_line_number(cf, cf->pc),
        .cf = cf,
    };
    if (CHECK_FLAG(P, PAW_HOOKCALL))
        P->hook(P, &d);

    --P->hook_count;

#undef CHECK_FLAG
}

// clang-format off
void pawR_execute(paw_Env *P, CallFrame *cf)
{
    Value const *K;
    OpCode const *pc;
    Closure *fn;

top:
    pc = cf->pc;
    fn = cf->fn;
    K = fn->p->k;

    if (P->hook_mask != 0)
        call_hook(P);

    for (;;) {
        OpCode const opcode = *pc++;
        Op const op = GET_OP(opcode);
        Value *ra = VM_RA(opcode);

        vm_switch(op)
        {
            vm_case(MOVE) :
            {
                *ra = *VM_RB(opcode);
            }

            vm_case(LOADSMI) :
            {
                V_SET_INT(ra, GET_sBx(opcode));
            }

            vm_case(LOADK) :
            {
                *ra = K[GET_Bx(opcode)];
            }

            vm_case(CEQ) : VM_COMPARISON(CHAR, ==)
            vm_case(CNE) : VM_COMPARISON(CHAR, !=)
            vm_case(CLT) : VM_COMPARISON(CHAR, <)
            vm_case(CLE) : VM_COMPARISON(CHAR, <=)

            vm_case(IEQ) : VM_COMPARISON(INT, ==)
            vm_case(INE) : VM_COMPARISON(INT, !=)
            vm_case(ILT) : VM_COMPARISON(INT, <)
            vm_case(ILE) : VM_COMPARISON(INT, <=)

            vm_case(INOT) : VM_UNARY_OP(INT, !)
            vm_case(INEG) : VM_UNARY_OP(INT, -)
            vm_case(IADD) : VM_BINARY_OP(INT, +)
            vm_case(ISUB) : VM_BINARY_OP(INT, -)
            vm_case(IMUL) : VM_BINARY_OP(INT, *)

            vm_case(BITNOT) : VM_UNARY_OP(INT, ~)
            vm_case(BITAND) : VM_BINARY_OP(INT, &)
            vm_case(BITOR) : VM_BINARY_OP(INT, |)
            vm_case(BITXOR) : VM_BINARY_OP(INT, ^)
            vm_case(SHL) : VM_INTEGRAL_SHL(INT)
            vm_case(SHR) : VM_INTEGRAL_SHR(INT)
            vm_case(IDIV) : VM_INTEGRAL_DIVMOD(INT, /)
            vm_case(IMOD) : VM_INTEGRAL_DIVMOD(INT, %)

            vm_case(FEQ) : VM_COMPARISON(FLOAT, ==)
            vm_case(FNE) : VM_COMPARISON(FLOAT, !=)
            vm_case(FLT) : VM_COMPARISON(FLOAT, <)
            vm_case(FLE) : VM_COMPARISON(FLOAT, <=)

            vm_case(FNEG) : VM_UNARY_OP(FLOAT, -)
            vm_case(FADD) : VM_BINARY_OP(FLOAT, +)
            vm_case(FSUB) : VM_BINARY_OP(FLOAT, -)
            vm_case(FMUL) : VM_BINARY_OP(FLOAT, *)

            vm_case(FDIV) :
            {
                paw_Float const x = V_FLOAT(*VM_RB(opcode));
                paw_Float const y = V_FLOAT(*VM_RC(opcode));
                if (y == 0.0)
                    DIVIDE_BY_0(P);
                V_SET_FLOAT(ra, x / y);
            }

            vm_case(FMOD) :
            {
                paw_Float const x = V_FLOAT(*VM_RB(opcode));
                paw_Float const y = V_FLOAT(*VM_RC(opcode));
                if (y == 0.0)
                    DIVIDE_BY_0(P);
                V_SET_FLOAT(ra, fmod(x, y));
            }

            vm_case(STRLEN) :
            {
                Value const *rb = VM_RB(opcode);
                pawR_str_length(P, cf, ra, rb);
            }

            vm_case(STREQ) : VM_COMPARISON(STR, ==)
            vm_case(STRNE) : VM_COMPARISON(STR, !=)
            vm_case(STRLT) : VM_COMPARISON(STR, <)
            vm_case(STRLE) : VM_COMPARISON(STR, <=)

            vm_case(STRCAT) :
            {
                VM_SAVE_PC();
                int const b = GET_B(opcode);
                P->top.p = ra + b;
                pawR_str_concat(P, cf, b);
                CHECK_GC(P);
            }

            vm_case(LISTLEN) :
            {
                Value const *rb = VM_RB(opcode);
                pawR_list_length(P, cf, ra, rb);
            }

            vm_case(LISTCAT) :
            {
                VM_SAVE_PC();
                int const b = GET_B(opcode);
                P->top.p = ra + b;
                pawR_list_concat(P, cf, b);
                CHECK_GC(P);
            }

            vm_case(STRGET) :
            {
                VM_SAVE_PC();
                Value const *rb = VM_RB(opcode);
                Value const *rc = VM_RC(opcode);
                pawR_str_get(P, cf, ra, rb, rc);
            }

            vm_case(STRGETN) :
            {
                VM_SAVE_PC();
                Value const *rb = VM_RB(opcode);
                Value *rc = VM_RC(opcode);
                P->top.p = rc + 1;
                pawR_str_getn(P, cf, ra, rb, rc);
            }

            vm_case(LISTGETP) :
            {
                VM_SAVE_PC();
                Value const *rb = VM_RB(opcode);
                Value const *rc = VM_RC(opcode);
                pawR_list_getp(P, cf, ra, rb, rc);
            }

            vm_case(LISTGET) :
            {
                VM_SAVE_PC();
                Value const *rb = VM_RB(opcode);
                Value const *rc = VM_RC(opcode);
                pawR_list_get(P, cf, ra, rb, rc);
            }

            vm_case(LISTSET) :
            {
                VM_SAVE_PC();
                Value const *rb = VM_RB(opcode);
                Value const *rc = VM_RC(opcode);
                pawR_list_set(P, cf, ra, rb, rc);
            }

            vm_case(LISTGETN) :
            {
                VM_SAVE_PC();
                Value const *rb = VM_RB(opcode);
                Value const *rc = VM_RC(opcode);
                Value *temp = VM_RC(opcode) + 1;
                P->top.p = temp + 1;
                pawR_list_getn(P, cf, ra, rb, rc, temp);
            }

            vm_case(LISTSETN) :
            {
                VM_SAVE_PC();
                Value const *rb = VM_RB(opcode);
                Value const *rc = VM_RC(opcode);
                Value *temp = VM_RC(opcode) + 1;
                P->top.p = temp + 1;
                pawR_list_setn(P, cf, ra, rb, rc, temp);
            }

            vm_case(MAPLEN) :
            {
                Value const *rb = VM_RB(opcode);
                pawR_map_length(P, cf, ra, rb);
            }

            vm_case(MAPGETP) :
            {
                VM_SAVE_PC();
                Value const *rb = VM_RB(opcode);
                Value *rc = VM_RC(opcode);
                P->top.p = rc + 1;
                // TODO: This won't work if the key is multiple values in size, need to call map.get() or allocate key registers contiguously
                if (pawR_map_getp(P, cf, ra, rb, rc))
                    pawR_error(P, PAW_EKEY, "key does not exist");
            }

            vm_case(MAPNEWP) :
            {
                VM_SAVE_PC();
                Value const *rb = VM_RB(opcode);
                Value *rc = VM_RC(opcode);
                P->top.p = rc + 1;
                pawR_map_newp(P, cf, ra, rb, rc);
            }

            vm_case(MAPGET) :
            {
                VM_SAVE_PC();
                Value const *rb = VM_RB(opcode);
                Value *rc = VM_RC(opcode);
                P->top.p = rc + 1;
                if (pawR_map_get(P, cf, ra, rb, rc)) {
                    pawR_error(P, PAW_EKEY, "key does not exist");
                }
            }

            vm_case(MAPSET) :
            {
                VM_SAVE_PC();
                Value const *rb = VM_RB(opcode);
                Value *rc = VM_RC(opcode);
                P->top.p = rc + 1;
                pawR_map_set(P, cf, ra, rb, rc);
            }

            vm_case(NEWTUPLE) :
            {
                VM_SAVE_PC();
                P->top.p = ra + 1;
                int const b = GET_B(opcode);
                pawR_new_tuple(P, cf, ra, b);
                CHECK_GC(P);
            }

            vm_case(NEWLIST) :
            {
                VM_SAVE_PC();
                P->top.p = ra + 1;
                int const b = GET_B(opcode);
                int const c = GET_C(opcode);
                pawR_new_list(P, cf, ra, b, c);
                CHECK_GC(P);
            }

            vm_case(NEWMAP) :
            {
                VM_SAVE_PC();
                P->top.p = ra + 1;
                int const b = GET_B(opcode);
                int const c = GET_C(opcode);
                pawR_new_map(P, cf, ra, b, c);
                CHECK_GC(P);
            }

            vm_case(GETVALUE) :
            {
                Value const *rb = VM_RB(opcode);
                int const c = GET_C(opcode);
                *ra = ((Value const *)rb->p)[c];

            }

            vm_case(SETVALUE) :
            {
                int const b = GET_B(opcode);
                Value const *rc = VM_RC(opcode);
                ((Value *)ra->p)[b] = *rc;
            }

            vm_case(CLOSE) :
            {
                pawR_close_upvalues(P, ra);
            }

            vm_case(TESTK) :
            {
                Value const *ra = VM_RA(opcode);
                Value const k = K[GET_Bx(opcode)];
                if (ra->u != k.u)
                    ++pc;
            }

            vm_case(SWITCHINT) :
            {
                Value const *ra = VM_RA(opcode);
                paw_Int const b = GET_B(opcode);
                if (V_INT(*ra) != b)
                    ++pc;
            }

            vm_case(BCASTF) :
            {
                Value const *rb = VM_RB(opcode);
                V_SET_FLOAT(ra, V_TRUE(*rb));
            }

            vm_case(CCASTI) :
            {
                Value const *rb = VM_RB(opcode);
                V_SET_INT(ra, V_CHAR(*rb));
            }

            vm_case(ICASTB) :
            {
                Value const *rb = VM_RB(opcode);
                V_SET_BOOL(ra, V_TRUE(*rb));
            }

            // TODO: check bounds
            vm_case(ICASTC) :
            {
                Value const *rb = VM_RB(opcode);
                V_SET_CHAR(ra, (paw_Char)V_INT(*rb));
            }

            vm_case(ICASTF) :
            {
                Value const *rb = VM_RB(opcode);
                V_SET_FLOAT(ra, (paw_Float)V_INT(*rb));
            }

            vm_case(FCASTB) :
            {
                Value const *rb = VM_RB(opcode);
                V_SET_BOOL(ra, (paw_Bool)V_FLOAT(*rb));
            }

            vm_case(FCASTI) :
            {
                Value const *rb = VM_RB(opcode);
                float2int(P, V_FLOAT(*rb), ra);
            }

            vm_case(GETUPVALUE) :
            {
                int const b = GET_B(opcode);
                *ra = *VM_UPVALUE(b);
            }

            vm_case(SETUPVALUE) :
            {
                Value const *rb = VM_RB(opcode);
                *VM_UPVALUE(GET_A(opcode)) = *rb;
            }

            vm_case(GETGLOBAL) :
            {
                int const bc = GET_Bx(opcode);
                *ra = *RTTI_PVAL(P, bc);
            }

            vm_case(GETFIELD) :
            {
                VM_SAVE_PC();
                Value const *rb = VM_RB(opcode);
                int const c = GET_C(opcode);
                pawR_tuple_get(cf, ra, rb, c);
            }

            vm_case(SETFIELD) :
            {
                VM_SAVE_PC();
                int const b = GET_B(opcode);
                Value const *rc = VM_RC(opcode);
                pawR_tuple_set(cf, ra, b, rc);
            }

            vm_case(CLOSURE) :
            {
                int const bx = GET_Bx(opcode);

                VM_SAVE_PC();
                P->top.p = ra + 1;
                Proto *proto = fn->p->p[bx];
                Closure *closure = pawV_new_closure(P, proto->nup);
                V_SET_OBJECT(ra, closure);
                closure->p = proto;

                // open upvalues
                StackPtr base = cf->base.p;
                for (int i = 0; i < closure->nup; ++i) {
                    struct UpValueInfo const u = proto->u[i];
                    closure->up[i] = u.is_local
                                         ? capture_upvalue(P, base + u.index)
                                         : fn->up[u.index];
                }
                CHECK_GC(P);
            }

            vm_case(CALL) :
            {
                uint8_t const b = GET_B(opcode);
                VM_SAVE_PC();

                P->top.p = ra + b + 1;
                CallFrame *callee = pawC_precall(P, ra, V_OBJECT(*ra), b);
                if (callee) {
                    cf = callee;
                    goto top;
                }
            }

            vm_case(RETURN) :
            {
                P->top.p = CF_STACK_RETURN(cf);
                VM_SAVE_PC();

                pawR_close_upvalues(P, P->top.p);
                P->top.p = ra;

                P->cf = cf->prev;
                if (CF_IS_ENTRY(cf)) {
                    // return from entrypoint function
                    return;
                }
                cf = P->cf;
                goto top;
            }

            vm_case(JUMP) :
            {
                pc += GET_sBx(opcode);
            }

            vm_case(JUMPT) :
            {
                if (V_TRUE(*ra))
                    pc += GET_sBx(opcode);
            }

            vm_case(JUMPF) :
            {
                if (!V_TRUE(*ra))
                    pc += GET_sBx(opcode);
            }

            vm_case(NOOP) :
            {
                // do nothing
            }

        vm_default:
            PAW_UNREACHABLE();
        }
    }
}
// clang-format on
