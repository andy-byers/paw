// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#include "prefix.h"
#include <stdlib.h>

#include "alloc.h"
#include "api.h"
#include "compile.h"
#include "env.h"
#include "gc.h"
#include "lib.h"
#include "list.h"
#include "map.h"
#include "parse.h"
#include "paw.h"
#include "rt.h"
#include "rtti.h"


static void *default_alloc(void *ud, void *ptr, size_t old_size, size_t new_size)
{
    PAW_UNUSED(ud);
    if (new_size == 0) {
        free(ptr);
        return NULL;
    }
    if (old_size == 0)
        return malloc(new_size);
    return realloc(ptr, new_size);
}

static void default_mem_hook(void *ud, void *ptr, size_t size0, size_t size)
{
    PAW_UNUSED(ud);
    PAW_UNUSED(ptr);
    PAW_UNUSED(size0);
    PAW_UNUSED(size);
}

static StackPtr at(paw_Env *P, int index)
{
    if (index == PAW_REGISTRY_INDEX)
        return &P->registry;

    int const i = paw_abs_index(P, index);
    API_CHECK(P, 0 <= i && i < paw_get_count(P), "index out of range");
    return &P->cf->base.p[i];
}

size_t paw_bytes_used(paw_Env const *P)
{
    return P->gc_bytes;
}

static void open_aux(paw_Env *P, void *arg)
{
    PAW_UNUSED(arg);
    pawC_init(P);
    pawG_init(P);
    pawS_init(P);
    pawP_init(P);
    pawR_init(P);
    pawL_init(P);
    CHECK_GC(P);
}

#define OR_DEFAULT(a, b) ((a) ? (a) : (b))
#define HEAP_MIN (PAW_ROUND_UP(sizeof(paw_Env)))

paw_Env *paw_open(struct paw_Options const *o)
{
    size_t heap_size = OR_DEFAULT(o->heap_size, PAW_HEAP_DEFAULT);
    heap_size &= ~(PAW_ALIGN - 1); // round down
    if (heap_size < HEAP_MIN)
        return NULL;

    void *ud = OR_DEFAULT(o->ud, NULL);
    paw_Alloc alloc = OR_DEFAULT(o->alloc, default_alloc);
    paw_MemHook mem_hook = OR_DEFAULT(o->mem_hook, default_mem_hook);
    void *heap = OR_DEFAULT(o->heap, alloc(ud, NULL, 0, heap_size));
    paw_Bool const owns_heap = o->heap == NULL;
    if (heap == NULL)
        return NULL;
    paw_assert(PAW_IS_ALIGNED(heap));
    size_t const zh = heap_size;
    void *ph = heap;

    paw_Env *P = heap;
    *P = (paw_Env){
        .heap_size = heap_size,
        .alloc = alloc,
        .ud = ud,
    };
    heap = BUMP_PTR(heap, PAW_ROUND_UP(sizeof(*P)));
    heap_size -= PAW_ROUND_UP(sizeof(*P));

    if (pawZ_init(P, heap, heap_size, owns_heap, mem_hook, ud)) {
        if (owns_heap)
            alloc(ud, ph, zh, 0);
        return NULL;
    }
    if (pawC_raw_try(P, open_aux, NULL)) {
        paw_close(P);
        return NULL;
    }
    return P;
}

void paw_close(paw_Env *P)
{
    pawL_uninit(P);
    pawE_uninit(P);
    pawC_uninit(P);
    pawG_uninit(P);
    pawS_uninit(P);
    pawZ_uninit(P);
}

static void mangle_arg(paw_Env *P, Buffer *buf, paw_Type code)
{
    RttiType const *type = RTTI_TYPE(P, code);
    pawRtti_mangle_add_arg(P, buf, type->hdr.code);
}

static void mangle_types(paw_Env *P, Buffer *buf, paw_Type *types)
{
    if (types != NULL) {
        pawRtti_mangle_start_generic_args(P, buf);
        while (*types >= 0) {
            mangle_arg(P, buf, *types++);
        }
        pawRtti_mangle_finish_generic_args(P, buf);
    }
}

void paw_mangle_start(paw_Env *P)
{
    PAW_PUSH_LITERAL(P, "_P");
}

#define MANGLE_ADD_NAME(P, prefix)              \
    do {                                        \
        paw_push_fstring(P, prefix "%I%s",      \
                         paw_str_rawlen(P, -1), \
                         paw_string(P, -1));    \
        paw_shift(P, 1);                        \
        paw_str_concat(P, 2);                   \
    } while (0)

void paw_mangle_add_module(paw_Env *P)
{
    MANGLE_ADD_NAME(P, "N");
}

void paw_mangle_add_name(paw_Env *P)
{
    MANGLE_ADD_NAME(P, "");
}

void paw_mangle_add_args(paw_Env *P, paw_Type *types)
{
    Buffer buf;
    pawL_init_buffer(P, &buf);
    mangle_types(P, &buf, types);
    pawL_push_result(P, &buf);

    paw_str_concat(P, 2);
}

int paw_lookup_item(paw_Env *P, int index, struct paw_Item *pitem)
{
    String const *name = V_STRING(*at(P, index));
    ItemId const iid = pawE_locate(P, name, PAW_TRUE);

    if (iid < 0)
        return PAW_ENAME;
    if (pitem == NULL)
        return 0;
    struct Def const *def = P->defs.data[iid];
    *pitem = (struct paw_Item){
        .global_id = def->hdr.kind == DEF_FUNC ? def->func.vid : //
            def->hdr.kind == DEF_CONST ? def->const_.vid : -1,
        .type = def->hdr.code,
    };
    return 0;
}

void paw_push_value(paw_Env *P, int index)
{
    Value const v = *at(P, index);
    *P->top.p = v;
    API_INCR_TOP(P, 1);
}

void paw_push_zero(paw_Env *P, int n)
{
    for (int i = 0; i < n; ++i) {
        V_SET_0(P->top.p + i);
    }
    API_INCR_TOP(P, n);
}

void paw_push_bool(paw_Env *P, paw_Bool b)
{
    V_SET_BOOL(P->top.p, b);
    API_INCR_TOP(P, 1);
}

void paw_push_float(paw_Env *P, paw_Float f)
{
    V_SET_FLOAT(P->top.p, f);
    API_INCR_TOP(P, 1);
}

void paw_push_int(paw_Env *P, paw_Int i)
{
    V_SET_INT(P->top.p, i);
    API_INCR_TOP(P, 1);
}

void paw_new_native(paw_Env *P, paw_Function fn, int nup)
{
    Native *o = pawV_new_native(P, fn, nup);
    V_SET_OBJECT(P->top.p, o);
    API_INCR_TOP(P, 1);

    StackPtr top = P->top.p;
    StackPtr const base = top - nup - 1;
    for (int i = 0; i < nup; ++i) {
        o->up[i] = base[i];
    }
    // replace upvalues with closure object
    base[0] = top[-1];
    P->top.p = base + 1;
}

char const *paw_push_string(paw_Env *P, char const *s)
{
    return paw_push_nstring(P, s, strlen(s));
}

char const *paw_push_nstring(paw_Env *P, char const *s, size_t n)
{
    String *str = pawS_new_nstr(P, s, n);
    V_SET_OBJECT(P->top.p, str);
    API_INCR_TOP(P, 1);
    return str->text;
}

char const *paw_push_vfstring(paw_Env *P, char const *fmt, va_list arg)
{
    Buffer buf;
    pawL_init_buffer(P, &buf);
    pawL_add_vfstring(P, &buf, fmt, arg);
    pawL_push_result(P, &buf);
    return paw_string(P, -1);
}

char const *paw_push_fstring(paw_Env *P, char const *fmt, ...)
{
    va_list arg;
    va_start(arg, fmt);
    char const *s = paw_push_vfstring(P, fmt, arg);
    va_end(arg);
    return s;
}

void paw_push_rawptr(paw_Env *P, void *ptr)
{
    P->top.p->p = ptr;
    API_INCR_TOP(P, 1);
}

paw_Bool paw_bool(paw_Env *P, int index)
{
    return V_TRUE(*at(P, index));
}

paw_Int paw_int(paw_Env *P, int index)
{
    return V_INT(*at(P, index));
}

paw_Uint paw_uint(paw_Env *P, int index)
{
    return V_UINT(*at(P, index));
}

paw_Float paw_float(paw_Env *P, int index)
{
    return V_FLOAT(*at(P, index));
}

char const *paw_string(paw_Env *P, int index)
{
    String const *s = V_STRING(*at(P, index));
    return s->text;
}

paw_Function paw_native(paw_Env *P, int index)
{
    Native const *f = V_NATIVE(*at(P, index));
    return f->func;
}

void *paw_userdata(paw_Env *P, int index)
{
    Value const v = *at(P, index);
    return V_FOREIGN(v)->data;
}

void *paw_rawptr(paw_Env *P, int index)
{
    return at(P, index)->p;
}

void *paw_pointer(paw_Env *P, int index)
{
    // must be an object
    Object *o = V_OBJECT(*at(P, index));
    if (o->gc_kind == VNATIVE) {
        return ERASE_TYPE(CAST_UPTR(O_NATIVE(o)->func));
    } else if (o->gc_kind == VFOREIGN) {
        return O_FOREIGN(o)->data;
    } else {
        return o;
    }
}

void paw_pop(paw_Env *P, int n)
{
    paw_assert(n <= pawC_stklen(P));
    pawC_stkdec(P, n);
}

struct CompileState {
    paw_Reader input;
    struct DynamicMem dm;
    char const *name;
    void *ud;
};

static void compile_aux(paw_Env *P, void *arg)
{
    struct Compiler C;
    struct CompileState *p = arg;
    pawP_startup(P, &C, &p->dm, p->name);
    pawP_compile(&C, p->input, p->ud);
}

int paw_load(paw_Env *P, paw_Reader input, char const *name, void *ud)
{
    struct CompileState p = {
        .input = input,
        .name = name,
        .ud = ud,
    };
    int const status = pawC_try(P, compile_aux, &p);
    pawP_teardown(P, &p.dm);
    return status;
}

void paw_load_symbols(paw_Env *P, int index)
{
    paw_push_value(P, index);
    pawL_load_symbols(P);
}

struct CallState {
    StackPtr fn;
    int argc;
};

void eval_aux(paw_Env *P, void *arg)
{
    struct CallState const *ctx = arg;
    pawC_call(P, V_OBJECT(*ctx->fn), ctx->argc);
}

int paw_call(paw_Env *P, int argc)
{
    StackPtr top = P->top.p;
    StackPtr fn = &top[-argc - 1];
    struct CallState c = {.fn = fn, .argc = argc};
    if (P->cf == &P->main)
        // wrap toplevel function calls in a panic context
        return pawC_try(P, eval_aux, &c);
    eval_aux(P, &c);
    return PAW_OK;
}

int paw_get_count(paw_Env *P)
{
    return CAST(int, P->top.p - P->cf->base.p);
}

void paw_get_typename(paw_Env *P, paw_Type code)
{
    Buffer buf;
    pawL_init_buffer(P, &buf);
    RttiType const *type = RTTI_TYPE(P, code);
    pawRtti_print_type(P, &buf, type->hdr.code);
    pawL_push_result(P, &buf);
}

void paw_get_global(paw_Env *P, int gid)
{
    *P->top.p = *RTTI_PVAL(P, gid);
    API_INCR_TOP(P, 1);
}

int paw_call_global(paw_Env *P, int gid, int argc)
{
    // a1..an f => f a1..an
    paw_get_global(P, gid);
    paw_insert(P, -argc - 1);
    return paw_call(P, argc);
}

static int upvalue_index(int nup, int index)
{
    if (index < 0) {
        paw_assert(-index <= nup);
        return nup - index;
    }
    paw_assert(index <= nup);
    return index;
}

void paw_get_upvalue(paw_Env *P, int ifn, int index)
{
    Object *o = at(P, ifn)->o;
    switch (o->gc_kind) {
        case VNATIVE: {
            Native *f = O_NATIVE(o);
            *P->top.p = f->up[upvalue_index(f->nup, index)];
            break;
        }
        case VCLOSURE: {
            Closure *f = O_CLOSURE(o);
            UpValue *u = f->up[upvalue_index(f->nup, index)];
            *P->top.p = *u->p.p;
            break;
        }
        default:
            pawR_error(P, PAW_ETYPE, "type of object has no upvalues");
    }
    API_INCR_TOP(P, 1);
}

void paw_set_upvalue(paw_Env *P, int ifn, int index)
{
    Object *o = at(P, ifn)->o;
    switch (o->gc_kind) {
        case VNATIVE: {
            Native *f = O_NATIVE(o);
            f->up[upvalue_index(f->nup, index)] = P->top.p[-1];
            break;
        }
        case VCLOSURE: {
            Closure *f = O_CLOSURE(o);
            UpValue *u = f->up[upvalue_index(f->nup, index)];
            *u->p.p = P->top.p[-1];
            break;
        }
        default:
            pawR_error(P, PAW_ETYPE, "type of object has no upvalues");
    }
    --P->top.p;
}

void paw_new_tuple(paw_Env *P, int n)
{
    Value *ra = pawC_push0(P);
    pawR_new_tuple(P, P->cf, ra, n);

    Value const *pv = at(P, -n - 1);
    for (int i = 0; i < n; ++i) {
        Value const *rb = pv++;
        pawR_tuple_set(P->cf, ra, i, rb);
    }
    paw_shift(P, n);
}

void paw_new_list(paw_Env *P, int n, paw_Type e)
{
    Value *ra = pawC_push0(P);
    pawR_new_list(P, P->cf, ra, n, e);

    Value const *pv = at(P, -n - 1);
    for (int i = 0; i < n; ++i) {
        Value const rb = {.i = i};
        Value const *rc = pv++;
        pawR_list_set(P, P->cf, ra, &rb, rc);
    }
    paw_shift(P, n);
}

void paw_new_map(paw_Env *P, int n, paw_Type k)
{
    Value *ra = pawC_push0(P);
    pawR_new_map(P, P->cf, ra, n, k);

    Value const *pv = at(P, -2 * n - 1);
    for (int i = 0; i < n; ++i) {
        Value const *rb = pv++;
        Value const *rc = pv++;
        pawR_map_set(P, P->cf, ra, rb, rc);
    }
    paw_shift(P, 2 * n);
}

void *paw_new_foreign(paw_Env *P, size_t size, int nfields)
{
    Value *pv = pawC_push0(P);
    Foreign *ud = pawV_new_foreign(P, size, nfields, 0, pv);
    return ud->data;
}

int paw_abs_index(paw_Env *P, int index)
{
    return index + (index < 0 ? paw_get_count(P) : 0);
}

static void reverse(StackPtr from, StackPtr to)
{
    for (; from < to; from++, to--) {
        Value temp = *from;
        *from = *to;
        *to = temp;
    }
}

// Rotate function from Lua. Original comment:
//   Let x = AB, where A is a prefix of length 'n'. Then,
//   rotate x n == BA. But BA == (A^r . B^r)^r.
void paw_rotate(paw_Env *P, int index, int n)
{
    StackPtr t = P->top.p - 1;
    StackPtr p = at(P, index);
    paw_assert((n >= 0 ? n : -n) <= t - p + 1);
    StackPtr m = n >= 0 ? t - n : p - n - 1;
    reverse(p, m); // reverse A to get A'
    reverse(m + 1, t); // reverse B to get B'
    reverse(p, t); // reverse A'B' to get BA
}

void paw_copy(paw_Env *P, int from, int to)
{
    StackPtr src = at(P, from);
    StackPtr dst = at(P, to);
    *dst = *src;
}

void paw_shift(paw_Env *P, int n)
{
    paw_copy(P, -1, -n - 1);
    paw_pop(P, n);
}

void paw_get_field(paw_Env *P, int index, int ifield)
{
    paw_push_value(P, index);
    StackPtr ra = at(P, -1);
    pawR_tuple_get(P->cf, ra, ra, ifield);
}

void paw_set_field(paw_Env *P, int index, int ifield)
{
    paw_push_value(P, index);
    StackPtr ra = at(P, -1);
    StackPtr rb = at(P, -2);
    pawR_tuple_set(P->cf, ra, ifield, rb);
}

char const *paw_int_to_string(paw_Env *P, int index, size_t *plen)
{
    Value *pv = at(P, index);
    return pawV_to_string(P, pv, PAW_TINT, plen);
}

char const *paw_float_to_string(paw_Env *P, int index, size_t *plen)
{
    Value *pv = at(P, index);
    return pawV_to_string(P, pv, PAW_TFLOAT, plen);
}

void paw_str_length(paw_Env *P, int index)
{
    Value *pv = at(P, index);
    String const *str = V_STRING(*pv);
    size_t const len = pawS_length(str);
    paw_push_int(P, PAW_CAST_INT(len));
}

void paw_str_concat(paw_Env *P, int count)
{
    pawR_str_concat(P, P->cf, count);
}

// void paw_str_getelem(paw_Env *P, int index)
//{
//     paw_push_value(P, index);
//     paw_rotate(P, -2, 1);
//     pawR_getelem(P, PAW_ADT_STR);
// }
//
// void paw_str_getrange(paw_Env *P, int index)
//{
//     paw_push_value(P, index);
//     paw_rotate(P, -3, 1);
//     pawR_getrange(P, PAW_ADT_STR);
// }

void paw_list_length(paw_Env *P, int index)
{
    Value *pv = at(P, index);
    Tuple const *list = V_TUPLE(*pv);
    size_t const len = pawList_length(P, list);
    paw_push_int(P, PAW_CAST_INT(len));
}

void paw_list_concat(paw_Env *P, int count)
{
    pawR_list_concat(P, P->cf, count);
}

// void paw_list_getelem(paw_Env *P, int index)
//{
//     paw_push_value(P, index);
//     paw_rotate(P, -2, 1);
//     pawR_getelem(P, PAW_ADT_LIST);
// }
//
// void paw_list_setelem(paw_Env *P, int index)
//{
//     paw_push_value(P, index);
//     paw_rotate(P, -3, 1);
//     pawR_list_setelem(P);
// }
//
// void paw_list_getrange(paw_Env *P, int index)
//{
//     paw_push_value(P, index);
//     paw_rotate(P, -3, 1);
//     pawR_list_getn(P);
// }
//
// void paw_list_setrange(paw_Env *P, int index)
//{
//     paw_push_value(P, index);
//     paw_rotate(P, -4, 1);
//     pawR_list_setn(P);
// }

paw_Bool paw_list_next(paw_Env *P, int index)
{
    API_CHECK_PUSH(P, 1);
    Tuple *list = V_TUPLE(*at(P, index));
    paw_Int *piter = &P->top.p[-1].i;
    if (pawList_iter(P, list, piter)) {
        P->top.p[0] = *pawList_get(P, list, *piter);
        API_INCR_TOP(P, 1);
        return PAW_TRUE;
    }
    return PAW_FALSE;
}

void paw_map_length(paw_Env *P, int index)
{
    Value *pv = at(P, index);
    Tuple const *map = V_TUPLE(*pv);
    size_t const len = pawMap_length(map);
    paw_push_int(P, PAW_CAST_INT(len));
}

int paw_map_get(paw_Env *P, int index)
{
    Value *ra = at(P, -1); // key/output
    Value const *rb = at(P, index); // map
    if (pawR_map_get(P, P->cf, ra, rb, ra)) {
        paw_pop(P, 1);
        return -1;
    }
    // output register contains value
    return 0;
}

void paw_map_set(paw_Env *P, int index)
{
    Value *ra = at(P, index);
    Value const *rb = at(P, -2);
    Value const *rc = at(P, -1);
    pawR_map_set(P, P->cf, ra, rb, rc);
    paw_pop(P, 2);
}

paw_Bool paw_map_next(paw_Env *P, int index)
{
    API_CHECK_PUSH(P, 2);
    Tuple *map = V_TUPLE(*at(P, index));
    paw_Int *piter = &P->top.p[-1].i;
    if (pawMap_iter(map, piter)) {
        P->top.p[0] = *pawMap_key(P, map, *piter);
        P->top.p[1] = *pawMap_value(P, map, *piter);
        API_INCR_TOP(P, 2);
        return PAW_TRUE;
    }
    return PAW_FALSE;
}

void paw_set_hook(paw_Env *P, paw_ExecHook hook, int mask, int count)
{
    if (hook == NULL || mask == 0) {
        hook = NULL;
        mask = 0;
    }
    P->hook = hook;
    P->hook_mask = mask;
    P->hook_count = count;
}
