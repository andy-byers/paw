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
#include "str.h"


#define ENSURE_SIZE1(P, Size_, What_) API_CHECK(P, (Size_) == 1, "expected " What_ " of size 1");


static StackPtr from_stackp(paw_Env *P, int offset) { return P->top.p + offset; }
static StackPtr from_basep(paw_Env *P, int offset) { return P->cf->base.p + offset; }
static StackPtr lastp(paw_Env *P) { return from_stackp(P, -1); }
static StackPtr stackp(paw_Env *P) { return from_stackp(P, 0); }
static StackPtr basep(paw_Env *P) { return from_basep(P, 0); }

static StackPtr at(paw_Env *P, int index)
{
    if (index == PAW_REGISTRY_INDEX)
        return &P->registry;

    index = paw_abs_index(P, index);
    API_CHECK(P, 0 <= index && index < paw_get_count(P), "index out of range");
    return from_basep(P, index);
}

static void push_values(paw_Env *P, Value const *pvalue, int count)
{
    API_INCR_TOP(P, count);
    pawV_copy(P->top.p - count, pvalue, count);
}


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
        paw_push_fstr(P, prefix "%I%s",         \
                         paw_str_rawlen(P, -1), \
                         paw_str(P, -1));       \
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
    Str const *name = V_STR(*at(P, index));
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

void paw_push_char(paw_Env *P, paw_Char x)
{
    V_SET_CHAR(P->top.p, x);
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

char const *paw_push_str(paw_Env *P, char const *s)
{
    return paw_push_nstr(P, s, strlen(s));
}

char const *paw_push_nstr(paw_Env *P, char const *s, size_t n)
{
    Str *str = pawS_new_nstr(P, s, n);
    V_SET_OBJECT(P->top.p, str);
    API_INCR_TOP(P, 1);
    return str->text;
}

char const *paw_push_vfstr(paw_Env *P, char const *fmt, va_list arg)
{
    Buffer buf;
    pawL_init_buffer(P, &buf);
    pawL_add_vfstring(P, &buf, fmt, arg);
    pawL_push_result(P, &buf);
    return paw_str(P, -1);
}

char const *paw_push_fstr(paw_Env *P, char const *fmt, ...)
{
    va_list arg;
    va_start(arg, fmt);
    char const *s = paw_push_vfstr(P, fmt, arg);
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

paw_Char paw_char(paw_Env *P, int index)
{
    return V_CHAR(*at(P, index));
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

char const *paw_str(paw_Env *P, int index)
{
    Str const *s = V_STR(*at(P, index));
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

void paw_new_list(paw_Env *P, int n, int element_size)
{
    Value *object = pawC_push0(P);
    Tuple *list = pawR_new_list(P, P->cf, object, n, element_size);

    Value *dst = LIST_BEGIN(list);
    Value const *src = at(P, -n * element_size - 1);
    while (dst != LIST_END(list))
        *dst++ = *src++;

    paw_shift(P, n * element_size);
}

static void init_basic_map(paw_Env *P, Value *map, int n)
{
    for (int k = -2 * n; k < 0; k += 2) {
        Value const *key = at(P, k - 1);
        Value const *value = key + 1;
        pawR_map_set(P, P->cf, map, key, value);
    }
    paw_shift(P, 2 * n);
}

static void init_wide_map(paw_Env *P, Value *map, MapPolicy p, int n)
{
    Value *temp = pawC_push0(P);
    int const z = p.key_size + p.value_size;
    for (int k = -z * n; k < 0; k += z) {
        Value const *key = at(P, k - 1 - 1);
        Value const *value = key + p.key_size;
        pawR_map_newp(P, P->cf, temp, map, key);
        pawV_copy(temp->p, value, p.value_size);
    }
    paw_pop(P, 1); // pop "temp"
    paw_shift(P, z * n); // shift "map"
}

void paw_new_map(paw_Env *P, int n, int policy)
{
    // allocate the map and obtain a stable pointer to it
    Value *object = pawC_push0(P);
    Tuple *map = pawR_new_map(P, P->cf, object, n, policy);
    Value *ra = &(Value){.o = CAST_OBJECT(map)};

    MapPolicy const p = GET_POLICY(P, map);
    if (p.key_size == 1 && p.value_size == 1) {
        init_basic_map(P, ra, n);
    } else {
        init_wide_map(P, ra, p, n);
    }
}

void *paw_new_foreign(paw_Env *P, size_t size, int nfields)
{
    Value *pv = pawC_push0(P);
    Foreign *ud = pawV_new_foreign(P, size, nfields, 0, pv);
    return ud->data;
}

int paw_register_policy(paw_Env *P, paw_Type map_type)
{
    API_CHECK(P, 0 <= map_type && map_type < P->types.count, "invalid map type");

    RttiType const *map = P->types.data[map_type];
    RttiType const *key = P->types.data[map->subtypes[0]];
    RttiType const *value = P->types.data[map->subtypes[1]];
    struct MapPolicyList *policies = &P->map_policies;
    if (policies->count < MAX_POLICIES) {
        policies->data[policies->count++] = (MapPolicy){
            .key_size = rtti_stack_size(P, key),
            .value_size = rtti_stack_size(P, value),
            .hash = *at(P, -2),
            .equals = *at(P,-1),
            .type = map_type,
        };
        paw_pop(P, 2);
        return 0;
    }
    return -1;
}

int paw_find_policy(paw_Env *P, paw_Type map_type)
{
    API_CHECK(P, 0 <= map_type && map_type < P->types.count, "invalid map type");

    struct MapPolicyList policies = P->map_policies;
    for (int i = 0; i < policies.count; ++i) {
        if (policies.data[i].type == map_type) return i;
    }
    return -1;
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

void paw_get_field(paw_Env *P, int object, int field)
{
    Value const *tuple = at(P, object);
    Value *output = stackp(P);
    API_INCR_TOP(P, 1);

    pawR_tuple_get(P->cf, output, tuple, field);
}

void paw_set_field(paw_Env *P, int object, int index)
{
    Value *tuple = at(P, object);
    Value const *value = lastp(P);
    --P->top.p;

    pawR_tuple_set(P->cf, tuple, index, value);
}

char const *paw_int_to_str(paw_Env *P, int index, size_t *plen)
{
    Value *pv = at(P, index);
    return pawV_to_str(P, pv, PAW_TINT, plen);
}

char const *paw_float_to_str(paw_Env *P, int index, size_t *plen)
{
    Value *pv = at(P, index);
    return pawV_to_str(P, pv, PAW_TFLOAT, plen);
}

void paw_str_length(paw_Env *P, int object)
{
    Value const *str = at(P, object);
    size_t const len = pawS_length(V_STR(*str));
    paw_push_int(P, PAW_CAST_INT(len));
}

void paw_str_concat(paw_Env *P, int count)
{
    pawR_str_concat(P, P->cf, count);
}

void paw_str_prepend(paw_Env *P, int object)
{
    Value *slot = at(P, object);
    paw_Char const c = paw_char(P, -1);
    Str const *str = V_STR(*slot);

    Str *copy = pawS_new_uninit(P, 1 + str->length);
    memcpy(copy->text + 1, str->text, str->length);
    copy->text[0] = (char)c;
    pawS_register(P, &copy);

    V_SET_OBJECT(slot, copy);
}

void paw_str_prependc(paw_Env *P, int object, paw_Char c)
{
    object = paw_abs_index(P, object);
    paw_push_char(P, c);
    paw_str_prepend(P, object);
}

void paw_str_append(paw_Env *P, int object)
{
    Value *slot = at(P, object);
    paw_Char const c = paw_char(P, -1);
    Str const *str = V_STR(*slot);

    Str *copy = pawS_new_uninit(P, str->length + 1);
    memcpy(copy->text, str->text, str->length);
    copy->text[str->length] = (char)c;
    pawS_register(P, &copy);

    V_SET_OBJECT(slot, copy);
}

void paw_str_appendc(paw_Env *P, int object, paw_Char c)
{
    object = paw_abs_index(P, object);
    paw_push_char(P, c);
    paw_str_append(P, object);
}

void paw_str_get(paw_Env *P, int object)
{
    Value const *str = at(P, object);
    Value const *index = lastp(P);
    Value *output = lastp(P);

    pawR_str_get(P, P->cf, output, str, index);
}

void paw_str_get_range(paw_Env *P, int object)
{
    Value const *str = at(P, object);
    Value const *lower = from_stackp(P, -2);
    Value *output = from_stackp(P, -2);

    pawR_str_getn(P, P->cf, output, str, lower);
    paw_pop(P, 1);
}

int paw_list_element_size(paw_Env *P, int object)
{
    Tuple const *list = V_TUPLE(*at(P, object));
    return LIST_ZELEMENT(list);
}

void paw_list_length(paw_Env *P, int object)
{
    Tuple const *list = V_TUPLE(*at(P, object));
    paw_push_int(P, pawList_length(P, list));
}

void paw_list_concat(paw_Env *P, int count)
{
    pawR_list_concat(P, P->cf, count);
}

int paw_list_get(paw_Env *P, int object)
{
    int const element_size = paw_list_element_size(P, object);
    API_CHECK_PUSH(P, element_size - 1);

    // replace index with element pointer
    paw_list_getp(P, object);

    // replace pointer with pointee
    Value const *pointer = (--P->top.p)->p;
    push_values(P, pointer, element_size);
    return element_size;
}

typedef void (*ListGetter)(paw_Env *, CallFrame *, Value *, Value const *, Value const *);
static void call_list_getter(paw_Env *P, int object, ListGetter getter)
{
    Value const *list = at(P, object);
    Value const *index = lastp(P);
    Value *output = lastp(P);

    getter(P, P->cf, output, list, index);
}

void paw_list_get1(paw_Env *P, int object)
{
    ENSURE_SIZE1(P, paw_list_element_size(P, object), "list element");

    // output = list[index]
    call_list_getter(P, object, pawR_list_get);
}

void paw_list_getp(paw_Env *P, int object)
{
    // output = &list[index]
    call_list_getter(P, object, pawR_list_getp);
}

int paw_list_iget(paw_Env *P, int object, paw_Int index)
{
    paw_push_int(P, index);
    return paw_list_get(P, object);
}

void paw_list_iget1(paw_Env *P, int object, paw_Int index)
{
    paw_push_int(P, index);
    paw_list_get1(P, object);
}

void paw_list_set(paw_Env *P, int object)
{
    int const element_size = paw_list_element_size(P, object);
    Value *output = from_stackp(P, -element_size - 1);
    Value const *element = from_stackp(P, -element_size);
    Value const *index = output;

    // list[index] = element
    Value *list = at(P, object);
    pawR_list_getp(P, P->cf, output, list, index);
    pawV_copy(output->p, element, element_size);
    paw_pop(P, 1 + element_size);
}

void paw_list_set1(paw_Env *P, int object)
{
    ENSURE_SIZE1(P, paw_list_element_size(P, object), "list element");

    Value *list = at(P, object);
    Value const *index = from_stackp(P, -2);
    Value const *element = from_stackp(P, -1);

    // list[index] = element
    pawR_list_set(P, P->cf, list, index, element);
    paw_pop(P, 2);
}

void paw_list_iset(paw_Env *P, int object, paw_Int index)
{
    int const element_size = paw_list_element_size(P, object);

    paw_push_int(P, index);
    paw_rotate(P, -element_size - 1, 1);
    paw_list_set(P, object);
}

void paw_list_iset1(paw_Env *P, int object, paw_Int index)
{
    paw_push_int(P, index);
    paw_rotate(P, -2, 1);
    paw_list_set1(P, object);
}

void paw_list_get_range(paw_Env *P, int object)
{
    Value const *list = at(P, object);
    Value const *lower = from_stackp(P, -2);
    Value *output = from_stackp(P, -2);
    Value *temp = lastp(P);

    // output = list[lower..upper]
    pawR_list_getn(P, P->cf, output, list, lower, temp);
    paw_pop(P, 1); // pop "temp"
}

void paw_list_iget_range(paw_Env *P, int object, paw_Int lower, paw_Int upper)
{
    paw_push_int(P, lower);
    paw_push_int(P, upper);
    paw_list_get_range(P, object);
}

void paw_list_set_range(paw_Env *P, int object)
{
    Value *list = at(P, object);
    Value const *lower = from_stackp(P, -2);
    Value const *upper = from_stackp(P, -1);
    Value *temp = lastp(P);

    // output = list[lower..upper]
    pawR_list_setn(P, P->cf, list, lower, upper, temp);
    paw_pop(P, 2); // pop bounds
}

void paw_list_iset_range(paw_Env *P, int object, paw_Int lower, paw_Int upper)
{
    int const element_size = paw_list_element_size(P, object);

    paw_push_int(P, lower);
    paw_push_int(P, upper);
    paw_rotate(P, -element_size - 2, 2);
    paw_list_set_range(P, object);
}

void paw_list_push(paw_Env *P, int object)
{
    int const element_size = paw_list_element_size(P, object);

    Tuple *list = V_TUPLE(*at(P, object));
    Value const *element = from_stackp(P, -element_size);

    pawList_push(P, list, element);
    paw_pop(P, element_size);
}

void paw_list_pop(paw_Env *P, int object)
{
    Tuple *list = V_TUPLE(*at(P, object));
    paw_Int const length = pawList_length(P, list);
    API_CHECK(P, length > 0, "pop from empty list");

    pawList_pop(P, list, length - 1);
}

void paw_list_insert(paw_Env *P, int object)
{
    int const element_size = paw_list_element_size(P, object);

    Tuple *list = V_TUPLE(*at(P, object));
    paw_Int const index = paw_int(P, -element_size - 1);
    Value const *value = at(P, -element_size);

    pawList_insert(P, list, index, value);
}

void paw_list_iinsert(paw_Env *P, int object, paw_Int index)
{
    object = paw_abs_index(P, object);
    paw_push_int(P, index);
    paw_list_insert(P, object);
}

void paw_list_remove(paw_Env *P, int object)
{
    int const element_size = paw_list_element_size(P, object);

    Tuple *list = V_TUPLE(*at(P, object));
    paw_Int const index = paw_int(P, -1);

    pawList_pop(P, list, index);
    paw_pop(P, 1); // pop "index"
}

void paw_list_iremove(paw_Env *P, int object, paw_Int index)
{
    object = paw_abs_index(P, object);
    paw_push_int(P, index);
    paw_list_remove(P, object);
}

paw_Bool paw_list_next(paw_Env *P, int object)
{
    Tuple const *list = V_TUPLE(*at(P, object));

    paw_Int *piter = &V_INT(*lastp(P));
    if (pawList_iter(P, list, piter)) {
        int const element_size = LIST_ZELEMENT(list);
        Value const *element = pawList_at(list, *piter);

        // push the payload to the stack
        push_values(P, element, element_size);
        return PAW_TRUE;
    }
    return PAW_FALSE;
}

int paw_map_key_size(paw_Env *P, int object)
{
    Tuple const *map = V_TUPLE(*at(P, object));
    return GET_POLICY(P, map).key_size;
}

int paw_map_value_size(paw_Env *P, int object)
{
    Tuple const *map = V_TUPLE(*at(P, object));
    return GET_POLICY(P, map).value_size;
}

void paw_map_length(paw_Env *P, int index)
{
    Value const *map = at(P, index);
    size_t const length = pawMap_length(V_TUPLE(*map));
    paw_push_int(P, PAW_CAST_INT(length));
}

int paw_map_get(paw_Env *P, int object)
{
    if (paw_map_getp(P, object))
        return -1;

    int const value_size = paw_map_value_size(P, object);
    ENSURE_STACK(P, value_size - 1);

    Value const *pointer = (--P->top.p)->p;
    push_values(P, pointer, value_size);
    return value_size;
}

typedef int (*MapGetter)(paw_Env *, CallFrame *, Value *, Value const *, Value const *);
static int call_map_getter(paw_Env *P, int object, MapGetter getter)
{
    int const key_size = paw_map_key_size(P, object);

    Value *output = from_stackp(P, -key_size);
    Value const *map = at(P, object);
    Value const *key = output;

    if (getter(P, P->cf, output, map, key)) {
        paw_pop(P, key_size);
        return -1;
    }

    // pop the rest of the key
    paw_pop(P, key_size - 1);
    return 0;
}

int paw_map_get1(paw_Env *P, int object)
{
    ENSURE_SIZE1(P, paw_map_value_size(P, object), "map value");

    return call_map_getter(P, object, pawR_map_get);
}

int paw_map_getp(paw_Env *P, int object)
{
    return call_map_getter(P, object, pawR_map_getp);
}

void paw_map_newp(paw_Env *P, int object)
{
    int const key_size = paw_map_key_size(P, object);

    Value *output = from_stackp(P, -key_size);
    Value const *map = at(P, object);
    Value const *key = output;

    pawR_map_newp(P, P->cf, output, map, key);
}

void paw_map_set(paw_Env *P, int object)
{
    int const key_size = paw_map_key_size(P, object);
    int const value_size = paw_map_value_size(P, object);
    int const item_size = key_size + value_size;

    // swap key and value, then replace key with value pointer
    paw_rotate(P, -item_size, -key_size);
    paw_map_newp(P, object);

    Value *pointer = lastp(P);
    Value const *value = from_stackp(P, -value_size - 1);
    pawV_copy(pointer->p, value, value_size);
    paw_pop(P, value_size + 1);
}

void paw_map_set1(paw_Env *P, int object)
{
    int const key_size = paw_map_key_size(P, object);
    ENSURE_SIZE1(P, paw_map_value_size(P, object), "map value");

    Value *map = at(P, object);
    Value const *key = from_stackp(P, -key_size - 1);
    Value const *value = lastp(P);

    pawR_map_set(P, P->cf, map, key, value);
    paw_pop(P, key_size + 1);
}

typedef void (*MapAt)(paw_Env *, Tuple *, paw_Int iter, void const *arg);
static paw_Bool call_map_next(paw_Env *P, int object, MapAt getter, void const *arg)
{
    Tuple *map = V_TUPLE(*at(P, object));
    paw_Int *piter = &V_INT(*lastp(P));
    if (pawMap_iter(map, piter)) {
        // push the payload to the stack
        getter(P, map, *piter, arg);
        return PAW_TRUE;
    }

    return PAW_FALSE;
}

static void get_map_key(paw_Env *P, Tuple *map, paw_Int iter, void const *arg)
{
    int const key_size = ((int const *)arg)[0];
    Value const *key = pawMap_key(P, map, iter);
    push_values(P, key, key_size);
}

static void get_map_value(paw_Env *P, Tuple *map, paw_Int iter, void const *arg)
{
    int const value_size = ((int const *)arg)[0];
    Value const *value = pawMap_value(P, map, iter);
    push_values(P, value, value_size);
}

static void get_map_item(paw_Env *P, Tuple *map, paw_Int iter, void const *arg)
{
    int const key_size = ((int const *)arg)[0];
    int const value_size = ((int const *)arg)[1];
    Value const *key = pawMap_key(P, map, iter);
    Value const *value = pawMap_value(P, map, iter);
    push_values(P, key, key_size);
    push_values(P, value, value_size);
}

paw_Bool paw_map_next(paw_Env *P, int object)
{
    int const item_size[] = {
        paw_map_key_size(P, object),
        paw_map_value_size(P, object),
    };
    return call_map_next(P, object, get_map_item, item_size);
}

paw_Bool paw_map_next_key(paw_Env *P, int object)
{
    int const key_size = paw_map_key_size(P, object);
    return call_map_next(P, object, get_map_key, &key_size);
}

paw_Bool paw_map_next_value(paw_Env *P, int object)
{
    int const value_size = paw_map_value_size(P, object);
    return call_map_next(P, object, get_map_value, &value_size);
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
