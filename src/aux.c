
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#include "aux.h"
#include "array.h"
#include "bigint.h"
#include "error.h"
#include "map.h"
#include "mem.h"
#include "util.h"
#include <float.h>
#include <math.h>
#include <stdlib.h>

static void grow_buffer(paw_Env *P, Buffer *buf)
{
    paw_assert(buf->alloc <= SIZE_MAX / 2);
    const size_t alloc = buf->alloc * 2;
    if (pawL_boxed(buf)) {
        UserData *ud = pawV_get_userdata(P->top[-1]);
        pawM_resize(P, buf->data, buf->alloc, alloc);
        ud->data = buf->data;
        ud->size = alloc;
    } else {
        // Push a new UserData 'box' to contain the buffer. Prevents a memory leak
        // if an error is thrown before the buffer is freed.
        UserData *ud = pawV_push_userdata(P, alloc);
        memcpy(ud->data, buf->stack, buf->size);
        buf->data = ud->data;
    }
    buf->alloc = alloc;
}

static char *reserve_memory(paw_Env *P, Buffer *buf, size_t n)
{
    while (buf->size + n > buf->alloc) {
        grow_buffer(P, buf);
    }
    char *ptr = buf->data + buf->size;
    buf->size += n;
    return ptr;
}

void pawL_init_buffer(Buffer *buf)
{
    *buf = (Buffer){.data = buf->stack, .alloc = BUFFER_LIMIT};
}

void pawL_discard_result(paw_Env *P, Buffer *buf)
{
    if (pawL_boxed(buf)) {
        paw_assert(paw_pointer(P, -1) == buf->data); // Must be on top
        paw_pop(P, 1);                               // Pop box
    }
}

void pawL_push_result(paw_Env *P, Buffer *buf)
{
    paw_push_nstring(P, buf->data, buf->size);
    if (pawL_boxed(buf)) {
        paw_replace(P, -2); // Replace box with string
    }
}

void pawL_buffer_resize(paw_Env *P, Buffer *buf, size_t n)
{
    while (n > buf->alloc) {
        grow_buffer(P, buf);
    }
    buf->size = n;
}

void pawL_add_char(paw_Env *P, Buffer *buf, char c)
{
    char *ptr = reserve_memory(P, buf, 1);
    ptr[0] = c;
}

void pawL_add_string(paw_Env *P, Buffer *buf, const char *s)
{
    pawL_add_nstring(P, buf, s, strlen(s));
}

void pawL_add_nstring(paw_Env *P, Buffer *buf, const char *s, size_t n)
{
    char *ptr = reserve_memory(P, buf, n);
    memcpy(ptr, s, n);
}

void pawL_add_integer(paw_Env *P, Buffer *buf, paw_Int i)
{
    char temp[32];
    const paw_Bool negative = i < 0;
    char *end = temp + paw_countof(temp);
    char *ptr = end - 1;

    // Don't call llabs(INT64_MIN). The result is undefined on 2s complement systems.
    uint64_t u = i == INT64_MIN ? (1ULL << 63) : (uint64_t)llabs(i);
    do {
        *ptr-- = (char)(u % 10 + '0');
        u /= 10;
    } while (u);
    if (negative) {
        *ptr = '-';
    } else {
        ++ptr;
    }

    pawL_add_nstring(P, buf, ptr, (size_t)(end - ptr));
}

void pawL_add_float(paw_Env *P, Buffer *buf, paw_Float f)
{
    if (isfinite(f)) {
        char temp[32];
        const int n = snprintf(temp, paw_countof(temp), "%.*g", DBL_DECIMAL_DIG, f);
        pawL_add_nstring(P, buf, temp, (size_t)n);
    } else if (isnan(f)) {
        pawL_add_string(P, buf, "NaN");
    } else {
        pawL_add_string(P, buf, f < 0 ? "-inf" : "inf");
    }
}

void pawL_add_pointer(paw_Env *P, Buffer *buf, void *p)
{
    char temp[32];
    const int n = snprintf(temp, sizeof(temp), "%p", p);
    pawL_add_nstring(P, buf, temp, (size_t)n);
}

static const char *add_non_fmt(paw_Env *P, Buffer *buf, const char *ptr)
{
    const char *p = ptr;
    while (*p && *p != '%') {
        ++p;
    }
    if (p != ptr) {
        pawL_add_nstring(P, buf, ptr, (size_t)(p - ptr));
        ptr = p;
    }
    return ptr;
}

void pawL_add_vfstring(paw_Env *P, Buffer *buf, const char *fmt, va_list arg)
{
    for (;; ++fmt) {
        fmt = add_non_fmt(P, buf, fmt);
        if (!*fmt) {
            break;
        }
        // Skip '%'.
        ++fmt;

        switch (*fmt) {
            case 's': {
                const char *s = va_arg(arg, char *);
                pawL_add_nstring(P, buf, s, strlen(s));
                break;
            }
            case '%':
                pawL_add_char(P, buf, '%');
                break;
            case 'd':
                pawL_add_integer(P, buf, va_arg(arg, int));
                break;
            case 'I':
                pawL_add_integer(P, buf, va_arg(arg, int64_t));
                break;
            case 'c':
                pawL_add_char(P, buf, *fmt);
                break;
            case 'f':
                pawL_add_float(P, buf, va_arg(arg, double));
                break;
            case 'p':
                pawL_add_pointer(P, buf, va_arg(arg, void *));
                break;
            default:
                pawE_error(P, PAW_EVALUE, "invalid format option '%%%c'", *fmt);
        }
    }
}

void pawL_add_fstring(paw_Env *P, Buffer *buf, const char *fmt, ...)
{
    va_list arg;
    va_start(arg, fmt);
    pawL_add_vfstring(P, buf, fmt, arg);
    va_end(arg);
}

static void add_value(paw_Env *P, Buffer *buf, Value o);

static void add_location(paw_Env *P, Buffer *buf, void *at)
{
    pawL_add_literal(P, buf, " at ");
    pawL_add_pointer(P, buf, at);
}

static void add_string(paw_Env *P, Buffer *buf, const String *s)
{
    pawL_add_nstring(P, buf, s->text, s->length);
}

static void add_bigint(paw_Env *P, Buffer *buf, const BigInt *bi)
{
    pawB_str(P, bi, PAW_BFALSE, "", 10); // Convert to string
    String *s = pawV_get_string(P->top[-1]);
    pawL_add_nstring(P, buf, s->text, s->length);
    pawC_stkdec(P, 1);
}

static void add_array(paw_Env *P, Buffer *buf, Array *a)
{
    pawL_add_char(P, buf, '[');

    paw_Int itr = PAW_ITER_INIT;
    for (size_t i = 0; pawA_iter(a, &itr); ++i) {
        if (i) {
            pawL_add_literal(P, buf, ", ");
        }
        add_value(P, buf, *pawA_get(P, a, itr));
    }
    pawL_add_char(P, buf, ']');
}

static void add_map(paw_Env *P, Buffer *buf, Map *m)
{
    pawL_add_char(P, buf, '{');

    paw_Int itr = PAW_ITER_INIT;
    for (size_t i = 0; pawH_iter(m, &itr); ++i) {
        if (i) {
            pawL_add_literal(P, buf, ", ");
        }
        add_value(P, buf, m->keys[itr]);
        pawL_add_literal(P, buf, ": ");
        add_value(P, buf, m->values[itr]);
    }
    pawL_add_char(P, buf, '}');
}

// Add a method value to the buf buffer
// Format is "<method 'self.func' at addr>".
static void add_method(paw_Env *P, Buffer *buf, Method *m)
{
    pawL_add_literal(P, buf, "<method '");
    if (pawV_is_instance(m->self)) {
        Instance *ins = pawV_get_instance(m->self);
        add_string(P, buf, ins->self->name);
    } else if (pawV_is_array(m->self)) {
        pawL_add_literal(P, buf, "Array");
    } else {
        paw_assert(pawV_is_map(m->self));
        pawL_add_literal(P, buf, "Map");
    }
    pawL_add_char(P, buf, '.');
    if (pawV_is_native(m->f)) {
        // TODO: It would be nice to have the builtin method's name here. Need something more than a paw_Function
        pawL_add_literal(P, buf, "builtin");
    } else {
        paw_assert(pawV_is_closure(m->f));
        Closure *f = pawV_get_closure(m->f);
        add_string(P, buf, f->p->name);
    }
    pawL_add_char(P, buf, '\'');
    add_location(P, buf, m);
    pawL_add_char(P, buf, '>');
}

static void add_object(paw_Env *P, Buffer *buf, const char *type, void *at)
{
    pawL_add_char(P, buf, '<');
    pawL_add_string(P, buf, type);
    add_location(P, buf, at);
    pawL_add_char(P, buf, '>');
}

static void add_named_object(paw_Env *P, Buffer *buf, const char *type, String *name, void *at)
{
    pawL_add_char(P, buf, '<');
    pawL_add_string(P, buf, type);
    pawL_add_literal(P, buf, " '");
    add_string(P, buf, name);
    pawL_add_char(P, buf, '\'');
    add_location(P, buf, at);
    pawL_add_char(P, buf, '>');
}

static void add_value(paw_Env *P, Buffer *buf, Value v)
{
    if (!pawV_is_number(v)) {
        switch (pawV_get_type(v)) {
            case VNULL:
                pawL_add_literal(P, buf, "null");
                break;
            case VTRUE:
                pawL_add_literal(P, buf, "true");
                break;
            case VFALSE:
                pawL_add_literal(P, buf, "false");
                break;
            case VSTRING:
                add_string(P, buf, pawV_get_string(v));
                break;
            case VBIGINT:
                add_bigint(P, buf, pawV_get_bigint(v));
                break;
            case VUPVALUE:
                add_object(P, buf, "upvalue", pawV_get_upvalue(v));
                break;
            case VCLOSURE:
                add_named_object(P, buf, "closure", pawV_get_closure(v)->p->name, pawV_get_object(v));
                break;
            case VPROTO:
                add_named_object(P, buf, "proto", pawV_get_proto(v)->name, pawV_get_object(v));
                break;
            case VNATIVE:
                add_object(P, buf, "native", pawV_get_native(v));
                break;
            case VCLASS:
                add_named_object(P, buf, "class", pawV_get_class(v)->name, pawV_get_object(v));
                break;
            case VINSTANCE:
                add_named_object(P, buf, "instance", pawV_get_instance(v)->self->name, pawV_get_object(v));
                break;
            case VMETHOD:
                add_method(P, buf, pawV_get_method(v));
                break;
            case VARRAY:
                add_array(P, buf, pawV_get_array(v));
                break;
            case VMAP:
                add_map(P, buf, pawV_get_map(v));
                break;
            default:
                paw_assert(PAW_BFALSE);
        }
    } else if (pawV_is_int(v)) {
        pawL_add_integer(P, buf, pawV_get_int(v));
    } else if (pawV_is_float(v)) {
        pawL_add_float(P, buf, pawV_get_float(v));
    }
}

void pawL_add_value(paw_Env *P, Buffer *buf, Value o)
{
    add_value(P, buf, o);
}

void pawL_reverse_buf(paw_Env *P, Buffer *buf)
{
    paw_unused(P);
    char *x = buf->data;
    char *y = buf->data + buf->size - 1;
    for (; x < y; ++x, --y) {
        char t = *x; // Reverse chars inplace
        *x = *y;
        *y = t;
    }
}

// Data and stringify algorithm modified from micropython
static const uint8_t kLogBase2[] = {
    0,
    1,
    1,
    2,
    2,
    2,
    2,
    3,
    3,
    3,
    3,
    3,
    3,
    3,
    3,
    4,
    4,
    4,
    4,
    4,
    4,
    4,
    4,
    4,
    4,
    4,
    4,
    4,
    4,
    4,
    4,
    5,
};

size_t pawL_integer_format_size(size_t nbits, int base)
{
    assert(2 <= base && base <= 32);
    return nbits / kLogBase2[base - 1] + 1;
}
