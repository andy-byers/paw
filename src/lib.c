// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#include "lib.h"
#include "api.h"
#include "auxlib.h"
#include "call.h"
#include "gc.h"
#include "map.h"
#include "mem.h"
#include "os.h"
#include "rt.h"
#include <errno.h>
#include <limits.h>
#include <time.h>

#define CF_BASE(i) P->cf->base.p[i]

void lib_error(paw_Env *P, int error, const char *fmt, ...)
{
    Buffer print;
    pawL_init_buffer(P, &print);

    va_list arg;
    va_start(arg, fmt);
    pawL_add_vfstring(P, &print, fmt, arg);
    va_end(arg);

    pawL_push_result(P, &print);
    pawC_throw(P, error);
}

static int get_argc(paw_Env *P) 
{
    return paw_get_count(P) - 1 /* context */; 
}

void pawL_check_argc(paw_Env *P, int argc)
{
    pawR_check_argc(P, get_argc(P), argc);
}

int pawL_check_varargc(paw_Env *P, int min, int max)
{
    const int narg = get_argc(P);
    pawR_check_varargc(P, narg, min, max);
    return narg;
}

static int base_assert(paw_Env *P)
{
    if (V_FALSE(CF_BASE(1))) {
        pawR_error(P, PAW_ERUNTIME, "assertion failed");
    }
    return 0;
}

static int base_print(paw_Env *P)
{
    const String *s = V_STRING(P->top.p[-1]);
    pawO_write(stdout, s->text, s->length);
    fflush(stdout);
    return 0;
}

static int list_insert(paw_Env *P)
{
    List *list = V_LIST(CF_BASE(1));
    const paw_Int index = V_INT(CF_BASE(2));
    pawV_list_insert(P, list, index, CF_BASE(3));
    return 0;
}

static int list_push(paw_Env *P)
{
    List *list = V_LIST(CF_BASE(1));
    pawV_list_push(P, list, CF_BASE(2));
    return 0;
}

static int list_pop(paw_Env *P)
{
    List *list = V_LIST(CF_BASE(1));
    const paw_Int length = PAW_CAST_INT(pawV_list_length(list));
    if (length == 0) {
        pawR_error(P, PAW_EVALUE, "pop from empty List");
    }
    P->top.p[-1] = *pawV_list_get(P, list, length - 1);
    pawV_list_pop(P, list, length - 1);
    return 1;
}

static paw_Int clamped_index(paw_Env *P, int loc, paw_Int n)
{
    const paw_Int i = V_INT(CF_BASE(loc));
    return i < 0 ? 0 : i >= n ? n - 1 : i;
}

// TODO: It would be nice to let pop() take an optional parameter indicating the
//       index at which to erase an element. To me, 'remove' seems like it
//       should remove the first matching element using something akin to
//       operator==. Attempting this will break, since we have no concept of
//       equality between user-defined types right now.
static int list_remove(paw_Env *P)
{
    List *list = V_LIST(CF_BASE(1));
    const paw_Int length = PAW_CAST_INT(pawV_list_length(list));
    if (length == 0) {
        pawR_error(P, PAW_EVALUE, "remove from empty List");
    }
    const paw_Int index = V_INT(CF_BASE(2));
    P->top.p[-1] = *pawV_list_get(P, list, index);
    pawV_list_pop(P, list, index);
    return 1;
}

static int list_clone(paw_Env *P)
{
    const List *list = V_LIST(CF_BASE(1));
    Value *pv = pawC_push0(P);
    pawV_list_clone(P, pv, list);
    return 1;
}

static const char *find_substr(const char *str, size_t nstr, const char *sub, size_t nsub)
{
    if (nsub == 0) return str;
    const char *ptr = str;
    const char *end = str + nstr;
    while ((ptr = strchr(ptr, sub[0]))) {
        if (nsub <= CAST_SIZE(end - ptr) && 
                0 == memcmp(ptr, sub, nsub)) {
            return ptr;
        }
        str = ptr + nsub;
    }
    return NULL;
}

static int string_find(paw_Env *P)
{
    pawL_check_argc(P, 2);
    const String *s = V_STRING(CF_BASE(1));
    const String *find = V_STRING(CF_BASE(2));
    const char *result = find_substr(
            s->text, s->length, 
            find->text, find->length);
    if (result) { // index of substring
        V_SET_INT(P->top.p - 1, result - s->text);
    } else { // not found
        V_SET_INT(P->top.p - 1, -1);
    }
    return 1;
}

// TODO: These should be methods, which take the context as the implicit first parameter
static int string_split(paw_Env *P)
{
     pawL_check_argc(P, 2);
     const String *sep = V_STRING(CF_BASE(2));
     String *s = V_STRING(CF_BASE(1));
     if (sep->length == 0) {
         pawR_error(P, PAW_EVALUE, "empty separator");
     }

     paw_Int npart = 0;
     const char *part;
     size_t nstr = s->length;
     const char *pstr = s->text;
     while ((part = find_substr(pstr, nstr, sep->text, sep->length))) {
         const size_t n = CAST_SIZE(part - pstr);
         pawC_pushns(P, pstr, n);
         part += sep->length; // skip separator
         pstr = part;
         nstr -= n;
         ++npart;
     }
     const char *end = s->text + s->length; // add the rest
     pawC_pushns(P, pstr, CAST_SIZE(end - pstr));
     ++npart;

     pawR_literal_list(P, npart);
     return 1;
 }

static int string_join(paw_Env *P)
{
     pawL_check_argc(P, 2);
     String *s = V_STRING(CF_BASE(1));
     const Value seq = CF_BASE(2);

     Buffer buf;
     pawL_init_buffer(P, &buf);
     paw_Int itr = PAW_ITER_INIT;
     List *a = V_LIST(seq);
     while (pawV_list_iter(a, &itr)) {
         const Value v = a->begin[itr];
         // Add a chunk, followed by the separator if necessary.
         const String *chunk = V_STRING(v);
         pawL_add_nstring(P, &buf, chunk->text, chunk->length);
         if (CAST_SIZE(itr + 1) < pawV_list_length(a)) {
             pawL_add_nstring(P, &buf, s->text, s->length);
         }
     }
     pawL_push_result(P, &buf);
     return 1;
 }

static int string_starts_with(paw_Env *P)
{
    pawL_check_argc(P, 2);
    String *s = V_STRING(CF_BASE(1));
    const String *prefix = V_STRING(CF_BASE(2));
    const size_t prelen = prefix->length;
    const paw_Bool b = s->length >= prelen && 
        0 == memcmp(prefix->text, s->text, prelen);
    V_SET_BOOL(P->top.p - 1, b);
    return 1;
}

static int string_ends_with(paw_Env *P)
{
    pawL_check_argc(P, 2);
    String *s = V_STRING(CF_BASE(1));
    const String *suffix = V_STRING(CF_BASE(2));
    const size_t suflen = suffix->length;
    paw_Bool b = PAW_FALSE;
    if (s->length >= suflen) {
        const char *ptr = s->text + s->length - suflen;
        b = 0 == memcmp(suffix->text, ptr, suflen);
    }
    V_SET_BOOL(P->top.p - 1, b);
    return 1;
}

static int int_to_string(paw_Env *P)
{
    pawL_check_argc(P, 1);
    const Value v = CF_BASE(1);
    pawV_to_string(P, v, PAW_TINT, NULL);
    return 1;
}

static int string_parse_float(paw_Env *P)
{
    pawL_check_argc(P, 1);
    const char *str = V_TEXT(CF_BASE(1));
    const int rc = pawV_parse_float(P, str);
    if (rc != PAW_OK) {
        pawR_error(P, PAW_ESYNTAX, "invalid float '%s'", str);
    }
    return 1;
}

static int string_parse_int(paw_Env *P)
{
    pawL_check_argc(P, 2);
    const char *str = V_TEXT(CF_BASE(1));
    const paw_Int base = V_INT(CF_BASE(2));
    pawC_pop(P);

    const int rc = pawV_parse_int(P, str, 0);
    if (rc == PAW_ESYNTAX) {
        pawR_error(P, PAW_ESYNTAX, "invalid integer '%s'", str);
    } else if (rc == PAW_EOVERFLOW) {
        pawR_error(P, PAW_EOVERFLOW, "integer '%s' is out of range", str);
    }
    return 1;
}

static int map_get(paw_Env *P)
{
    Map *m = V_MAP(CF_BASE(1));
    const Value key = CF_BASE(2);
    const Value *pv = pawH_get(m, key);
    if (pv != NULL) P->top.p[-1] = *pv;
    return 1;
}

static int map_erase(paw_Env *P)
{
    pawL_check_argc(P, 2);
    Map *m = V_MAP(CF_BASE(1));
    pawH_erase(m, CF_BASE(2));
    return 0;
}

static int map_clone(paw_Env *P)
{
    pawL_check_argc(P, 1);
    Map *m = V_MAP(CF_BASE(1));
    Value *pv = pawC_push0(P);
    pawH_clone(P, pv, m);
    return 1;
}

void pawL_new_func(paw_Env *P, paw_Function func, int nup)
{
    Value *pv = pawC_push0(P);
    Native *nat = pawV_new_native(P, func, nup);
    V_SET_OBJECT(pv, nat);

    const Value *up = P->top.p - nup - 1;
    for (int i = 0; i < nup; ++i) {
        nat->up[i] = *up++;
    }
    paw_shift(P, nup);
}

static void add_builtin_func(paw_Env *P, const char *name, paw_Function func)
{
    paw_push_value(P, -1);
    paw_push_string(P, name);
    pawL_new_func(P, func, 0);
    pawR_setelem(P, PAW_TMAP);
    pawC_stkdec(P, 1);
}

void pawL_init(paw_Env *P)
{
    Value *pv = pawC_push0(P);
    P->libs = pawH_new(P);
    P->builtin = pawH_new(P);
    V_SET_OBJECT(pv, P->builtin);

    // Builtin functions:
    add_builtin_func(P, "assert", base_assert); // fn assert(bool)
    add_builtin_func(P, "print", base_print); // fn print(string)

    // TODO: Replace with real methods
    add_builtin_func(P, "_int_to_string", int_to_string);
    add_builtin_func(P, "_string_parse_int", string_parse_int);
    add_builtin_func(P, "_string_parse_float", string_parse_float);
    add_builtin_func(P, "_string_starts_with", string_starts_with);
    add_builtin_func(P, "_string_ends_with", string_ends_with);
    add_builtin_func(P, "_string_find", string_find);
    add_builtin_func(P, "_string_split", string_split);
    add_builtin_func(P, "_string_join", string_join);
    add_builtin_func(P, "_list_push", list_push);
    add_builtin_func(P, "_list_pop", list_pop);
    add_builtin_func(P, "_list_insert", list_insert);
    add_builtin_func(P, "_list_erase", list_remove);
    add_builtin_func(P, "_list_clone", list_clone);

    pawC_pop(P);
}

// 'pawL_register_*lib' functions defined in corresponding '*lib.c' source files
extern void pawL_require_iolib(paw_Env *P);
extern void pawL_require_mathlib(paw_Env *P);

static paw_Bool load_cached(paw_Env *P, const char *name)
{
    paw_push_string(P, name);
    const Value key = P->top.p[-1];

    Value *pvalue = pawH_get(P->libs, key);
    if (pvalue == NULL) return PAW_FALSE;

    // replace library name
    pawC_pushv(P, *pvalue);
    paw_shift(P, 1);
    return PAW_TRUE;
}

void pawL_require_lib(paw_Env *P, const char *name)
{
    //    paw_assert(name);
    //    if (load_cached(P, name)) {
    //        return; // already loaded
    //    }
    //    // Automatically register base libraries. This will prevent libraries
    //    with any of
    //    // the base library names from being registered.
    //    if (0 == strcmp(name, IOLIB_NAME)) {
    //        pawL_require_iolib(P);
    //    } else if (0 == strcmp(name, MATHLIB_NAME)) {
    //        pawL_require_mathlib(P);
    //    } else {
    //        pawR_error(P, PAW_ENAME, "library '%s' has not been registered",
    //        name);
    //    }
}

struct FileReader {
    char data[512];
    FILE *file;
};

static const char *file_reader(paw_Env *P, void *ud, size_t *psize)
{
    paw_unused(P);
    struct FileReader *fr = ud;
    *psize = pawO_read(fr->file, fr->data, sizeof(fr->data));
    return fr->data;
}

int pawL_load_file(paw_Env *P, const char *pathname)
{
    struct FileReader fr;
    fr.file = pawO_open(pathname, "r");
    if (fr.file == NULL) {
        paw_push_string(P, strerror(errno));
        return PAW_ESYSTEM;
    }
    return paw_load(P, file_reader, pathname, &fr);
}

struct ChunkReader {
    const char *data;
    size_t size;
};

static const char *chunk_reader(paw_Env *P, void *ud, size_t *psize)
{
    paw_unused(P);
    struct ChunkReader *cr = ud;
    *psize = cr->size;
    cr->size = 0;
    return cr->data;
}

int pawL_load_nchunk(paw_Env *P, const char *name, const char *source, size_t length)
{
    struct ChunkReader cr = {.data = source, .size = length};
    return paw_load(P, chunk_reader, name, &cr);
}

int pawL_load_chunk(paw_Env *P, const char *name, const char *source)
{
    return pawL_load_nchunk(P, name, source, strlen(source));
}
