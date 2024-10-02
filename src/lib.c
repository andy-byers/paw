// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "prefix.h"

#include <errno.h>
#include <stdio.h>

#include "lib.h"
#include "api.h"
#include "auxlib.h"
#include "call.h"
#include "gc.h"
#include "os.h"
#include "map.h"
#include "mem.h"
#include "os.h"
#include "rt.h"

#define CF_BASE(i) (P->cf->base.p + i)

static int base_assert(paw_Env *P)
{
    if (V_FALSE(*CF_BASE(1))) {
        pawR_error(P, PAW_ERUNTIME, "assertion failed");
    }
    return 0;
}

static int base_print(paw_Env *P)
{
    const String *s = V_STRING(P->top.p[-1]);
    pawO_write_all(P, pawO_stdout(), s->text, s->length);
    pawO_flush(pawO_stdout());
    return 0;
}

static int list_insert(paw_Env *P)
{
    List *list = V_LIST(*CF_BASE(1));
    const paw_Int index = V_INT(*CF_BASE(2));
    pawV_list_insert(P, list, index, *CF_BASE(3));
    paw_pop(P, 2); // return 'self'
    return 1;
}

static int enum_is_zero(paw_Env *P)
{
    const Variant *var = V_VARIANT(*CF_BASE(1));
    V_SET_BOOL(CF_BASE(1), var->k == 0);
    return 1;
}

static int enum_is_one(paw_Env *P)
{
    const Variant *var = V_VARIANT(*CF_BASE(1));
    V_SET_BOOL(CF_BASE(1), var->k == 1);
    return 1;
}

static int enum_unwrap(paw_Env *P)
{
    Variant *var = V_VARIANT(*CF_BASE(1));
    if (var->k != 0) pawR_error(P, PAW_ERUNTIME, "failed to unwrap");
    *CF_BASE(1) = var->fields[0];
    return 1;
}

static int enum_unwrap_or(paw_Env *P)
{
    Variant *var = V_VARIANT(*CF_BASE(1));
    if (var->k == 0) *CF_BASE(2) = var->fields[0];
    return 1;
}

static int list_length(paw_Env *P)
{
    pawR_length(P, PAW_ADT_LIST);
    return 1;
}

static int list_push(paw_Env *P)
{
    List *list = V_LIST(*CF_BASE(1));
    pawV_list_push(P, list, *CF_BASE(2));
    paw_pop(P, 1); // return 'self'
    return 1;
}

static int list_pop(paw_Env *P)
{
    List *list = V_LIST(*CF_BASE(1));
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
    const paw_Int i = V_INT(*CF_BASE(loc));
    return i < 0 ? 0 : i >= n ? n - 1 : i;
}

// TODO: It would be nice to let pop() take an optional parameter indicating the
//       index at which to erase an element. To me, 'remove' seems like it
//       should remove the first matching element using something akin to
//       operator==. Attempting this will break, since we have no concept of
//       equality between user-defined types right now.
static int list_remove(paw_Env *P)
{
    List *list = V_LIST(*CF_BASE(1));
    const paw_Int length = PAW_CAST_INT(pawV_list_length(list));
    if (length == 0) {
        pawR_error(P, PAW_EVALUE, "remove from empty List");
    }
    const paw_Int index = V_INT(*CF_BASE(2));
    P->top.p[-1] = *pawV_list_get(P, list, index);
    pawV_list_pop(P, list, index);
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
    const String *s = V_STRING(*CF_BASE(1));
    const String *find = V_STRING(*CF_BASE(2));
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

static int string_split(paw_Env *P)
{
     const String *sep = V_STRING(*CF_BASE(2));
     String *s = V_STRING(*CF_BASE(1));
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
     String *s = V_STRING(*CF_BASE(1));
     const Value seq = *CF_BASE(2);

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
    String *s = V_STRING(*CF_BASE(1));
    const String *prefix = V_STRING(*CF_BASE(2));
    const size_t prelen = prefix->length;
    const paw_Bool b = s->length >= prelen && 
        0 == memcmp(prefix->text, s->text, prelen);
    V_SET_BOOL(P->top.p - 1, b);
    return 1;
}

static int string_ends_with(paw_Env *P)
{
    String *s = V_STRING(*CF_BASE(1));
    const String *suffix = V_STRING(*CF_BASE(2));
    const size_t suflen = suffix->length;
    paw_Bool b = PAW_FALSE;
    if (s->length >= suflen) {
        const char *ptr = s->text + s->length - suflen;
        b = 0 == memcmp(suffix->text, ptr, suflen);
    }
    V_SET_BOOL(P->top.p - 1, b);
    return 1;
}

static int bool_to_string(paw_Env *P)
{
    Value *pv = CF_BASE(1);
    pawV_to_string(P, pv, PAW_TBOOL, NULL);
    return 1;
}

static int int_to_string(paw_Env *P)
{
    Value *pv = CF_BASE(1);
    pawV_to_string(P, pv, PAW_TINT, NULL);
    return 1;
}

static int float_to_string(paw_Env *P)
{
    Value *pv = CF_BASE(1);
    pawV_to_string(P, pv, PAW_TFLOAT, NULL);
    return 1;
}

static int string_parse_float(paw_Env *P)
{
    paw_Float f;
    const char *str = V_TEXT(*CF_BASE(1));
    const int status = pawV_parse_float(P, str, &f);
    if (status != PAW_OK) pawR_error(P, PAW_ESYNTAX, "invalid float '%s'", str);
    V_SET_FLOAT(CF_BASE(1), f);
    return 1;
}

static int string_parse_int(paw_Env *P)
{
    const char *str = V_TEXT(*CF_BASE(1));
    const paw_Int base = V_INT(*CF_BASE(2));

    paw_Int i;
    const int rc = pawV_parse_int(P, str, base, &i);
    if (rc == PAW_ESYNTAX) {
        pawR_error(P, PAW_ESYNTAX, "invalid integer '%s'", str);
    } else if (rc == PAW_EOVERFLOW) {
        pawR_error(P, PAW_EOVERFLOW, "integer '%s' is out of range", str);
    }
    V_SET_INT(&P->top.p[-1], i);
    return 1;
}

static int map_length(paw_Env *P)
{
    pawR_length(P, PAW_ADT_MAP);
    return 1;
}

static int map_get_or(paw_Env *P)
{
    Map *m = V_MAP(*CF_BASE(1));
    const Value key = *CF_BASE(2);
    const Value *pv = pawH_get(m, key);
    if (pv != NULL) P->top.p[-1] = *pv;
    return 1;
}

static int map_erase(paw_Env *P)
{
    Map *m = V_MAP(*CF_BASE(1));
    pawH_erase(m, *CF_BASE(2));
    paw_pop(P, 1); // return 'self'
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

static void add_prelude_func(paw_Env *P, const char *name, paw_Function func)
{
    paw_push_value(P, -1);
    paw_push_string(P, name);
    paw_mangle_name(P, NULL, PAW_FALSE);
    pawL_new_func(P, func, 0);

    pawR_setelem(P, PAW_ADT_MAP);
    pawC_stkdec(P, 1);
}

static void add_prelude_method(paw_Env *P, const char *self, const char *name, paw_Function func)
{
    paw_push_value(P, -1);
    paw_push_string(P, name);
    paw_mangle_name(P, NULL, PAW_FALSE);
    paw_push_string(P, self);
    paw_mangle_self(P, NULL, PAW_FALSE);
    pawL_new_func(P, func, 0);

    pawR_setelem(P, PAW_ADT_MAP);
    pawC_stkdec(P, 1);
}

void pawL_init(paw_Env *P)
{
    Value *pv = pawC_push0(P);
    P->libs = pawH_new(P);
    P->builtin = pawH_new(P);
    V_SET_OBJECT(pv, P->builtin);

    // Builtin functions:
    add_prelude_func(P, "assert", base_assert); // fn assert(bool)
    add_prelude_func(P, "print", base_print); // fn print(string)

    add_prelude_method(P, "bool", "to_string", bool_to_string);
    add_prelude_method(P, "int", "to_string", int_to_string);
    add_prelude_method(P, "float", "to_string", float_to_string);

    add_prelude_method(P, "str", "parse_int", string_parse_int);
    add_prelude_method(P, "str", "parse_float", string_parse_float);
    add_prelude_method(P, "str", "split", string_split);
    add_prelude_method(P, "str", "join", string_join);
    add_prelude_method(P, "str", "find", string_find);
    add_prelude_method(P, "str", "starts_with", string_starts_with);
    add_prelude_method(P, "str", "ends_with", string_ends_with);

    add_prelude_method(P, "_List", "length", list_length);
    add_prelude_method(P, "_List", "push", list_push);
    add_prelude_method(P, "_List", "insert", list_insert);
    add_prelude_method(P, "_List", "remove", list_remove);
    add_prelude_method(P, "_List", "pop", list_pop);

    add_prelude_method(P, "_Map", "length", map_length);
    add_prelude_method(P, "_Map", "get_or", map_get_or);
    add_prelude_method(P, "_Map", "erase", map_erase);

    add_prelude_method(P, "Option", "is_some", enum_is_zero);
    add_prelude_method(P, "Option", "is_none", enum_is_one);
    add_prelude_method(P, "Option", "unwrap", enum_unwrap);
    add_prelude_method(P, "Option", "unwrap_or", enum_unwrap_or);

    add_prelude_method(P, "Result", "is_ok", enum_is_zero);
    add_prelude_method(P, "Result", "is_err", enum_is_one);
    add_prelude_method(P, "Result", "unwrap", enum_unwrap);
    add_prelude_method(P, "Result", "unwrap_or", enum_unwrap_or);

    pawC_pop(P);
}

struct FileReader {
    char data[512];
    File *file;
    paw_Bool err;
};

static const char *file_reader(paw_Env *P, void *ud, size_t *psize)
{
    PAW_UNUSED(P);
    struct FileReader *fr = ud;
    const size_t zchunk = sizeof(fr->data);
    *psize = pawO_read(P, fr->file, fr->data, zchunk);
// TODO: don't throw errors in os.c    if (*psize != zchunk) fr->err = ferror(fr->file);
    return *psize > 0 ? fr->data : NULL;
}

int pawL_load_file(paw_Env *P, const char *pathname)
{
    struct FileReader fr = {0};
    fr.file = pawO_new_file(P);
    const int rc = pawO_open(fr.file, pathname, "r");
    if (rc == 0) {
        const int status = paw_load(P, file_reader, pathname, &fr);
        if (!fr.err) return status;
    }
    paw_push_string(P, strerror(errno));
    return PAW_ESYSTEM;
}

struct ChunkReader {
    const char *data;
    size_t size;
};

static const char *chunk_reader(paw_Env *P, void *ud, size_t *psize)
{
    PAW_UNUSED(P);
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

int pawL_register_func(paw_Env *P, const char *name, paw_Function func, int nup)
{
    paw_new_native(P, func, nup);

    V_SET_OBJECT(P->top.p, P->builtin);
    API_INCR_TOP(P, 1);

    paw_push_string(P, name);
    paw_mangle_name(P, NULL, PAW_FALSE);

    // func, builtin, name => builtin, name, func
    paw_rotate(P, -3, -1);
    paw_setelem(P, PAW_ADT_MAP);
    return 0;
}

void *pawL_chunk_reader(paw_Env *P, const char *text, size_t length, paw_Reader *preader)
{
    struct ChunkReader *r = paw_new_foreign(P, sizeof(struct ChunkReader), 0);
    *preader = chunk_reader;
    *r = (struct ChunkReader){
        .size = length,
        .data = text,
    };
    return r;
}

void pawL_add_extern_func(paw_Env *P, const char *modname, const char *name, paw_Function func)
{
    paw_push_value(P, -1);
    paw_push_string(P, modname);
    paw_push_string(P, name);
    paw_mangle_name(P, NULL, PAW_TRUE);
    pawL_new_func(P, func, 0);

    pawR_setelem(P, PAW_ADT_MAP);
    pawC_stkdec(P, 1);
}

void pawL_add_extern_method(paw_Env *P, const char *modname, const char *self, const char *name, paw_Function func)
{
    paw_push_value(P, -1);
    paw_push_string(P, modname);
    paw_push_string(P, name);
    paw_mangle_name(P, NULL, PAW_TRUE);
    paw_push_string(P, modname);
    paw_push_string(P, self);
    paw_mangle_self(P, NULL, PAW_TRUE);
    pawL_new_func(P, func, 0);

    pawR_setelem(P, PAW_ADT_MAP);
    pawC_stkdec(P, 1);
}

#define MATCHES_MODULE(str, lit) ((str)->length == PAW_LENGTHOF(lit) && \
                                  memcmp((str)->text, lit, PAW_LENGTHOF(lit)) == 0)

static const char *file_import_reader(paw_Env *P, void *ud, size_t *psize)
{
    PAW_UNUSED(P);
    struct FileReader *fr = ud;
    if (fr->err) {
        *psize = 0;
        return NULL;
    }
    *psize = pawO_read(P, fr->file, fr->data, sizeof(fr->data));
// TODO    if (*psize != sizeof(fr->data)) fr->err = ferror(fr->file);
    return *psize > 0 ? fr->data : NULL;
}

void *pawL_start_import(paw_Env *P, paw_Reader *preader)
{
    void *l_import_io(paw_Env *P, paw_Reader *preader);
    void *l_import_math(paw_Env *P, paw_Reader *preader);

    Value *pv = &P->top.p[-1];
    const String *name = V_STRING(*pv);
    if (MATCHES_MODULE(name, "io")) {
        return l_import_io(P, preader);
    } else if (MATCHES_MODULE(name, "math")) {
        return l_import_math(P, preader);
    }
    struct FileReader *r = paw_new_foreign(P, sizeof(struct FileReader), 0);
    Foreign *f = V_FOREIGN(P->top.p[-1]);
    f->flags = 0 /* TODO: flag for GC */;

    File *file = pawO_new_file(P);
    const int rc = pawO_open(file, name->text, "r"); // TODO: look in various directories
    if (rc != 0) pawO_error(P);
    *preader = file_import_reader;
    *r = (struct FileReader){
        .file = file,
    };
    return r;
}

void pawL_finish_import(paw_Env *P)
{
    paw_pop(P, 1);
}
