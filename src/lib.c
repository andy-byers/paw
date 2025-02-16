// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "prefix.h"

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>

#include "lib.h"
#include "api.h"
#include "auxlib.h"
#include "call.h"
#include "gc.h"
#include "list.h"
#include "os.h"
#include "map.h"
#include "mem.h"
#include "os.h"
#include "rt.h"

// TODO: avoid using an upper bound on path length, or at least make sure not to overflow
//       the automatic buffers used to hold paths in this file
#if defined(PAW_OS_POSIX)
# define PAW_PATH_MAX PATH_MAX
#else
# define PAW_PATH_MAX 2048
#endif

#define CF_BASE(i) (P->cf->base.p + i)

static int base_assert(paw_Env *P)
{
    if (!paw_bool(P, 1)) {
        // TODO: pass source text of falsy expression as second argument, display here
        pawR_error(P, PAW_ERUNTIME, "assertion failed");
    }
    return 0;
}

static int base_print(paw_Env *P)
{
    const char *string = paw_string(P, 1);
    const size_t length = paw_str_rawlen(P, 1);
    pawO_write_all(P, pawO_stdout(), string, length);
    pawO_flush(pawO_stdout());
    return 0;
}

#define STOP_LOOP(a, b, c) \
    (((c) < 0 && (a) <= (b)) || ((c) > 0 && (a) >= (b)))

static int range_iterator(paw_Env *P)
{
    paw_get_upvalue(P, 0, 0);
    paw_get_upvalue(P, 0, 1);
    paw_get_upvalue(P, 0, 2);
    const paw_Int begin = paw_int(P, 1);
    const paw_Int end = paw_int(P, 2);
    const paw_Int step = paw_int(P, 3);
    Value *pval = P->top.p++;
    if (!STOP_LOOP(begin, end, step)) {
        paw_push_int(P, begin + step);
        paw_set_upvalue(P, 0, 0);
        pawR_new_tuple(P, P->cf, pval, 2);
        V_TUPLE(*pval)->elems[0].i = 0;
        V_TUPLE(*pval)->elems[1].i = begin;
    } else {
        pawR_new_tuple(P, P->cf, pval, 1);
        V_TUPLE(*pval)->elems[0].i = 1;
    }
    return 1;
}

static int base_range(paw_Env *P)
{
    const paw_Int begin = paw_int(P, 1);
    const paw_Int end = paw_int(P, 2);
    const paw_Int step = paw_int(P, 3);
    if (step == 0) pawR_error(P, PAW_EVALUE, "step cannot be 0");
    paw_new_native(P, range_iterator, 3);
    return 1;
}

static void new_option_some(paw_Env *P, Value *pval, Value value)
{
    pawR_new_tuple(P, P->cf, pval, 2);
    V_TUPLE(*pval)->elems[0].i = 0;
    V_TUPLE(*pval)->elems[1] = value;
}

static void new_option_none(paw_Env *P, Value *pval)
{
    pawR_new_tuple(P, P->cf, pval, 1);
    V_TUPLE(*pval)->elems[0].i = 1;
}

static int list_insert(paw_Env *P)
{
    Tuple *list = V_TUPLE(*CF_BASE(1));
    const paw_Int index = paw_int(P, 2);
    pawList_insert(P, list, index, *CF_BASE(3));
    paw_pop(P, 2); // return 'self'
    return 1;
}

static int enum_is_zero(paw_Env *P)
{
    const int k = V_DISCR(*CF_BASE(1));
    V_SET_BOOL(CF_BASE(1), k == 0);
    return 1;
}

static int enum_is_one(paw_Env *P)
{
    const int k = V_DISCR(*CF_BASE(1));
    V_SET_BOOL(CF_BASE(1), k == 1);
    return 1;
}

static int enum_unwrap(paw_Env *P)
{
    const Value v = *CF_BASE(1);
    if (V_DISCR(v) != 0) pawR_error(P, PAW_ERUNTIME, "failed to unwrap");
    *CF_BASE(1) = V_TUPLE(v)->elems[1];
    return 1;
}

static int enum_unwrap_or(paw_Env *P)
{
    const Value v = *CF_BASE(1);
    if (V_DISCR(v) == 0) *CF_BASE(2) = V_TUPLE(v)->elems[1];
    return 1;
}

static int result_unwrap_err(paw_Env *P)
{
    const Value v = *CF_BASE(1);
    if (V_DISCR(v) == 0) pawR_error(P, PAW_ERUNTIME, "failed to unwrap error");
    *CF_BASE(1) = V_TUPLE(v)->elems[1];
    return 1;
}

static int list_length(paw_Env *P)
{
    paw_list_length(P, 1);
    return 1;
}

static int list_push(paw_Env *P)
{
    Tuple *list = V_TUPLE(*CF_BASE(1));
    pawList_push(P, list, *CF_BASE(2));
    paw_pop(P, 1); // return 'self'
    return 1;
}

static int list_pop(paw_Env *P)
{
    Tuple *list = V_TUPLE(*CF_BASE(1));
    const paw_Int length = pawList_length(list);
    if (length == 0) {
        pawR_error(P, PAW_EVALUE, "pop from empty List");
    }
    // overwrites the register containing the list, which is fine because
    // pawList_pop doesn't allocate
    *CF_BASE(1) = *pawList_get(P, list, length - 1);
    pawList_pop(P, list, length - 1);
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
    Tuple *list = V_TUPLE(*CF_BASE(1));
    if (pawList_length(list) == 0) {
        pawR_error(P, PAW_EVALUE, "remove from empty List");
    }
    const paw_Int index = paw_int(P, 2);
    P->top.p[-1] = *pawList_get(P, list, index);
    pawList_pop(P, list, index);
    return 1;
}

static const char *find_substr(const char *str, size_t nstr, const char *sub, size_t nsub)
{
    if (nsub == 0) return str;
    const char *end = str + nstr;
    while ((str = strchr(str, sub[0]))) {
        if (nsub <= CAST_SIZE(end - str)
                && memcmp(str, sub, nsub) == 0) {
            return str;
        }
        str += nsub;
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

     int npart = 0;
     const char *part;
     size_t nstr = s->length;
     const char *pstr = s->text;
     while ((part = find_substr(pstr, nstr, sep->text, sep->length))) {
         if (npart == INT_MAX) pawR_error(P, PAW_EOVERFLOW, "too many substrings");
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

     paw_new_list(P, npart);
     return 1;
 }

static int string_join(paw_Env *P)
{
    String *sep = V_STRING(*CF_BASE(1));

    Buffer buf;
    pawL_init_buffer(P, &buf);
    paw_push_int(P, PAW_ITER_INIT);
    while (paw_list_next(P, 2)) {
        const char *chunk = paw_string(P, -1);
        const paw_Int chunklen = paw_str_rawlen(P, -1);
        pawL_add_nstring(P, &buf, chunk, chunklen);
        L_ADD_STRING(P, &buf, sep);
        paw_pop(P, 1);
    }
    paw_pop(P, 1);

    if (buf.size > 0) {
        // remove the last separator
        paw_assert(buf.size >= sep->length);
        buf.size -= sep->length;
    }
    pawL_push_result(P, &buf);
    return 1;
 }

static int string_starts_with(paw_Env *P)
{
    const String *s = V_STRING(*CF_BASE(1));
    const String *prefix = V_STRING(*CF_BASE(2));
    const size_t prelen = prefix->length;
    const paw_Bool b = s->length >= prelen &&
        0 == memcmp(prefix->text, s->text, prelen);
    V_SET_BOOL(P->top.p - 1, b);
    return 1;
}

static int string_ends_with(paw_Env *P)
{
    const String *s = V_STRING(*CF_BASE(1));
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
    const char *str = paw_string(P, 1);
    const int status = pawV_parse_float(P, str, &f);
    if (status != PAW_OK) pawR_error(P, PAW_ESYNTAX, "invalid float '%s'", str);
    V_SET_FLOAT(CF_BASE(1), f);
    return 1;
}

static int string_parse_int(paw_Env *P)
{
    const char *str = paw_string(P, 1);
    const paw_Int base = paw_int(P, 2);
    if (base > INT_MAX) {
        pawR_error(P, PAW_EOVERFLOW, "base '%I' is too large", base);
    }

    paw_Int i;
    const int rc = pawV_parse_int(P, str, CAST(int, base), &i);
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
//    pawR_length(P, PAW_ADT_MAP);
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

const char *find_last_sep(const char *s, size_t n, size_t *pn)
{
    paw_assert(n > 0);
    const char *s0 = s;
    s += n; *pn = 0;

    do {
        --s; ++*pn;
        const char *q = PAW_FOLDER_SEPS;
        while (*q != '\0') {
            if (*q++ == *s) return s;
        }
    } while (s != s0);
    return NULL;
}

static void path_to_modname(const char *pathname, size_t pathlen, char *modname)
{
    size_t modlen;
    const char *begin = find_last_sep(pathname, pathlen, &modlen);
    if (begin != NULL) {
        // skip separator
        --modlen;
        ++begin;
    } else {
        begin = pathname;
        modlen = pathlen;
    }
    const char *end = strchr(begin, '.');
    if (end != NULL) modlen = end - begin;
    memcpy(modname, begin, modlen);
    modname[modlen] = '\0';
}

struct FileReader {
    struct LoaderState state;
    char data[512];
    File *file;
    paw_Bool err;
};

static const char *file_reader(paw_Env *P, void *ud, size_t *psize)
{
    struct FileReader *fr = ud;
    const size_t zchunk = sizeof(fr->data);
    *psize = pawO_read(P, fr->file, fr->data, zchunk);
// TODO: don't throw errors in os.c    if (*psize != zchunk) fr->err = ferror(fr->file);
    return *psize > 0 ? fr->data : NULL;
}

struct ChunkReader {
    struct LoaderState state;
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

void pawL_close_loader(paw_Env *P, void *state)
{
    if (state == NULL) return;
    struct FileReader *fr = state;
    pawO_close(fr->file);
    pawO_free_file(P, fr->file);
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
    paw_mangle_start(P);
    paw_push_string(P, name);
    paw_mangle_add_name(P);
    pawL_new_func(P, func, 0);
    paw_map_set(P, -3);
}

static void add_prelude_method(paw_Env *P, const char *self, const char *name, paw_Function func)
{
    paw_mangle_start(P);
    paw_push_string(P, self);
    paw_mangle_add_name(P);
    paw_push_string(P, name);
    paw_mangle_add_name(P);
    pawL_new_func(P, func, 0);
    paw_map_set(P, -3);
}

void pawL_push_builtin_map(paw_Env *P)
{
    pawE_push_cstr(P, CSTR_KBUILTIN);
    paw_map_get(P, PAW_REGISTRY_INDEX);
}

void pawL_push_modules_map(paw_Env *P)
{
    pawE_push_cstr(P, CSTR_KMODULES);
    paw_map_get(P, PAW_REGISTRY_INDEX);
}

// Load function pointers for prelude functions
// Expects the 'paw.builtin' map (from the registry) on top of the stack
static void load_builtins(paw_Env *P)
{
    add_prelude_func(P, "assert", base_assert);
    add_prelude_func(P, "print", base_print);
    add_prelude_func(P, "range", base_range);

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

    add_prelude_method(P, "List", "length", list_length);
    add_prelude_method(P, "List", "push", list_push);
    add_prelude_method(P, "List", "insert", list_insert);
    add_prelude_method(P, "List", "remove", list_remove);
    add_prelude_method(P, "List", "pop", list_pop);

    add_prelude_method(P, "Map", "length", map_length);
    add_prelude_method(P, "Map", "get_or", map_get_or);
    add_prelude_method(P, "Map", "erase", map_erase);

    add_prelude_method(P, "Option", "is_some", enum_is_zero);
    add_prelude_method(P, "Option", "is_none", enum_is_one);
    add_prelude_method(P, "Option", "unwrap", enum_unwrap);
    add_prelude_method(P, "Option", "unwrap_or", enum_unwrap_or);

    add_prelude_method(P, "Result", "is_ok", enum_is_zero);
    add_prelude_method(P, "Result", "is_err", enum_is_one);
    add_prelude_method(P, "Result", "unwrap", enum_unwrap);
    add_prelude_method(P, "Result", "unwrap_err", result_unwrap_err);
    add_prelude_method(P, "Result", "unwrap_or", enum_unwrap_or);
}

static paw_Bool lib_getenv(paw_Env *P)
{
    const char *env = getenv(paw_string(P, -1));
    paw_pop(P, 1);

    if (env == NULL) return PAW_FALSE;
    paw_push_string(P, env);
    return PAW_TRUE;
}

static paw_Bool matches_modname(paw_Env *P, const char *modname)
{
    return strncmp(modname, paw_string(P, -1), paw_str_rawlen(P, -1)) == 0;
}

static struct FileReader *new_file_reader(paw_Env *P, const char *pathname)
{
    File *file = pawO_new_file(P);
    const int rc = pawO_open(file, pathname, "r");
    if (rc == -ENOENT) {
        paw_pop(P, 1);
        return NULL;
    } else if (rc < 0) {
        pawO_error(P);
    }

    struct FileReader *r = paw_new_foreign(P, sizeof(struct FileReader), 0);
    Foreign *f = V_FOREIGN(P->top.p[-1]);
    r->file = pawO_detach_file(P, file);
    r->state.f = file_reader;
    f->flags = VBOX_LOADER;
    paw_shift(P, 1);

    return r;
}

static int searcher_Paw(paw_Env *P)
{
    void l_import_io(paw_Env *P);
    void l_import_math(paw_Env *P);

    if (matches_modname(P, "io")) {
        l_import_io(P);
    } else if (matches_modname(P, "math")) {
        l_import_math(P);
    } else {
        paw_push_zero(P, 1);
    }
    return 1;
}

static int searcher_cwd(paw_Env *P)
{
    if (P->modname != NULL) {
        const char *pathname = P->modname->text;
        const size_t pathlen = P->modname->length;
        paw_assert(pathlen <= PAW_PATH_MAX); // TODO: throw an error at least

        size_t modlen;
        const char *sep = find_last_sep(pathname, pathlen, &modlen);
        if (sep == NULL) goto use_current_dir;
        paw_push_nstring(P, pathname, sep - pathname + 1);
    } else {
use_current_dir:;
        char prefix[] = {'.', PAW_FOLDER_SEPS[0], '\0'};
        paw_push_string(P, prefix);
    }
    paw_rotate(P, -2, 1);
    PAW_PUSH_LITERAL(P, PAW_MODULE_EXT);
    paw_str_concat(P, 3);

    struct FileReader *fr = new_file_reader(P, paw_string(P, -1));
    if (fr == NULL) paw_push_zero(P, 1); // indicate failure

    return 1;
}

static void push_prelude_method(paw_Env *P, const char *self, const char *name)
{
    pawE_push_cstr(P, CSTR_KBUILTIN);
    paw_map_get(P, PAW_REGISTRY_INDEX);

    paw_mangle_start(P);
    paw_push_string(P, self);
    paw_mangle_add_name(P);
    paw_push_string(P, name);
    paw_mangle_add_name(P);

    paw_map_get(P, -2);
    paw_shift(P, 1);
}

static int searcher_env(paw_Env *P)
{
    PAW_PUSH_LITERAL(P, PAW_PATH_VAR);
    if (lib_getenv(P)) {
        // split on the path separator
        push_prelude_method(P, "str", "split");
        paw_rotate(P, -2, 1);
        PAW_PUSH_LITERAL(P, PAW_PATH_SEP);
        paw_call(P, 2);

        paw_push_int(P, PAW_ITER_INIT);
        while (paw_list_next(P, -2)) {
            // path + sep + modname + ext
            paw_push_nstring(P, PAW_FOLDER_SEPS, 1);
            paw_push_value(P, 1); // modname
            PAW_PUSH_LITERAL(P, PAW_MODULE_EXT);
            paw_str_concat(P, 4);

            struct FileReader *fr = new_file_reader(P, paw_string(P, -1));
            if (fr != NULL) return 1;

            paw_pop(P, 1);
        }
    }
    paw_push_zero(P, 1);
    return 1;
}

static int init_searchers(paw_Env *P)
{
    const paw_Function fs[] = {
        searcher_Paw, // check stdlib first
        searcher_cwd,
        searcher_env,
    };
    for (int i = 0; i < PAW_COUNTOF(fs); ++i) {
        pawL_new_func(P, fs[i], 0);
    }
    return PAW_COUNTOF(fs);
}

void pawL_init(paw_Env *P)
{
    // create system registry objects
    pawE_push_cstr(P, CSTR_KSEARCHERS);
    paw_new_list(P, init_searchers(P));
    pawE_push_cstr(P, CSTR_KMODULES);
    paw_new_map(P, 0);
    pawE_push_cstr(P, CSTR_KBUILTIN);
    paw_new_map(P, 0);
    load_builtins(P);

    // create the registry itself
    paw_new_map(P, 3);
    P->registry = P->top.p[-1];
    paw_pop(P, 1);
}

void pawL_uninit(paw_Env *P)
{
    // clear GC root
    P->registry = (Value){0};
}

int pawL_load_file(paw_Env *P, const char *pathname)
{
    struct FileReader fr = {
        .file = pawO_new_file(P),
        .state.f = file_reader,
    };
    const int rc = pawO_open(fr.file, pathname, "r");
    if (rc == 0) {
        char modname[PAW_PATH_MAX + 1];
        const size_t pathlen = strlen(pathname);
        paw_assert(pathlen <= PAW_PATH_MAX); // TODO
        path_to_modname(pathname, pathlen, modname);
        const int status = paw_load(P, file_reader, modname, &fr);
        if (!fr.err) return status;
    }
    paw_push_string(P, strerror(errno));
    return PAW_ESYSTEM;
}

int pawL_load_nchunk(paw_Env *P, const char *name, const char *source, size_t length)
{
    struct ChunkReader cr = {
        .state.f = chunk_reader,
        .data = source,
        .size = length,
    };
    return paw_load(P, chunk_reader, name, &cr);
}

int pawL_load_chunk(paw_Env *P, const char *name, const char *source)
{
    return pawL_load_nchunk(P, name, source, strlen(source));
}

int pawL_register_func(paw_Env *P, const char *name, paw_Function func, int nup)
{
    // map[mangle(name)] = func
    pawL_push_builtin_map(P);
    paw_mangle_start(P);
    paw_push_string(P, name);
    paw_mangle_add_name(P);
    paw_new_native(P, func, nup);
    paw_map_set(P, -3);
    return 0;
}

void *pawL_chunk_reader(paw_Env *P, const char *text, size_t length)
{
    struct ChunkReader *r = paw_new_foreign(P, sizeof(struct ChunkReader), 0);
    *r = (struct ChunkReader){
        .state.f = chunk_reader,
        .size = length,
        .data = text,
    };
    return r;
}

void pawL_add_extern_func(paw_Env *P, const char *modname, const char *name, paw_Function func)
{
    paw_mangle_start(P);
    paw_push_string(P, modname);
    paw_mangle_add_module(P);
    paw_push_string(P, name);
    paw_mangle_add_name(P);
    pawL_new_func(P, func, 0);
    paw_map_set(P, -3);
}

void pawL_add_extern_method(paw_Env *P, const char *modname, const char *self, const char *name, paw_Function func)
{
    paw_mangle_start(P);
    paw_push_string(P, modname);
    paw_mangle_add_module(P);
    paw_push_string(P, self);
    paw_mangle_add_name(P);
    paw_push_string(P, name);
    paw_mangle_add_name(P);
    pawL_new_func(P, func, 0);
    paw_map_set(P, -3);
}

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

struct LoaderState *pawL_start_import(paw_Env *P)
{
    pawE_push_cstr(P, CSTR_KSEARCHERS);
    paw_map_get(P, PAW_REGISTRY_INDEX);

    paw_push_int(P, PAW_ITER_INIT);
    // .. name searchers iter f
    while (paw_list_next(P, -2)) {
        paw_push_value(P, -4); // name
        const int status = paw_call(P, 1); // state = f(name)
        if (status != PAW_OK) pawC_throw(P, status);
        if (paw_rawptr(P, -1) != NULL) {
            paw_shift(P, 3);
            return paw_pointer(P, -1);
        }
        paw_pop(P, 1);
    }
    paw_pop(P, 3);
    return NULL;
}

void pawL_finish_import(paw_Env *P)
{
    paw_pop(P, 1);
}
