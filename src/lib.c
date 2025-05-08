// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "prefix.h"

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>

#include "api.h"
#include "auxlib.h"
#include "call.h"
#include "gc.h"
#include "lib.h"
#include "list.h"
#include "map.h"
#include "mem.h"
#include "os.h"
#include "rt.h"

// TODO: avoid using an upper bound on path length, or at least make sure not to overflow
//       the automatic buffers used to hold paths in this file
#if defined(PAW_OS_POSIX)
#define PAW_PATH_MAX PATH_MAX
#else
#define PAW_PATH_MAX 2048
#endif

#define CF_BASE(i) (P->cf->base.p + i)

static int base_panic(paw_Env *P)
{
    // string argument on top of the stack
    pawC_throw(P, PAW_ERUNTIME);
}

static int base_assert(paw_Env *P)
{
    if (!paw_bool(P, 1)) {
        // TODO: pass source text of false expression as second argument, display here
        pawR_error(P, PAW_ERUNTIME, "assertion failed");
    }
    return 0;
}

static int base_print(paw_Env *P)
{
    char const *string = paw_string(P, 1);
    size_t const length = paw_str_rawlen(P, 1);
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
    paw_Int const begin = paw_int(P, 1);
    paw_Int const end = paw_int(P, 2);
    paw_Int const step = paw_int(P, 3);
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
    paw_Int const begin = paw_int(P, 1);
    paw_Int const end = paw_int(P, 2);
    paw_Int const step = paw_int(P, 3);
    if (step == 0)
        pawR_error(P, PAW_EVALUE, "step cannot be 0");
    paw_new_native(P, range_iterator, 3);
    return 1;
}

static void new_option_some(paw_Env *P, Value *presult, Value const *pvalue, int count)
{
    V_SET_INT(presult, PAW_OPTION_SOME);
    while (count-- > 0)
        *++presult = *pvalue++;

}

static void new_option_none(paw_Env *P, Value *presult)
{
    V_SET_INT(presult, PAW_OPTION_NONE);
}

static int list_insert(paw_Env *P)
{
    Tuple *list = V_TUPLE(*CF_BASE(1));
    paw_Int const index = paw_int(P, 2);
    pawList_insert(P, list, index, CF_BASE(3));
    paw_pop(P, 2); // return 'self'
    return 1;
}

static int enum_unwrap(paw_Env *P)
{
    Value const v = *CF_BASE(1);
    if (V_INT(*CF_BASE(1)) != 0)
        pawR_error(P, PAW_ERUNTIME, "failed to unwrap");
    return paw_get_count(P) - 2; // callable + discriminant
}

static int result_unwrap_err(paw_Env *P)
{
    Value const v = *CF_BASE(1);
    if (V_INT(*CF_BASE(1)) == 0)
        pawR_error(P, PAW_ERUNTIME, "failed to unwrap error");
    return paw_get_count(P) - 2; // callable + discriminant
}

static int list_length(paw_Env *P)
{
    paw_list_length(P, 1);
    return 1;
}

static int list_push(paw_Env *P)
{
    Tuple *list = V_TUPLE(*CF_BASE(1));
    pawList_push(P, list, CF_BASE(2));
    paw_pop(P, 1); // return 'self'
    return 1;
}

static int list_pop(paw_Env *P)
{
    Tuple *list = V_TUPLE(*CF_BASE(1));
    paw_Int const length = pawList_length(P, list);
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
    paw_Int const i = V_INT(*CF_BASE(loc));
    return i < 0 ? 0 : i >= n ? n - 1
                              : i;
}

static int list_get(paw_Env *P)
{
    Tuple *list = V_TUPLE(*CF_BASE(1));
    paw_Int const index = V_INT(*CF_BASE(2));
    int const z = LIST_ZELEMENT(list);
    Value const *pvalue = pawList_get(P, list, index);
    pawV_copy(CF_BASE(0), pvalue, z);
    return z;
}

static int list_set(paw_Env *P)
{
    Tuple *list = V_TUPLE(*CF_BASE(1));
    paw_Int const index = V_INT(*CF_BASE(2));
    Value const *pvalue = CF_BASE(3);
    int const z = LIST_ZELEMENT(list);
    Value *pslot = pawList_get(P, list, index);
    pawV_copy(pslot, pvalue, z);
    return z;
}

// TODO: It would be nice to let pop() take an optional parameter indicating the
//       index at which to erase an element. To me, 'remove' seems like it
//       should remove the first matching element using something akin to
//       operator==. Attempting this will break, since we have no concept of
//       equality between user-defined types right now.
static int list_remove(paw_Env *P)
{
    Tuple *list = V_TUPLE(*CF_BASE(1));
    if (pawList_length(P, list) == 0) {
        pawR_error(P, PAW_EVALUE, "remove from empty List");
    }
    paw_Int const index = paw_int(P, 2);
    int const z = LIST_ZELEMENT(list);
    Value const *pvalue = pawList_get(P, list, index);
    pawV_copy(CF_BASE(0), pvalue, z);
    pawList_pop(P, list, index);
    return z;
}

static int map_get(paw_Env *P)
{
    Tuple *map = V_TUPLE(*CF_BASE(1));
    Value const *pkey = CF_BASE(2);
    int const z = pawMap_value_size(P, map);
    Value const *pvalue = pawMap_get(P, map, pkey);

    P->top.p = CF_BASE(0);
    if (pvalue != NULL) {
        paw_push_int(P, PAW_OPTION_SOME);
        pawV_copy(CF_BASE(1), pvalue, z);
    } else {
        paw_push_int(P, PAW_OPTION_NONE);
    }
    P->top.p = CF_BASE(z);
    return z;
}

static int map_set(paw_Env *P)
{
    Tuple *map = V_TUPLE(*CF_BASE(1));
    Value const *pkey = CF_BASE(2);
    Value const *pvalue = CF_BASE(3);
    Value const *preplaced = pawMap_insert(P, map, pkey, pvalue);
    (void)preplaced; // TODO: return Option<V>
    return 0;
}

static int map_iter_next(paw_Env *P)
{
    paw_get_field(P, 1, 0); // 2: map
    paw_get_field(P, 1, 1); // 3: index

    StackPtr ra = CF_BASE(2);
    StackPtr rb = CF_BASE(3);
    Tuple *map = V_TUPLE(*ra);

    int const key_size = pawMap_key_size(P, map);
    paw_Int index = V_INT(*rb);
    if (pawMap_iter(map, &index)) {
        V_SET_INT(rb, index);
        paw_set_field(P, 1, 1);

        Value const *key = pawMap_key(P, map, index);
        new_option_some(P, CF_BASE(0), key, key_size);
    } else {
        new_option_none(P, CF_BASE(0));
    }
    P->top.p = CF_BASE(1 + key_size);
    return 1 + key_size;
}

static char const *find_substr(char const *str, size_t nstr, char const *sub, size_t nsub)
{
    if (nsub == 0)
        return str;
    char const *end = str + nstr;
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
    String const *str = V_STRING(*CF_BASE(1));
    String const *find = V_STRING(*CF_BASE(2));
    char const *result = find_substr(
        str->text, str->length,
        find->text, find->length);
    if (result) { // index of substring
        V_SET_INT(P->top.p - 1, result - str->text);
    } else { // not found
        V_SET_INT(P->top.p - 1, -1);
    }
    return 1;
}

static int string_split(paw_Env *P)
{
    String const *sep = V_STRING(*CF_BASE(2));
    String *str = V_STRING(*CF_BASE(1));
    if (sep->length == 0) {
        pawR_error(P, PAW_EVALUE, "empty separator");
    }

    int npart = 0;
    char const *part;
    size_t nstr = str->length;
    char const *pstr = str->text;
    while ((part = find_substr(pstr, nstr, sep->text, sep->length))) {
        if (npart == INT_MAX)
            pawR_error(P, PAW_EOVERFLOW, "too many substrings");
        size_t const n = CAST_SIZE(part - pstr);
        pawC_pushns(P, pstr, n);
        part += sep->length; // skip separator
        pstr = part;
        nstr -= n;
        ++npart;
    }
    char const *end = str->text + str->length; // add the rest
    pawC_pushns(P, pstr, CAST_SIZE(end - pstr));
    ++npart;

    paw_new_list(P, npart, 1);
    return 1;
}

static int string_join(paw_Env *P)
{
    String *sep = V_STRING(*CF_BASE(1));

    Buffer buf;
    pawL_init_buffer(P, &buf);
    paw_push_int(P, PAW_ITER_INIT);
    while (paw_list_next(P, 2)) {
        char const *chunk = paw_string(P, -1);
        paw_Int const chunklen = paw_str_rawlen(P, -1);
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
    String const *str = V_STRING(*CF_BASE(1));
    String const *prefix = V_STRING(*CF_BASE(2));
    size_t const prelen = prefix->length;
    paw_Bool const b = str->length >= prelen
        && 0 == memcmp(prefix->text, str->text, prelen);
    V_SET_BOOL(P->top.p - 1, b);
    return 1;
}

static int string_ends_with(paw_Env *P)
{
    String const *str = V_STRING(*CF_BASE(1));
    String const *suffix = V_STRING(*CF_BASE(2));
    size_t const suflen = suffix->length;
    paw_Bool b = PAW_FALSE;
    if (str->length >= suflen) {
        char const *ptr = str->text + str->length - suflen;
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

static int float_hash(paw_Env *P)
{
    // reinterpret as integer
    PAW_UNUSED(P);
    return 1;
}

static int float_to_string(paw_Env *P)
{
    Value *pv = CF_BASE(1);
    pawV_to_string(P, pv, PAW_TFLOAT, NULL);
    return 1;
}

static int string_hash(paw_Env *P)
{
    // reinterpret pointer to interned string as integer
    PAW_UNUSED(P);
    return 1;
}

static int string_parse_float(paw_Env *P)
{
    paw_Float f;
    char const *str = paw_string(P, 1);
    int const status = pawV_parse_float(P, str, &f);
    if (status != PAW_OK)
        pawR_error(P, PAW_ESYNTAX, "invalid float '%s'", str);
    V_SET_FLOAT(CF_BASE(1), f);
    return 1;
}

static int string_parse_int(paw_Env *P)
{
    char const *str = paw_string(P, 1);
    paw_Int const base = paw_int(P, 2);
    if (base > INT_MAX) {
        pawR_error(P, PAW_EOVERFLOW, "base '%I' is too large", base);
    }

    paw_Int i;
    int const rc = pawV_parse_int(P, str, CAST(int, base), &i);
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
    pawR_map_length(P, P->cf, CF_BASE(1), CF_BASE(1));
    return 1;
}

static int map_get_or(paw_Env *P)
{
    Tuple *m = V_TUPLE(*CF_BASE(1));
    Value const *key = CF_BASE(2);
    Value const *pv = pawMap_get(P, m, key);
    if (pv != NULL)
        P->top.p[-1] = *pv;
    return 1;
}

static int map_erase(paw_Env *P)
{
    Tuple *m = V_TUPLE(*CF_BASE(1));
    pawMap_remove(P, m, CF_BASE(2));
    paw_pop(P, 1); // return 'self'
    return 1;
}

char const *find_last_sep(char const *s, size_t n, size_t *pn)
{
    paw_assert(n > 0);
    char const *s0 = s;
    s += n;
    *pn = 0;

    do {
        --s;
        ++*pn;
        char const *q = PAW_FOLDER_SEPS;
        while (*q != '\0') {
            if (*q++ == *s)
                return s;
        }
    } while (s != s0);
    return NULL;
}

static void path_to_modname(char const *pathname, size_t pathlen, char *modname)
{
    size_t modlen;
    char const *begin = find_last_sep(pathname, pathlen, &modlen);
    if (begin != NULL) {
        // skip separator
        --modlen;
        ++begin;
    } else {
        begin = pathname;
        modlen = pathlen;
    }
    char const *end = strchr(begin, '.');
    if (end != NULL)
        modlen = end - begin;
    memcpy(modname, begin, modlen);
    modname[modlen] = '\0';
}

struct FileReader {
    struct LoaderState state;
    char data[512];
    File *file;
    paw_Bool err;
};

static char const *file_reader(paw_Env *P, void *ud, size_t *psize)
{
    struct FileReader *fr = ud;
    size_t const zchunk = sizeof(fr->data);
    *psize = pawO_read(P, fr->file, fr->data, zchunk);
    // TODO: don't throw errors in os.c    if (*psize != zchunk) fr->err = ferror(fr->file);
    return *psize > 0 ? fr->data : NULL;
}

struct ChunkReader {
    struct LoaderState state;
    char const *data;
    size_t size;
};

static char const *chunk_reader(paw_Env *P, void *ud, size_t *psize)
{
    PAW_UNUSED(P);
    struct ChunkReader *cr = ud;
    *psize = cr->size;
    cr->size = 0;
    return cr->data;
}

void pawL_close_loader(paw_Env *P, void *state)
{
    if (state == NULL)
        return;
    struct FileReader *fr = state;
    pawO_close(fr->file);
    pawO_free_file(P, fr->file);
}

void pawL_new_func(paw_Env *P, paw_Function func, int nup)
{
    Value *pv = pawC_push0(P);
    Native *nat = pawV_new_native(P, func, nup);
    V_SET_OBJECT(pv, nat);

    Value const *up = P->top.p - nup - 1;
    for (int i = 0; i < nup; ++i) {
        nat->up[i] = *up++;
    }
    paw_shift(P, nup);
}

static void add_prelude_func(paw_Env *P, char const *name, paw_Function func)
{
    paw_mangle_start(P);
    paw_push_string(P, name);
    paw_mangle_add_name(P);
    pawL_new_func(P, func, 0);
    paw_map_set(P, -3);
}

static void add_prelude_method(paw_Env *P, char const *self, char const *name, paw_Function func)
{
    paw_mangle_start(P);
    paw_push_string(P, self);
    paw_mangle_add_name(P);
    paw_push_string(P, name);
    paw_mangle_add_name(P);
    pawL_new_func(P, func, 0);
    paw_map_set(P, -3);
}

void pawL_push_symbols_map(paw_Env *P)
{
    pawE_push_cstr(P, CSTR_KSYMBOLS);
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
    add_prelude_func(P, "panic", base_panic);
    add_prelude_func(P, "assert", base_assert);
    add_prelude_func(P, "print", base_print);
    add_prelude_func(P, "range", base_range);

    add_prelude_method(P, "bool", "to_string", bool_to_string);
    add_prelude_method(P, "int", "to_string", int_to_string);
    add_prelude_method(P, "float", "hash", float_hash);
    add_prelude_method(P, "float", "to_string", float_to_string);

    add_prelude_method(P, "str", "hash", string_hash);
    add_prelude_method(P, "str", "parse_int", string_parse_int);
    add_prelude_method(P, "str", "parse_float", string_parse_float);
    add_prelude_method(P, "str", "split", string_split);
    add_prelude_method(P, "str", "join", string_join);
    add_prelude_method(P, "str", "find", string_find);
    add_prelude_method(P, "str", "starts_with", string_starts_with);
    add_prelude_method(P, "str", "ends_with", string_ends_with);

    add_prelude_method(P, "List", "length", list_length);
    add_prelude_method(P, "List", "get", list_get);
    add_prelude_method(P, "List", "set", list_set);
    add_prelude_method(P, "List", "push", list_push);
    add_prelude_method(P, "List", "insert", list_insert);
    add_prelude_method(P, "List", "remove", list_remove);
    add_prelude_method(P, "List", "pop", list_pop);

    add_prelude_method(P, "Map", "length", map_length);
    add_prelude_method(P, "Map", "get", map_get);
    add_prelude_method(P, "Map", "set", map_set);
    add_prelude_method(P, "Map", "get_or", map_get_or);
    add_prelude_method(P, "Map", "erase", map_erase);

    add_prelude_method(P, "Option", "unwrap", enum_unwrap);

    add_prelude_method(P, "Result", "unwrap", enum_unwrap);
    add_prelude_method(P, "Result", "unwrap_err", result_unwrap_err);

    add_prelude_method(P, "MapIterator", "next", map_iter_next);
}

static paw_Bool lib_getenv(paw_Env *P)
{
    char const *env = getenv(paw_string(P, -1));
    paw_pop(P, 1);

    if (env == NULL)
        return PAW_FALSE;
    paw_push_string(P, env);
    return PAW_TRUE;
}

static paw_Bool matches_modname(paw_Env *P, char const *modname)
{
    return strncmp(modname, paw_string(P, -1), paw_str_rawlen(P, -1)) == 0;
}

static struct FileReader *new_file_reader(paw_Env *P, char const *pathname)
{
    File *file = pawO_new_file(P);
    int const rc = pawO_open(file, pathname, "r");
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
    void l_import_io(paw_Env * P);
    void l_import_math(paw_Env * P);

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
        char const *pathname = P->modname->text;
        size_t const pathlen = P->modname->length;
        paw_assert(pathlen <= PAW_PATH_MAX); // TODO: throw an error at least

        size_t modlen;
        char const *sep = find_last_sep(pathname, pathlen, &modlen);
        if (sep == NULL)
            goto use_current_dir;
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
    if (fr == NULL)
        paw_push_zero(P, 1); // indicate failure

    return 1;
}

static void push_prelude_method(paw_Env *P, char const *self, char const *name)
{
    pawE_push_cstr(P, CSTR_KSYMBOLS);
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
            if (fr != NULL)
                return 1;

            paw_pop(P, 1);
        }
    }
    paw_push_zero(P, 1);
    return 1;
}

static int init_searchers(paw_Env *P)
{
    paw_Function const fs[] = {
        searcher_Paw, // check stdlib first
        searcher_cwd,
        searcher_env,
    };
    for (int i = 0; i < PAW_COUNTOF(fs); ++i) {
        pawL_new_func(P, fs[i], 0);
    }
    return PAW_COUNTOF(fs);
}

#define NBUILTIN_POLICIES 5

void pawL_init(paw_Env *P)
{
    MapPolicy const base_policy = {
        .key_size = 1,
        .value_size = 1,
    };
    MapPolicy const float_policy = {
        .fp = PAW_TRUE,
        .key_size = 1,
        .value_size = 1,
    };

    P->map_policies.alloc = NBUILTIN_POLICIES;
    P->map_policies.data = pawM_new_vec(P, NBUILTIN_POLICIES, MapPolicy);
    P->map_policies.data[PAW_TUNIT] = base_policy;
    P->map_policies.data[PAW_TBOOL] = base_policy;
    P->map_policies.data[PAW_TINT] = base_policy;
    P->map_policies.data[PAW_TFLOAT] = float_policy;
    P->map_policies.data[PAW_TSTR] = base_policy;
    P->map_policies.count = NBUILTIN_POLICIES;

    // create system registry objects
    pawE_push_cstr(P, CSTR_KSEARCHERS);
    paw_new_list(P, init_searchers(P), 1);
    pawE_push_cstr(P, CSTR_KMODULES);
    paw_new_map(P, 0, PAW_TSTR);
    pawE_push_cstr(P, CSTR_KSYMBOLS);
    paw_new_map(P, 0, PAW_TSTR);
    load_builtins(P);

    // create the registry itself
    paw_new_map(P, 3, PAW_TSTR);
    P->registry = P->top.p[-1];
    paw_pop(P, 1);
}

void pawL_uninit(paw_Env *P)
{
    // clear GC roots
    P->registry = (Value){0};
}

int pawL_load_file(paw_Env *P, char const *pathname)
{
    struct FileReader fr = {
        .file = pawO_new_file(P),
        .state.f = file_reader,
    };
    int const rc = pawO_open(fr.file, pathname, "r");
    if (rc == 0) {
        char modname[PAW_PATH_MAX + 1];
        size_t const pathlen = strlen(pathname);
        paw_assert(pathlen <= PAW_PATH_MAX); // TODO
        path_to_modname(pathname, pathlen, modname);
        int const status = paw_load(P, file_reader, modname, &fr);
        if (!fr.err)
            return status;
        // TODO: just return status??
    }
    paw_push_string(P, strerror(errno));
    return PAW_ESYSTEM;
}

int pawL_load_nchunk(paw_Env *P, char const *name, char const *source, size_t length)
{
    struct ChunkReader cr = {
        .state.f = chunk_reader,
        .data = source,
        .size = length,
    };
    return paw_load(P, chunk_reader, name, &cr);
}

int pawL_load_chunk(paw_Env *P, char const *name, char const *source)
{
    return pawL_load_nchunk(P, name, source, strlen(source));
}

void pawL_load_symbols(paw_Env *P)
{
    pawE_push_cstr(P, CSTR_KSYMBOLS);
    paw_map_get(P, PAW_REGISTRY_INDEX);

    paw_push_int(P, PAW_ITER_INIT);
    // at the top of the loop body the stack looks like:
    //     .. symbols paw.symbols i key value
    while (paw_map_next(P, -3)) {
        // paw.symbols[key] = value
        paw_map_set(P, -4);
        paw_pop(P, 2);
    }
    paw_pop(P, 3);
}

int pawL_register_func(paw_Env *P, char const *name, paw_Function func, int nup)
{
    // paw.symbols[mangle(name)] = func
    pawL_push_symbols_map(P);
    paw_mangle_start(P);
    paw_push_string(P, name);
    paw_mangle_add_name(P);
    paw_new_native(P, func, nup);
    paw_map_set(P, -3);
    return 0;
}

void *pawL_chunk_reader(paw_Env *P, char const *text, size_t length)
{
    struct ChunkReader *r = paw_new_foreign(P, sizeof(struct ChunkReader), 0);
    *r = (struct ChunkReader){
        .state.f = chunk_reader,
        .size = length,
        .data = text,
    };
    return r;
}

void pawL_add_extern_value(paw_Env *P, char const *modname, char const *name)
{
    paw_mangle_start(P);
    paw_push_string(P, modname);
    paw_mangle_add_module(P);
    paw_push_string(P, name);
    paw_mangle_add_name(P);
    paw_rotate(P, -2, 1);
    paw_map_set(P, -3);
}

void pawL_add_extern_func(paw_Env *P, char const *modname, char const *name, paw_Function func)
{
    paw_mangle_start(P);
    paw_push_string(P, modname);
    paw_mangle_add_module(P);
    paw_push_string(P, name);
    paw_mangle_add_name(P);
    pawL_new_func(P, func, 0);
    paw_map_set(P, -3);
}

void pawL_add_extern_method(paw_Env *P, char const *modname, char const *self, char const *name, paw_Function func)
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

static char const *file_import_reader(paw_Env *P, void *ud, size_t *psize)
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
        int const status = paw_call(P, 1); // state = f(name)
        if (status != PAW_OK)
            pawC_throw(P, status);
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
