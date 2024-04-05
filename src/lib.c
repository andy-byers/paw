#include "lib.h"
#include "api.h"
#include "array.h"
#include "auxlib.h"
#include "call.h"
#include "gc.h"
#include "map.h"
#include "os.h"
#include "rt.h"
#include <errno.h>
#include <limits.h>
#include <time.h>

#define cf_base(i) P->cf->base.p[i]

void pawL_check_type(paw_Env *P, int index, int type)
{
    const int i = paw_abs_index(P, index);
    const Value v = cf_base(i);
    if (api_type(v) != type) {
        pawR_error(P, PAW_ETYPE, "expected '%s' but found '%s'",
                   api_typename(type), api_typename(api_type(v)));
    }
}

paw_Int pawL_check_int(paw_Env *P, int index)
{
    const int i = paw_abs_index(P, index);
    return pawR_check_int(P, cf_base(i));
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

//static void try_aux(paw_Env *P, void *arg)
//{
//    const int argc = *(int *)arg;
//    pawC_call(P, cf_base(1), argc - 1);
//}
//
//static int base_try(paw_Env *P)
//{
//    int argc = pawL_check_varargc(P, 1, UINT8_MAX);
//    pawL_check_type(P, 1, PAW_TFUNCTION);
//    const int status = pawC_try(P, try_aux, &argc);
//    paw_push_int(P, status);
//    return 1;
//}
//
//static int base_require(paw_Env *P)
//{
//    pawL_check_argc(P, 1);
//    const char *name = pawL_check_string(P, 1);
//    pawL_require_lib(P, name);
//    return 1;
//}
//
//static int base_bool(paw_Env *P)
//{
//    pawL_check_argc(P, 1);
//    Value *pv = &P->top.p[-1];
//    pawV_set_bool(pv, pawV_truthy(*pv));
//    return 1;
//}
//
//static int base_int(paw_Env *P)
//{
//    pawL_check_argc(P, 1);
//    pawR_to_integer(P);
//    return 1;
//}
//
//static int base_float(paw_Env *P)
//{
//    pawL_check_argc(P, 1);
//    pawR_to_float(P);
//    return 1;
//}
//
//static int base_str(paw_Env *P)
//{
//    size_t len;
//    pawL_check_argc(P, 1);
//    pawR_to_string(P, &len);
//    return 1;
//}
//
//static int base_chr(paw_Env *P)
//{
//    pawL_check_argc(P, 1);
//    const paw_Int ord = pawL_check_int(P, 1);
//    if (0x00 <= ord && ord <= 0xFF) {
//        const uint8_t chr[] = {ord};
//        paw_push_nstring(P, (const char *)chr, 1);
//    } else {
//        // TODO: Encode UTF-8 codepoint
//        pawR_error(P, PAW_EOVERFLOW, "FIXME: Support UTF-8!");
//    }
//    return 1;
//}
//
//static int base_ord(paw_Env *P)
//{
//    pawL_check_argc(P, 1);
//    const char *str = pawL_check_string(P, 1);
//    const size_t len = paw_length(P, 1);
//    if (!len || len > 4) {
//        pawR_error(P, PAW_EVALUE, "invalid UTF-8");
//    }
//    // TODO: Decode UTF-8 codepoint
//    paw_push_int(P, str[0]);
//    return 1;
//}

static int base_assert(paw_Env *P)
{
    pawL_check_argc(P, 1);
    if (!paw_boolean(P, 1)) {
        pawR_error(P, PAW_ERUNTIME, "assertion failed");
    }
    return 0;
}

//static int base_print(paw_Env *P)
//{
//    const int argc = get_argc(P);
//
//    Buffer print;
//    pawL_init_buffer(P, &print);
//    for (int i = 1; i <= argc; ++i) {
//        pawC_pushv(P, cf_base(i));
//        pawL_add_value(P, &print);
//        if (i < argc) {
//            pawL_add_char(P, &print, ' ');
//        }
//    }
//    pawL_add_char(P, &print, '\n');
//    pawO_write(stdout, print.data, print.size);
//    fflush(stdout);
//
//    pawL_discard_result(P, &print);
//    return 0;
//}
//
//struct ReaderState {
//    const char *data;
//    size_t size;
//};
//
//static const char *reader(paw_Env *P, void *ud, size_t *size)
//{
//    paw_unused(P);
//    struct ReaderState *ld = ud;
//
//    // Return the whole string.
//    *size = ld->size;
//    ld->size = 0;
//    return ld->data;
//}
//
//static int base_load(paw_Env *P)
//{
//    // load() takes a single parameter: a string containing source code.
//    pawL_check_argc(P, 1);
//    pawL_check_type(P, 1, PAW_TSTRING);
//
//    struct ReaderState state = {
//        .data = paw_string(P, 1),
//        .size = paw_length(P, 1),
//    };
//    // Load the code and leave it in a closure object on the stack.
//    const int status = paw_load(P, reader, "", &state);
//    if (status != PAW_OK) {
//        // Rethrow the error. The error message is already on the stack.
//        pawC_throw(P, status);
//    }
//    return 1;
//}
//
//static int base_getattr(paw_Env *P)
//{
//    const int argc = pawL_check_varargc(P, 2, 3);
//    const paw_Bool fallback = argc == 3;
//    if (fallback) {
//        paw_rotate(P, -3, 1);
//    }
//    if (pawR_getattr_raw(P, fallback)) {
//        pawR_attr_error(P, P->top.p[-1]);
//    }
//    return 1;
//}
//
//static int base_setattr(paw_Env *P)
//{
//    pawL_check_argc(P, 3);
//    pawR_setattr(P);
//    return 0;
//}
//
//static int array_insert(paw_Env *P)
//{
//    pawL_check_argc(P, 2);
//    Array *a = pawV_get_array(cf_base(0));
//    const paw_Int i = pawL_check_int(P, 1);
//    pawA_insert(P, a, i, cf_base(2));
//    return 0;
//}
//
//static int array_push(paw_Env *P)
//{
//    const int argc = pawL_check_varargc(P, 1, UINT8_MAX);
//    Array *a = pawV_get_array(cf_base(0));
//    for (int i = 0; i < argc; ++i) {
//        pawA_push(P, a, cf_base(i + 1));
//    }
//    return 0;
//}
//
//static int array_pop(paw_Env *P)
//{
//    const int argc = pawL_check_varargc(P, 0, 1);
//    // Argument, if present, indicates the index at which to remove an
//    // element. Default to -1: the last element.
//    const paw_Int i = argc ? pawV_get_int(cf_base(1)) : -1;
//    Array *a = pawV_get_array(cf_base(0));
//
//    P->top.p[-1] = *pawA_get(P, a, i); // may replace 'a'
//    pawA_pop(P, a, i); // never allocates, last line OK
//    return 1;
//}
//
//static int array_clone(paw_Env *P)
//{
//    pawL_check_argc(P, 0);
//    Array *a = pawV_get_array(cf_base(0));
//    Value *pv = pawC_push0(P);
//    pawA_clone(P, pv, a);
//    return 1;
//}
//
//static String *check_string(paw_Env *P, int i)
//{
//    const Value v = cf_base(i);
//    if (pawV_get_type(v) != VSTRING) {
//        pawR_error(P, PAW_ETYPE, "expected string");
//    }
//    return pawV_get_string(v);
//}
//
//static const char *find_substr(const char *str, size_t nstr, const char *sub, size_t nsub)
//{
//    if (nsub == 0) {
//        return str;
//    }
//    const char *ptr = str;
//    const char *end = str + nstr;
//    while ((ptr = strchr(ptr, sub[0]))) {
//        if (nsub <= cast_size(end - ptr) &&
//            0 == memcmp(ptr, sub, nsub)) {
//            return ptr;
//        }
//        str = ptr + nsub;
//    }
//    return NULL;
//}
//
//static int string_find(paw_Env *P)
//{
//    pawL_check_argc(P, 1);
//    const String *find = check_string(P, 1);
//    String *s = pawV_get_string(cf_base(0));
//    const char *result = find_substr(s->text, s->length, find->text, find->length);
//    if (result) {
//        pawV_set_int(P->top.p - 1, result - s->text); // index of substring
//    } else {
//        pawV_set_int(P->top.p - 1, -1); // not found
//    }
//    return 1;
//}
//
//static int string_split(paw_Env *P)
//{
//    pawL_check_argc(P, 1);
//    const String *sep = check_string(P, 1);
//    String *s = pawV_get_string(cf_base(0));
//    if (sep->length == 0) {
//        pawR_error(P, PAW_EVALUE, "empty separator");
//    }
//
//    paw_Int npart = 0;
//    const char *part;
//    size_t nstr = s->length;
//    const char *pstr = s->text;
//    while ((part = find_substr(pstr, nstr, sep->text, sep->length))) {
//        const size_t n = cast_size(part - pstr);
//        pawC_pushns(P, pstr, n);
//        part += sep->length; // skip separator
//        pstr = part;
//        nstr -= n;
//        ++npart;
//    }
//    const char *end = s->text + s->length; // add the rest
//    pawC_pushns(P, pstr, cast_size(end - pstr));
//    ++npart;
//
//    pawR_literal_array(P, npart);
//    return 1;
//}
//
//static int string_join(paw_Env *P)
//{
//    pawL_check_argc(P, 1);
//    const Value seq = cf_base(1);
//    String *s = pawV_get_string(cf_base(0));
//
//    Buffer buf;
//    pawL_init_buffer(P, &buf);
//    paw_Int itr = PAW_ITER_INIT;
//    if (pawV_is_array(seq)) {
//        Array *a = pawV_get_array(seq);
//        while (pawA_iter(a, &itr)) {
//            const Value v = a->begin[itr];
//            if (!pawV_is_string(v)) {
//                pawR_type_error(P, "join");
//            }
//            // Add a chunk, followed by the separator if necessary.
//            const String *chunk = pawV_get_string(v);
//            pawL_add_nstring(P, &buf, chunk->text, chunk->length);
//            if (cast_size(itr + 1) < pawA_length(a)) {
//                pawL_add_nstring(P, &buf, s->text, s->length);
//            }
//        }
//    } else {
//        pawR_type_error(P, "join");
//    }
//    pawL_push_result(P, &buf);
//    return 1;
//}
//
//static int string_starts_with(paw_Env *P)
//{
//    pawL_check_argc(P, 1);
//    const String *prefix = check_string(P, 1);
//    const size_t prelen = prefix->length;
//    String *s = pawV_get_string(cf_base(0));
//    const paw_Bool b = s->length >= prelen &&
//                       0 == memcmp(prefix->text, s->text, prelen);
//    pawV_set_bool(P->top.p - 1, b);
//    return 1;
//}
//
//static int string_ends_with(paw_Env *P)
//{
//    pawL_check_argc(P, 1);
//    const String *suffix = check_string(P, 1);
//    const size_t suflen = suffix->length;
//    paw_Bool b = PAW_FALSE;
//    String *s = pawV_get_string(cf_base(0));
//    if (s->length >= suflen) {
//        const char *ptr = s->text + s->length - suflen;
//        b = 0 == memcmp(suffix->text, ptr, suflen);
//    }
//    pawV_set_bool(P->top.p - 1, b);
//    return 1;
//}
//
//static int string_clone(paw_Env *P)
//{
//    // Leave the string receiver on top of the stack. The copy of the string into
//    // this function's receiver slot serves as the clone. Strings are immutable:
//    // and have copy-on-write sematics.
//    pawL_check_argc(P, 0);
//    return 1;
//}
//
//static int map_get(paw_Env *P)
//{
//    const int argc = pawL_check_varargc(P, 1, 2);
//    const Value key = cf_base(1);
//    Map *m = pawV_get_map(cf_base(0));
//    const Value *pv = pawH_get(P, m, key);
//    if (pv) {
//        P->top.p[-1] = *pv;
//    } else if (argc != 2) {
//        pawH_key_error(P, key);
//    }
//    return 1;
//}
//
//static int map_erase(paw_Env *P)
//{
//    pawL_check_argc(P, 1);
//    Map *m = pawV_get_map(cf_base(0));
//    pawH_remove(P, m, cf_base(1));
//    return 0;
//}
//
//static int map_clone(paw_Env *P)
//{
//    pawL_check_argc(P, 0);
//    Map *m = pawV_get_map(cf_base(0));
//    Value *pv = pawC_push0(P);
//    pawH_clone(P, pv, m);
//    return 1;
//}

static int count_bindings(paw_Env *P, const pawL_Attr *attr)
{
    int nbound = 0;
    for (; attr->name; ++attr, ++nbound) {
        if (nbound == INT_MAX) {
            pawR_error(P, PAW_EOVERFLOW, "too many bindings");
        }
    }
    return nbound;
}

static void load_bindings(paw_Env *P, Foreign *ud, int nup, const pawL_Attr *a, int na, paw_Bool fix)
{
    Value *pv = ud->bound;
    for (; na > 0; --na, ++a) {
        String *name = pawS_new_str(P, a->name);
        pawV_set_string(pv++, name);
        Native *nt = pawV_new_native(P, a->func, nup);
        pawV_set_native(pv++, nt);
        if (fix) { // fix builtin methods
            pawG_fix_object(P, cast_object(nt));
        }

        for (int u = 0; u < nup; ++u) {
            // '-1' accounts for library container on top
            nt->up[u] = P->top.p[nup - u - 1];
        }
    }
}

void pawL_register_lib(paw_Env *P, const char *name, int nup, const pawL_Attr *attr)
{
    const int nbound = count_bindings(P, attr);

    Foreign *ud;
    if (name) {
        // Create a foreign object to hold the library attributes,
        ud = pawV_push_foreign(P, 0, nbound);
    } else {
        // Use the foreign object on top of the stack.
        pawL_check_type(P, -1, PAW_TFOREIGN);
        ud = pawV_get_foreign(P->top.p[-1]);
        if (ud->nbound < nbound) {
            pawR_error(P, PAW_ERUNTIME, "not enough bindings");
        }
    }
    // Load the library functions.
    load_bindings(P, ud, nup, attr, nbound, PAW_FALSE);

    // Add the library to the map of registered libraries.
    if (name) {
        paw_push_value(P, -1);
        StackPtr sp = pawC_stkinc(P, 1);
        pawV_set_map(sp, P->libs);
        paw_push_string(P, name);
        paw_rotate(P, -3, -1);
        paw_set_item(P, -3);
        paw_pop(P, 1);
    }
    // Remove the upvalues, leaving the library on top.
    paw_shift(P, nup);
}

// clang-format off
static const pawL_Attr kBaseLib[] = {
    {"assert", base_assert},
//    {"try", base_try},
//    {"require", base_require},
//    {"print", base_print},
//    {"load", base_load},
//    {"int", base_int},
//    {"bool_", base_bool},
//    {"str", base_str},
//    {"float", base_float},
//    {"ord", base_ord},
//    {"chr", base_chr},
//    {"getattr", base_getattr},
//    {"setattr", base_setattr},
    {0}
};

static const pawL_Attr kArrayMethods[] = {
//    {"insert", array_insert},
//    {"push", array_push},
//    {"pop", array_pop},
//    {"clone", array_clone},
    {0}
};

static const pawL_Attr kMapMethods[] = {
//    {"get", map_get},
//    {"erase", map_erase},
//    {"clone", map_clone},
    {0}
};

static const pawL_Attr kStringMethods[] = {
//    {"find", string_find},
//    {"join", string_join},
//    {"split", string_split},
//    {"starts_with", string_starts_with},
//    {"ends_with", string_ends_with},
//    {"clone", string_clone},
    {0}
};

static struct Builtin {
    const pawL_Attr *attr;
    int nattr;
} kBuiltin[NOBJECTS + 1] = {
#define X(i, a) [i] = {a, paw_countof(a) - 1},
    X(obj_index(VSTRING), kStringMethods)
    X(obj_index(VARRAY), kArrayMethods)
    X(obj_index(VMAP), kMapMethods)
#undef X
};
// clang-format on

static void init_object(paw_Env *P, Foreign **pfr, int i)
{
    const struct Builtin b = kBuiltin[i];
    *pfr = pawV_new_builtin(P, b.nattr);
    pawG_fix_object(P, cast_object(*pfr));
    for (int i = 0; i < b.nattr; ++i) {
        String *name = pawS_new_str(P, b.attr[i].name);
        if (cast_object(name) == P->gc_all) {
            pawG_fix_object(P, cast_object(name));
        }
    }
    load_bindings(P, *pfr, 0, b.attr, b.nattr, PAW_TRUE);
}

void pawL_init(paw_Env *P)
{
    // Add the base library functions as global variables.
    pawC_stkinc(P, 1);
    pawV_set_map(&P->top.p[-1], P->globals);
    for (const pawL_Attr *a = kBaseLib; a->name; ++a) {
        paw_push_string(P, a->name);
        paw_push_native(P, a->func, 0);
        paw_set_item(P, -3);
    }
    pawC_pop(P);

    // Create a map for caching loaded libraries.
    P->libs = pawH_new(P);

    Foreign **pfr = P->builtin;
    for (size_t i = 0; i < NOBJECTS; ++i, ++pfr) {
        init_object(P, pfr, i);
    }
}

// 'pawL_register_*lib' functions defined in corresponding '*lib.c' source files
extern void pawL_require_iolib(paw_Env *P);
extern void pawL_require_mathlib(paw_Env *P);

static paw_Bool load_cached(paw_Env *P, const char *name)
{
    paw_push_string(P, name);
    const Value key = P->top.p[-1];

    Value *pvalue = pawH_get(P, P->libs, key);
    if (!pvalue) {
        return PAW_FALSE;
    }
    // replace library name
    pawC_pushv(P, *pvalue);
    paw_shift(P, 1);
    return PAW_TRUE;
}

void pawL_require_lib(paw_Env *P, const char *name)
{
    paw_assert(name);
    if (load_cached(P, name)) {
        return; // already loaded
    }
    // Automatically register base libraries. This will prevent libraries with any of
    // the base library names from being registered.
    if (0 == strcmp(name, IOLIB_NAME)) {
        pawL_require_iolib(P);
    } else if (0 == strcmp(name, MATHLIB_NAME)) {
        pawL_require_mathlib(P);
    } else {
        pawR_error(P, PAW_ENAME, "library '%s' has not been registered", name);
    }
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
