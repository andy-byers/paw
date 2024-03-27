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
    const int narg = get_argc(P);
    if (narg != argc) {
        pawR_error(P, PAW_ERUNTIME, "expected %d arguments but found %d", argc, narg);
    }
}

int pawL_check_varargc(paw_Env *P, int min, int max)
{
    const int narg = get_argc(P);
    if (narg < min || narg > max) {
        int n;
        const char *s;
        if (narg > max) {
            n = max;
            s = "most";
        } else {
            n = min;
            s = "least;";
        }
        pawR_error(P, PAW_ERUNTIME, "expected at %s %d argument(s) but found %d",
                   s, n, narg);
    }
    return narg;
}

static void try_aux(paw_Env *P, void *arg)
{
    const int argc = *(int *)arg;
    pawC_call(P, cf_base(1), argc - 1);
}

static int base_try(paw_Env *P)
{
    int argc = pawL_check_varargc(P, 1, UINT8_MAX);
    pawL_check_type(P, 1, PAW_TFUNCTION);
    const int status = pawC_try(P, try_aux, &argc);
    paw_push_int(P, status);
    return 1;
}

static int base_require(paw_Env *P)
{
    pawL_check_argc(P, 1);
    const char *name = pawL_check_string(P, 1);
    pawL_require_lib(P, name);
    return 1;
}

static int base_bool(paw_Env *P)
{
    pawL_check_argc(P, 1);
    Value *pv = &P->top.p[-1];
    pawV_set_bool(pv, pawV_truthy(*pv));
    return 1;
}

static int base_int(paw_Env *P)
{
    pawL_check_argc(P, 1);
    pawR_to_integer(P);
    return 1;
}

static int base_float(paw_Env *P)
{
    pawL_check_argc(P, 1);
    pawR_to_float(P);
    return 1;
}

static int base_str(paw_Env *P)
{
    size_t len;
    pawL_check_argc(P, 1);
    pawR_to_string(P, &len);
    return 1;
}

static int base_chr(paw_Env *P)
{
    pawL_check_argc(P, 1);
    const paw_Int ord = pawL_check_int(P, 1);
    if (0x00 <= ord && ord <= 0xFF) {
        const uint8_t chr[] = {ord};
        paw_push_nstring(P, (const char *)chr, 1);
    } else {
        // TODO: Encode UTF-8 codepoint
        pawR_error(P, PAW_EOVERFLOW, "FIXME: Support UTF-8!");
    }
    return 1;
}

static int base_ord(paw_Env *P)
{
    pawL_check_argc(P, 1);
    const char *str = pawL_check_string(P, 1);
    const size_t len = paw_length(P, 1);
    if (!len || len > 4) {
        pawR_error(P, PAW_EVALUE, "invalid UTF-8");
    }
    // TODO: Decode UTF-8 codepoint
    paw_push_int(P, str[0]);
    return 1;
}

static int base_assert(paw_Env *P)
{
    pawL_check_argc(P, 1);
    if (!paw_boolean(P, 1)) {
        pawR_error(P, PAW_ERUNTIME, "assertion failed");
    }
    return 0;
}

static int base_print(paw_Env *P)
{
    const int argc = get_argc(P);

    Buffer print;
    pawL_init_buffer(P, &print);
    for (int i = 1; i <= argc; ++i) {
        pawC_pushv(P, cf_base(i));
        pawL_add_value(P, &print);
        if (i < argc) {
            pawL_add_char(P, &print, ' ');
        }
    }
    pawL_add_char(P, &print, '\n');
    pawO_write(stdout, print.data, print.size);
    fflush(stdout);

    pawL_discard_result(P, &print);
    return 0;
}

struct ReaderState {
    const char *data;
    size_t size;
};

static const char *reader(paw_Env *P, void *ud, size_t *size)
{
    paw_unused(P);
    struct ReaderState *ld = ud;

    // Return the whole string.
    *size = ld->size;
    ld->size = 0;
    return ld->data;
}

static int base_load(paw_Env *P)
{
    // load() takes a single parameter: a string containing source code.
    pawL_check_argc(P, 1);
    pawL_check_type(P, 1, PAW_TSTRING);

    struct ReaderState state = {
        .data = paw_string(P, 1),
        .size = paw_length(P, 1),
    };
    // Load the code and leave it in a closure object on the stack.
    const int status = paw_load(P, reader, "", &state);
    if (status != PAW_OK) {
        // Rethrow the error. The error message is already on the stack.
        pawC_throw(P, status);
    }
    return 1;
}

static int string_starts_with(paw_Env *P)
{
    pawL_check_argc(P, 1);
    const char *prefix = pawL_check_string(P, 1);
    const size_t length = paw_length(P, 1);
    const String *str = pawV_get_string(cf_base(0));
    paw_push_boolean(P, str->length >= length &&
                            0 == memcmp(prefix, str->text, length));
    return 1;
}

static int string_ends_with(paw_Env *P)
{
    pawL_check_argc(P, 1);
    const char *prefix = pawL_check_string(P, 1);
    const size_t length = paw_length(P, 1);
    const String *str = pawV_get_string(cf_base(0));
    paw_Bool result = PAW_FALSE;
    if (str->length >= length) {
        const char *ptr = str->text + str->length - length;
        result = 0 == memcmp(prefix, ptr, length);
    }
    paw_push_boolean(P, result);
    return 1;
}

static int map_erase(paw_Env *P)
{
    pawL_check_argc(P, 1);
    Map *m = pawV_get_map(cf_base(0));
    pawH_remove(P, m, cf_base(1));
    return 0;
}

static int object_clone(paw_Env *P)
{
    pawL_check_argc(P, 0);
    Value *pv = pawC_push0(P);
    Value v = cf_base(0);
    if (pawV_is_map(v)) {
        pawH_clone(P, pv, pawV_get_map(v));
    } else if (pawV_is_array(v)) {
        pawA_clone(P, pv, pawV_get_array(v));
    } else {
        paw_assert(pawV_is_string(v));
        *pv = v; // strings are internalized
    }
    return 1;
}

static int base_getattr(paw_Env *P)
{
    const int argc = pawL_check_varargc(P, 2, 3);
    const paw_Bool fallback = argc == 3;
    if (fallback) {
        paw_rotate(P, -3, 1);
    }
    if (pawR_getattr_raw(P, fallback)) {
        pawR_attr_error(P, P->top.p[-1]);
    }
    return 1;
}

static int base_setattr(paw_Env *P)
{
    pawL_check_argc(P, 3);
    pawR_setattr(P);
    return 0;
}

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
        // Create a foreign to hold the library attributes,
        ud = pawV_push_foreign(P, 0, nbound);
    } else {
        // Use the foreign object on top of the stack.
        pawL_check_type(P, -1, PAW_TUSERDATA);
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

static String *fix_name(paw_Env *P, const char *name)
{
    String *s = pawS_new_nstr(P, name, strlen(name));
    if (cast_object(s) == P->gc_all) {
        // fix if just allocated
        pawG_fix_object(P, cast_object(s));
    }
    return s;
}

// clang-format off
static const pawL_Attr kBaseLib[] = {
    {"try", base_try},
    {"require", base_require},
    {"assert", base_assert},
    {"print", base_print},
    {"load", base_load},
    {"int", base_int},
    {"bool", base_bool},
    {"str", base_str},
    {"float", base_float},
    {"ord", base_ord},
    {"chr", base_chr},
    {"getattr", base_getattr},
    {"setattr", base_setattr},
    {0}
};

static const pawL_Attr kArrayMethods[] = {
    {0}
};

static const pawL_Attr kMapMethods[] = {
    {"erase", map_erase},
    {0}
};

static const pawL_Attr kStringMethods[] = {
    {"starts_with", string_starts_with},
    {"ends_with", string_ends_with},
    {0}
};

static const pawL_Attr kObjectMethods[] = {
    {"clone", object_clone},
    {0} // end
};

static struct Builtin {
    const pawL_Attr *attr;
    int nattr;
} kBuiltin[NOBJECTS + 1] = {
#define X(i, a) [i] = {a, paw_countof(a) - 1},
    X(obj_index(VSTRING), kStringMethods)
    X(obj_index(VARRAY), kArrayMethods)
    X(obj_index(VMAP), kMapMethods)
    X(NOBJECTS, kObjectMethods)
#undef X
};
// clang-format on

static void init_object(paw_Env *P, Foreign **pfr, int i)
{
    const struct Builtin b = kBuiltin[i];
    *pfr = pawV_new_builtin(P, b.nattr);
    pawG_fix_object(P, cast_object(*pfr));
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

    // Fix builtin method names. The foreign objects containing builtin methods
    // are not traversed by the GC, since they are effectively immutable.
    for (size_t i = 0; i < paw_countof(kBuiltin); ++i) {
        const struct Builtin b = kBuiltin[i];
        for (int j = 0; j < b.nattr; ++j) {
            fix_name(P, b.attr[j].name);
        }
    }

    Foreign **pfr = P->builtin;
    for (size_t i = 0; i < NOBJECTS; ++i, ++pfr) {
        init_object(P, pfr, i);
    }

    Foreign *obj;
    // Create an object to hold common functions on builtin types.
    init_object(P, &obj, NOBJECTS);
    pawV_set_foreign(&P->object, obj);
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
