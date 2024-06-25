#include "lib.h"
#include "api.h"
#include "auxlib.h"
#include "call.h"
#include "gc_aux.h"
#include "map.h"
#include "mem.h"
#include "os.h"
#include "rt.h"
#include "type.h"
#include "vector.h"
#include <errno.h>
#include <limits.h>
#include <time.h>

#define cf_base(i) P->cf->base.p[i]

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

// static void try_aux(paw_Env *P, void *arg)
//{
//     const int argc = *(int *)arg;
//     pawC_call(P, cf_base(1), argc - 1);
// }
//
// static int base_try(paw_Env *P)
//{
//     int argc = pawL_check_varargc(P, 1, UINT8_MAX);
//     pawL_check_type(P, 1, PAW_TFUNCTION);
//     const int status = pawC_try(P, try_aux, &argc);
//     paw_push_int(P, status);
//     return 1;
// }
//
// static int base_require(paw_Env *P)
//{
//     pawL_check_argc(P, 1);
//     const char *name = pawL_check_string(P, 1);
//     pawL_require_lib(P, name);
//     return 1;
// }
//
// #def ine  make_to_bool(suffix, T) \
//    static int base_to_bool_ ## suffix(paw_Env *P) \
//    { \
//        pawL_check_argc(P, 1); \
//        pawR_to_bool(P, T); \
//        return 1; \
//    }
// make_to_bool(s, PAW_TSTRING)
// make_to_bool(i, PAW_TINT)
// make_to_bool(f, PAW_TFLOAT)
//
// #def ine  make_to_int(suffix, T) \
//    static int base_to_int_ ## suffix(paw_Env *P) \
//    { \
//        pawL_check_argc(P, 1); \
//        pawR_to_int(P, T); \
//        return 1; \
//    }
// make_to_int(s, PAW_TSTRING)
// make_to_int(i, PAW_TINT)
// make_to_int(f, PAW_TFLOAT)
//
// #def ine  make_to_float(suffix, T) \
//    static int base_to_float_ ## suffix(paw_Env *P) \
//    { \
//        pawL_check_argc(P, 1); \
//        pawR_to_float(P, T); \
//        return 1; \
//    }
// make_to_float(s, PAW_TSTRING)
// make_to_float(i, PAW_TINT)
// make_to_float(f, PAW_TFLOAT)
//
// static int base_chr(paw_Env *P)
//{
//     pawL_check_argc(P, 1);
//     const paw_Int ord = pawL_check_int(P, 1);
//     if (0x00 <= ord && ord <= 0xFF) {
//         const uint8_t chr[] = {ord};
//         paw_push_nstring(P, (const char *)chr, 1);
//     } else {
//         // TODO: Encode UTF-8 codepoint
//         pawR_error(P, PAW_EOVERFLOW, "FIXME: Support UTF-8!");
//     }
//     return 1;
// }
//
// static int base_ord(paw_Env *P)
//{
//     pawL_check_argc(P, 1);
//     const char *str = pawL_check_string(P, 1);
//     const size_t len = paw_length(P, 1);
//     if (!len || len > 4) {
//         pawR_error(P, PAW_EVALUE, "invalid UTF-8");
//     }
//     // TODO: Decode UTF-8 codepoint
//     paw_push_int(P, str[0]);
//     return 1;
// }

static int base_assert(paw_Env *P)
{
    if (v_false(cf_base(1))) {
        pawR_error(P, PAW_ERUNTIME, "assertion failed");
    }
    return 0;
}

static int base_print(paw_Env *P)
{
    const String *s = v_string(P->top.p[-1]);
    pawO_write(stdout, s->text, s->length);
    fflush(stdout);
    return 0;
}

// struct ReaderState {
//     const char *data;
//     size_t size;
// };
//
// static const char *reader(paw_Env *P, void *ud, size_t *size)
//{
//     paw_unused(P);
//     struct ReaderState *ld = ud;
//
//     // Return the whole string.
//     *size = ld->size;
//     ld->size = 0;
//     return ld->data;
// }
//
// static int base_load(paw_Env *P)
//{
//     // load() takes a single parameter: a string containing source code.
//     pawL_check_argc(P, 1);
//     pawL_check_type(P, 1, PAW_TSTRING);
//
//     struct ReaderState state = {
//         .data = paw_string(P, 1),
//         .size = paw_length(P, 1),
//     };
//     // Load the code and leave it in a closure object on the stack.
//     const int status = paw_load(P, reader, "", &state);
//     if (status != PAW_OK) {
//         // Rethrow the error. The error message is already on the stack.
//         pawC_throw(P, status);
//     }
//     return 1;
// }
//
// static int base_getattr(paw_Env *P)
//{
//     const int argc = pawL_check_varargc(P, 2, 3);
//     const paw_Bool fallback = argc == 3;
//     if (fallback) {
//         paw_rotate(P, -3, 1);
//     }
//     if (pawR_getattr_raw(P, fallback)) {
//         pawR_attr_error(P, P->top.p[-1]);
//     }
//     return 1;
// }
//
// static int base_setattr(paw_Env *P)
//{
//     pawL_check_argc(P, 3);
//     pawR_setattr(P);
//     return 0;
// }

// TODO: Remove '/*TODO*/+1' with regex, these should be methods, which take the
//       context as the implicit first parameter
static int vector_insert(paw_Env *P)
{
    Vector *vec = v_vector(cf_base(0 /*TODO*/ + 1));
    const paw_Int i = paw_int(P, 1 /*TODO*/ + 1);
    pawV_vec_insert(P, vec, i, cf_base(2 /*TODO*/ + 1));
    return 0;
}

static int vector_push(paw_Env *P)
{
    Vector *vec = v_vector(cf_base(0 /*TODO*/ + 1));
    pawV_vec_push(P, vec, cf_base(1 /*TODO*/ + 1));
    return 0;
}

static int vector_pop(paw_Env *P)
{
    Vector *vec = v_vector(cf_base(0 /*TODO*/ + 1));
    const paw_Int length = paw_cast_int(pawA_length(vec));
    if (length == 0) {
        pawR_error(P, PAW_EVALUE, "pop from empty Vector");
    }
    P->top.p[-1] = *pawA_get(P, vec, length - 1);
    pawA_pop(P, vec, length - 1);
    return 1;
}

static int vector_clone(paw_Env *P)
{
    Vector *a = v_vector(cf_base(0 /*TODO*/ + 1));
    Value *pv = pawC_push0(P);
    pawA_clone(P, pv, a);
    return 1;
}

// static String *check_string(paw_Env *P, int i)
//{
//     const Value v = cf_base(i);
//     if (v_type(v) != VSTRING) {
//         pawR_error(P, PAW_ETYPE, "expected string");
//     }
//     return v_string(v);
// }

static const char *find_substr(const char *str, size_t nstr, const char *sub, size_t nsub)
{
    if (nsub == 0) {
        return str;
    }
    const char *ptr = str;
    const char *end = str + nstr;
    while ((ptr = strchr(ptr, sub[0]))) {
        if (nsub <= cast_size(end - ptr) &&
            0 == memcmp(ptr, sub, nsub)) {
            return ptr;
        }
        str = ptr + nsub;
    }
    return NULL;
}

static int string_find(paw_Env *P)
{
    pawL_check_argc(P, 1);
    const String *s = v_string(cf_base(0));
    const String *find = v_string(cf_base(1));
    const char *result = find_substr(s->text, s->length, find->text, find->length);
    if (result) { // index of substring
        v_set_int(P->top.p - 1, result - s->text);
    } else { // not found
        v_set_int(P->top.p - 1, -1);
    }
    return 1;
}

// static int string_split(paw_Env *P)
//{
//     pawL_check_argc(P, 1);
//     const String *sep = v_string(cf_base(1));
//     String *s = v_string(cf_base(0));
//     if (sep->length == 0) {
//         pawR_error(P, PAW_EVALUE, "empty separator");
//     }
//
//     paw_Int npart = 0;
//     const char *part;
//     size_t nstr = s->length;
//     const char *pstr = s->text;
//     while ((part = find_substr(pstr, nstr, sep->text, sep->length))) {
//         const size_t n = cast_size(part - pstr);
//         pawC_pushns(P, pstr, n);
//         part += sep->length; // skip separator
//         pstr = part;
//         nstr -= n;
//         ++npart;
//     }
//     const char *end = s->text + s->length; // add the rest
//     pawC_pushns(P, pstr, cast_size(end - pstr));
//     ++npart;
//
//     pawR_literal_array(P, npart);
//     return 1;
// }
//
// static int string_join(paw_Env *P)
//{
//     pawL_check_argc(P, 1);
//     const Value seq = cf_base(1);
//     String *s = v_string(cf_base(0));
//
//     Buffer buf;
//     pawL_init_buffer(P, &buf);
//     paw_Int itr = PAW_ITER_INIT;
//     Vector *a = v_vector(seq);
//     while (pawA_iter(a, &itr)) {
//         const Value v = a->begin[itr];
//         // Add a chunk, followed by the separator if necessary.
//         const String *chunk = v_string(v);
//         pawL_add_nstring(P, &buf, chunk->text, chunk->length);
//         if (cast_size(itr + 1) < pawA_length(a)) {
//             pawL_add_nstring(P, &buf, s->text, s->length);
//         }
//     }
//     pawL_push_result(P, &buf);
//     return 1;
// }

static int string_starts_with(paw_Env *P)
{
    pawL_check_argc(P, 1);
    String *s = v_string(cf_base(0));
    const String *prefix = v_string(cf_base(1));
    const size_t prelen = prefix->length;
    const paw_Bool b = s->length >= prelen &&
                       0 == memcmp(prefix->text, s->text, prelen);
    v_set_bool(P->top.p - 1, b);
    return 1;
}

static int string_ends_with(paw_Env *P)
{
    pawL_check_argc(P, 1);
    String *s = v_string(cf_base(0));
    const String *suffix = v_string(cf_base(1));
    const size_t suflen = suffix->length;
    paw_Bool b = PAW_FALSE;
    if (s->length >= suflen) {
        const char *ptr = s->text + s->length - suflen;
        b = 0 == memcmp(suffix->text, ptr, suflen);
    }
    v_set_bool(P->top.p - 1, b);
    return 1;
}

static int string_clone(paw_Env *P)
{
    // Leave the string receiver on top of the stack. The copy of the string into
    // this function's receiver slot serves as the clone. Strings are immutable:
    // and have copy-on-write sematics.
    paw_unused(P);
    return 1;
}

static int map_get(paw_Env *P)
{
    const Value key = cf_base(1 /*TODO*/ + 1);
    Map *m = v_map(cf_base(0 /*TODO*/ + 1));
    const Value *pv = pawH_get(P, m, key);
    if (pv != NULL) {
        // replace default value
        P->top.p[-1] = *pv;
    }
    return 1;
}

static int map_erase(paw_Env *P)
{
    Map *m = v_map(cf_base(0 /*TODO*/ + 1));
    pawH_remove(P, m, cf_base(1 /*TODO*/ + 1));
    return 0;
}

static int map_clone(paw_Env *P)
{
    Map *m = v_map(cf_base(0 /*TODO*/ + 1));
    Value *pv = pawC_push0(P);
    pawH_clone(P, pv, m);
    return 1;
}

void pawL_new_func(paw_Env *P, paw_Function func, int nup)
{
    Native *nat = pawV_new_native(P, func, nup); // TODO: take upvalues off top of stack
    pawC_pusho(P, cast_object(nat));
}

static void add_builtin_func(paw_Env *P, const char *name, paw_Function func)
{
    paw_push_value(P, -1);
    paw_push_string(P, name);
    pawL_new_func(P, func, 0);
    pawR_setitem(P, PAW_TMAP);
}

void pawL_init(paw_Env *P)
{
    Value *pv = pawC_push0(P);
    P->builtin = pawH_new(P);
    P->libs = pawH_new(P);
    v_set_object(pv, P->builtin);

    // Builtin functions:
    add_builtin_func(P, "assert", base_assert); // fn assert(bool)
    add_builtin_func(P, "print", base_print); // fn print(string)

    add_builtin_func(P, "v_push", vector_push);
    add_builtin_func(P, "v_pop", vector_pop);
    add_builtin_func(P, "v_insert", vector_insert);
    add_builtin_func(P, "v_clone", vector_clone);

    add_builtin_func(P, "m_get", map_get);
    add_builtin_func(P, "m_erase", map_erase);
    add_builtin_func(P, "m_clone", map_clone);

    pawC_pop(P);
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
    //    paw_assert(name);
    //    if (load_cached(P, name)) {
    //        return; // already loaded
    //    }
    //    // Automatically register base libraries. This will prevent libraries with any of
    //    // the base library names from being registered.
    //    if (0 == strcmp(name, IOLIB_NAME)) {
    //        pawL_require_iolib(P);
    //    } else if (0 == strcmp(name, MATHLIB_NAME)) {
    //        pawL_require_mathlib(P);
    //    } else {
    //        pawR_error(P, PAW_ENAME, "library '%s' has not been registered", name);
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
