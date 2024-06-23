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
//#define make_to_bool(suffix, T) \
//    static int base_to_bool_ ## suffix(paw_Env *P) \
//    { \
//        pawL_check_argc(P, 1); \
//        pawR_to_bool(P, T); \
//        return 1; \
//    }
//make_to_bool(s, PAW_TSTRING)
//make_to_bool(i, PAW_TINT)
//make_to_bool(f, PAW_TFLOAT)
//
//#define make_to_int(suffix, T) \
//    static int base_to_int_ ## suffix(paw_Env *P) \
//    { \
//        pawL_check_argc(P, 1); \
//        pawR_to_int(P, T); \
//        return 1; \
//    }
//make_to_int(s, PAW_TSTRING)
//make_to_int(i, PAW_TINT)
//make_to_int(f, PAW_TFLOAT)
//
//#define make_to_float(suffix, T) \
//    static int base_to_float_ ## suffix(paw_Env *P) \
//    { \
//        pawL_check_argc(P, 1); \
//        pawR_to_float(P, T); \
//        return 1; \
//    }
//make_to_float(s, PAW_TSTRING)
//make_to_float(i, PAW_TINT)
//make_to_float(f, PAW_TFLOAT)
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
//    Vector *a = v_vector(cf_base(0));
//    const paw_Int i = pawL_check_int(P, 1);
//    pawA_insert(P, a, i, cf_base(2));
//    return 0;
//}
//
//static int array_push(paw_Env *P)
//{
//    const int argc = pawL_check_varargc(P, 1, UINT8_MAX);
//    Vector *a = v_vector(cf_base(0));
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
//    const paw_Int i = argc ? v_int(cf_base(1)) : -1;
//    Vector *a = v_vector(cf_base(0));
//
//    P->top.p[-1] = *pawA_get(P, a, i); // may replace 'a'
//    pawA_pop(P, a, i); // never allocates, last line OK
//    return 1;
//}
//
//static int array_clone(paw_Env *P)
//{
//    pawL_check_argc(P, 0);
//    Vector *a = v_vector(cf_base(0));
//    Value *pv = pawC_push0(P);
//    pawA_clone(P, pv, a);
//    return 1;
//}
//
//static String *check_string(paw_Env *P, int i)
//{
//    const Value v = cf_base(i);
//    if (v_type(v) != VSTRING) {
//        pawR_error(P, PAW_ETYPE, "expected string");
//    }
//    return v_string(v);
//}

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

//static int string_split(paw_Env *P)
//{
//    pawL_check_argc(P, 1);
//    const String *sep = v_string(cf_base(1));
//    String *s = v_string(cf_base(0));
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
//    String *s = v_string(cf_base(0));
//
//    Buffer buf;
//    pawL_init_buffer(P, &buf);
//    paw_Int itr = PAW_ITER_INIT;
//    Vector *a = v_vector(seq);
//    while (pawA_iter(a, &itr)) {
//        const Value v = a->begin[itr];
//        // Add a chunk, followed by the separator if necessary.
//        const String *chunk = v_string(v);
//        pawL_add_nstring(P, &buf, chunk->text, chunk->length);
//        if (cast_size(itr + 1) < pawA_length(a)) {
//            pawL_add_nstring(P, &buf, s->text, s->length);
//        }
//    }
//    pawL_push_result(P, &buf);
//    return 1;
//}

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
    pawL_check_argc(P, 0);
    return 1;
}

static int map_get(paw_Env *P)
{
    const int argc = pawL_check_varargc(P, 1, 2);
    const Value key = cf_base(1);
    Map *m = v_map(cf_base(0));
    const Value *pv = pawH_get(P, m, key);
    if (pv) {
        P->top.p[-1] = *pv;
    } else if (argc != 2) {
        pawH_key_error(P, key, PAW_TUNIT); // TODO: key type for printing
    }
    return 1;
}

static int map_erase(paw_Env *P)
{
    pawL_check_argc(P, 1);
    Map *m = v_map(cf_base(0));
    pawH_remove(P, m, cf_base(1));
    return 0;
}

static int map_clone(paw_Env *P)
{
    pawL_check_argc(P, 0);
    Map *m = v_map(cf_base(0));
    Value *pv = pawC_push0(P);
    pawH_clone(P, pv, m);
    return 1;
}

#define L_MAX_SIZE 256

typedef struct pawL_Property {
    const char *name;
    paw_Type type;
} pawL_Property;

typedef struct pawL_Signature {
    paw_Type *params;
    paw_Type return_;
    int ngenerics;
} pawL_Signature;

typedef struct pawL_Layout {
    const char *name;
    int ngenerics;
    int nfields;
    int nmethods;
} pawL_Layout;

typedef struct pawL_GenericCtx {
    int ngenerics;
} pawL_GenericCtx;

paw_Type pawL_new_generic_type(paw_Env *P, pawL_GenericCtx *ctx);
paw_Type pawL_new_func_type(paw_Env *P, paw_Type *pars, paw_Type return_, int ngenerics);
paw_Type pawL_new_struct_type(paw_Env *P, pawL_Layout *layout);
void pawL_new_func(paw_Env *P, paw_Function func, int nup);
void pawL_new_struct(paw_Env *P, paw_Function *methods);

paw_Type pawL_instantiate_func(paw_Env *P, paw_Type base, paw_Type *types);
paw_Type pawL_instantiate_struct(paw_Env *P, paw_Type base, paw_Type *types);

// Create a global symbol and bind to it the value on top of the stack
int pawL_new_global(paw_Env *P, const char *name, paw_Type type);

paw_Type pawL_bind_method(paw_Env *P, paw_Function func, int index);

static void create_type_vars(paw_Env *P, int ngenerics, Binder *binder)
{
    if (ngenerics > L_GENERIC_MAX) {
        lib_error(P, PAW_EOVERFLOW, "too many generics");
    }
    binder->types = pawM_new_vec(P, ngenerics, Type *);
    binder->count = ngenerics;
    for (int i = 0; i < ngenerics; ++i) {
        Type *type = pawY_type_new(P, P->mod);
        type->generic.kind = TYPE_GENERIC;
        type->generic.def = 11111; // TODO: Create a global def
        binder->types[i] = type;
    }
}

static Type *resolve_type(paw_Env *P, paw_Type type, Binder *generics)
{
    if (type >= 0) {
        if (type >= P->mod->ntypes) {
            lib_error(P, PAW_ETYPE, "unrecognized type");
        }
        return P->mod->types[type];
    } else if (type < l_generic(generics->count)) {
        lib_error(P, PAW_ETYPE, "invalid generic parameter"); 
    }
    type = l_generic(type);
    return generics->types[type];
}

static Type *register_native(paw_Env *P, paw_Type *pars, paw_Type return_, int ngenerics)
{
    int n = 0;
    Type *buffer[ARGC_MAX];
    Type *r = pawY_type_new(P, P->mod);
    r->func.kind = TYPE_FUNC;
    create_type_vars(P, ngenerics, &r->func.types);

    // Validate the parameter and return types.
    for (; *pars != L_LIST_END; ++n, ++pars) {
        if (n == L_PARAM_MAX) { 
            lib_error(P, PAW_EOVERFLOW, "too many parameters"); 
        }
        buffer[n] = resolve_type(P, *pars, &r->func.types); 
    }

    r->func.params.count = n;
    r->func.params.types = pawM_new_vec(P, n, Type *);
    memcpy(r->func.params.types, buffer, cast_size(n) * sizeof(buffer[0]));
    r->func.return_ = resolve_type(P, return_, &r->func.types);
    return r;
}

struct MethodDesc {
    const char *name;
    paw_Function func;
    paw_Type *pars;
    paw_Type ret;
};

#define count_list(L, n) for (; (L) != NULL; ++(L), ++(n));

// TODO: Mangle template instance names to disambiguate between instances of the same template
int pawL_new_global(paw_Env *P, const char *name, paw_Type type)
{
    String *gname = pawS_new_str(P, name);
    const int g = pawE_new_global(P, gname, type);
    GlobalVar *var = pawE_get_global(P, g);
    var->value = P->top.p[-1];
    pawC_pop(P);
    return g;
}

paw_Type pawL_new_func_type(paw_Env *P, paw_Type *pars, paw_Type return_, int ngenerics)
{
    Type *type = register_native(P, pars, return_, ngenerics);
    return type->hdr.def;
}

void pawL_new_func(paw_Env *P, paw_Function func, int nup)
{
    Native *nat = pawV_new_native(P, func, nup); // TODO: take upvalues off top of stack
    pawC_pusho(P, cast_object(nat));
}

//paw_Type pawL_new_struct_type(paw_Env *P, pawL_Class *struct_)
//{
//    Type *type = pawY_new_type(P, P->mod);
//    type->cls.name = pawS_new_str(P, struct_->name);
//    type->cls.super = type_at(P, struct_->super);
//    type->cls.nfields = register_props(P, struct_->fields, &type->cls.fields);
//    type->cls.nmethods = register_props(P, struct_->methods, &type->cls.methods);
//
//    Value *pv = pawC_push0(P); // anchor here
//    Class *c = pawV_new_struct(P, pv);
//
//    const int g = pawE_new_global(P, type->hdr.name, type);
//    GlobalVar *var = pawE_get_global(P, g);
//    v_set_object(&var->value, c);
//    var->desc.type = type;
//    pawC_pop(P); // pop 'c'
//}
//
//paw_Type pawL_bind_method(paw_Env *P, int g, paw_Function func, int index)
//{
//    GlobalVar *var = pawE_get_global(P, g);
//    Class *struct_ = v_struct(var->value);
//    Value *pmethod = pawA_get(P, struct_->methods, index);
//    v_set_object(pmethod, func);
//}

#define reset_ctx(p) ((p)->ngenerics = 0)

void pawL_init(paw_Env *P)
{
    pawL_GenericCtx ctx;
    paw_Type type, base;
    paw_Type T, T2;

    // Builtin functions:

    // fn assert(bool)
    type = pawL_new_func_type(P, l_list(PAW_TBOOL), PAW_TUNIT, 0);
    pawL_new_func(P, base_assert, 0);
    pawL_new_global(P, "assert", type);

    // fn print(string)
    type = pawL_new_func_type(P, l_list(PAW_TSTRING), PAW_TUNIT, 0);
    pawL_new_func(P, base_print, 0);
    pawL_new_global(P, "print", type);

    // TODO: Builtin function templates
//    // fn add[T](a: T, b: T) -> T
//    reset_ctx(&ctx);
//    T = pawL_new_generic_type(P, &ctx);
//    base = pawL_new_func_type(P, l_list(T, T), T, 1);
//    type = pawL_instantiate_func(P, base, l_list(PAW_TINT));
//    pawL_new_func(P, base_add_i, 0);
//    pawL_new_global(P, "addi_", type); // TODO: name mangling

//    reset_ctx(&ctx);
//    T = pawL_new_generic_type(P, &ctx);
//    base = pawL_new_struct_type(P, &(pawL_Layout){
//                .name = "Vec",
//                .ngenerics = 1,
//            });
//    type = pawL_new_func_type(P, l_list(T), PAW_TUNIT, 0);
//    pawL_new_func(P, base_)

    // Create a map for caching loaded libraries.
    P->libs = pawH_new(P);
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
