#include "lib.h"
#include "api.h"
#include "array.h"
#include "auxlib.h"
#include "call.h"
#include "gc.h"
#include "map.h"
#include "mem.h"
#include "os.h"
#include "rt.h"
#include "type.h"
#include <errno.h>
#include <limits.h>
#include <time.h>

#define cf_base(i) P->cf->base.p[i]

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
//    v_set_bool(pv, pawV_truthy(*pv));
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
    if (v_false(cf_base(1))) {
        pawR_error(P, PAW_ERUNTIME, "assertion failed");
    }
    return 0;
}

static int base_print(paw_Env *P)
{
    String *s = v_string(P->top.p[-1]);
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
//    Array *a = v_array(cf_base(0));
//    const paw_Int i = pawL_check_int(P, 1);
//    pawA_insert(P, a, i, cf_base(2));
//    return 0;
//}
//
//static int array_push(paw_Env *P)
//{
//    const int argc = pawL_check_varargc(P, 1, UINT8_MAX);
//    Array *a = v_array(cf_base(0));
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
//    Array *a = v_array(cf_base(0));
//
//    P->top.p[-1] = *pawA_get(P, a, i); // may replace 'a'
//    pawA_pop(P, a, i); // never allocates, last line OK
//    return 1;
//}
//
//static int array_clone(paw_Env *P)
//{
//    pawL_check_argc(P, 0);
//    Array *a = v_array(cf_base(0));
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

static int string_split(paw_Env *P)
{
    pawL_check_argc(P, 1);
    const String *sep = v_string(cf_base(1));
    String *s = v_string(cf_base(0));
    if (sep->length == 0) {
        pawR_error(P, PAW_EVALUE, "empty separator");
    }

    paw_Int npart = 0;
    const char *part;
    size_t nstr = s->length;
    const char *pstr = s->text;
    while ((part = find_substr(pstr, nstr, sep->text, sep->length))) {
        const size_t n = cast_size(part - pstr);
        pawC_pushns(P, pstr, n);
        part += sep->length; // skip separator
        pstr = part;
        nstr -= n;
        ++npart;
    }
    const char *end = s->text + s->length; // add the rest
    pawC_pushns(P, pstr, cast_size(end - pstr));
    ++npart;

    pawR_literal_array(P, npart);
    return 1;
}

static int string_join(paw_Env *P)
{
    pawL_check_argc(P, 1);
    const Value seq = cf_base(1);
    String *s = v_string(cf_base(0));

    Buffer buf;
    pawL_init_buffer(P, &buf);
    paw_Int itr = PAW_ITER_INIT;
    Array *a = v_array(seq);
    while (pawA_iter(a, &itr)) {
        const Value v = a->begin[itr];
        // Add a chunk, followed by the separator if necessary.
        const String *chunk = v_string(v);
        pawL_add_nstring(P, &buf, chunk->text, chunk->length);
        if (cast_size(itr + 1) < pawA_length(a)) {
            pawL_add_nstring(P, &buf, s->text, s->length);
        }
    }
    pawL_push_result(P, &buf);
    return 1;
}

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

//static int map_get(paw_Env *P)
//{
//    const int argc = pawL_check_varargc(P, 1, 2);
//    const Value key = cf_base(1);
//    Map *m = v_map(cf_base(0));
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
//    Map *m = v_map(cf_base(0));
//    pawH_remove(P, m, cf_base(1));
//    return 0;
//}
//
//static int map_clone(paw_Env *P)
//{
//    pawL_check_argc(P, 0);
//    Map *m = v_map(cf_base(0));
//    Value *pv = pawC_push0(P);
//    pawH_clone(P, pv, m);
//    return 1;
//}

static int count_types(paw_Env *P, paw_Type *ts)
{
    int nts = 0;
    for (paw_Type *t = ts; *t != PAW_NULL; ++t) {
        if (nts == ARGC_MAX) {
            pawC_throw(P, 123); // TODO
        }
        ++nts;
    }
    return nts;
}

static TypeTag register_native(paw_Env *P, paw_Type *argt, paw_Type ret)
{
    const int narg = count_types(P, argt);
    TypeTag *tags = pawM_new_vec(P, narg, Type *);
    for (int i = 0; i < narg; ++i) {
        tags[i] = e_tag(P, argt[i]);
    }

    // register function signature
    return pawY_register_function(P, tags, narg, e_tag(P, ret));
}

struct MethodDesc {
    const char *name;
    paw_Function func;
    paw_Type *args;
    paw_Type ret;
};

#define count_list(L, pn) for (; (L) != NULL; ++(L), ++*(pn))

#define push_builtin_class(a, b, c, d) push_builtin_class_aux(a, b, c, d, paw_countof(d))
static void push_builtin_class_aux(paw_Env *P, paw_Type base, const char *name, struct MethodDesc *mds, int nattrs)
{
    // allocate class object
    Value *pv = pawC_push0(P);
    Class *cls = pawV_new_class(P);
    v_set_object(pv, cls);

    // register method types and attach to class object
    Attribute *attrs = pawM_new_vec(P, nattrs, Attribute);
    for (int i = 0; i < nattrs; ++i) {
        struct MethodDesc *md = &mds[i];
        TypeTag tag = register_native(P, md->args, md->ret);
        String *str = pawS_new_str(P, md->name);
        Native *nat = pawV_new_native(P, str, md->func);
        attrs[i].name = str;
        attrs[i].attr = tag;

        Value key, value;
        v_set_object(&key, str);
        v_set_object(&value, nat);
        pawH_insert(P, cls->methods, key, value);
    }

    // register class type
    String *str = pawS_new_str(P, name);
    TypeTag tag = pawY_register_class(P, str, attrs, nattrs);

    // set global variable
    const int g = pawE_new_global(P, str, tag);
    GlobalVar *var = pawE_get_global(P, g);
    v_set_object(&var->value, cls);
    var->desc.type = tag;

    P->builtin[base] = v_class(P->top.p[-1]);
    pawC_pop(P);
}

Attribute attach_method(paw_Env *P, Value *pv, struct MethodDesc *m)
{
    TypeTag tag = register_native(P, m->args, m->ret);
    String *str = pawS_new_str(P, m->name);
    Native *nat = pawV_new_native(P, str, m->func);
    v_set_object(pv, nat);
    return (Attribute){
        .name = str,
        .attr = tag,
    };
}

void pawL_bind_method(paw_Env *P, int index, const char *name, paw_Function func, paw_Type *argt, paw_Type ret)
{
    Foreign *fr = v_foreign(P->top.p[-1]);
    attach_method(P, &fr->attrs[index], &(struct MethodDesc){
                .name = name,
                .func = func,
                .args = argt,
                .ret = ret,
            });
}

void pawL_register_function(paw_Env *P, const char *name, paw_Function func, paw_Type *argt, paw_Type ret)
{
    TypeTag tag = register_native(P, argt, ret);
    String *str = pawS_new_str(P, name);
    Native *nat = pawV_new_native(P, str, func);
    const int g = pawE_new_global(P, str, tag);
    GlobalVar *var = pawE_get_global(P, g);
    v_set_object(&var->value, nat);
    var->desc.type = tag;
}

void pawL_init(paw_Env *P)
{
    pawL_register_function(P, "assert", base_assert, t_list_1(PAW_TBOOL), PAW_NULL);
    pawL_register_function(P, "print", base_print, t_list_1(PAW_TSTRING), PAW_NULL);
//    pawL_register_function(P, "try", base_try, t_list_1(PAW_TFUNCTION), PAW_TINT);
//    pawL_register_function(P, "require", base_require, t_list_1(PAW_TSTRING), PAW_TMODULE);
//    pawL_register_function(P, "load", base_load, t_list_1(PAW_TSTRING), PAW_TFUNCTION);
//    pawL_register_function(P, "ord", base_ord, t_list_1(PAW_TSTRING), PAW_TINT);
//    pawL_register_function(P, "chr", base_chr, t_list_1(PAW_TINT), PAW_TSTRING);

//#define PAW_TSELF 0
//    struct MethodDesc kBigIntDesc[] = {
//        {"__eq", pawB_eq_bi, t_list_2(PAW_TSELF, PAW_TSELF), PAW_TBOOL},
//        {"__eq", pawB_eq_i, t_list_2(PAW_TSELF, PAW_TINT), PAW_TBOOL},
//        {"__eq", pawB_eq_f, t_list_2(PAW_TSELF, PAW_TFLOAT), PAW_TBOOL},
//        {"__ne", pawB_ne_bi, t_list_2(PAW_TSELF, PAW_TSELF), PAW_TBOOL},
//        {"__ne", pawB_ne_i, t_list_2(PAW_TSELF, PAW_TINT), PAW_TBOOL},
//        {"__ne", pawB_ne_f, t_list_2(PAW_TSELF, PAW_TFLOAT), PAW_TBOOL},
//        {"__lt", pawB_lt_bi, t_list_2(PAW_TSELF, PAW_TSELF), PAW_TBOOL},
//        {"__lt", pawB_lt_i, t_list_2(PAW_TSELF, PAW_TINT), PAW_TBOOL},
//        {"__lt", pawB_lt_f, t_list_2(PAW_TSELF, PAW_TFLOAT), PAW_TBOOL},
//        {"__le", pawB_le_bi, t_list_2(PAW_TSELF, PAW_TSELF), PAW_TBOOL},
//        {"__le", pawB_le_i, t_list_2(PAW_TSELF, PAW_TINT), PAW_TBOOL},
//        {"__le", pawB_le_f, t_list_2(PAW_TSELF, PAW_TFLOAT), PAW_TBOOL},
//        {"__gt", pawB_gt_bi, t_list_2(PAW_TSELF, PAW_TSELF), PAW_TBOOL},
//        {"__gt", pawB_gt_i, t_list_2(PAW_TSELF, PAW_TINT), PAW_TBOOL},
//        {"__gt", pawB_gt_f, t_list_2(PAW_TSELF, PAW_TFLOAT), PAW_TBOOL},
//        {"__ge", pawB_ge_bi, t_list_2(PAW_TSELF, PAW_TSELF), PAW_TBOOL},
//        {"__ge", pawB_ge_i, t_list_2(PAW_TSELF, PAW_TINT), PAW_TBOOL},
//        {"__ge", pawB_ge_f, t_list_2(PAW_TSELF, PAW_TFLOAT), PAW_TBOOL},
//        {"__add", pawB_add_bi, t_list_2(PAW_TSELF, PAW_TSELF), PAW_TSELF},
//        {"__add", pawB_add_i, t_list_2(PAW_TSELF, PAW_TINT), PAW_TSELF},
//        {"__add", pawB_add_f, t_list_2(PAW_TSELF, PAW_TFLOAT), PAW_TFLOAT},
//        {"__sub", pawB_sub_bi, t_list_2(PAW_TSELF, PAW_TSELF), PAW_TSELF},
//        {"__sub", pawB_sub_i, t_list_2(PAW_TSELF, PAW_TINT), PAW_TSELF},
//        {"__sub", pawB_sub_f, t_list_2(PAW_TSELF, PAW_TFLOAT), PAW_TFLOAT},
//        {"__mul", pawB_mul_bi, t_list_2(PAW_TSELF, PAW_TSELF), PAW_TSELF},
//        {"__mul", pawB_mul_i, t_list_2(PAW_TSELF, PAW_TINT), PAW_TSELF},
//        {"__mul", pawB_mul_f, t_list_2(PAW_TSELF, PAW_TFLOAT), PAW_TFLOAT},
//        {"__div", pawB_div_bi, t_list_2(PAW_TSELF, PAW_TSELF), PAW_TSELF},
//        {"__div", pawB_div_i, t_list_2(PAW_TSELF, PAW_TINT), PAW_TSELF},
//        {"__div", pawB_div_f, t_list_2(PAW_TSELF, PAW_TFLOAT), PAW_TFLOAT},
//        {"__mod", pawB_mod_bi, t_list_2(PAW_TSELF, PAW_TSELF), PAW_TSELF},
//        {"__mod", pawB_mod_i, t_list_2(PAW_TSELF, PAW_TINT), PAW_TSELF},
//        {"__mod", pawB_mod_f, t_list_2(PAW_TSELF, PAW_TFLOAT), PAW_TFLOAT},
//        {"__bxor", pawB_bxor_bi, t_list_2(PAW_TSELF, PAW_TSELF), PAW_TSELF},
//        {"__bxor", pawB_bxor_i, t_list_2(PAW_TSELF, PAW_TINT), PAW_TSELF},
//        {"__band", pawB_band_bi, t_list_2(PAW_TSELF, PAW_TSELF), PAW_TSELF},
//        {"__band", pawB_band_i, t_list_2(PAW_TSELF, PAW_TINT), PAW_TSELF},
//        {"__bor", pawB_bor_bi, t_list_2(PAW_TSELF, PAW_TSELF), PAW_TSELF},
//        {"__bor", pawB_bor_i, t_list_2(PAW_TSELF, PAW_TINT), PAW_TSELF},
//        {"__shl", pawB_shl_bi, t_list_2(PAW_TSELF, PAW_TSELF), PAW_TSELF},
//        {"__shl", pawB_shl_i, t_list_2(PAW_TSELF, PAW_TINT), PAW_TSELF},
//        {"__shr", pawB_shr_bi, t_list_2(PAW_TSELF, PAW_TSELF), PAW_TSELF},
//        {"__shr", pawB_shr_i, t_list_2(PAW_TSELF, PAW_TINT), PAW_TSELF},
//        {"__radd", pawB_radd_bi, t_list_2(PAW_TSELF, PAW_TSELF), PAW_TSELF},
//        {"__radd", pawB_radd_i, t_list_2(PAW_TSELF, PAW_TINT), PAW_TSELF},
//        {"__radd", pawB_radd_f, t_list_2(PAW_TSELF, PAW_TFLOAT), PAW_TFLOAT},
//        {"__rsub", pawB_rsub_bi, t_list_2(PAW_TSELF, PAW_TSELF), PAW_TSELF},
//        {"__rsub", pawB_rsub_i, t_list_2(PAW_TSELF, PAW_TINT), PAW_TSELF},
//        {"__rsub", pawB_rsub_f, t_list_2(PAW_TSELF, PAW_TFLOAT), PAW_TFLOAT},
//        {"__rmul", pawB_rmul_bi, t_list_2(PAW_TSELF, PAW_TSELF), PAW_TSELF},
//        {"__rmul", pawB_rmul_i, t_list_2(PAW_TSELF, PAW_TINT), PAW_TSELF},
//        {"__rmul", pawB_rmul_f, t_list_2(PAW_TSELF, PAW_TFLOAT), PAW_TFLOAT},
//        {"__rdiv", pawB_rdiv_bi, t_list_2(PAW_TSELF, PAW_TSELF), PAW_TSELF},
//        {"__rdiv", pawB_rdiv_i, t_list_2(PAW_TSELF, PAW_TINT), PAW_TSELF},
//        {"__rdiv", pawB_rdiv_f, t_list_2(PAW_TSELF, PAW_TFLOAT), PAW_TFLOAT},
//        {"__rmod", pawB_rmod_bi, t_list_2(PAW_TSELF, PAW_TSELF), PAW_TSELF},
//        {"__rmod", pawB_rmod_i, t_list_2(PAW_TSELF, PAW_TINT), PAW_TSELF},
//        {"__rmod", pawB_rmod_f, t_list_2(PAW_TSELF, PAW_TFLOAT), PAW_TFLOAT},
//        {"__rbxor", pawB_rbxor_bi, t_list_2(PAW_TSELF, PAW_TSELF), PAW_TSELF},
//        {"__rbxor", pawB_rbxor_i, t_list_2(PAW_TSELF, PAW_TINT), PAW_TSELF},
//        {"__rband", pawB_rband_bi, t_list_2(PAW_TSELF, PAW_TSELF), PAW_TSELF},
//        {"__rband", pawB_rband_i, t_list_2(PAW_TSELF, PAW_TINT), PAW_TSELF},
//        {"__rbor", pawB_rbor_bi, t_list_2(PAW_TSELF, PAW_TSELF), PAW_TSELF},
//        {"__rbor", pawB_rbor_i, t_list_2(PAW_TSELF, PAW_TINT), PAW_TSELF},
//        {"__rshl", pawB_rshl_bi, t_list_2(PAW_TSELF, PAW_TSELF), PAW_TSELF},
//        {"__rshl", pawB_rshl_i, t_list_2(PAW_TSELF, PAW_TINT), PAW_TSELF},
//        {"__rshr", pawB_rshr_bi, t_list_2(PAW_TSELF, PAW_TSELF), PAW_TSELF},
//        {"__rshr", pawB_rshr_i, t_list_2(PAW_TSELF, PAW_TINT), PAW_TSELF},
//    };
//    push_builtin_class(P, "string", kStringDesc);

    TypeTag string_array_tag = pawY_register_array(P, e_tag(P, PAW_TSTRING));
    paw_Type string_array = t_type(string_array_tag);

    struct MethodDesc kStringDesc[] = {
        {"starts_with", string_starts_with, t_list_1(PAW_TSTRING), PAW_TBOOL},
        {"ends_with", string_ends_with, t_list_1(PAW_TSTRING), PAW_TBOOL},
        {"split", string_split, t_list_1(PAW_TSTRING), string_array},
        {"join", string_join, t_list_1(string_array), PAW_TSTRING},
    };
    push_builtin_class(P, PAW_TSTRING, "string", kStringDesc);

//    struct MethodDesc kObjectDesc[] = {
//        {"clone", object_clone, t_list_0(), PAW_TSELF},
//    };
//    push_builtin_class(P, "object", kObjectDesc);

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
