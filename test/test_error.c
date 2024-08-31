#include "auxlib.h"
#include "call.h"
#include "lib.h"
#include "opcode.h"
#include "os.h"
#include "paw.h"
#include "rt.h"
#include "test.h"
#include <limits.h>

typedef uint64_t TypeSet;

static void write_main(char *out, const char *items, const char *text)
{
#define ADD_CHUNK(o, p) \
        memcpy(o, p, strlen(p)); \
        (o) += strlen(p); \
        *(o)++ = '\n'
    ADD_CHUNK(out, items);
    ADD_CHUNK(out, "pub fn main() {\n");
    ADD_CHUNK(out, text);
    ADD_CHUNK(out, "}\n");
    *out++ = '\0';
#undef ADD_CHUNK
}

static void test_compiler_status(int expect, const char *name, const char *item, const char *text)
{
    char buffer[4096];
    write_main(buffer, item, text);

    paw_Env *P = paw_open(&(struct paw_Options){0});
    int status = pawL_load_chunk(P, name, buffer);
    check(status == expect);

    paw_close(P);
}

static void test_runtime_status(int expect, const char *name, const char *item, const char *text)
{
    char buffer[4096];
    write_main(buffer, item, text);

    paw_Env *P = paw_open(&(struct paw_Options){0});
    int status = pawL_load_chunk(P, name, buffer);
    check(status == PAW_OK);

    paw_push_string(P, "main");
    paw_mangle_name(P, NULL);

    struct paw_Item info;
    status = paw_lookup_item(P, &info);
    check(status == PAW_OK && info.global_id >= 0);
    paw_get_global(P, info.global_id);

    status = paw_call(P, 0);
    check(status == expect);

    paw_close(P);
}

static void test_name_error(void)
{
    test_compiler_status(PAW_ENAME, "use_before_def_local", "", "let x = x;");
    test_compiler_status(PAW_ENAME, "undef_variable", "", "x = 1;");
    test_compiler_status(PAW_ENAME, "undef_field", "struct A;", "let a = A.value;");
}

static const char *get_literal(int kind)
{
    switch (kind) {
        case PAW_TUNIT:
            return "()";
        case PAW_TINT:
            return "123";
        case PAW_TFLOAT:
            return "1.0";
        case PAW_TBOOL:
            return "true";
        case PAW_TSTR:
            return "'abc'";
        default:
            check(0);
            return NULL;
    }
}

static void check_unop_type_error(const char *op, paw_Type k)
{
    char name_buf[256] = {0};
    snprintf(name_buf, sizeof(name_buf), "unop_type_error('%s', %s)",
             op, get_literal(k));

    char text_buf[256] = {0};
    snprintf(text_buf, sizeof(text_buf), "let x = %s%s;",
             op, get_literal(k));

    test_compiler_status(PAW_ETYPE, name_buf, "", text_buf);
}

static void check_unification_errors(void)
{
    for (int k = PAW_TUNIT; k <= PAW_TSTR; ++k) {
        for (int k2 = PAW_TUNIT; k2 <= PAW_TSTR; ++k2) {
            if (k == k2) {
                continue;
            }
            char name_buf[256] = {0};
            snprintf(name_buf, sizeof(name_buf), "unification_error(%s, %s)",
                     get_literal(k), get_literal(k2));

            char text_buf[256] = {0};
            snprintf(text_buf, sizeof(text_buf), "let x = %s; let y = %s; x = y;",
                     get_literal(k), get_literal(k2));

            test_compiler_status(PAW_ETYPE, name_buf, "", text_buf);
        }
    }
}

static void check_binop_type_error(const char *op, paw_Type k, paw_Type k2)
{
    char name_buf[256] = {0};
    snprintf(name_buf, sizeof(name_buf), "binop_type_error('%s', %s, %s)",
             op, get_literal(k), get_literal(k2));

    char text_buf[256] = {0};
    snprintf(text_buf, sizeof(text_buf), "let x = %s %s %s;",
             get_literal(k), op, get_literal(k2));

    test_compiler_status(PAW_ETYPE, name_buf, "", text_buf);
}

static void check_binop_type_errors(const char *op, paw_Type *types)
{
    for (int k = PAW_TUNIT; k <= PAW_TSTR; ++k) {
        for (int k2 = PAW_TUNIT; k2 <= PAW_TSTR; ++k2) {
            paw_Type *pt = types;
            for (int t = *pt; t >= 0; t = *++pt) {
                if (k == t && k2 == t) {
                    goto next_round;
                }
            }
            check_binop_type_error(op, k, k2);
next_round: /* combination of types is valid, skip check */;
        }
    }
}

static void test_type_error(void)
{
    check_unification_errors();

    check_unop_type_error("#", PAW_TUNIT);
    check_unop_type_error("#", PAW_TBOOL);
    check_unop_type_error("#", PAW_TINT);
    check_unop_type_error("#", PAW_TFLOAT);
    check_unop_type_error("!", PAW_TUNIT);
    check_unop_type_error("-", PAW_TUNIT);
    check_unop_type_error("-", PAW_TBOOL);
    check_unop_type_error("-", PAW_TSTR);
    check_unop_type_error("~", PAW_TUNIT);
    check_unop_type_error("~", PAW_TBOOL);
    check_unop_type_error("~", PAW_TFLOAT);
    check_unop_type_error("~", PAW_TSTR);

#define mklist(...) \
    (paw_Type[]) { __VA_ARGS__, -1 }
#define mklist0() \
    (paw_Type[]) { -1 }
    check_binop_type_errors("+", mklist(PAW_TINT, PAW_TFLOAT, PAW_TSTR));
    check_binop_type_errors("-", mklist(PAW_TINT, PAW_TFLOAT));
    check_binop_type_errors("*", mklist(PAW_TINT, PAW_TFLOAT));
    check_binop_type_errors("%", mklist(PAW_TINT, PAW_TFLOAT));
    check_binop_type_errors("/", mklist(PAW_TINT, PAW_TFLOAT));
    check_binop_type_errors("&", mklist(PAW_TINT));
    check_binop_type_errors("|", mklist(PAW_TINT));
    check_binop_type_errors("^", mklist(PAW_TINT));
    check_binop_type_errors("<", mklist(PAW_TINT, PAW_TFLOAT, PAW_TSTR));
    check_binop_type_errors(">", mklist(PAW_TINT, PAW_TFLOAT, PAW_TSTR));
    check_binop_type_errors("<=", mklist(PAW_TINT, PAW_TFLOAT, PAW_TSTR));
    check_binop_type_errors(">=", mklist(PAW_TINT, PAW_TFLOAT, PAW_TSTR));
    check_binop_type_errors("==", mklist(PAW_TBOOL, PAW_TINT, PAW_TFLOAT, PAW_TSTR));
    check_binop_type_errors("!=", mklist(PAW_TBOOL, PAW_TINT, PAW_TFLOAT, PAW_TSTR));

    test_compiler_status(PAW_ETYPE, "call_unit_variant", "enum E {X}", "let x = E::X();");
    test_compiler_status(PAW_ETYPE, "wrong_constructor_args", "enum E {X(int)}", "let x = E::X(1.0);");
    test_compiler_status(PAW_ETYPE, "selector_on_function", "fn func() {}", "let a = func.field;");
}

static void test_syntax_error(void)
{
    test_compiler_status(PAW_ESYNTAX, "string_missing_second_surrogate", "", "let s = '\\ud801';");
    test_compiler_status(PAW_ESYNTAX, "string_missing_first_surrogate", "", "let s = '\\udc00';");
    test_compiler_status(PAW_ESYNTAX, "string_malformed_surrogate_1", "", "let s = '\\ud801\\....';");
    test_compiler_status(PAW_ESYNTAX, "string_malformed_surrogate_2", "", "let s = '\\ud801\\u....';");
    test_compiler_status(PAW_ESYNTAX, "string_invalid_surrogate_low", "", "let s = '\\ud801\\udbff';");
    test_compiler_status(PAW_ESYNTAX, "string_invalid_surrogate_high", "", "let s = '\\ud801\\ue000';");

    test_compiler_status(PAW_ESYNTAX, "misplaced_2dots", "", "let x = ..;");
    test_compiler_status(PAW_ESYNTAX, "misplaced_3dots", "", "let x = ...;");
    test_compiler_status(PAW_ESYNTAX, "misplaced_fat_arrow", "", "let x => 1;");
    test_compiler_status(PAW_ESYNTAX, "missing_end_of_block_comment", "", "/* block comment");
    test_compiler_status(PAW_ESYNTAX, "overflow_integer", "", "let d = -9223372036854775808;"); // overflows before '-' applied
    test_compiler_status(PAW_ESYNTAX, "binary_digit_range", "", "let b = 0b001201;");
    test_compiler_status(PAW_ESYNTAX, "octal_digit_range", "", "let o = 0o385273;");
    test_compiler_status(PAW_ESYNTAX, "hex_digit_range", "", "let x = 0x5A2CG3;");
    test_compiler_status(PAW_ESYNTAX, "malformed_binary", "", "let b = 0b00$101;");
    test_compiler_status(PAW_ESYNTAX, "malformed_octal", "", "let o = 0o37=273;");
    test_compiler_status(PAW_ESYNTAX, "malformed_hex", "", "let x = 0y5A2CF3;");
    test_compiler_status(PAW_ESYNTAX, "stmt_after_return", "fn f() {return; f()}", "");
    test_compiler_status(PAW_ESYNTAX, "missing_right_paren", "fn f(a: int, b: int, c: int -> int {return (a + b + c);}", "");
    test_compiler_status(PAW_ESYNTAX, "missing_left_paren", "fn fa: int, b: int, c: int) -> int {return (a + b + c);}", "");
    test_compiler_status(PAW_ESYNTAX, "missing_right_curly", "fn f(a: int, b: int, c: int) -> int {return (a + b + c);", "");
    test_compiler_status(PAW_ESYNTAX, "missing_left_curly", "fn f(a: int, b: int, c: int) -> int return (a + b + c);}", "");
    test_compiler_status(PAW_ESYNTAX, "missing_right_angle", "fn f<A, B, C() {}", "");
    test_compiler_status(PAW_ESYNTAX, "missing_left_angle", "fn fA, B, C>() {}", "");
    test_compiler_status(PAW_ESYNTAX, "missing_turbo", "struct A<T>", "let a = A<int>;");
    test_compiler_status(PAW_ESYNTAX, "partial_turbo", "struct A<T>", "let a = A:<int>;");
    test_compiler_status(PAW_ESYNTAX, "missing_left_angle_tubofish", "struct A<T>", "let a = A::int>;");
    test_compiler_status(PAW_ESYNTAX, "missing_right_angle_turbofish", "struct A<T>", "let a = A::<int;");
    test_compiler_status(PAW_ESYNTAX, "square_bracket_generics", "fn f[A, B, C]() {}", "");
    test_compiler_status(PAW_ESYNTAX, "nested_fn", "", "fn f() {}");
    test_compiler_status(PAW_ESYNTAX, "nested_struct", "", "struct S {x: int};");
    test_compiler_status(PAW_ESYNTAX, "nested_enum", "", "enum E {X};");
    test_compiler_status(PAW_ESYNTAX, "toplevel_var", "let v = 1", ";");
    test_compiler_status(PAW_ESYNTAX, "bad_float", "", "let f = -1.0-;");
    test_compiler_status(PAW_ESYNTAX, "bad_float_2", "", "let f = 1-.0-;");
    test_compiler_status(PAW_ESYNTAX, "bad_float_3", "", "let f = 1e--1;");
    test_compiler_status(PAW_ESYNTAX, "bad_float_4", "", "let f = 1e++1;");
    test_compiler_status(PAW_ESYNTAX, "bad_float_5", "", "let f = 1e-1.0;");
    test_compiler_status(PAW_ESYNTAX, "bad_float_6", "", "let f = 1e+1.0;");
    test_compiler_status(PAW_ESYNTAX, "bad_float_7", "", "let f = 1e-1e1;");
    test_compiler_status(PAW_ESYNTAX, "bad_float_8", "", "let f = 1e+1e1;");
    test_compiler_status(PAW_ESYNTAX, "bad_float_9", "", "let f = 1.0.0;");
    test_compiler_status(PAW_ESYNTAX, "float_with_base_prefix", "", "let f = 0x1.0;");

    test_compiler_status(PAW_ESYNTAX, "missing_semicolon_after_stmt", "", "let a = 1");
    test_compiler_status(PAW_ESYNTAX, "missing_semicolon_between_stmts", "", "let a = 2\nlet b = 3;");
    test_compiler_status(PAW_ESYNTAX, "semicolon_instead_of_comma", "", "let a = [1, 2; 3, 4];");
    test_compiler_status(PAW_ESYNTAX, "semicolon_after_comma", "", "let a = [5, 6,; 7, 8];");
    test_compiler_status(PAW_ESYNTAX, "binop_missing_rhs", "", "let a = 1 +");
    test_compiler_status(PAW_ESYNTAX, "binop_invalid_rhs", "", "let a = 1 + $;");
    test_compiler_status(PAW_ESYNTAX, "binop_missing_lhs", "", "let a = + 2");
    test_compiler_status(PAW_ESYNTAX, "binop_invalid_lhs", "", "let a = & + 2;");

    test_compiler_status(PAW_ETYPE, "primitive_type_is_not_a_value_1", "", "let a = int;");
    test_compiler_status(PAW_ETYPE, "primitive_type_is_not_a_value_2", "", "let a = (1, float,);");
    test_compiler_status(PAW_ETYPE, "primitive_type_is_not_a_value_3", "", "let a = ['two', str];");
    test_compiler_status(PAW_ETYPE, "generic_type_is_not_a_value", "fn f<T>() {let t = T;}", "");
    test_compiler_status(PAW_ETYPE, "function_is_not_a_type", "fn test() {}", "let a: test = test;");
    test_compiler_status(PAW_ETYPE, "variable_is_not_a_type", "", "let a = 1; let b: a = a;");
    test_compiler_status(PAW_ETYPE, "own_name_is_not_a_type", "", "let a: a = 1;");

    test_compiler_status(PAW_ENAME, "duplicate_global", "struct A; struct A;", "");
}

static void test_closure_error(void)
{
    test_compiler_status(PAW_OK, "infer_by_usage", "", "let f = |x| {}; f(1);");

    test_compiler_status(PAW_ETYPE, "call_with_wrong_type_annotation", "", "let f = |x: int| x; f(2.0);");
    test_compiler_status(PAW_ETYPE, "call_with_wrong_type_inference", "", "let f = |x| x; f(1); f(2.0);");
    test_compiler_status(PAW_ETYPE, "cannot_infer_unused_param", "", "let f = |x| {};");
}

static void test_arithmetic_error(void)
{
    test_runtime_status(PAW_ERUNTIME, "division_by_0_int", "", "let x = 1 / 0;");
    test_runtime_status(PAW_ERUNTIME, "division_by_0_float", "", "let x = 1.0 / 0.0;");
    test_runtime_status(PAW_ERUNTIME, "negative_left_shift", "", "let x = 1 << -2;");
    test_runtime_status(PAW_ERUNTIME, "negative_right_shift", "", "let x = 1 >> -2;");
}

static void test_tuple_error(void)
{
    test_compiler_status(PAW_ETYPE, "tuple_square_brackets", "", "let x = (1, 2); let y = x[0];");
    test_compiler_status(PAW_ETYPE, "tuple_named_field", "", "let x = (1, 2); let y = x.first;");
    test_compiler_status(PAW_ETYPE, "tuple_index_out_of_range", "", "let x = (1, 2); let y = x.2;");
}

static void test_struct_error(void)
{
    test_compiler_status(PAW_ESYNTAX, "struct_unit_with_braces_on_def", "struct A {}", "let a = A;");
    test_compiler_status(PAW_ESYNTAX, "struct_unit_with_braces_on_init", "struct A;", "let a = A{};");
    test_compiler_status(PAW_ESYNTAX, "struct_unit_without_semicolon", "struct A", "");
    test_compiler_status(PAW_ESYNTAX, "struct_missing_braces", "struct A {a: int}", "let a = A;");
    test_compiler_status(PAW_ENAME, "struct_missing_only_field", "struct A {a: int}", "let a = A{};");
    test_compiler_status(PAW_ENAME, "struct_missing_field", "struct A {a: int, b: float}", "let a = A{a: 1};");
    test_compiler_status(PAW_ENAME, "struct_extra_field", "struct A {a: int}", "let a = A{a: 1, b: 2};");
    test_compiler_status(PAW_ENAME, "struct_duplicate_field", "struct A {a: int}", "let a = A{a: 1, a: 1};");
    test_compiler_status(PAW_ENAME, "struct_wrong_field", "struct A {a: int}", "let a = A{b: 2};");
    test_compiler_status(PAW_ETYPE, "struct_access_by_index", "struct S{x: int}", "let x = S{x: 1}; let y = x.0;");
    test_compiler_status(PAW_ETYPE, "struct_not_enough_types", "struct S<A, B, C>;", "let x = S::<int, float>;");
    test_compiler_status(PAW_ETYPE, "struct_too_many_types", "struct S<A, B>;", "let x = S::<int, float, bool>;");
}

static void test_enum_error(void)
{
    test_compiler_status(PAW_ESYNTAX, "enum_unit_with_braces_on_def", "enum A {}", "let a = A;");
    test_compiler_status(PAW_ETYPE, "enum_unit_with_braces_on_init", "enum A;", 
                                   "let a = A{}; // looks like struct literal");
    test_compiler_status(PAW_ESYNTAX, "enum_unit_without_semicolon", "enum A", "");
    test_compiler_status(PAW_ESYNTAX, "enum_missing_variant", "enum A {X}", "let a = A;");
    test_compiler_status(PAW_ENAME, "enum_duplicate_variant", "enum A {X, X}", "");
    test_compiler_status(PAW_ENAME, "enum_nonexistent_variant", "enum A {X}", "let a = A::Y;");
    test_compiler_status(PAW_ETYPE, "enum_missing_only_field", "enum A {X(int)}", "let a = A::X;");
    test_compiler_status(PAW_ESYNTAX, "enum_missing_field", "enum A {X(int, float)}", "let a = A::X(42);");
    test_compiler_status(PAW_ESYNTAX, "enum_extra_field", "enum A {X(int)}", "let a = A::X(42, true);");
    test_compiler_status(PAW_ETYPE, "enum_wrong_field_type", "enum A {X(int)}", "let a = A::X(1.0);");
    test_compiler_status(PAW_ETYPE, "enum_requires_pattern_matching", "enum E{X(int)}", "let x = E::X(1); let y = x.0;");
}

static void test_list_error(void)
{
    test_compiler_status(PAW_ETYPE, "list_cannot_infer", "", "let a = [];");
    test_compiler_status(PAW_ETYPE, "list_cannot_infer_binop", "", "let a = [] + [];");
    test_compiler_status(PAW_ETYPE, "list_use_before_inference", "", "let a = []; let b = #a;");
    test_compiler_status(PAW_ETYPE, "list_incompatible_types", "", "let a = [1]; a = [2.0];");
    test_compiler_status(PAW_ETYPE, "list_incompatible_types_2", "", "let a = []; if true {a = [0];} else {a = [true];}");
    test_compiler_status(PAW_ETYPE, "list_mixed_types", "", "let a = [1, 2, 3, 4, '5'];");
    test_compiler_status(PAW_ETYPE, "list_mixed_nesting", "", "let a = [[[1]], [[2]], [3]];");
}

static void test_map_error(void)
{
    test_compiler_status(PAW_ETYPE, "map_cannot_infer", "", "let a = [:];");
    test_compiler_status(PAW_ETYPE, "map_use_before_inference", "", "let a = [:]; let b = #a;");
    test_compiler_status(PAW_ETYPE, "map_incompatible_types", "", "let a = [1: 2]; a = [3: 4.0];");
    test_compiler_status(PAW_ETYPE, "map_incompatible_types_2", "", "let a = [:]; if true {a = [0: 0];} else {a = [1: true];}");
    test_compiler_status(PAW_ETYPE, "map_mixed_types", "", "let a = [1: 2, 3: 4, 5: '6'];");
    test_compiler_status(PAW_ETYPE, "map_mixed_nesting", "", "let a = [1: [1: 1], 2: [2: 2], 3: [3: [3: 3]]];");
    test_compiler_status(PAW_ETYPE, "map_nonhashable_literal_key", "", "let map = [[1]: 1];");
    test_compiler_status(PAW_ETYPE, "map_nonhashable_type_key", "", "let map: [[int]: int] = [:];");
    test_compiler_status(PAW_ETYPE, "map_slice", "", "let map = [:]; let val = map[0:10];");
}

static int next_conflicting_int(paw_Env *P)
{
    paw_unused(P);
    // return the pointer, caller reinterprets as an integer
    return 1;
}

static void test_gc_conflict(void)
{
    const char source[] = 
        "pub fn conflicting_int<T>(t: T) -> int;\n"
        "pub fn main() {\n"
        "    let N = 500;\n"
        // create a bunch of dynamically-allocated objects
        "    let objects = [];\n"
        "    for i = 0, N {_list_push(objects, [i, i + 1, i + 2]);}\n"
        // fill a list with integers that conflict with the object addresses
        "    let conflicts = [];\n"
        "    for i = 0, N {_list_push(conflicts, conflicting_int(objects[i]));}\n"
        // use a lot of memory to cause garbage collections
        "    let memory = [];\n"
        "    for i = 0, N {_list_push(memory, [[i], [i + 1], [i + 2]]);}\n"
        "}\n";

    paw_Env *P = paw_open(&(struct paw_Options){0});
    pawL_register_func(P, "conflicting_int", next_conflicting_int, 0);

    int status = pawL_load_chunk(P, "gc_conflict", source);
    check(status == PAW_OK);

    paw_push_string(P, "main");
    paw_mangle_name(P, NULL);

    struct paw_Item info;
    status = paw_lookup_item(P, &info);
    check(status == PAW_OK && info.global_id >= 0);
    paw_get_global(P, info.global_id);

    status = paw_call(P, 0);
    check(status == PAW_OK);

    paw_close(P);
}

int main(void)
{
    test_gc_conflict();
    test_enum_error();
    test_name_error();
    test_syntax_error();
    test_type_error();
    test_closure_error();
    test_arithmetic_error();
    test_tuple_error();
    test_struct_error();
    test_list_error();
    test_map_error();
}
