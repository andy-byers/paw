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

static void write_main(char *out, char const *items, char const *text)
{
#define ADD_CHUNK(o, p)      \
    memcpy(o, p, strlen(p)); \
    (o) += strlen(p);        \
    *(o)++ = '\n'
    ADD_CHUNK(out, items);
    ADD_CHUNK(out, "pub fn main() {\n");
    ADD_CHUNK(out, text);
    ADD_CHUNK(out, "}\n");
    *out++ = '\0';
#undef ADD_CHUNK
}

static void check_status(paw_Env *P, int have, int want)
{
    check(have == want);
    if (have != PAW_OK) {
        fprintf(stderr, "message: %s\n", paw_string(P, -1));
        paw_pop(P, 1);
    }
}

static void test_compiler_status(int expect, char const *name, char const *item, char const *text)
{
    char buffer[4096];
    write_main(buffer, item, text);

    paw_Env *P = paw_open(&(struct paw_Options){0});
    int status = pawL_load_chunk(P, name, buffer);
    check_status(P, status, expect);

    paw_close(P);
}

static void test_runtime_status(int expect, char const *name, char const *item, char const *text)
{
    char buffer[4096];
    write_main(buffer, item, text);

    paw_Env *P = paw_open(&(struct paw_Options){0});
    int status = pawL_load_chunk(P, name, buffer);
    check_status(P, status, PAW_OK);

    paw_mangle_start(P);
    paw_push_string(P, "main");
    paw_mangle_add_name(P);

    struct paw_Item info;
    status = paw_lookup_item(P, -1, &info);
    check_status(P, status, PAW_OK);
    check(info.global_id >= 0);
    paw_get_global(P, info.global_id);

    status = paw_call(P, 0);
    check_status(P, status, expect);

    paw_close(P);
}

static void test_name_error(void)
{
    test_compiler_status(PAW_ENAME, "use_before_def_local", "", "let x = x;");
    test_compiler_status(PAW_ENAME, "undef_variable", "", "x = 1;");
    test_compiler_status(PAW_ENAME, "undef_field", "struct A;", "let a = A.value;");
}

static char const *get_literal(int kind)
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

static void check_unop_error(int expect, char const *op, paw_Type k)
{
    char name_buf[256] = {0};
    snprintf(name_buf, sizeof(name_buf), "unop_type_error('%s', %s)",
        op, get_literal(k));

    char text_buf[256] = {0};
    snprintf(text_buf, sizeof(text_buf), "let x = %s%s;",
        op, get_literal(k));

    test_compiler_status(expect, name_buf, "", text_buf);
}

static void check_unop_type_error(char const *op, paw_Type k)
{
    check_unop_error(PAW_ETYPE, op, k);
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

static void check_binop_type_error(char const *op, paw_Type k, paw_Type k2)
{
    char name_buf[256] = {0};
    snprintf(name_buf, sizeof(name_buf), "binop_type_error('%s', %s, %s)",
        op, get_literal(k), get_literal(k2));

    char text_buf[256] = {0};
    snprintf(text_buf, sizeof(text_buf), "let x = %s %s %s;",
        get_literal(k), op, get_literal(k2));

    test_compiler_status(PAW_ETYPE, name_buf, "", text_buf);
}

static void check_binop_type_errors(char const *op, paw_Type *types)
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
    check_unop_type_error("~", PAW_TUNIT);
    check_unop_type_error("~", PAW_TBOOL);
    check_unop_type_error("~", PAW_TFLOAT);
    check_unop_type_error("~", PAW_TSTR);
    check_unop_type_error("-", PAW_TUNIT);
    check_unop_type_error("-", PAW_TBOOL);
    check_unop_type_error("-", PAW_TSTR);

#define MAKE_LIST(...) \
    (paw_Type[]){__VA_ARGS__, -1}
    check_binop_type_errors("+", MAKE_LIST(PAW_TINT, PAW_TFLOAT, PAW_TSTR));
    check_binop_type_errors("-", MAKE_LIST(PAW_TINT, PAW_TFLOAT));
    check_binop_type_errors("*", MAKE_LIST(PAW_TINT, PAW_TFLOAT));
    check_binop_type_errors("%", MAKE_LIST(PAW_TINT, PAW_TFLOAT));
    check_binop_type_errors("/", MAKE_LIST(PAW_TINT, PAW_TFLOAT));
    check_binop_type_errors("&", MAKE_LIST(PAW_TINT));
    check_binop_type_errors("|", MAKE_LIST(PAW_TINT));
    check_binop_type_errors("^", MAKE_LIST(PAW_TINT));
    check_binop_type_errors("<", MAKE_LIST(PAW_TINT, PAW_TFLOAT, PAW_TSTR));
    check_binop_type_errors(">", MAKE_LIST(PAW_TINT, PAW_TFLOAT, PAW_TSTR));
    check_binop_type_errors("<=", MAKE_LIST(PAW_TINT, PAW_TFLOAT, PAW_TSTR));
    check_binop_type_errors(">=", MAKE_LIST(PAW_TINT, PAW_TFLOAT, PAW_TSTR));
    check_binop_type_errors("==", MAKE_LIST(PAW_TBOOL, PAW_TINT, PAW_TFLOAT, PAW_TSTR));
    check_binop_type_errors("!=", MAKE_LIST(PAW_TBOOL, PAW_TINT, PAW_TFLOAT, PAW_TSTR));

    test_compiler_status(PAW_ETYPE, "call_unit_variant", "enum E {X}", "let x = E::X();");
    test_compiler_status(PAW_ETYPE, "wrong_constructor_args", "enum E {X(int)}", "let x = E::X(1.0);");
    test_compiler_status(PAW_ETYPE, "selector_on_function", "fn func() {}", "let a = func.field;");
    test_compiler_status(PAW_ETYPE, "selector_on_module", "use io;", "let s = io.abc;");
    test_compiler_status(PAW_ETYPE, "extraneous_method_access",
        "struct S {pub fn f() {}}", "S::f::f(); ");
    test_compiler_status(PAW_ETYPE, "extraneous_variant_access",
        "enum E {A}", "let e = E::A::A; ");

    test_compiler_status(PAW_ETYPE, "missing_return_type", "pub fn f() {123}", "");
    test_compiler_status(PAW_ETYPE, "missing_return_value", "pub fn f() -> int {}", "");
    test_compiler_status(PAW_ETYPE, "non_exhaustive_branch",
        "pub fn f(x: bool) -> int {if x {} else {123}}", "");
    test_compiler_status(PAW_ETYPE, "non_exhaustive_return",
        "pub fn f(x: bool) -> int {if x {return 123;}}", "");
    test_compiler_status(PAW_ETYPE, "non_unit_guard", "pub fn f(x: bool) {if x {123}}", "");
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
    test_compiler_status(PAW_ESYNTAX, "int_digit_sep_before_bin_digits", "", "let x = 0b_01;");
    test_compiler_status(PAW_ESYNTAX, "int_digit_sep_before_oct_digits", "", "let x = 0o_23;");
    test_compiler_status(PAW_ESYNTAX, "int_digit_sep_before_hex_digits", "", "let x = 0x_45;");
    test_compiler_status(PAW_ESYNTAX, "int_digit_sep_before_b", "", "let x = 0_b01;");
    test_compiler_status(PAW_ESYNTAX, "int_digit_sep_before_o", "", "let x = 0_o23;");
    test_compiler_status(PAW_ESYNTAX, "int_digit_sep_before_x", "", "let x = 0_x45;");
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
    test_compiler_status(PAW_ESYNTAX, "nested_struct", "", "struct S {pub x: int};");
    test_compiler_status(PAW_ESYNTAX, "nested_enum", "", "enum E {X};");
    test_compiler_status(PAW_ESYNTAX, "toplevel_var", "let v = 1", ";");
    test_compiler_status(PAW_ESYNTAX, "bad_float", "", "let f = -1.0-;");
    test_compiler_status(PAW_ESYNTAX, "bad_float_2", "", "let f = 1-.0-;");
    test_compiler_status(PAW_ESYNTAX, "bad_float_3", "", "let f = 1e--1;");
    test_compiler_status(PAW_ESYNTAX, "bad_float_4", "", "let f = 1e++1;");
    // NOTE: type error is becuase the follwing is parsed as a tuple selector on a
    //       float (accessing element 0 on "1e-1")
    test_compiler_status(PAW_ETYPE, "bad_float_5", "", "let f = 1e-1.0;");
    test_compiler_status(PAW_ETYPE, "bad_float_6", "", "let f = 1e+1.0;");
    test_compiler_status(PAW_ESYNTAX, "bad_float_7", "", "let f = 1e-1e1;");
    test_compiler_status(PAW_ESYNTAX, "bad_float_8", "", "let f = 1e+1e1;");
    test_compiler_status(PAW_ETYPE, "bad_float_9", "", "let f = 1.0.0;");
    // NOTE: name error is because the following is parsed as a field access on an
    //       integer (accessing field "_0" on "1")
    test_compiler_status(PAW_ENAME, "float_digit_sep_after_dot", "", "let f = 1._0;");
    test_compiler_status(PAW_ESYNTAX, "float_digit_sep_after_e", "", "let f = 1e_0;");
    test_compiler_status(PAW_ESYNTAX, "float_digit_sep_after_-", "", "let f = 1e-_0;");
    test_compiler_status(PAW_ESYNTAX, "float_digit_sep_after_+", "", "let f = 1e+_0;");
    test_compiler_status(PAW_ETYPE, "float_with_base_prefix", "", "let f = 0x1.0;");

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
    test_compiler_status(PAW_ENAME, "own_name_is_not_a_type", "", "let a: a = 1;");

    test_compiler_status(PAW_ENAME, "duplicate_global", "struct A; struct A;", "");
    test_compiler_status(PAW_ESYNTAX, "return_outside_function", "return;", "");
    test_compiler_status(PAW_ESYNTAX, "break_outside_loop", "", "break;");
    test_compiler_status(PAW_ESYNTAX, "continue_outside_loop", "", "continue;");
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
    test_compiler_status(PAW_EVALUE, "constant_division_by_0_int", "", "let x = 1 / 0;");
    test_compiler_status(PAW_EVALUE, "constant_division_by_0_float", "", "let x = 1.0 / 0.0;");
    test_compiler_status(PAW_EVALUE, "constant_negative_left_shift", "", "let x = 1 << -2;");
    test_compiler_status(PAW_EVALUE, "constant_negative_right_shift", "", "let x = 1 >> -2;");

    test_runtime_status(PAW_ERUNTIME, "division_by_0_int", "fn f(x: int) -> int {x / 0}", "f(1);");
    test_runtime_status(PAW_ERUNTIME, "division_by_0_float", "fn f(x: float) -> float {x / 0.0}", "f(1.0);");
    test_runtime_status(PAW_ERUNTIME, "negative_left_shift", "fn f(x: int) -> int {x << -2}", "f(1);");
    test_runtime_status(PAW_ERUNTIME, "negative_right_shift", "fn f(x: int) -> int {x >> -2}", "f(1);");
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
    test_compiler_status(PAW_ESYNTAX, "struct_missing_braces", "struct A {pub a: int}", "let a = A;");
    test_compiler_status(PAW_ENAME, "struct_missing_only_field", "struct A {pub a: int}", "let a = A{};");
    test_compiler_status(PAW_ENAME, "struct_missing_field", "struct A {pub a: int, pub b: float}", "let a = A{a: 1};");
    test_compiler_status(PAW_ENAME, "struct_extra_field", "struct A {pub a: int}", "let a = A{a: 1, b: 2};");
    test_compiler_status(PAW_ENAME, "struct_duplicate_field", "struct A {pub a: int}", "let a = A{a: 1, a: 1};");
    test_compiler_status(PAW_ENAME, "struct_field_conflicts_with_method", "struct A {pub a: int, fn a() {}}", "");
    test_compiler_status(PAW_ENAME, "struct_wrong_field", "struct A {pub a: int}", "let a = A{b: 2};");
    test_compiler_status(PAW_ETYPE, "struct_access_by_index", "struct S{pub x: int}", "let x = S{x: 1}; let y = x.0;");
    test_compiler_status(PAW_ETYPE, "struct_not_enough_types", "struct S<A, B, C>;", "let x = S::<int, float>;");
    test_compiler_status(PAW_ETYPE, "struct_too_many_types", "struct S<A, B>;", "let x = S::<int, float, bool>;");

    test_compiler_status(PAW_ENAME, "struct_select_private_field",
        "struct S {pub a: int, b: int, pub fn new() -> S {return S{a: 1, b: 2};}}",
        "let x = S::new(); let a = x.a; let b = x.b;");
    test_compiler_status(PAW_ENAME, "struct_literal_private_field", "struct S {pub a: int, b: int}", "let x = S{a: 1, b: 2};");
    test_compiler_status(PAW_ENAME, "struct_call_private_method", "struct S {fn private(self) {}}", "let x = S; x.private();");

    test_compiler_status(PAW_ETYPE, "struct_not_a_method", "struct S {pub fn f(s: Self) {}}", "let x = S; x.f();");
    test_compiler_status(PAW_ETYPE, "struct_invalid_self", "struct S {pub fn f(self: int) {}}", "");
    test_compiler_status(PAW_ETYPE, "struct_invalid_self_poly", "struct S<A, B> {fn f(self: S<B, A>) {}}", "");
}

static void test_enum_error(void)
{
    test_compiler_status(PAW_ESYNTAX, "enum_without_variants", "enum A {pub fn f() {}};", "");
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
    test_compiler_status(PAW_ETYPE, "list_cyclic_type", "", "let x = []; x = [x];");
    test_compiler_status(PAW_ETYPE, "list_nested_cyclic_type", "", "let x = []; x = [[x]];");
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
    test_compiler_status(PAW_ETYPE, "map_cyclic_type", "", "let x = [:]; x = ['cyclic': x];");
    test_compiler_status(PAW_ETYPE, "map_nested_cyclic_type", "", "let x = [:]; x = ['cyclic': ['nested': x]];");
    test_compiler_status(PAW_ETYPE, "map_cannot_infer", "", "let a = [:];");
    test_compiler_status(PAW_ETYPE, "map_use_before_inference", "", "let a = [:]; let b = #a;");
    test_compiler_status(PAW_ETYPE, "map_incompatible_types", "", "let a = [1: 2]; a = [3: 4.0];");
    test_compiler_status(PAW_ETYPE, "map_incompatible_types_2", "", "let a = [:]; if true {a = [0: 0];} else {a = [1: true];}");
    test_compiler_status(PAW_ETYPE, "map_mixed_types", "", "let a = [1: 2, 3: 4, 5: '6'];");
    test_compiler_status(PAW_ETYPE, "map_mixed_nesting", "", "let a = [1: [1: 1], 2: [2: 2], 3: [3: [3: 3]]];");
    // TODO    test_compiler_status(PAW_ETYPE, "map_unhashable_literal_key", "", "let map = [[1]: 1];");
    // TODO    test_compiler_status(PAW_ETYPE, "map_unhashable_type_key", "", "let map: [[int]: int] = [:];");
    test_compiler_status(PAW_ETYPE, "map_slice", "", "let map = [:]; let val = map[0:10];");
}

static void test_import_error(void)
{
    test_compiler_status(PAW_ENAME, "missing_import", "use import_not_found;", "");
    test_compiler_status(PAW_ENAME, "missing_import_item", "use io;", "let t = io::NotFound;");
}

static int run_main(paw_Env *P, int nargs)
{
    paw_mangle_start(P);
    paw_push_string(P, "main");
    paw_mangle_add_name(P);

    struct paw_Item info;
    int const status = paw_lookup_item(P, -1, &info);
    check_status(P, status, PAW_OK);
    check(info.global_id >= 0);
    paw_get_global(P, info.global_id);

    return paw_call(P, nargs);
}

static int next_conflicting_int(paw_Env *P)
{
    PAW_UNUSED(P);
    // return the pointer, caller reinterprets as an integer
    return 1;
}

static void test_gc_conflict(void)
{
    char const source[] =
        "#[extern] pub fn conflicting_int<T>(t: T) -> int;\n"
        "pub fn main() {\n"
        "    let N = 500;\n"
        // create a bunch of dynamically-allocated objects
        "    let objects = [];\n"
        "    for i in range(0, N, 1) {objects.push([i, i + 1, i + 2]);}\n"
        // fill a list with integers that conflict with the object addresses
        "    let conflicts = [];\n"
        "    for i in range(0, N, 1) {conflicts.push(conflicting_int(objects[i]));}\n"
        // use a lot of memory to cause garbage collections
        "    let memory = [];\n"
        "    for i in range(0, N, 1) {memory.push([[i], [i + 1], [i + 2]]);}\n"
        "}\n";

    paw_Env *P = paw_open(&(struct paw_Options){0});
    pawL_register_func(P, "conflicting_int", next_conflicting_int, 0);

    int status = pawL_load_chunk(P, "gc_conflict", source);
    check_status(P, status, PAW_OK);

    status = run_main(P, 0);
    check_status(P, status, PAW_OK);

    paw_close(P);
}

static void test_invalid_case(char const *name, int expect, char const *item, char const *target, char const *pat)
{
    char const fmt[] = "match %s {\n"
                       "    %s => {},\n"
                       "}\n";
    char buffer[sizeof(fmt) + 1024];
    snprintf(buffer, sizeof(buffer), fmt, target, pat);
    test_compiler_status(expect, name, item, buffer);
}

static void test_variant_match_error(void)
{
    char const *enumeration =
        "enum Choice {\n"
        "    First,\n"
        "    Second(Choice),\n"
        "}\n";

    test_compiler_status(PAW_ETYPE, "match_int_non_exhaustive", enumeration,
        "match 123 {\n"
        "    123 => {},\n"
        "}\n");
    test_compiler_status(PAW_ETYPE, "match_variant_non_exhaustive", enumeration,
        "match Choice::First {\n"
        "    Choice::First => {},\n"
        "}\n");

    test_compiler_status(PAW_ETYPE, "match_variant_non_exhaustive_2", enumeration,
        "match Choice::First {\n"
        "    Choice::First => {},\n"
        "    Choice::Second(Choice::First) => {},"
        "}\n");

    test_compiler_status(PAW_ETYPE, "match_variant_non_exhaustive_3", enumeration,
        "match Choice::First {\n"
        "    Choice::First => {},\n"
        "    Choice::Second(Choice::First) => {},"
        "    Choice::Second(Choice::Second(Choice::First)) => {},"
        "}\n");

    // sanity check: exhaustive versions
    test_compiler_status(PAW_OK, "sanity_check_match_wildcard", enumeration,
        "match Choice::First {\n"
        "    _ => {},\n"
        "}\n");
    test_compiler_status(PAW_OK, "sanity_check_match_variant_exhaustive", enumeration,
        "match Choice::First {\n"
        "    Choice::First => {},\n"
        "    Choice::Second(_) => {},\n"
        "}\n");
    test_compiler_status(PAW_OK, "sanity_check_match_variant_exhaustive_2", enumeration,
        "match Choice::First {\n"
        "    Choice::First => {},\n"
        "    Choice::Second(Choice::First) => {},"
        "    Choice::Second(Choice::Second(_)) => {},"
        "}\n");
    test_compiler_status(PAW_OK, "sanity_check_match_variant_exhaustive_3", enumeration,
        "match Choice::First {\n"
        "    Choice::First => {},\n"
        "    Choice::Second(Choice::First) => {},"
        "    Choice::Second(Choice::Second(Choice::First)) => {},"
        "    Choice::Second(Choice::Second(Choice::Second(_))) => {},"
        "}\n");

    test_invalid_case("duplicate_binding", PAW_ENAME, "",
        "(0, 0)", "(x, x)");
    test_invalid_case("duplicate_binding_nested", PAW_ENAME, "",
        "(((0,),), 0)", "(((x,),), x)");
    test_invalid_case("or_binding_missing", PAW_ENAME, "",
        "(0, 0)", "(x, 2) | (2, 3)");
    test_invalid_case("or_binding_unrecognized", PAW_ENAME, "",
        "(0, 0)", "(1, 2) | (x, 3)");
    test_invalid_case("or_binding_unrecognized_int", PAW_ENAME, "",
        "0", "0 | x");
    test_invalid_case("or_binding_missing_int", PAW_ENAME, "",
        "0", "x | 0");
    // 'x' has a different type in each alternative
    test_invalid_case("or_binding_type_mismatch", PAW_ETYPE, "",
        "(0, '')", "(x, 'b') | (1, x)");
}

static void test_match_error(void)
{
    test_variant_match_error();
}

static void test_uninit_local(void)
{
    test_compiler_status(PAW_ETYPE, "uninit_var", "", "let x; x;"); // type of "x" cannot be inferred
    test_compiler_status(PAW_EVALUE, "uninit_int", "", "let x: int; x;");
    test_compiler_status(PAW_EVALUE, "uninit_if_without_else", "", "let x; if true {x = 1;} x;");
    test_compiler_status(PAW_EVALUE, "uninit_ifelse", "", "let x; if true {x = 1;} else {} x;");
    test_compiler_status(PAW_EVALUE, "uninit_ifelse_chain", "", "let x; if true {x = 1;} else if true {} else {x = 3;} x;");
    test_compiler_status(PAW_EVALUE, "uninit_ifelse_return", "", "let x; if true {return;} else if true {x = 2;} else {} x;");
    test_compiler_status(PAW_EVALUE, "uninit_match", "",
        "let x;\n"
        "match 123 {\n"
        "    123 => x = 1,\n"
        "    _ => {},\n"
        "}\n"
        "x;");
    test_compiler_status(PAW_EVALUE, "uninit_match_nested", "",
        "let x;\n"
        "match 123 {\n"
        "    1 => x = 1,\n"
        "    2 => x = 2,\n"
        "    3 => {\n"
        "        if true {\n"
        "            if true { x = 3; }\n"
        "        } else {\n"
        "            x = 4;\n"
        "        }\n"
        "    },\n"
        "    _ => x = 5,\n"
        "}\n"
        "x;");
}

static void test_trait_error(void)
{
#define TRAIT             \
    "pub trait Trait {\n" \
    "    fn f(self);\n"   \
    "}\n"

    test_compiler_status(PAW_ENAME, "trait_missing_method",
        TRAIT "struct S: Trait {v: int}", "");
    test_compiler_status(PAW_ETYPE, "trait_wrong_type",
        TRAIT "struct S: Trait {pub fn f(self) -> int {123}}", "");
    test_compiler_status(PAW_ETYPE, "trait_mismatched_visibility",
        TRAIT "struct S: Trait {fn f(self) {}}", "");
    test_compiler_status(PAW_ETYPE, "generic_missing_bound",
        TRAIT "struct S: Trait {pub fn f(self) {}}\n"
              "pub fn call_f<T>(t: T) {t.f();}",
        "let x = S; call_f(x);");
    test_compiler_status(PAW_ETYPE, "trait_generic_mismatch",
        TRAIT "struct S<T>: Trait<T> {pub fn f(self) {}}\n", "");

#define POLY_TRAIT           \
    "pub trait Trait<T> {\n" \
    "    fn f(self) -> T;\n" \
    "}\n"
#define POLY_STRUCT               \
    "struct S<T>: Trait<T> {\n"   \
    "    pub v: T,\n"             \
    "    pub fn f(self) -> T {\n" \
    "        self.v\n"            \
    "    }\n"                     \
    "}\n"
#define POLY_FUNCTION(g, rest)                         \
    "pub fn call_f<T: Trait<" g ">" rest ">(t: T) {\n" \
    "    t.f();\n"                                     \
    "}"

    test_compiler_status(PAW_ETYPE, "trait_not_implemented",
        POLY_TRAIT "struct S;" POLY_FUNCTION("int", ),
        "let x = S; call_f(x);");
    test_compiler_status(PAW_ETYPE, "trait_generic_mismatch",
        POLY_TRAIT POLY_STRUCT POLY_FUNCTION("int", ),
        "let x = S{v: true}; call_f(x);");
    test_compiler_status(PAW_ETYPE, "trait_type_as_trait",
        "struct Type; struct S: Type;", "");
    test_compiler_status(PAW_ENAME, "trait_missing_function_bound",
        "struct S: Trait;", "");
    test_compiler_status(PAW_ENAME, "trait_missing_function_bound",
        "fn f<T: Trait>(t: T) {}", "");
    test_compiler_status(PAW_ENAME, "trait_missing_function_bound",
        "struct S: Trait;", "");
    test_compiler_status(PAW_ENAME, "trait_missing_generic_in_bounds",
        POLY_TRAIT POLY_STRUCT POLY_FUNCTION("X", ), "");
    test_compiler_status(PAW_ENAME, "trait_missing_generic_in_bounds",
        POLY_TRAIT POLY_STRUCT POLY_FUNCTION("X", ), "");
    test_compiler_status(PAW_ETYPE, "trait_cannot_infer_generic",
        POLY_TRAIT POLY_STRUCT "fn call_f<T: Trait>(t: T) {t.f();}",
        "let x = S{v: 123}; call_f(x);");
}

static void test_underscore(void)
{
    test_compiler_status(PAW_ESYNTAX, "underscore_as_generic", "fn f<_>() {}", "");
    test_compiler_status(PAW_ESYNTAX, "underscore_as_adt_name", "struct _;", "");
    test_compiler_status(PAW_ESYNTAX, "underscore_as_type_name", "type _ = int", "");
    test_compiler_status(PAW_ESYNTAX, "underscore_as_return_type", "fn f() -> _ {}", "");
    test_compiler_status(PAW_ESYNTAX, "underscore_as_function_name", "fn _() {}", "");
    test_compiler_status(PAW_ESYNTAX, "underscore_as_method_name", "struct S {fn _() {}}", "");
    test_compiler_status(PAW_ESYNTAX, "underscore_as_parameter_type", "fn f(v: _) {}", "");
    test_compiler_status(PAW_ESYNTAX, "underscore_as_field_name", "struct S {_: int}", "");
    test_compiler_status(PAW_ESYNTAX, "underscore_as_field_type", "struct S {value: _}", "");
    test_compiler_status(PAW_ESYNTAX, "underscore_as_bound", "fn f<T: _>(t: T) {}", "");
    test_compiler_status(PAW_ESYNTAX, "underscore_in_parameter", "fn f(v: [_]) {}", "");
    test_compiler_status(PAW_ESYNTAX, "underscore_in_bound", "fn f<T: Trait<_>>(t: T) {}", "");
    test_compiler_status(PAW_ESYNTAX, "underscore_in_field_type", "struct S {value: [_]}", "");

    test_compiler_status(PAW_ETYPE, "underscore_bad_scalar_inference",
        "fn f(b: bool) {let v: _ = if b {1} else {'a'};}", "");
    test_compiler_status(PAW_ETYPE, "underscore_bad_container_inference",
        "fn f(b: bool) {let v: [_]; if b {v = [1]} else {v = ['a']};}", "");
}

static void test_global_const(void)
{
    // TODO: not currently supported, but should be eventually
    // TODO: call will need to be a "const fn"
    test_compiler_status(PAW_EVALUE, "const_list", "const C: [int] = [];", "");
    test_compiler_status(PAW_EVALUE, "const_map", "const C: [int: int] = [:];", "");
    test_compiler_status(PAW_EVALUE, "const_struct", "struct X; const C: X = X;", "");
    test_compiler_status(PAW_EVALUE, "const_enum", "enum X {E} const C: X = X::E;", "");
    test_compiler_status(PAW_EVALUE, "const_call", "fn f() {} const C: () = f();", "");
    test_compiler_status(PAW_EVALUE, "const_function", "fn f() {} const C: fn() = f;", "");

    test_compiler_status(PAW_ESYNTAX, "const_return", "const C: () = return;", "");
    test_compiler_status(PAW_ESYNTAX, "const_break", "const C: () = break;", "");
    test_compiler_status(PAW_ESYNTAX, "const_continue", "const C: () = break;", "");
    test_compiler_status(PAW_ESYNTAX, "const_chain", "const C: Option<int> = Option::Some(123)?;", "");

    test_compiler_status(PAW_EVALUE, "const_cycle_1",
            "const C: int = C;", "");
    test_compiler_status(PAW_EVALUE, "const_cycle_2",
            "const C1: int = C2 + 1;"
            "const C2: int = 1 + C1;", "");
    test_compiler_status(PAW_EVALUE, "const_cycle_3",
            "const C1: int = C2 + 1;"
            "const C2: int = 1 + C3;"
            "const C3: int = C1 + 1;", "");
}

static void test_annotations(void)
{
    test_compiler_status(PAW_EVALUE, "const_unexpected_initializer", "#[extern] const C: int = 42;", "");
    test_compiler_status(PAW_EVALUE, "function_unexpected_body", "#[extern] pub fn f() {}", "");
    // NOTE: "not_extern" annotation doesn't do anything
    test_compiler_status(PAW_EVALUE, "const_expected_initializer", "#[not_extern] const C: int;", "");
    test_compiler_status(PAW_EVALUE, "function_expected_body", "#[not_extern] pub fn f();", "");

}

int main(void)
{
    test_underscore();
    test_annotations();
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
    test_import_error();
    test_uninit_local();
    test_global_const();
    test_match_error();
    test_trait_error();
}
