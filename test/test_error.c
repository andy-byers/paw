// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// TODO: there are a few commented-out things that are broken...

#include "auxlib.h"
#include "call.h"
#include "error.h"
#include "lib.h"
#include "opcode.h"
#include "os.h"
#include "paw.h"
#include "rt.h"
#include "test.h"
#include <limits.h>
#include <inttypes.h>

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
    if (have != PAW_OK) {
        fprintf(stderr, "message: %s\n", paw_string(P, -1));
        paw_pop(P, 1);
    }
    if (have != want) {
        fprintf(stderr, "expected error code %d but got %d\n", want, have);
        abort();
    }
}

static void test_compiler_status(enum ErrorKind expect, char const *name, char const *item, char const *text)
{
    static char buffer[100000];
    write_main(buffer, item, text);

    paw_Env *P = paw_open(&(struct paw_Options){0});
    enum ErrorKind status = (enum ErrorKind)pawL_load_chunk(P, name, buffer);
    check_status(P, (int)status, (int)expect);

    paw_close(P);
}

static void test_runtime_status(int expect, char const *name, char const *item, char const *text)
{
    static char buffer[100000];
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
    test_compiler_status(E_UNKNOWN_PATH, "use_before_def_local", "", "let x = x;");
    test_compiler_status(E_UNKNOWN_PATH, "undef_variable", "", "x = 1;");
    test_compiler_status(E_UNKNOWN_FIELD, "undef_field", "struct A;", "let a = A.value;");
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

static void check_unop_error(enum ErrorKind expect, char const *op, paw_Type k)
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
    check_unop_error(E_INVALID_UNARY_OPERAND, op, k);
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

            test_compiler_status(E_INCOMPATIBLE_TYPES, name_buf, "", text_buf);
        }
    }
}

static void check_binop_type_error(unsigned error, char const *op, paw_Type t, paw_Type t2)
{
    char name_buf[256] = {0};
    snprintf(name_buf, sizeof(name_buf), "binop_type_error('%s', %s, %s)",
        op, get_literal(t), get_literal(t2));

    char text_buf[256] = {0};
    snprintf(text_buf, sizeof(text_buf), "let x = %s %s %s;",
        get_literal(t), op, get_literal(t2));

    test_compiler_status(error, name_buf, "", text_buf);
}

static paw_Bool type_in_list(paw_Type t, paw_Type const *types)
{
    for (paw_Type type = *types; type >= 0; type = *++types) {
        if (t == type) return PAW_TRUE;
    }
    return PAW_FALSE;
}

static void check_binop_type_errors(char const *op, paw_Type *types)
{
    for (int t = PAW_TUNIT; t <= PAW_TSTR; ++t) {
        for (int t2 = PAW_TUNIT; t2 <= PAW_TSTR; ++t2) {
            if (t != t2 && type_in_list(t, types) && type_in_list(t2, types))
                check_binop_type_error(E_INCOMPATIBLE_TYPES, op, t, t2);
            if (t == t2 && !type_in_list(t, types))
                check_binop_type_error(E_INVALID_BINARY_OPERAND, op, t, t2);
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

    test_compiler_status(E_UNIT_VARIANT_WITH_PARENTHESIS, "call_unit_variant", "enum E {X}", "let x = E::X();");
    test_compiler_status(E_INCOMPATIBLE_TYPES, "wrong_constructor_args", "enum E {X(int)}", "let x = E::X(1.0);");
    test_compiler_status(E_EXPECTED_ADT, "selector_on_function", "fn func() {}", "let a = func.field;");
    test_compiler_status(E_UNEXPECTED_MODULE_NAME, "selector_on_module", "use io;", "let s = io.abc;");
    test_compiler_status(E_EXTRA_SEGMENT, "extraneous_method_access",
        "struct S {pub fn f() {}}", "S::f::f(); ");
    test_compiler_status(E_EXTRA_SEGMENT, "extraneous_variant_access",
        "enum E {A}", "let e = E::A::A; ");

    test_compiler_status(E_INCOMPATIBLE_TYPES, "missing_return_type", "fn f() {123}", "");
    test_compiler_status(E_INCOMPATIBLE_TYPES, "missing_return_value", "fn f() -> int {}", "");
    test_compiler_status(E_INCOMPATIBLE_TYPES, "non_unit_guard", "fn f(x: bool) {if x {123}}", "");
    test_compiler_status(E_EXPECTED_BASIC_TYPE, "nonscalar_cast", "", "let x = 123 as str;");
    test_compiler_status(E_UNSATISFIED_TRAIT_BOUNDS, "invalid_map_key", "struct S;", "let x = [S: 123];");
}

static void test_name_too_long(void)
{
    char long_name[1000] = "let ";
    size_t index = 4;
    while (index < PAW_LENGTHOF(long_name) - 2)
        long_name[index++] = 'x';
    long_name[index++] = ';';
    long_name[index++] = '\0';
    test_compiler_status(E_NAME_TOO_LONG, "name_too_long", "", long_name);
}

// TODO: currently fails because paw_Int overflows before '-' can be applied. need to handle as special case
// TODO: once this works, remove this function and add to match.paw or something
static void test_minimum_integer(void)
{
    char buffer[256];
    snprintf(buffer, sizeof(buffer), "match 42 {%" PRId64 " => {}}", PAW_INT_MIN);
    test_compiler_status(E_INTEGER_OUT_OF_RANGE, "minimum_integer", "", buffer);
}

static void test_syntax_error(void)
{
    test_compiler_status(E_INVALID_UNICODE_CODEPOINT, "string_missing_second_surrogate", "", "let s = '\\u{d801}';");
    test_compiler_status(E_INVALID_UNICODE_CODEPOINT, "string_missing_first_surrogate", "", "let s = '\\u{dc00}';");
    test_compiler_status(E_INVALID_UNICODE_CODEPOINT, "string_malformed_surrogate_1", "", "let s = '\\u{d801}\\....';");
    test_compiler_status(E_INVALID_UNICODE_CODEPOINT, "string_malformed_surrogate_2", "", "let s = '\\u{d801}\\u....';");
    test_compiler_status(E_INVALID_UNICODE_CODEPOINT, "string_invalid_surrogate_low", "", "let s = '\\u{d801}\\u{dbff}';");
    test_compiler_status(E_INVALID_UNICODE_CODEPOINT, "string_invalid_surrogate_high", "", "let s = '\\u{d801}\\u{e000}';");
    test_compiler_status(E_UNICODE_ESCAPE_TOO_LONG, "string_unicode_escape_too_long", "", "let s = '\\u{1000001}';");

    test_compiler_status(E_EXPECTED_EXPRESSION, "misplaced_2dots", "", "let x = ..;");
    test_compiler_status(E_EXPECTED_EXPRESSION, "misplaced_3dots", "", "let x = ...;");
    test_compiler_status(E_EXPECTED_SEMICOLON, "misplaced_fat_arrow", "", "let x => 1;");
    test_compiler_status(E_INTEGER_OUT_OF_RANGE, "overflow_integer", "", "let d = -9223372036854775808;"); // overflows before '-' applied
    test_compiler_status(E_UNEXPECTED_INTEGER_CHAR, "binary_digit_range", "", "let b = 0b001201;");
    test_compiler_status(E_UNEXPECTED_INTEGER_CHAR, "octal_digit_range", "", "let o = 0o385273;");
    test_compiler_status(E_UNEXPECTED_INTEGER_CHAR, "hex_digit_range", "", "let x = 0x5A2CG3;");
    test_compiler_status(E_EXPECTED_SEMICOLON, "malformed_binary", "", "let b = 0b00$101;");
    test_compiler_status(E_INVALID_ASSIGNMENT_TARGET, "malformed_octal", "", "let o = 0o37=273;");
    test_compiler_status(E_EXPECTED_SEMICOLON, "malformed_hex", "", "let x = 0y5A2CF3;");
    test_compiler_status(E_EXPECTED_INTEGER_DIGIT, "int_digit_sep_before_bin_digits", "", "let x = 0b_01;");
    test_compiler_status(E_EXPECTED_INTEGER_DIGIT, "int_digit_sep_before_oct_digits", "", "let x = 0o_23;");
    test_compiler_status(E_EXPECTED_INTEGER_DIGIT, "int_digit_sep_before_hex_digits", "", "let x = 0x_45;");
    test_compiler_status(E_EXPECTED_SEMICOLON, "int_digit_sep_before_b", "", "let x = 0_b01;");
    test_compiler_status(E_EXPECTED_SEMICOLON, "int_digit_sep_before_o", "", "let x = 0_o23;");
    test_compiler_status(E_EXPECTED_SEMICOLON, "int_digit_sep_before_x", "", "let x = 0_x45;");
    test_compiler_status(E_EXPECTED_DELIMITER, "missing_right_paren", "fn f(a: int, b: int, c: int -> int {return (a + b + c);}", "");
    test_compiler_status(E_EXPECTED_SYMBOL, "missing_left_paren", "fn fa: int, b: int, c: int) -> int {return (a + b + c);}", "");
    test_compiler_status(E_EXPECTED_EXPRESSION, "missing_right_curly", "fn f(a: int, b: int, c: int) -> int {return (a + b + c);", "");
    test_compiler_status(E_EXPECTED_SYMBOL, "missing_left_curly", "fn f(a: int, b: int, c: int) -> int return (a + b + c);}", "");
    test_compiler_status(E_EXPECTED_DELIMITER, "missing_right_angle", "fn f<A, B, C() {}", "");
    test_compiler_status(E_EXPECTED_SYMBOL, "missing_left_angle", "fn fA, B, C>() {}", "");
    test_compiler_status(E_EXPECTED_SEMICOLON, "missing_turbo", "struct A<T>", "let a = A<int>;");
    test_compiler_status(E_EXPECTED_SEMICOLON, "partial_turbo", "struct A<T>", "let a = A:<int>;");
    test_compiler_status(E_EXPECTED_SEMICOLON, "missing_left_angle_tubofish", "struct A<T>", "let a = A::int>;");
    test_compiler_status(E_EXPECTED_SEMICOLON, "missing_right_angle_turbofish", "struct A<T>", "let a = A::<int;");
    test_compiler_status(E_EXPECTED_SYMBOL, "square_bracket_generics", "fn f[A, B, C]() {}", "");
    test_compiler_status(E_EXPECTED_EXPRESSION, "nested_fn", "", "fn f() {}");
    test_compiler_status(E_EXPECTED_EXPRESSION, "nested_struct", "", "struct S {pub x: int};");
    test_compiler_status(E_EXPECTED_EXPRESSION, "nested_enum", "", "enum E {X};");
    test_compiler_status(E_EXPECTED_TOPLEVEL_ITEM, "toplevel_var", "let v = 1", ";");
    test_compiler_status(E_EXPECTED_EXPRESSION, "bad_float", "", "let f = -1.0-;");
    test_compiler_status(E_EXPECTED_EXPRESSION, "bad_float_2", "", "let f = 1-.0-;");
    test_compiler_status(E_EXPECTED_SYMBOL, "bad_float_3", "", "let f = 1e--1;");
    test_compiler_status(E_EXPECTED_SYMBOL, "bad_float_4", "", "let f = 1e++1;");
    test_compiler_status(E_EXPECTED_FIELD_SELECTOR, "bad_float_5", "", "let f = 1e-1.0;");
    test_compiler_status(E_EXPECTED_FIELD_SELECTOR, "bad_float_6", "", "let f = 1e+1.0;");
    test_compiler_status(E_EXPECTED_SEMICOLON, "bad_float_7", "", "let f = 1e-1e1;");
    test_compiler_status(E_EXPECTED_SEMICOLON, "bad_float_8", "", "let f = 1e+1e1;");
    test_compiler_status(E_EXPECTED_FIELD_SELECTOR, "bad_float_9", "", "let f = 1.0.0;");
    test_compiler_status(E_UNKNOWN_FIELD, "float_digit_sep_after_dot", "", "let f = 1._0;");
    test_compiler_status(E_EXPECTED_SYMBOL, "float_digit_sep_after_e", "", "let f = 1e_0;");
    test_compiler_status(E_EXPECTED_SYMBOL, "float_digit_sep_after_-", "", "let f = 1e-_0;");
    test_compiler_status(E_EXPECTED_SYMBOL, "float_digit_sep_after_+", "", "let f = 1e+_0;");
    test_compiler_status(E_EXPECTED_FIELD_SELECTOR, "float_with_base_prefix", "", "let f = 0x1.0;");

    test_compiler_status(E_EXPECTED_SEMICOLON, "missing_semicolon_after_stmt", "", "let a = 1");
    test_compiler_status(E_EXPECTED_SEMICOLON, "missing_semicolon_between_stmts", "", "let a = 2\nlet b = 3;");
    test_compiler_status(E_EXPECTED_DELIMITER, "semicolon_instead_of_comma", "", "let a = [1, 2; 3, 4];");
    test_compiler_status(E_EXPECTED_EXPRESSION, "semicolon_after_comma", "", "let a = [5, 6,; 7, 8];");
    test_compiler_status(E_EXPECTED_EXPRESSION, "binop_missing_rhs", "", "let a = 1 +");
    test_compiler_status(E_EXPECTED_EXPRESSION, "binop_invalid_rhs", "", "let a = 1 + $;");
    test_compiler_status(E_EXPECTED_EXPRESSION, "binop_missing_lhs", "", "let a = + 2");
    test_compiler_status(E_EXPECTED_EXPRESSION, "binop_invalid_lhs", "", "let a = & + 2;");

    test_compiler_status(E_EXPECTED_VALUE, "primitive_type_is_not_a_value_1", "", "let a = int;");
    test_compiler_status(E_EXPECTED_VALUE, "primitive_type_is_not_a_value_2", "", "let a = (1, float,);");
    test_compiler_status(E_EXPECTED_VALUE, "primitive_type_is_not_a_value_3", "", "let a = ['two', str];");
    test_compiler_status(E_EXPECTED_VALUE, "generic_type_is_not_a_value", "fn f<T>() {let t = T;}", "");
    test_compiler_status(E_INCORRECT_ITEM_CLASS, "function_is_not_a_type", "fn test() {}", "let a: test = test;");
    test_compiler_status(E_INCORRECT_ITEM_CLASS, "variable_is_not_a_type", "", "let a = 1; let b: a = a;");
    test_compiler_status(E_UNKNOWN_PATH, "own_name_is_not_a_type", "", "let a: a = 1;");

    test_compiler_status(E_DUPLICATE_ITEM, "duplicate_global", "struct A; struct A;", "");
    test_compiler_status(E_EXPECTED_TOPLEVEL_ITEM, "return_outside_function", "return;", "");
    test_compiler_status(E_JUMP_OUTSIDE_LOOP, "break_outside_loop", "", "break;");
    test_compiler_status(E_JUMP_OUTSIDE_LOOP, "continue_outside_loop", "", "continue;");

    test_compiler_status(E_INVALID_ESCAPE, "invalid_escape", "", "let x = '\\x;';");
    test_compiler_status(E_INVALID_UNICODE_ESCAPE, "invalid_unicode_escape", "", "let x = '\\u{D8O}';");
    test_compiler_status(E_INVALID_UNICODE_CODEPOINT, "invalid_unicode_codepoint", "", "let x = '\xD8\x05';");
    test_compiler_status(E_INVALID_INTEGER, "invalid_integer", "", "let x = 0123;");
    test_compiler_status(E_INVALID_FLOAT, "invalid_float", "", "let x = 01.0;");
    test_name_too_long();
    test_compiler_status(E_UNEXPECTED_VISIBILITY_QUALIFIER, "unexpected_visibility_qualifier", "pub use io;", "");
    test_compiler_status(E_EMPTY_TYPE_LIST, "empty_type_list", "pub fn f<>() {}", "");
    test_minimum_integer();
    test_compiler_status(E_INVALID_LITERAL_NEGATION, "invalid_literal_negation", "", "match 'abc' {-'abc' => {}}");
    test_compiler_status(E_NONLITERAL_PATTERN, "interpolated_pattern", "", "match 'abc123' {'abc\\{123}' => {}}");
    test_compiler_status(E_INVALID_SELECTOR, "invalid_selector", "", "let x = 'abc'.1e-2;");
    test_compiler_status(E_EMPTY_VARIANT_FIELD_LIST, "empty_variant_field_list", "enum E {X()}", "");
    test_compiler_status(E_FUNCTION_TYPE_DECL, "function_type_decl", "type F = fn();", "");
    test_compiler_status(E_EXPECTED_COLON_AFTER_MAP_KEY, "expected_colon_after_map_key", "", "let x = [1: 2, 3];");
    test_compiler_status(E_COLON_AFTER_LIST_ELEMENT, "colon_after_list_element", "", "let x = [1, 2: 3];");
    test_compiler_status(E_EXPECTED_COMMA_SEPARATOR, "expected_comma_separator", "struct X {a: int b: int}", "");
    test_compiler_status(E_NONPRIMITIVE_ANNOTATION_VALUE, "nonprimitive_annotation_value", "#[anno=(1,)] fn f() {}", "");

    test_compiler_status(E_EXPECTED_SYMBOL, "missing_quote", "", "let s = '");
    test_compiler_status(E_EXPECTED_SYMBOL, "missing_quote", "", "let s = '\";");
    test_compiler_status(E_EXPECTED_EXPRESSION, "unpaired_curly_close", "", "let s = };");
}

static void test_closure_error(void)
{
    test_compiler_status(PAW_OK, "infer_by_usage", "", "let f = |x| {}; f(1);");

    test_compiler_status(E_INCOMPATIBLE_TYPES, "call_with_wrong_type_annotation", "", "let f = |x: int| x; f(2.0);");
    test_compiler_status(E_INCOMPATIBLE_TYPES, "call_with_wrong_type_inference", "", "let f = |x| x; f(1); f(2.0);");
    test_compiler_status(E_CANNOT_INFER, "cannot_infer_unused_param", "", "let f = |x| {};");
}

static void test_arithmetic_error(void)
{
    test_compiler_status(E_CONSTANT_DIVIDE_BY_ZERO, "constant_division_by_0_int", "", "let x = 1 / 0;");
    test_compiler_status(E_CONSTANT_DIVIDE_BY_ZERO, "constant_division_by_0_float", "", "let x = 1.0 / 0.0;");
    test_compiler_status(E_CONSTANT_NEGATIVE_SHIFT_COUNT, "constant_negative_left_shift", "", "let x = 1 << -2;");
    test_compiler_status(E_CONSTANT_NEGATIVE_SHIFT_COUNT, "constant_negative_right_shift", "", "let x = 1 >> -2;");

    test_compiler_status(E_CONSTANT_DIVIDE_BY_ZERO, "special_division_by_0_int", "fn f(x: int) -> int {x / 0}", "f(1);");
    test_compiler_status(E_CONSTANT_DIVIDE_BY_ZERO, "special_division_by_0_float", "fn f(x: float) -> float {x / 0.0}", "f(1.0);");
    test_compiler_status(E_CONSTANT_NEGATIVE_SHIFT_COUNT, "special_negative_left_shift", "fn f(x: int) -> int {x << -2}", "f(1);");
    test_compiler_status(E_CONSTANT_NEGATIVE_SHIFT_COUNT, "special_negative_right_shift", "fn f(x: int) -> int {x >> -2}", "f(1);");

    test_runtime_status(PAW_ERUNTIME, "division_by_0_int", "fn f(x: int) -> int {42 / x}", "f(0);");
    test_runtime_status(PAW_ERUNTIME, "division_by_0_float", "fn f(x: float) -> float {4.2 / x}", "f(0.0);");
    test_runtime_status(PAW_ERUNTIME, "negative_left_shift", "fn f(x: int) -> int {2 << x}", "f(-1);");
    test_runtime_status(PAW_ERUNTIME, "negative_right_shift", "fn f(x: int) -> int {2 >> x}", "f(-1);");
}

static void test_tuple_error(void)
{
    test_compiler_status(E_INVALID_INDEX_TARGET, "tuple_square_brackets", "", "let x = (1, 2); let y = x[0];");
    test_compiler_status(E_EXPECTED_ELEMENT_SELECTOR, "tuple_named_field", "", "let x = (1, 2); let y = x.first;");
    test_compiler_status(E_ELEMENT_SELECTOR_OUT_OF_RANGE, "tuple_index_out_of_range", "", "let x = (1, 2); let y = x.2;");
}

static void test_struct_error(void)
{
    test_compiler_status(E_EMPTY_STRUCT_BODY, "struct_unit_with_braces_on_def", "struct A {}", "let a = A;");
    test_compiler_status(E_EXPECTED_SEMICOLON, "struct_unit_without_semicolon", "struct A", "");
    test_compiler_status(E_MISSING_FIELDS, "struct_missing_braces", "struct A {pub a: int}", "let a = A;");
    test_compiler_status(E_UNIT_STRUCT_WITH_BRACES, "struct_unit_with_braces_on_init", "struct A;", "let a = A{};");
    test_compiler_status(E_MISSING_FIELD, "struct_missing_only_field", "struct A {pub a: int}", "let a = A{};");
    test_compiler_status(E_MISSING_FIELD, "struct_missing_field", "struct A {pub a: int, pub b: float}", "let a = A{a: 1};");
    test_compiler_status(E_UNKNOWN_FIELD, "struct_extra_field", "struct A {pub a: int}", "let a = A{a: 1, b: 2};");
    test_compiler_status(E_DUPLICATE_FIELD, "struct_duplicate_field", "struct A {pub a: int}", "let a = A{a: 1, a: 1};");
    test_compiler_status(E_EXPECTED_FIELD_SELECTOR, "struct_access_by_index", "struct S{pub x: int}", "let x = S{x: 1}; let y = x.0;");
    test_compiler_status(E_INCORRECT_TYPE_ARITY, "struct_not_enough_types", "struct S<A, B, C>;", "let x = S::<int, float>;");
    test_compiler_status(E_INCORRECT_TYPE_ARITY, "struct_too_many_types", "struct S<A, B>;", "let x = S::<int, float, bool>;");

    test_compiler_status(E_ASSOCIATED_ITEM_VISIBILITY, "struct_select_private_field",
        "struct S {pub a: int, b: int, pub fn new() -> S {return S{a: 1, b: 2};}}",
        "let x = S::new(); let a = x.a; let b = x.b;");
    test_compiler_status(E_ASSOCIATED_ITEM_VISIBILITY, "struct_literal_private_field", "struct S {pub a: int, b: int}", "let x = S{a: 1, b: 2};");
    test_compiler_status(E_ASSOCIATED_ITEM_VISIBILITY, "struct_call_private_method", "struct S {fn private(self) {}}", "let x = S; x.private();");

    test_compiler_status(E_NOT_A_METHOD, "struct_not_a_method", "struct S {pub fn f(s: Self) {}}", "let x = S; x.f();");
    test_compiler_status(E_INCOMPATIBLE_TYPES, "struct_invalid_self", "struct S {pub fn f(self: int) {}}", "");
    test_compiler_status(E_INCOMPATIBLE_TYPES, "struct_invalid_self_poly", "struct S<A, B> {fn f(self: S<B, A>) {}}", "");

    test_compiler_status(E_INFINITE_SIZE_OBJECT, "struct_infinite_size", "inline struct S{pub x: Option<S>}", "let x = S{x: Option::None};");
    test_compiler_status(E_INFINITE_SIZE_OBJECT, "struct_infinite_size_2", "inline struct S{pub x: Option<S2>} inline struct S2{pub x: Option<S>}", "let x = S{x: Option::None};");
}

static void test_enum_error(void)
{
    test_compiler_status(E_EMPTY_ENUMERATION, "enum_without_variants", "enum A {pub fn f() {}};", "");
    test_compiler_status(E_EXPECTED_VALUE, "enum_missing_variant", "enum A {X}", "let a = A;");
    test_compiler_status(E_DUPLICATE_ITEM, "enum_duplicate_variant", "enum A {X, X}", "");
    test_compiler_status(E_UNKNOWN_ASSOCIATED_ITEM, "enum_nonexistent_variant", "enum A {X}", "let a = A::Y;");
    test_compiler_status(E_MISSING_VARIANT_ARGS, "variant_missing_only_field", "enum A {X(int)}", "let a = A::X;");
    test_compiler_status(E_INCORRECT_ARITY, "variant_missing_field", "enum A {X(int, float)}", "let a = A::X(42);");
    test_compiler_status(E_INCORRECT_ARITY, "variant_extra_field", "enum A {X(int)}", "let a = A::X(42, true);");
    test_compiler_status(E_INCOMPATIBLE_TYPES, "variant_wrong_field_type", "enum A {X(int)}", "let a = A::X(1.0);");
    test_compiler_status(E_EXPECTED_FIELD_SELECTOR, "enum_requires_pattern_matching", "enum E{X(int)}", "let x = E::X(1); let y = x.0;");

    test_compiler_status(E_INFINITE_SIZE_OBJECT, "enum_infinite_size", "inline enum E{X(Option<E>)}", "let x = E::X(Option::None);");
    test_compiler_status(E_INFINITE_SIZE_OBJECT, "enum_infinite_size_2", "inline enum E{X(Option<E2>)} inline enum E2{X(Option<E>)}", "let x = E::X(Option::None);");

    // boxing the enum adds the necessary indirection to give the object a finite size
    test_compiler_status(PAW_OK, "enum_finite_size", "struct P<T> {v: T} enum E{X(Option<P<E>>)}", "let x = E::X(Option::None);");
    test_compiler_status(PAW_OK, "enum_finite_size_2", "struct P<T> {v: T} enum E{X(Option<P<E2>>)} enum E2{X(Option<P<E>>)}", "let x = E::X(Option::None);");
}

static void test_list_error(void)
{
    test_compiler_status(E_CYCLIC_TYPE, "list_cyclic_type", "", "let x = []; x = [x];");
    test_compiler_status(E_CYCLIC_TYPE, "list_nested_cyclic_type", "", "let x = []; x = [[x]];");
    test_compiler_status(E_CANNOT_INFER, "list_cannot_infer", "", "let a = [];");
    test_compiler_status(E_CANNOT_INFER, "list_cannot_infer_binop", "", "let a = [] + [];");
    test_compiler_status(E_CANNOT_INFER, "list_use_before_inference", "", "let a = []; let b = #a;");
    test_compiler_status(E_INCOMPATIBLE_TYPES, "list_incompatible_types", "", "let a = [1]; a = [2.0];");
    test_compiler_status(E_INCOMPATIBLE_TYPES, "list_incompatible_types_2", "", "let a = []; if true {a = [0];} else {a = [true];}");
    test_compiler_status(E_INCOMPATIBLE_TYPES, "list_mixed_types", "", "let a = [1, 2, 3, 4, '5'];");
    test_compiler_status(E_INCOMPATIBLE_TYPES, "list_mixed_nesting", "", "let a = [[[1]], [[2]], [3]];");
}

static void test_map_error(void)
{
    test_compiler_status(E_CYCLIC_TYPE, "map_cyclic_type", "", "let x = [:]; x = ['cyclic': x];");
    test_compiler_status(E_CYCLIC_TYPE, "map_nested_cyclic_type", "", "let x = [:]; x = ['cyclic': ['nested': x]];");
    test_compiler_status(E_CANNOT_INFER, "map_cannot_infer", "", "let a = [:];");
    test_compiler_status(E_CANNOT_INFER, "map_use_before_inference", "", "let a = [:]; let b = #a;");
    test_compiler_status(E_INCOMPATIBLE_TYPES, "map_incompatible_types", "", "let a = [1: 2]; a = [3: 4.0];");
    test_compiler_status(E_INCOMPATIBLE_TYPES, "map_incompatible_types_2", "", "let a = [:]; if true {a = [0: 0];} else {a = [1: true];}");
    test_compiler_status(E_INCOMPATIBLE_TYPES, "map_mixed_types", "", "let a = [1: 2, 3: 4, 5: '6'];");
    test_compiler_status(E_INCOMPATIBLE_TYPES, "map_mixed_nesting", "", "let a = [1: [1: 1], 2: [2: 2], 3: [3: [3: 3]]];");
    test_compiler_status(E_UNSATISFIED_TRAIT_BOUNDS, "map_unhashable_literal_key", "", "let map = [[1]: 1];");
    test_compiler_status(E_UNSATISFIED_TRAIT_BOUNDS, "map_unhashable_type_key", "", "let map: [[int]: int] = [:];");

    // TODO: get this working (either slices or allow range expr to be used, and would need half-open ranges, etc.)
//    test_compiler_status(E_INVALID_SLICE_TARGET, "map_slice", "", "let map = [:]; let val = map[0:10];");
}

static void test_import_error(void)
{
    test_compiler_status(E_MODULE_NOT_FOUND, "unrecognized_import", "use import_not_found;", "");
    test_compiler_status(E_UNKNOWN_ITEM, "unrecognized_import_item", "use io::NotFound;", "");
    test_compiler_status(E_UNKNOWN_PATH, "missing_import_item", "use io;", "let t = io::NotFound;");
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
        // return the address of "T" as an "int"
        "#[extern] pub fn conflicting_int<T>(t: T) -> int;\n"
        "pub fn main() {\n"
        "    let N = 500;\n"
        // create a bunch of dynamically-allocated objects
        "    let objects = [];\n"
        "    for i in 0..N {objects.push([i, i + 1, i + 2]);}\n"
        // fill a list with integers that conflict with the object addresses
        "    let conflicts = [];\n"
        "    for i in 0..N {conflicts.push(conflicting_int(objects[i]));}\n"
        // use a lot of memory to cause garbage collections
        "    let memory = [];\n"
        "    for i in 0..N {memory.push([[i], [i + 1], [i + 2]]);}\n"
        "}\n";

    paw_Env *P = paw_open(&(struct paw_Options){0});
    pawL_register_func(P, "conflicting_int", next_conflicting_int, 0);

    int status = pawL_load_chunk(P, "gc_conflict", source);
    check_status(P, status, PAW_OK);

    status = run_main(P, 0);
    check_status(P, status, PAW_OK);

    paw_close(P);
}

static void test_invalid_case(char const *name, enum ErrorKind expect, char const *item, char const *target, char const *pat)
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

    test_compiler_status(E_NONEXHAUSTIVE_PATTERN_MATCH, "match_int_non_exhaustive", enumeration,
        "match 123 {\n"
        "    123 => {},\n"
        "}\n");
    test_compiler_status(E_NONEXHAUSTIVE_PATTERN_MATCH, "match_variant_non_exhaustive", enumeration,
        "match Choice::First {\n"
        "    Choice::First => {},\n"
        "}\n");

    test_compiler_status(E_NONEXHAUSTIVE_PATTERN_MATCH, "match_variant_non_exhaustive_2", enumeration,
        "match Choice::First {\n"
        "    Choice::First => {},\n"
        "    Choice::Second(Choice::First) => {},"
        "}\n");

    test_compiler_status(E_NONEXHAUSTIVE_PATTERN_MATCH, "match_variant_non_exhaustive_3", enumeration,
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

    test_invalid_case("duplicate_binding", E_DUPLICATE_BINDING, "",
        "(0, 0)", "(x, x)");
    test_invalid_case("duplicate_binding_nested", E_DUPLICATE_BINDING, "",
        "(((0,),), 0)", "(((x,),), x)");
    test_invalid_case("or_binding_missing", E_MISSING_BINDING_IN_ALTERNATIVE, "",
        "(0, 0)", "(x, 2) | (2, 3)");
    test_invalid_case("or_binding_unrecognized", E_MISSING_BINDING_IN_ALTERNATIVE, "",
        "(0, 0)", "(1, 2) | (x, 3)");
    test_invalid_case("or_binding_unrecognized_int", E_MISSING_BINDING_IN_ALTERNATIVE, "",
        "0", "0 | x");
    test_invalid_case("or_binding_missing_int", E_MISSING_BINDING_IN_ALTERNATIVE, "",
        "0", "x | 0");
    // 'x' has a different type in each alternative
    test_invalid_case("or_binding_type_mismatch", E_INCOMPATIBLE_TYPES, "",
        "(0, '')", "(x, 'b') | (1, x)");
}

static void test_match_error(void)
{
    test_variant_match_error();
}

static void test_uninit_local(void)
{
    test_compiler_status(E_CANNOT_INFER, "uninit_var", "", "let x; x;"); // type of "x" cannot be inferred
    test_compiler_status(E_USE_BEFORE_INITIALIZATION, "uninit_int", "", "let x: int; let y = x;");
    test_compiler_status(E_USE_BEFORE_INITIALIZATION, "uninit_if_without_else", "", "let x; if true {x = 1;} let y = x;");
    test_compiler_status(E_USE_BEFORE_INITIALIZATION, "uninit_ifelse", "", "let x; if true {x = 1;} else {} let y = x;");
    test_compiler_status(E_USE_BEFORE_INITIALIZATION, "uninit_ifelse_chain", "", "let x; if true {x = 1;} else if true {} else {x = 3;} let y = x;");
    test_compiler_status(E_USE_BEFORE_INITIALIZATION, "uninit_ifelse_return", "", "let x; if true {return;} else if true {x = 2;} else {} let y = x;");
    test_compiler_status(E_USE_BEFORE_INITIALIZATION, "uninit_match", "",
        "let x;\n"
        "match 123 {\n"
        "    123 => x = 1,\n"
        "    _ => {},\n" // missing assignment to "x"
        "}\n"
        "let y = x;");
    test_compiler_status(E_USE_BEFORE_INITIALIZATION, "uninit_match_nested", "",
        "let x;\n"
        "match 123 {\n"
        "    1 => x = 1,\n"
        "    2 => x = 2,\n"
        "    3 => {\n"
        "        if true {\n"
        "            if true { x = 3; }\n" // "if" with no "else"
        "        } else {\n"
        "            x = 4;\n"
        "        }\n"
        "    },\n"
        "    _ => x = 5,\n"
        "}\n"
        "let y = x;");
}

static void test_trait_error(void)
{
#define TRAIT             \
    "pub trait Trait {\n" \
    "    fn f(self);\n"   \
    "}\n"

    test_compiler_status(E_MISSING_TRAIT_METHOD, "trait_missing_method",
        TRAIT "struct S: Trait {v: int}", "");
    test_compiler_status(E_INCOMPATIBLE_TYPES, "trait_wrong_type",
        TRAIT "struct S: Trait {pub fn f(self) -> int {123}}", "");
    test_compiler_status(E_TRAIT_METHOD_VISIBILITY_MISMATCH, "trait_mismatched_visibility",
        TRAIT "struct S: Trait {fn f(self) {}}", "");
    test_compiler_status(E_MISSING_TRAIT_BOUNDS, "generic_missing_bound",
        TRAIT "struct S: Trait {pub fn f(self) {}}\n"
              "pub fn call_f<T>(t: T) {t.f();}",
        "let x = S; call_f(x);");
    test_compiler_status(E_INCORRECT_TYPE_ARITY, "trait_generic_mismatch",
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

    test_compiler_status(E_UNSATISFIED_TRAIT_BOUNDS, "trait_not_implemented",
        POLY_TRAIT "struct S;" POLY_FUNCTION("int", ),
        "let x = S; call_f(x);");
    test_compiler_status(E_INCOMPATIBLE_TYPES, "trait_generic_mismatch",
        POLY_TRAIT POLY_STRUCT POLY_FUNCTION("int", ),
        "let x = S{v: true}; call_f(x);");
    test_compiler_status(E_EXPECTED_TRAIT, "trait_type_as_trait",
        "struct Type; struct S: Type;", "");
    test_compiler_status(E_UNKNOWN_TRAIT, "trait_missing_function_bound",
        "struct S: Trait;", "");
    test_compiler_status(E_UNKNOWN_PATH, "trait_missing_generic_in_bounds",
        POLY_TRAIT POLY_STRUCT POLY_FUNCTION("X", ), "");
    test_compiler_status(E_INCORRECT_TYPE_ARITY, "trait_cannot_infer_generic",
        POLY_TRAIT POLY_STRUCT "fn call_f<T: Trait>(t: T) {t.f();}",
        "let x = S{v: 123}; call_f(x);");
}

static void test_underscore(void)
{
    test_compiler_status(E_EXPECTED_SYMBOL, "underscore_as_generic", "fn f<_>() {}", "");
    test_compiler_status(E_EXPECTED_SYMBOL, "underscore_as_adt_name", "struct _;", "");
    test_compiler_status(E_EXPECTED_SYMBOL, "underscore_as_type_name", "type _ = int", "");
    test_compiler_status(E_EXPECTED_SYMBOL, "underscore_as_function_name", "fn _() {}", "");
    test_compiler_status(E_EXPECTED_SYMBOL, "underscore_as_method_name", "struct S {fn _() {}}", "");
    test_compiler_status(E_EXPECTED_SYMBOL, "underscore_as_field_name", "struct S {_: int}", "");
    test_compiler_status(E_EXPECTED_SYMBOL, "underscore_as_bound", "fn f<T: _>(t: T) {}", "");
    test_compiler_status(E_UNEXPECTED_UNDERSCORE, "underscore_in_bound", "fn f<T: Trait<_>>(t: T) {}", "");
    test_compiler_status(E_UNEXPECTED_UNDERSCORE, "underscore_as_return_type", "fn f() -> _ {}", "");
    test_compiler_status(E_UNEXPECTED_UNDERSCORE, "underscore_as_parameter_type", "fn f(v: _) {}", "");
    test_compiler_status(E_UNEXPECTED_UNDERSCORE, "underscore_as_field_type", "struct S {value: _}", "");
    test_compiler_status(E_UNEXPECTED_UNDERSCORE, "underscore_in_parameter", "fn f(v: [_]) {}", "");
    test_compiler_status(E_UNEXPECTED_UNDERSCORE, "underscore_in_field_type", "struct S {value: [_]}", "");

    test_compiler_status(E_INCOMPATIBLE_TYPES, "underscore_bad_scalar_inference",
        "fn f(b: bool) {let v: _ = if b {1} else {'a'};}", "");
    test_compiler_status(E_INCOMPATIBLE_TYPES, "underscore_bad_container_inference",
        "fn f(b: bool) {let v: [_]; if b {v = [1]} else {v = ['a']};}", "");
}

static void test_global_const(void)
{
    test_compiler_status(E_NONPRIMITIVE_CONSTANT, "const_list", "const C: [int] = [];", "");
    test_compiler_status(E_NONPRIMITIVE_CONSTANT, "const_map", "const C: [int: int] = [:];", "");
    test_compiler_status(E_NONPRIMITIVE_CONSTANT, "const_struct", "struct X; const C: X = X;", "");
    test_compiler_status(E_NONPRIMITIVE_CONSTANT, "const_enum", "enum X {E} const C: X = X::E;", "");
    test_compiler_status(E_CANNOT_CONSTANT_EVALUATE, "const_call", "fn f() {} const C: () = f();", "");
    test_compiler_status(E_NONPRIMITIVE_CONSTANT, "const_function", "fn f() {} const C: fn() = f;", "");

    test_compiler_status(E_RETURN_OUTSIDE_FUNCTION, "const_return", "const C: () = return;", "");
    test_compiler_status(E_JUMP_OUTSIDE_LOOP, "const_break", "const C: () = break;", "");
    test_compiler_status(E_JUMP_OUTSIDE_LOOP, "const_continue", "const C: () = continue;", "");
    test_compiler_status(E_CHAIN_OUTSIDE_FUNCTION, "const_chain", "const C: Option<int> = Option::Some(123)?;", "");

    test_compiler_status(E_GLOBAL_CONSTANT_CYCLE, "const_cycle_1",
            "const C: int = C;", "");
    test_compiler_status(E_GLOBAL_CONSTANT_CYCLE, "const_cycle_2",
            "const C1: int = C2 + 1;"
            "const C2: int = 1 + C1;", "");
    test_compiler_status(E_GLOBAL_CONSTANT_CYCLE, "const_cycle_3",
            "const C1: int = C2 + 1;"
            "const C2: int = 1 + C3;"
            "const C3: int = C1 + 1;", "");

    // TODO: need to store constants in MirPlace during earlier middle-end phases, in part to allow detection of this case
//    test_compiler_status(E_MODIFIED_CONSTANT, "const_assignment", "const C: int = 1;", "C = 2;");
}

static void test_annotations(void)
{
    test_compiler_status(E_INITIALIZED_EXTERN_CONSTANT, "const_unexpected_initializer", "#[extern] const C: int = 42;", "");
    test_compiler_status(E_EXTERN_FUNCTION_BODY, "function_unexpected_body", "#[extern] pub fn f() {}", "");
    // NOTE: "not_extern" annotation doesn't do anything
    test_compiler_status(E_UNINITIALIZED_CONSTANT, "const_expected_initializer", "#[not_extern] const C: int;", "");
    test_compiler_status(E_MISSING_FUNCTION_BODY, "function_expected_body", "#[not_extern] pub fn f();", "");
}

static void test_destructuring(void)
{
    char const *structure = "struct Fields {pub a: int, pub b: int}";
    test_compiler_status(E_DUPLICATE_BINDING, "destructure_duplicate_binding", "", "let (x, (x,)) = (1, (2,));");
    test_compiler_status(E_INCOMPATIBLE_TYPES, "destructure_wrong_type", "", "let (a, (b,)) = (1, 2);");
    test_compiler_status(E_INCOMPATIBLE_TYPES, "destructure_too_many_elems", "", "let (a, b) = (1, 2, 3);");
    test_compiler_status(E_INCOMPATIBLE_TYPES, "destructure_not_enough_elems", "", "let (a, b, c) = (1, 2);");
    test_compiler_status(E_MISSING_FIELD, "destructure_missing_field", structure,
            "let Fields{a} = Fields{a: 1, b: 2};");
    test_compiler_status(E_UNKNOWN_FIELD, "destructure_extra_field", structure,
            "let Fields{a, b, c} = Fields{a: 1, b: 2};");
    test_compiler_status(E_NONEXHAUSTIVE_PATTERN_MATCH, "destructure_non_exhaustive", "", "let Option::Some(x) = Option::Some(1);");
    test_compiler_status(E_EXPECTED_EXPRESSION, "destructure_wildcard_name", "", "let _ = 123; let x = _;");
    test_compiler_status(E_NONEXHAUSTIVE_PATTERN_MATCH, "destructure_or", "", "let (a, 1) | (a, 2) = (123, 456);");
    test_compiler_status(E_UNINITIALIZED_DESTRUCTURING, "uninitialized_destructuring", "", "let (a,); a = 123;");
    test_compiler_status(E_RESERVED_IDENTIFIER, "reserved_identifier", "", "let int = 123;");
    // NOTE: List<T> is considered a unit struct by the compiler. This would not result in an error
    //       if "List" was not treated as a reserved name.
    test_compiler_status(E_RESERVED_IDENTIFIER, "reserved_identifier_list", "", "let List = [1];");
}

static void test_deferred_init(void)
{
    test_compiler_status(E_USE_BEFORE_INITIALIZATION, "use_before_init", "", "let a; let b = a; b = 123;");
    test_compiler_status(E_USE_BEFORE_INITIALIZATION, "capture_before_init", "", "let a; let f = || -> int {a};");
    test_compiler_status(E_USE_BEFORE_INITIALIZATION, "missing_init_in_branch", "", "let a; if true {a = 1;} let b = a;");
    test_compiler_status(E_USE_BEFORE_INITIALIZATION, "use_in_branch", "", "let a; if true {a = 1;} else {let b = a;}");
    test_compiler_status(E_USE_BEFORE_INITIALIZATION, "uninit_if_else", "", "let a; if true {a = 1;} else if true {return;} else {} let b = a;");
}

static void test_interpolation(void)
{
    test_compiler_status(E_EXPECTED_SYMBOL, "missing_close_braces", "", "let s = '\\{{123}';");
    test_compiler_status(E_EXPECTED_EXPRESSION, "extra_close_braces", "", "let s = '\\{103 +} 20}';");
    test_compiler_status(E_EXPECTED_SYMBOL, "mismatched_braces", "", "let s = '\\{{{100} + {20 + {3}}}';");
    test_compiler_status(E_EXPECTED_DELIMITER, "mismatched_braces_nested", "", "let s = '\\{{\"abc\" + \"\\{{{100} + {20 + {3}})\"}}';");
    test_compiler_status(E_EXPECTED_SYMBOL, "missing_expr_close", "", "let s = 'abc\\{123';");
    test_compiler_status(E_EXPECTED_SYMBOL, "only_expr_open", "", "let s = '\\{';");
    test_compiler_status(E_EXPECTED_EXPRESSION, "empty_expr", "", "let s = '\\{}';");
}

static void test_panic(void)
{
    test_runtime_status(PAW_ERUNTIME, "panic", "", "panic('panic message');");
}

static void test_divergence(void)
{
#define FUNC(Text_) "fn f(x: int) -> int {" Text_ "}"

    test_compiler_status(E_INCOMPATIBLE_TYPES, "non_exhaustive_branch",
        FUNC("if x == 0 {} else {123}"), "");
    test_compiler_status(E_INCOMPATIBLE_TYPES, "non_exhaustive_return",
        FUNC("if x == 0 {return 123;}"), "");

    // The loop might break, depending on "x", and there is no return or result expression at the bottom
    // of the function. If the loop has type "!" then the expression is well-typed. If there is a "break",
    // then the loop has type "()".
    test_compiler_status(PAW_OK, "exhaustive_loop",
        FUNC("loop {if x == 0 {return x;}}"), "");
    test_compiler_status(PAW_OK, "exhaustive_loop_2",
        FUNC("loop {if x == 0 {return x;} else {}}"), "");
    test_compiler_status(E_INCOMPATIBLE_TYPES, "nonexhaustive_loop",
        FUNC("loop {if x == 0 {break;}}"), "");
    test_compiler_status(E_INCOMPATIBLE_TYPES, "nonexhaustive_loop_2",
        FUNC("loop {if x == 0 {} else if x == 1 {} else {break;}}"), "");

    test_compiler_status(PAW_OK, "type_after_return",
        FUNC("if x == 0 {return 123; x} else {x}"), "");
    test_compiler_status(E_INCOMPATIBLE_TYPES, "wrong_type_after_return",
        FUNC("if x == 0 {return 123; \"abc\"} else {x}"), "");

#undef FUNC

    test_runtime_status(PAW_ERUNTIME, "custom_diverging_function",
        "fn diverge() -> ! {panic(\"diverging\")}", "diverge();");
    test_runtime_status(PAW_ERUNTIME, "custom_diverging_function_2",
        "fn diverge() -> ! {if true {panic(\"first divergence\")} else {panic(\"second divergence\")}}", "diverge();");

    // TODO: Need to throw a compiler error when a function is lying about its divergence status
    //       i.e. it has a return type of "!" but does not unconditionally call a diverging function.
    //       This should be checked when building the MIR or in a later pass.
#if 0
    test_compiler_status(E_EXPECTED_DIVERGENCE, "function_lies_about_divergence",
        "fn diverge() -> ! {}", "");
    test_compiler_status(E_EXPECTED_DIVERGENCE, "function_lies_about_divergence_2",
        "fn diverge() -> ! {if true {panic(\"conditionally diverge\")}}", "");
    test_compiler_status(E_EXPECTED_DIVERGENCE, "function_lies_about_divergence_3",
        "fn diverge() -> ! {match 123 {0 => return, _ => panic(\"conditionally diverge\")}}", "");
#endif // 0
}

int main(void)
{
    test_syntax_error();
    test_underscore();
    test_annotations();
    test_gc_conflict();
    test_enum_error();
    test_name_error();
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
    test_destructuring();
    test_deferred_init();
    test_interpolation();
    test_panic();
    test_divergence();
}
