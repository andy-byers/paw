// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// TODO: there are quite a few commented-out things that need to be updated to reflect changes in syntax/semantics...

#include "auxlib.h"
#include "call.h"
#include "compile.h"
#include "env.h"
#include "error.h"
#include "lib.h"
#include "os.h"
#include "test.h"
#include <limits.h>
#include <inttypes.h>

static void write_main(char *out, char const *items, char const *text)
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

static void check_status(paw_Env *P, int have, int want)
{
    if (have != PAW_OK)
        fprintf(stderr, "message: %s\n", P->current_errmsg->text);

    if (have != want && have != -1) {
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
    // TODO: need to run from script. maybe just generate
//    static char buffer[100000];
//    write_main(buffer, item, text);
//
//    paw_Env *P = paw_open(&(struct paw_Options){0});
//    int status = pawL_load_chunk(P, name, buffer);
//    check_status(P, status, PAW_OK);
//
//    Buffer b;
//    pawP_mangle_start(P, &b, P->C);
//    paw_mangle_start(P);
//    paw_push_str(P, "main");
//    paw_mangle_add_name(P);
//
//    struct paw_Item info;
//    status = paw_lookup_item(P, -1, &info);
//    check_status(P, status, PAW_OK);
//    check(info.global_id >= 0);
//    paw_get_global(P, info.global_id);
//
//    status = paw_call(P, 0);
//    check_status(P, status, expect);
//
//    paw_close(P);
}

static void test_name_error(void)
{
    test_compiler_status(kErrUnknownPath, "use_before_def_local", "", "let x = x;");
    test_compiler_status(kErrUnknownPath, "undef_variable", "", "x = 1;");
    test_compiler_status(kErrUnknownField, "undef_field", "struct A;", "let a = A.value;");
}

static char const *get_literal(int kind)
{
    switch (kind) {
        case PAW_TUNIT:
            return "()";
        case PAW_TCHAR:
            return "'x'";
        case PAW_TINT:
            return "123";
        case PAW_TFLOAT:
            return "1.0";
        case PAW_TBOOL:
            return "true";
        case PAW_TSTR:
            return "\"abc\"";
        default:
            check(0);
            return NULL;
    }
}

static void check_unop_error(enum ErrorKind expect, char const *op, paw_Type k)
{
    char name_buf[256] = {0};
    snprintf(name_buf, sizeof(name_buf), "unop_type_error(\"%s\", %s)",
        op, get_literal(k));

    char text_buf[256] = {0};
    snprintf(text_buf, sizeof(text_buf), "let x = %s%s;",
        op, get_literal(k));

    test_compiler_status(expect, name_buf, "", text_buf);
}

static void check_unop_type_error(char const *op, paw_Type k)
{
    check_unop_error(kErrInvalidUnaryOperand, op, k);
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

            test_compiler_status(kErrIncompatibleTypes, name_buf, "", text_buf);
        }
    }
}

static void check_binop_type_error(unsigned error, char const *op, paw_Type t, paw_Type t2)
{
    char name_buf[256] = {0};
    snprintf(name_buf, sizeof(name_buf), "binop_type_error(\"%s\", %s, %s)",
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
                check_binop_type_error(kErrIncompatibleTypes, op, t, t2);
            if (t == t2 && !type_in_list(t, types))
                check_binop_type_error(kErrInvalidBinaryOperand, op, t, t2);
        }
    }
}

static void test_type_error(void)
{
    check_unification_errors();

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
    check_binop_type_errors("<", MAKE_LIST(PAW_TCHAR, PAW_TINT, PAW_TFLOAT, PAW_TSTR));
    check_binop_type_errors(">", MAKE_LIST(PAW_TCHAR, PAW_TINT, PAW_TFLOAT, PAW_TSTR));
    check_binop_type_errors("<=", MAKE_LIST(PAW_TCHAR, PAW_TINT, PAW_TFLOAT, PAW_TSTR));
    check_binop_type_errors(">=", MAKE_LIST(PAW_TCHAR, PAW_TINT, PAW_TFLOAT, PAW_TSTR));
    check_binop_type_errors("==", MAKE_LIST(PAW_TBOOL, PAW_TCHAR, PAW_TINT, PAW_TFLOAT, PAW_TSTR));
    check_binop_type_errors("!=", MAKE_LIST(PAW_TBOOL, PAW_TCHAR, PAW_TINT, PAW_TFLOAT, PAW_TSTR));

    test_compiler_status(kErrUnitVariantWithParenthesis, "call_unit_variant", "enum E {X}", "let x = E::X();");
    test_compiler_status(kErrIncompatibleTypes, "wrong_constructor_args", "enum E {X(int)}", "let x = E::X(1.0);");
    test_compiler_status(kErrExpectedAdt, "selector_on_function", "fn func() {}", "let a = func.field;");
    test_compiler_status(kErrUnknownPath, "selector_on_module", "use io;", "let s = io.abc;");
    test_compiler_status(kErrExtraSegment, "extraneous_method_access",
        "struct S; impl S {pub fn f() {}}", "S::f::f(); ");
    test_compiler_status(kErrExtraSegment, "extraneous_variant_access",
        "enum E {A}", "let e = E::A::A; ");

    test_compiler_status(kErrIncompatibleTypes, "missing_return_type", "fn f() {123}", "");
    test_compiler_status(kErrIncompatibleTypes, "missing_return_value", "fn f() -> int {}", "");
    test_compiler_status(kErrIncompatibleTypes, "non_unit_guard", "fn f(x: bool) {if x {123}}", "");
    test_compiler_status(kErrIncompatibleTypes, "nonscalar_cast", "", "let x = 123 as str;");
    test_compiler_status(kErrFalseObligation, "invalid_map_key", "use hashmap::HashMap; struct S;", "let x: HashMap<S, int> = HashMap::new();");
}

static void test_name_too_long(void)
{
    char long_name[1000] = "let ";
    size_t index = 4;
    while (index < PAW_LENGTHOF(long_name) - 2)
        long_name[index++] = 'x';
    long_name[index++] = ';';
    long_name[index++] = '\0';
    test_compiler_status(kErrNameTooLong, "name_too_long", "", long_name);
}

static void test_syntax_error(void)
{
    test_compiler_status(kErrInvalidUnicodeCodepoint, "string_missing_second_surrogate", "", "let s = \"\\u{d801}\";");
    test_compiler_status(kErrInvalidUnicodeCodepoint, "string_missing_first_surrogate", "", "let s = \"\\u{dc00}\";");
    test_compiler_status(kErrInvalidUnicodeCodepoint, "string_malformed_surrogate_1", "", "let s = \"\\u{d801}\\....\";");
    test_compiler_status(kErrInvalidUnicodeCodepoint, "string_malformed_surrogate_2", "", "let s = \"\\u{d801}\\u....\";");
    test_compiler_status(kErrInvalidUnicodeCodepoint, "string_invalid_surrogate_low", "", "let s = \"\\u{d801}\\u{dbff}\";");
    test_compiler_status(kErrInvalidUnicodeCodepoint, "string_invalid_surrogate_high", "", "let s = \"\\u{d801}\\u{e000}\";");
    test_compiler_status(kErrUnicodeEscapeTooLong, "string_unicode_escape_too_long", "", "let s = \"\\u{1000001}\";");

    test_compiler_status(kErrExpectedExpression, "misplaced_3dots", "", "let x = ...;");
    test_compiler_status(kErrExpectedSemicolon, "misplaced_fat_arrow", "", "let x => 1;");
    test_compiler_status(kErrIntegerOutOfRange, "overflow_integer", "", "let d = -9223372036854775809;");
    test_compiler_status(kErrInvalidCharInInteger, "binary_digit_range", "", "let b = 0b001201;");
    test_compiler_status(kErrInvalidCharInInteger, "octal_digit_range", "", "let o = 0o385273;");
    test_compiler_status(kErrInvalidCharInInteger, "hex_digit_range", "", "let x = 0x5A2CG3;");
    test_compiler_status(kErrExpectedSemicolon, "malformed_binary", "", "let b = 0b00$101;");
    test_compiler_status(kErrInvalidAssignmentTarget, "malformed_octal", "", "let o = 0o37=273;");
    test_compiler_status(kErrExpectedSemicolon, "malformed_hex", "", "let x = 0y5A2CF3;");
    test_compiler_status(kErrExpectedIntegerDigit, "int_digit_sep_before_bin_digits", "", "let x = 0b_01;");
    test_compiler_status(kErrExpectedIntegerDigit, "int_digit_sep_before_oct_digits", "", "let x = 0o_23;");
    test_compiler_status(kErrExpectedIntegerDigit, "int_digit_sep_before_hex_digits", "", "let x = 0x_45;");
    test_compiler_status(kErrExpectedSemicolon, "int_digit_sep_before_b", "", "let x = 0_b01;");
    test_compiler_status(kErrExpectedSemicolon, "int_digit_sep_before_o", "", "let x = 0_o23;");
    test_compiler_status(kErrExpectedSemicolon, "int_digit_sep_before_x", "", "let x = 0_x45;");
    test_compiler_status(kErrExpectedDelimiter, "missing_right_paren", "fn f(a: int, b: int, c: int -> int {return (a + b + c);}", "");
    test_compiler_status(kErrUnexpectedSymbol, "missing_left_paren", "fn fa: int, b: int, c: int) -> int {return (a + b + c);}", "");
    test_compiler_status(kErrExpectedExpression, "missing_right_curly", "fn f(a: int, b: int, c: int) -> int {return (a + b + c);", "");
    test_compiler_status(kErrUnexpectedSymbol, "missing_left_curly", "fn f(a: int, b: int, c: int) -> int return (a + b + c);}", "");
    test_compiler_status(kErrExpectedDelimiter, "missing_right_angle", "fn f<A, B, C() {}", "");
    test_compiler_status(kErrUnexpectedSymbol, "missing_left_angle", "fn fA, B, C>() {}", "");
    test_compiler_status(kErrExpectedSemicolon, "missing_turbo", "struct A<T>", "let a = A<int>;");
    test_compiler_status(kErrExpectedSemicolon, "partial_turbo", "struct A<T>", "let a = A:<int>;");
    test_compiler_status(kErrExpectedSemicolon, "missing_left_angle_tubofish", "struct A<T>", "let a = A::int>;");
    test_compiler_status(kErrExpectedSemicolon, "missing_right_angle_turbofish", "struct A<T>", "let a = A::<int;");
    test_compiler_status(kErrUnexpectedSymbol, "square_bracket_generics", "fn f[A, B, C]() {}", "");
    test_compiler_status(kErrExpectedExpression, "nested_fn", "", "fn f() {}");
    test_compiler_status(kErrExpectedExpression, "nested_struct", "", "struct S {pub x: int};");
    test_compiler_status(kErrExpectedExpression, "nested_enum", "", "enum E {X};");
    test_compiler_status(kErrExpectedToplevelItem, "toplevel_var", "let v = 1", ";");
    test_compiler_status(kErrExpectedExpression, "bad_float", "", "let f = -1.0-;");
    test_compiler_status(kErrExpectedExpression, "bad_float_2", "", "let f = 1-.0-;");
    test_compiler_status(kErrInvalidFloatLiteral, "bad_float_3", "", "let f = 1e--1;");
    test_compiler_status(kErrInvalidFloatLiteral, "bad_float_4", "", "let f = 1e++1;");
    test_compiler_status(kErrExpectedAdt, "bad_float_5", "", "let f = 1e-1.0;");
    test_compiler_status(kErrExpectedAdt, "bad_float_6", "", "let f = 1e+1.0;");
    test_compiler_status(kErrExpectedSemicolon, "bad_float_7", "", "let f = 1e-1e1;");
    test_compiler_status(kErrExpectedSemicolon, "bad_float_8", "", "let f = 1e+1e1;");
    test_compiler_status(kErrExpectedAdt, "bad_float_9", "", "let f = 1.0.0;");
    test_compiler_status(kErrExpectedAdt, "float_digit_sep_after_dot", "", "let f = 1._0;");
    test_compiler_status(kErrInvalidFloatLiteral, "float_digit_sep_after_e", "", "let f = 1e_0;");
    test_compiler_status(kErrInvalidFloatLiteral, "float_digit_sep_after_-", "", "let f = 1e-_0;");
    test_compiler_status(kErrInvalidFloatLiteral, "float_digit_sep_after_+", "", "let f = 1e+_0;");
    test_compiler_status(kErrExpectedAdt, "float_with_base_prefix", "", "let f = 0x1.0;");

    test_compiler_status(kErrExpectedSemicolon, "missing_semicolon_after_stmt", "", "let a = 1");
    test_compiler_status(kErrExpectedSemicolon, "missing_semicolon_between_stmts", "", "let a = 2\nlet b = 3;");
    test_compiler_status(kErrExpectedDelimiter, "semicolon_instead_of_comma", "", "let a = [1, 2; 3, 4];");
    test_compiler_status(kErrExpectedExpression, "semicolon_after_comma", "", "let a = [5, 6,; 7, 8];");
    test_compiler_status(kErrExpectedExpression, "binop_missing_rhs", "", "let a = 1 +");
    test_compiler_status(kErrExpectedExpression, "binop_invalid_rhs", "", "let a = 1 + $;");
    test_compiler_status(kErrExpectedExpression, "binop_missing_lhs", "", "let a = + 2");
    test_compiler_status(kErrExpectedExpression, "binop_invalid_lhs", "", "let a = & + 2;");

    test_compiler_status(kErrUnknownPath, "primitive_type_is_not_a_value_1", "", "let a = int;");
    test_compiler_status(kErrUnknownPath, "primitive_type_is_not_a_value_2", "", "let a = (1, float,);");
    test_compiler_status(kErrUnknownPath, "primitive_type_is_not_a_value_3", "", "let a = [\"two\", str];");
    test_compiler_status(kErrUnknownPath, "generic_type_is_not_a_value", "fn f<T>() {let t = T;}", "");
    test_compiler_status(kErrUnknownPath, "function_is_not_a_type", "fn test() {}", "let a: test = test;");
    test_compiler_status(kErrUnknownPath, "variable_is_not_a_type", "", "let a = 1; let b: a = a;");
    test_compiler_status(kErrUnknownPath, "own_name_is_not_a_type", "", "let a: a = 1;");

    test_compiler_status(kErrDuplicateName, "duplicate_global", "struct A; struct A;", "");
    test_compiler_status(kErrExpectedToplevelItem, "return_outside_function", "return;", "");
    test_compiler_status(kErrJumpOutsideLoop, "break_outside_loop", "", "break;");
    test_compiler_status(kErrJumpOutsideLoop, "continue_outside_loop", "", "continue;");

    test_compiler_status(kErrHexEscapeTooShort, "empty_hex_escape", "", "let x = \"\\x\";");
    test_compiler_status(kErrHexEscapeTooShort, "short_hex_escape", "", "let x = \"\\x0\";");
    test_compiler_status(kErrInvalidCharInHexEscape, "invalid_hex_escape", "", "let x = \"\\x;\";");
    test_compiler_status(kErrUnterminatedStrLiteral, "unterminated_unicode_escape", "", "let x = \"\\u{7f");
    test_compiler_status(kErrInvalidCharInUnicodeEscape, "invalid_unicode_escape", "", "let x = \"\\u{D8O}\";");
// TODO    test_compiler_status(kErrInvalidUnicodeCodepoint, "invalid_unicode_codepoint", "", "let x = \"\xD8\x05\";");
    test_compiler_status(kErrInvalidFloatLiteral, "invalid_float", "", "let x = 01.0;");
    test_name_too_long();
    test_compiler_status(kErrEmptyTypeList, "empty_type_list", "pub fn f<>() {}", "");
    test_compiler_status(kErrInvalidLiteralNegation, "invalid_literal_negation", "", "match \"abc\" {-\"abc\" => {}}");
//TODO    test_compiler_status(kErrNonliteralPattern, "interpolated_pattern", "", "match \"abc123\" {\"abc\\{123}\" => {}}");
    test_compiler_status(kErrInvalidSelector, "invalid_selector", "", "let x = \"abc\".1e-2;");
    test_compiler_status(kErrEmptyVariantFieldList, "empty_variant_field_list", "enum E {X()}", "");
    test_compiler_status(kErrFunctionTypeDecl, "function_type_decl", "type F = fn();", "");
    test_compiler_status(kErrUnsupported, "trait_bounds_on_alias_generic", "struct Struct<X>; type T<X: Hash> = Struct<X>;", "");
    test_compiler_status(kErrUnsupported, "trait_bounds_on_local_alias_generic", "struct Struct<X>;", "type T<X: Hash> = Struct<X>;");
    test_compiler_status(kErrExpectedCommaSeparator, "expected_comma_separator", "struct X {a: int b: int}", "");
    test_compiler_status(kErrNonprimitiveAnnotationValue, "nonprimitive_annotation_value", "#[anno=(1,)] fn f() {}", "");

    test_compiler_status(kErrUnterminatedStrLiteral, "missing_quote", "", "let s = \"");
    test_compiler_status(kErrUnterminatedCharLiteral, "missing_quote", "", "let s = '\";");
    test_compiler_status(kErrExpectedExpression, "unpaired_curly_close", "", "let s = };");
}

static void test_closure_error(void)
{
    test_compiler_status(PAW_OK, "infer_by_usage", "", "let f = |x| {}; f(1);");

    test_compiler_status(kErrIncompatibleTypes, "call_with_wrong_type_annotation", "", "let f = |x: int| x; f(2.0);");
    test_compiler_status(kErrIncompatibleTypes, "call_with_wrong_type_inference", "", "let f = |x| x; f(1); f(2.0);");
    test_compiler_status(kErrCannotInfer, "cannot_infer_unused_param", "", "let f = |x| {};");

#define ONCE_CLOSURE_TEST(Status_, Name_, Code_) \
    test_compiler_status(Status_, Name_, "struct MoveOnly;", \
            "let m = MoveOnly; let f = || m;" Code_);

    ONCE_CLOSURE_TEST(kErrUseAfterMove, "capture_moves_variable", "let m2 = m;");
    ONCE_CLOSURE_TEST(kErrUseAfterMove, "use_once_closure_after_move", "let g = f; f();");
    ONCE_CLOSURE_TEST(kErrUseAfterMove, "once_closure_consumed_by_call", "f(); f();");
    ONCE_CLOSURE_TEST(kErrIncompatibleTypes, "once_closure_not_compatible_with_fn_ptr", "let g: fn() -> MoveOnly = f;");
}

static void test_arithmetic_error(void)
{
    test_compiler_status(kErrConstantDivideByZero, "constant_division_by_0_int", "", "let x = 1 / 0;");
    test_compiler_status(kErrConstantDivideByZero, "constant_division_by_0_float", "", "let x = 1.0 / 0.0;");
    test_compiler_status(kErrConstantNegativeShiftCount, "constant_negative_left_shift", "", "let x = 1 << -2;");
    test_compiler_status(kErrConstantNegativeShiftCount, "constant_negative_right_shift", "", "let x = 1 >> -2;");

    test_compiler_status(kErrConstantDivideByZero, "special_division_by_0_int", "fn f(x: int) -> int {x / 0}", "f(1);");
    test_compiler_status(kErrConstantDivideByZero, "special_division_by_0_float", "fn f(x: float) -> float {x / 0.0}", "f(1.0);");
    test_compiler_status(kErrConstantNegativeShiftCount, "special_negative_left_shift", "fn f(x: int) -> int {x << -2}", "f(1);");
    test_compiler_status(kErrConstantNegativeShiftCount, "special_negative_right_shift", "fn f(x: int) -> int {x >> -2}", "f(1);");

    test_runtime_status(PAW_ERUNTIME, "division_by_0_int", "fn f(x: int) -> int {42 / x}", "f(0);");
    test_runtime_status(PAW_ERUNTIME, "division_by_0_float", "fn f(x: float) -> float {4.2 / x}", "f(0.0);");
    test_runtime_status(PAW_ERUNTIME, "negative_left_shift", "fn f(x: int) -> int {2 << x}", "f(-1);");
    test_runtime_status(PAW_ERUNTIME, "negative_right_shift", "fn f(x: int) -> int {2 >> x}", "f(-1);");
}

static void test_tuple_error(void)
{
    test_compiler_status(kErrInvalidIndexTarget, "tuple_square_brackets", "", "let x = (1, 2); let y = x[0];");
    test_compiler_status(kErrExpectedElementSelector, "tuple_named_field", "", "let x = (1, 2); let y = x.first;");
    test_compiler_status(kErrElementSelectorOutOfRange, "tuple_index_out_of_range", "", "let x = (1, 2); let y = x.2;");
}

static void test_struct_error(void)
{
    test_compiler_status(kErrEmptyStructBody, "struct_unit_with_braces_on_def", "struct A {}", "let a = A;");
    test_compiler_status(kErrExpectedSemicolon, "struct_unit_without_semicolon", "struct A", "");
    test_compiler_status(kErrUnknownPath, "struct_missing_braces", "struct A {pub a: int}", "let a = A;");
    test_compiler_status(kErrUnitStructWithBraces, "struct_unit_with_braces_on_init", "struct A;", "let a = A{};");
    test_compiler_status(kErrMissingField, "struct_missing_only_field", "struct A {pub a: int}", "let a = A{};");
    test_compiler_status(kErrMissingField, "struct_missing_field", "struct A {pub a: int, pub b: float}", "let a = A{a: 1};");
    test_compiler_status(kErrUnknownField, "struct_extra_field", "struct A {pub a: int}", "let a = A{a: 1, b: 2};");
    test_compiler_status(kErrDuplicateName, "struct_duplicate_field", "struct A {pub a: int}", "let a = A{a: 1, a: 1};");
    test_compiler_status(kErrExpectedFieldSelector, "struct_access_by_index", "struct S{pub x: int}", "let x = S{x: 1}; let y = x.0;");
    test_compiler_status(kErrIncorrectTypeArity, "struct_not_enough_types", "struct S<A, B, C>;", "let x = S::<int, float>;");
    test_compiler_status(kErrIncorrectTypeArity, "struct_too_many_types", "struct S<A, B>;", "let x = S::<int, float, bool>;");

    // TODO: non-pub fields/items can be accessed from anywhere in the same module as the type was defined
//    test_compiler_status(kErrAssociatedItemVisibility, "struct_select_private_field",
//        "struct S {pub a: int, b: int} impl S {pub fn new() -> S {return S{a: 1, b: 2};}}",
//        "let x = S::new(); let a = x.a; let b = x.b;");
//    test_compiler_status(kErrAssociatedItemVisibility, "struct_literal_private_field", "struct S {pub a: int, b: int}", "let x = S{a: 1, b: 2};");
//    test_compiler_status(kErrAssociatedItemVisibility, "struct_call_private_method", "struct S; impl S {fn private(self) {}}", "let x = S; x.private();");

    test_compiler_status(kErrNotAMethod, "struct_not_a_method", "struct S; impl S {pub fn f(s: Self) {}}", "let x = S; x.f();");
    test_compiler_status(kErrIncompatibleTypes, "struct_invalid_self", "struct S; impl S {pub fn f(self: int) {}}", "");
    test_compiler_status(kErrIncompatibleTypes, "struct_invalid_self_poly", "struct S<A, B>; impl<A, B> S<A, B> {fn f(self: S<B, A>) {}}", "");

    test_compiler_status(kErrTypeContainsSelf, "struct_infinite_size", "struct S{pub x: Option<S>}", "let x = S{x: Option::None};");
    test_compiler_status(kErrTypeContainsSelf, "struct_infinite_size_2", "struct S{pub x: Option<S2>} struct S2{pub x: Option<S>}", "let x = S{x: Option::None};");
}

static void test_enum_error(void)
{
    test_compiler_status(kErrEmptyEnumeration, "enum_without_variants", "enum A {};", "");
    test_compiler_status(kErrUnknownPath, "enum_missing_variant", "enum A {X}", "let a = A;");
    test_compiler_status(kErrDuplicateName, "enum_duplicate_variant", "enum A {X, X}", "");
    test_compiler_status(kErrUnknownAssociatedItem, "enum_nonexistent_variant", "enum A {X}", "let a = A::Y;");
    test_compiler_status(kErrMissingVariantArgs, "variant_missing_only_field", "enum A {X(int)}", "let a = A::X;");
    test_compiler_status(kErrIncorrectArity, "variant_missing_field", "enum A {X(int, float)}", "let a = A::X(42);");
    test_compiler_status(kErrIncorrectArity, "variant_extra_field", "enum A {X(int)}", "let a = A::X(42, true);");
    test_compiler_status(kErrIncompatibleTypes, "variant_wrong_field_type", "enum A {X(int)}", "let a = A::X(1.0);");
    test_compiler_status(kErrExpectedFieldSelector, "enum_requires_pattern_matching", "enum E{X(int)}", "let x = E::X(1); let y = x.0;");

    test_compiler_status(kErrTypeContainsSelf, "enum_infinite_size", "enum E{X(Option<E>)}", "let x = E::X(Option::None);");
    test_compiler_status(kErrTypeContainsSelf, "enum_infinite_size_2", "enum E{X(Option<E2>)} enum E2{X(Option<E>)}", "let x = E::X(Option::None);");

    // boxing the enum adds the necessary indirection to give the object a finite size
    test_compiler_status(PAW_OK, "enum_finite_size", "struct P<T> {v: *T} enum E{X(Option<P<E>>)}", "let x = E::X(Option::None);");
    test_compiler_status(PAW_OK, "enum_finite_size_2", "struct P<T> {v: *T} enum E{X(Option<P<E2>>)} enum E2{X(Option<P<E>>)}", "let x = E::X(Option::None);");
}

static void test_list_error(void)
{
    test_compiler_status(kErrCyclicType, "list_cyclic_type", "", "let x = []; x = [x];");
    test_compiler_status(kErrCyclicType, "list_nested_cyclic_type", "", "let x = []; x = [[x]];");
    test_compiler_status(kErrCannotInfer, "list_cannot_infer", "", "let a = [];");
    test_compiler_status(kErrCannotInfer, "list_cannot_infer_binop", "", "let a = [] ++ [];");
    test_compiler_status(kErrCannotInfer, "list_use_before_inference", "", "let a = []; let b = #a;");
    test_compiler_status(kErrIncompatibleTypes, "list_incompatible_types", "", "let a = [1]; a = [2.0];");
    test_compiler_status(kErrIncompatibleTypes, "list_incompatible_types_2", "", "let a = []; if true {a = [0];} else {a = [true];}");
    test_compiler_status(kErrIncompatibleTypes, "list_mixed_types", "", "let a = [1, 2, 3, 4, '5'];");
    test_compiler_status(kErrIncompatibleTypes, "list_mixed_nesting", "", "let a = [[[1]], [[2]], [3]];");
    test_runtime_status(PAW_EINDEX, "list_out_of_bounds_get", "fn f(list: [int]) -> int {list[100]}", "f([]);");
    test_runtime_status(PAW_EINDEX, "list_out_of_bounds_set", "fn f(list: [int]) {list[100] = 100}", "f([]);");
    test_runtime_status(PAW_EINDEX, "list_pop_while_empty", "fn f(list: [int]) -> int {list.pop()}", "f([]);");
}

static void test_map_error(void)
{
    test_compiler_status(kErrCyclicType, "map_cyclic_type", "", "let x = [:]; x = [\"cyclic\": x];");
    test_compiler_status(kErrCyclicType, "map_nested_cyclic_type", "", "let x = [:]; x = [\"cyclic\": [\"nested\": x]];");
    test_compiler_status(kErrCannotInfer, "map_cannot_infer", "", "let a = [:];");
    test_compiler_status(kErrCannotInfer, "map_use_before_inference", "", "let a = [:]; let b = #a;");
    test_compiler_status(kErrIncompatibleTypes, "map_incompatible_types", "", "let a = [1: 2]; a = [3: 4.0];");
    test_compiler_status(kErrIncompatibleTypes, "map_incompatible_types_2", "", "let a = [:]; if true {a = [0: 0];} else {a = [1: true];}");
    test_compiler_status(kErrIncompatibleTypes, "map_mixed_types", "", "let a = [1: 2, 3: 4, 5: '6'];");
    test_compiler_status(kErrIncompatibleTypes, "map_mixed_nesting", "", "let a = [1: [1: 1], 2: [2: 2], 3: [3: [3: 3]]];");
    test_compiler_status(kErrFalseObligation, "map_unhashable_literal_key", "", "let map = [[1]: 1];");
    test_compiler_status(kErrFalseObligation, "map_unhashable_type_key", "", "let map: [[int]: int] = [:];");
}

static void test_range_error(void)
{
    test_runtime_status(PAW_ERUNTIME, "list_start_out_of_bounds", "", "let x = [1, 2, 3]; let _ = x[-4..];");
    test_runtime_status(PAW_ERUNTIME, "list_end_out_of_bounds", "", "let x = [1, 2, 3]; let _ = x[..4];");
    test_runtime_status(PAW_ERUNTIME, "list_range_out_of_order", "", "let x = [1, 2, 3]; let _ = x[2..1];");

    test_runtime_status(PAW_ERUNTIME, "str_start_out_of_bounds", "", "let x = \"abc\"; let _ = x[-4..];");
    test_runtime_status(PAW_ERUNTIME, "str_end_out_of_bounds", "", "let x = \"abc\"; let _ = x[..4];");
    test_runtime_status(PAW_ERUNTIME, "str_range_out_of_order", "", "let x = \"abc\"; let _ = x[2..1];");
}

static void test_import_error(void)
{
    test_compiler_status(kErrUnknownPath, "unrecognized_import", "use import_not_found;", "");
    test_compiler_status(kErrUnknownPath, "unrecognized_import_item", "use io::NotFound;", "");
    test_compiler_status(kErrUnknownPath, "missing_import_item", "use io;", "let t = io::NotFound;");
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
        "    Second(*Choice),\n"
        "}\n";

    test_compiler_status(kErrNonexhaustivePatternMatch, "match_int_non_exhaustive", enumeration,
        "match 123 {\n"
        "    123 => {},\n"
        "}\n");
    test_compiler_status(kErrNonexhaustivePatternMatch, "match_variant_non_exhaustive", enumeration,
        "match Choice::First {\n"
        "    Choice::First => {},\n"
        "}\n");

    test_compiler_status(kErrNonexhaustivePatternMatch, "match_variant_non_exhaustive_2", enumeration,
        "match Choice::First {\n"
        "    Choice::First => {},\n"
        "    Choice::Second(*Choice::First) => {},"
        "}\n");

    test_compiler_status(kErrNonexhaustivePatternMatch, "match_variant_non_exhaustive_3", enumeration,
        "match Choice::First {\n"
        "    Choice::First => {},\n"
        "    Choice::Second(*Choice::First) => {},"
        "    Choice::Second(*Choice::Second(*Choice::First)) => {},"
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
        "    Choice::Second(*Choice::First) => {},"
        "    Choice::Second(*Choice::Second(_)) => {},"
        "}\n");
    test_compiler_status(PAW_OK, "sanity_check_match_variant_exhaustive_3", enumeration,
        "match Choice::First {\n"
        "    Choice::First => {},\n"
        "    Choice::Second(*Choice::First) => {},"
        "    Choice::Second(*Choice::Second(*Choice::First)) => {},"
        "    Choice::Second(*Choice::Second(*Choice::Second(_))) => {},"
        "}\n");

    test_invalid_case("duplicate_binding", kErrDuplicateBinding, "",
        "(0, 0)", "(x, x)");
    test_invalid_case("duplicate_binding_nested", kErrDuplicateBinding, "",
        "(((0,),), 0)", "(((x,),), x)");
    test_invalid_case("or_binding_missing", kErrMissingBindingInAlternative, "",
        "(0, 0)", "(x, 2) | (2, 3)");
    test_invalid_case("or_binding_unrecognized", kErrMissingBindingInAlternative, "",
        "(0, 0)", "(1, 2) | (x, 3)");
    test_invalid_case("or_binding_unrecognized_int", kErrMissingBindingInAlternative, "",
        "0", "0 | x");
    test_invalid_case("or_binding_missing_int", kErrMissingBindingInAlternative, "",
        "0", "x | 0");
    // 'x' has a different type in each alternative
    test_invalid_case("or_binding_type_mismatch", kErrIncompatibleTypes, "",
        "(0, \"\")", "(x, \"b\") | (1, x)");
}

static void test_match_error(void)
{
    test_variant_match_error();
}

static void test_uninit_local(void)
{
    test_compiler_status(kErrCannotInfer, "uninit_var", "", "let x; x;"); // type of "x" cannot be inferred
    test_compiler_status(kErrUseBeforeInitialization, "uninit_int", "", "let x: int; let y = x;");
    test_compiler_status(kErrUseBeforeInitialization, "uninit_if_without_else", "", "let x; if true {x = 1;} let y = x;");
    test_compiler_status(kErrUseBeforeInitialization, "uninit_ifelse", "", "let x; if true {x = 1;} else {} let y = x;");
    test_compiler_status(kErrUseBeforeInitialization, "uninit_ifelse_chain", "", "let x; if true {x = 1;} else if true {} else {x = 3;} let y = x;");
    test_compiler_status(kErrUseBeforeInitialization, "uninit_ifelse_return", "", "let x; if true {return;} else if true {x = 2;} else {} let y = x;");
    test_compiler_status(kErrUseBeforeInitialization, "uninit_match", "",
        "let x;\n"
        "match 123 {\n"
        "    123 => x = 1,\n"
        "    _ => {},\n" // missing assignment to "x"
        "}\n"
        "let y = x;");
    test_compiler_status(kErrUseBeforeInitialization, "uninit_match_nested", "",
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
#define TRAIT \
    "pub trait Trait {\n" \
    "    fn f(self);\n" \
    "}\n"

    test_compiler_status(kErrTraitImplMissingAssocItem, "trait_missing_method",
        TRAIT "struct S; impl Trait for S {}", "");
    test_compiler_status(kErrVisibilityQualifierNotAllowed, "visibility_qualifier_on_trait_impl_method",
        TRAIT "struct S; impl Trait for S {pub fn f(self) -> int {123}}", "");
    test_compiler_status(kErrTraitImplAssocItemNotCompatible, "trait_wrong_type",
        TRAIT "struct S; impl Trait for S {fn f(self) -> int {123}}", "");
    test_compiler_status(kErrUnknownMethod, "generic_missing_bound",
        TRAIT "struct S; impl Trait for S {fn f(self) {}}\n"
              "pub fn call_f<T>(t: T) {t.f();}",
        "let x = S; call_f(x);");
    test_compiler_status(kErrUnexpectedTypeArguments, "trait_unexpected_generics",
        TRAIT "struct S<T>; impl<T> Trait<T> for S<T> {fn f(self) {}}\n", "");

#undef TRAIT

#define POLY_TRAIT \
    "pub trait Trait<T: Clone> {\n" \
    "    fn f(self) -> T;\n" \
    "}\n"
#define POLY_STRUCT \
    "struct S<T> {\n" \
    "    pub v: T,\n" \
    "}\n" \
    "impl<T: Clone> Trait<T> for S<T> {" \
    "    fn f(self) -> T {\n" \
    "        self.v.clone()\n" \
    "    }\n" \
    "}\n"
#define POLY_FUNCTION(g, rest) \
    "pub fn call_f<T: Trait<" g ">" rest ">(t: T) {\n" \
    "    t.f();\n" \
    "}"

    test_compiler_status(kErrFalseObligation, "trait_not_implemented",
        POLY_TRAIT "struct S;" POLY_FUNCTION("int", " + Clone"),
        "let x = S; call_f(x);");
    test_compiler_status(kErrFalseObligation, "trait_generic_mismatch",
        POLY_TRAIT POLY_STRUCT POLY_FUNCTION("int", " + Clone"),
        "let x = S{v: true}; call_f(x);");
    test_compiler_status(kErrExpectedTrait, "trait_type_as_trait",
        "struct Type; struct S; impl Type for S {}", "");
    test_compiler_status(kErrUnknownPath, "trait_does_not_exist",
        "struct S; impl Trait<int> for S {}", "");
    test_compiler_status(kErrUnknownPath, "trait_missing_generic_in_bounds",
        POLY_TRAIT POLY_STRUCT POLY_FUNCTION("X", " + Clone"), "");
    test_compiler_status(kErrExpectedTypeArguments, "trait_bound_missing_args",
        POLY_TRAIT POLY_STRUCT "fn call_f<T: Trait>(t: T) {t.f();}",
        "let x = S{v: 123}; call_f(x);");
    test_compiler_status(kErrExpectedTypeArguments, "trait_target_missing_args",
        POLY_TRAIT "struct S<T> {v: T}"
        "impl<T> Trait for S {fn f(self) -> T {self.v}}", "");

#undef POLY_FUNCTION
#undef POLY_STRUCT
#undef POLY_TRAIT
}

static void test_underscore(void)
{
    test_compiler_status(kErrUnexpectedSymbol, "underscore_as_generic", "fn f<_>() {}", "");
    test_compiler_status(kErrUnexpectedSymbol, "underscore_as_adt_name", "struct _;", "");
    test_compiler_status(kErrUnexpectedSymbol, "underscore_as_type_name", "type _ = int", "");
    test_compiler_status(kErrUnexpectedSymbol, "underscore_as_function_name", "fn _() {}", "");
    test_compiler_status(kErrUnexpectedSymbol, "underscore_as_method_name", "struct S; impl S {fn _() {}}", "");
    test_compiler_status(kErrUnexpectedSymbol, "underscore_as_field_name", "struct S {_: int}", "");
    test_compiler_status(kErrUnexpectedSymbol, "underscore_as_bound", "fn f<T: _>(t: T) {}", "");
    test_compiler_status(kErrUnexpectedUnderscore, "underscore_in_bound", "fn f<T: Trait<_>>(t: T) {}", "");
    test_compiler_status(kErrUnexpectedUnderscore, "underscore_as_return_type", "fn f() -> _ {}", "");
    test_compiler_status(kErrUnexpectedUnderscore, "underscore_as_parameter_type", "fn f(v: _) {}", "");
    test_compiler_status(kErrUnexpectedUnderscore, "underscore_as_field_type", "struct S {value: _}", "");
    test_compiler_status(kErrUnexpectedUnderscore, "underscore_in_parameter", "fn f(v: Option<_>) {}", "");
    test_compiler_status(kErrUnexpectedUnderscore, "underscore_in_field_type", "struct S {value: Option<_>}", "");

    test_compiler_status(kErrIncompatibleTypes, "underscore_bad_scalar_inference",
        "fn f(b: bool) {let v: _ = if b {1} else {'a'};}", "");
    test_compiler_status(kErrIncompatibleTypes, "underscore_bad_array_inference",
        "fn f(b: bool) {let v: [1]_; if b {v = [1]} else {v = ['a']};}", "");
}

static void test_global_const(void)
{
    test_compiler_status(kErrNonprimitiveConstant, "const_struct", "struct X; const C: X = X;", "");
    test_compiler_status(kErrNonprimitiveConstant, "const_enum", "enum X {E} const C: X = X::E;", "");
    test_compiler_status(kErrCannotConstantEvaluate, "const_call", "fn f() {} const C: () = f();", "");
    test_compiler_status(kErrNonprimitiveConstant, "const_function", "fn f() {} const C: fn() = f;", "");

    test_compiler_status(kErrReturnOutsideFunction, "const_return", "const C: () = return;", "");
    test_compiler_status(kErrJumpOutsideLoop, "const_break", "const C: () = break;", "");
    test_compiler_status(kErrJumpOutsideLoop, "const_continue", "const C: () = continue;", "");
    test_compiler_status(kErrChainOutsideFunction, "const_chain", "const C: Option<int> = Option::Some(123)?;", "");

    test_compiler_status(kErrGlobalConstantCycle, "const_cycle_1",
            "const C: int = C;", "");
    test_compiler_status(kErrGlobalConstantCycle, "const_cycle_2",
            "const C1: int = C2 + 1;"
            "const C2: int = 1 + C1;", "");
    test_compiler_status(kErrGlobalConstantCycle, "const_cycle_3",
            "const C1: int = C2 + 1;"
            "const C2: int = 1 + C3;"
            "const C3: int = C1 + 1;", "");

    // TODO: need to store constants in MirPlace during earlier middle-end phases, in part to allow detection of this case
//    test_compiler_status(kErrModifiedConstant, "const_assignment", "const C: int = 1;", "C = 2;");
}

static void test_annotations(void)
{
//    test_compiler_status(kErrInitializedExternConstant, "const_unexpected_initializer", "#[extern] const C: int = 42;", "");
    test_compiler_status(kErrExternFunctionBody, "function_unexpected_body", "#[extern] pub fn f() {}", "");
    // NOTE: "not_extern" annotation doesn't do anything
    test_compiler_status(kErrUninitializedConstant, "const_expected_initializer", "#[not_extern] const C: int;", "");
    test_compiler_status(kErrMissingFunctionBody, "function_expected_body", "#[not_extern] pub fn f();", "");
}

static void test_destructuring(void)
{
    char const *structure = "struct Fields {pub a: int, pub b: int}";
    test_compiler_status(kErrDuplicateBinding, "destructure_duplicate_binding", "", "let (x, (x,)) = (1, (2,));");
    test_compiler_status(kErrIncompatibleTypes, "destructure_wrong_type", "", "let (a, (b,)) = (1, 2);");
    test_compiler_status(kErrIncompatibleTypes, "destructure_too_many_elems", "", "let (a, b) = (1, 2, 3);");
    test_compiler_status(kErrIncompatibleTypes, "destructure_not_enough_elems", "", "let (a, b, c) = (1, 2);");
    test_compiler_status(kErrMissingField, "destructure_missing_field", structure,
            "let Fields{a} = Fields{a: 1, b: 2};");
    test_compiler_status(kErrUnknownField, "destructure_extra_field", structure,
            "let Fields{a, b, c} = Fields{a: 1, b: 2};");
    test_compiler_status(kErrNonexhaustivePatternMatch, "destructure_non_exhaustive", "", "let Option::Some(x) = Option::Some(1);");
    test_compiler_status(kErrExpectedExpression, "destructure_wildcard_name", "", "let _ = 123; let x = _;");
    test_compiler_status(kErrNonexhaustivePatternMatch, "destructure_or", "", "let (a, 1) | (a, 2) = (123, 456);");
    test_compiler_status(kErrUninitializedDestructuring, "uninitialized_destructuring", "", "let (a,); a = 123;");
    test_compiler_status(kErrUseOfReservedIdentifier, "reserved_identifier", "", "let int = 123;");
}

static void test_deferred_init(void)
{
    test_compiler_status(kErrUseBeforeInitialization, "use_before_init", "", "let a; let b = a; b = 123;");
//TODO    test_compiler_status(kErrUseBeforeInitialization, "capture_before_init", "", "let a; let f = || -> int {a};");
    test_compiler_status(kErrUseBeforeInitialization, "missing_init_in_branch", "", "let a; if true {a = 1;} let b = a;");
    test_compiler_status(kErrUseBeforeInitialization, "use_in_branch", "", "let a; if true {a = 1;} else {let b = a;}");
    test_compiler_status(kErrUseBeforeInitialization, "uninit_if_else", "", "let a; if true {a = 1;} else if true {return;} else {} let b = a;");
}

#define CODELINE(Line_) Line_ "\n"

static void test_projections(void)
{
#define HEADER \
    CODELINE("pub trait Trait {") \
    CODELINE("  type Type;") \
    CODELINE("  fn method(*self);") \
    CODELINE("}") \
    CODELINE("struct Struct;") \
    CODELINE("impl Trait for Struct {") \
    CODELINE("  type Type = int;") \
    CODELINE("  fn method(*self) {}") \
    CODELINE("}")

    test_compiler_status(kErrUnknownAssociatedItem, "unknown_assoc_item_path",
            HEADER "fn f<T: Trait>(t: T, item: T::Nonexistent) {}", "");
    test_compiler_status(kErrMultipleApplicableItems, "ambiguous_assoc_item_path",
            HEADER "pub trait Trait2 { type Type; }"
            "fn f<T: Trait + Trait2>(t: T, item: T::Type) {}", "");

    test_compiler_status(kErrUnknownAssociatedItem, "unknown_assoc_item_projection",
            HEADER "fn f<T: Trait>(t: T, item: <T as Trait>::Nonexistent) {}", "");
    test_compiler_status(kErrNone, "SANITY_CHECK_disambiguate_assoc_item_using_projection",
            HEADER "pub trait Trait2 { type Type; }"
            "fn f<T: Trait + Trait2>(t: T, item: <T as Trait>::Type) {}", "");

    test_compiler_status(kErrUnknownMethod, "unknown_assoc_item_path_expr",
            HEADER "fn f<T: Trait>(t: T) {t.nonexistent();}", "");
    test_compiler_status(kErrMultipleApplicableItems, "ambiguous_assoc_item_path_expr",
            HEADER "pub trait Trait2 { fn method(*self); }"
            "fn f<T: Trait + Trait2>(t: T) {t.method();}", "");

    test_compiler_status(kErrUnknownMethod, "unknown_assoc_item_projection_expr",
            HEADER "fn f<T: Trait>(t: T) {<T as Trait>::nonexistent(&t);}", "");
    test_compiler_status(kErrNone, "SANITY_CHECK_disambiguate_assoc_item_using_projection_expr",
            HEADER
            CODELINE("pub trait Trait2 {")
            CODELINE("  type Type;")
            CODELINE("  fn method(*self);")
            CODELINE("}")
            CODELINE("fn f<T: Trait + Trait2>(t: T) {")
            CODELINE("  <T as Trait>::method(&t);")
            CODELINE("  <T as Trait2>::method(&t);")
            CODELINE("}"), "");

#undef HEADER
}

static void test_interpolation(void)
{
    test_compiler_status(kErrExpectedExpression, "extra_close_braces", "", "let s = \"\\{103 +} 20}\";");
    test_compiler_status(kErrUnterminatedStrLiteral, "mismatched_braces", "", "let s = \"\\{100 + 20 + 3\";");
    test_compiler_status(kErrExpectedSymbol, "mismatched_braces_nested", "", "let s = \"\\{\"abc\" + \"\\{100 + 20 + 3\"}\";");
    test_compiler_status(kErrUnterminatedStrLiteral, "missing_expr_close", "", "let s = \"abc\\{123\";");
    test_compiler_status(kErrUnterminatedStrLiteral, "only_expr_open", "", "let s = \"\\{\";");
    test_compiler_status(kErrExpectedExpression, "empty_expr", "", "let s = \"\\{}\";");
    test_compiler_status(kErrUnterminatedStrLiteral, "missing_close_braces", "", "let s = \"\\{123\";");

    // looks like the interpolated expression is a block followed by an unterminated string literal
    test_compiler_status(kErrUnterminatedStrLiteral, "missing_close_braces_2", "", "let s = \"\\{{123}\";");
}

static void test_panic(void)
{
    test_runtime_status(PAW_ERUNTIME, "panic", "", "panic(\"panic message\");");
}

static void test_divergence(void)
{
#define FUNC(Text_) "fn f(x: int) -> int {" Text_ "}"

    test_compiler_status(kErrIncompatibleTypes, "non_exhaustive_branch",
        FUNC("if x == 0 {} else {123}"), "");
    test_compiler_status(kErrIncompatibleTypes, "non_exhaustive_return",
        FUNC("if x == 0 {return 123;}"), "");

    // The loop might break, depending on "x", and there is no return or result expression at the bottom
    // of the function. If the loop has type "!" then the expression is well-typed. If there is a "break",
    // then the loop has type "()".
    test_compiler_status(PAW_OK, "exhaustive_loop",
        FUNC("loop {if x == 0 {return x;}}"), "");
    test_compiler_status(PAW_OK, "exhaustive_loop_2",
        FUNC("loop {if x == 0 {return x;} else {}}"), "");
    test_compiler_status(kErrIncompatibleTypes, "nonexhaustive_loop",
        FUNC("loop {if x == 0 {break;}}"), "");
    test_compiler_status(kErrIncompatibleTypes, "nonexhaustive_loop_2",
        FUNC("loop {if x == 0 {} else if x == 1 {} else {break;}}"), "");

    test_compiler_status(PAW_OK, "type_after_return",
        FUNC("if x == 0 {return 123; x} else {x}"), "");
//TODO    test_compiler_status(kErrIncompatibleTypes, "wrong_type_after_return",
//TODO        FUNC("if x == 0 {return 123; \"abc\"} else {x}"), "");

#undef FUNC

    test_runtime_status(PAW_ERUNTIME, "custom_diverging_function",
        "fn diverge() -> ! {panic(\"diverging\")}", "diverge();");
    test_runtime_status(PAW_ERUNTIME, "custom_diverging_function_2",
        "fn diverge() -> ! {if true {panic(\"first divergence\")} else {panic(\"second divergence\")}}", "diverge();");

    // TODO: Need to throw a compiler error when a function is lying about its divergence status
    //       i.e. it has a return type of "!" but does not unconditionally call a diverging function.
    //       This should be checked when building the MIR or in a later pass.
#if 0
    test_compiler_status(kErrExpectedDivergence, "function_lies_about_divergence",
        "fn diverge() -> ! {}", "");
    test_compiler_status(kErrExpectedDivergence, "function_lies_about_divergence_2",
        "fn diverge() -> ! {if true {panic(\"conditionally diverge\")}}", "");
    test_compiler_status(kErrExpectedDivergence, "function_lies_about_divergence_3",
        "fn diverge() -> ! {match 123 {0 => return, _ => panic(\"conditionally diverge\")}}", "");
#endif // 0
}

static void test_impl_error(void)
{
#define GENERATE(TraitBody_, ImplBody_) \
        "struct S; pub trait T {" TraitBody_ "} impl T for S {" ImplBody_ "}"
    test_compiler_status(kErrTraitImplMissingAssocItem, "missing_trait_method",
            GENERATE("fn f();", ""), "");
    test_compiler_status(kErrTraitImplAssocItemNotCompatible, "invalid_trait_method_type",
            GENERATE("fn f();", "fn f(x: int) {}"), "");
    test_compiler_status(kErrTraitImplUnknownAssocItem, "extra_trait_method",
            GENERATE("", "fn f() {}"), "");
#undef GENERATE

    test_compiler_status(kErrExpectedTypeArguments, "missing_type_args",
            "struct Struct<T>; pub trait Trait {} impl Trait for Struct {}", "");

#define TESTCASE(A_, B_) \
    CODELINE("struct Struct;") \
    CODELINE("pub trait Constraint {}") \
    CODELINE("pub trait Trait {") \
    CODELINE("    fn method<T"A_">(*self, y: T);") \
    CODELINE("}") \
    CODELINE("impl Trait for Struct {") \
    CODELINE("    fn method<T"B_">(*self, y: T) {}") \
    CODELINE("}")

    test_compiler_status(kErrFalseObligation, "missing_constraint_on_method_arg",
            TESTCASE(": Constraint", ""), "");
    test_compiler_status(kErrFalseObligation, "extra_constraint_on_method_arg",
            TESTCASE("", ": Constraint"), "");

#undef TESTCASE

}

static void test_deref_pat(void)
{
    test_compiler_status(kErrMoveOutOfPointer, "bind_noncopyable_type",
            "struct Resource;",
            "match &Resource {*binding => {}}");

    test_compiler_status(kErrMoveOutOfPointer, "bind_noncopyable_type_in_struct",
            "struct Resource; struct Container {pub r: Resource}",
            "let c = Container{r: Resource}; match &c {*Container{r} => {}}");

    // TODO: make Copy::copy call clone under-the-hood, add Self: Clone bound on Copy
//    test_compiler_status(kErrNone, "deref_noncopyable_type",
//            "struct Resource; struct Container {pub r: Resource}"
//            "impl Copy for Resource {}"
//            "impl Clone for Resource {"
//            "    fn clone(*self) -> Self {Resource}"
//            "}",
//            "let c = Container{r: Resource};"
//            "match &c {*Container{r} => {}}");
}

static void test_definite_assignment(void)
{
#define TESTCASE(Name_, Status_, Code_) \
        test_compiler_status(Status_, #Name_, COMMON_TYPES, Code_);

    char const *COMMON_TYPES =
        "struct Copyable; impl Copy for Copyable {}\n"
        "struct MoveOnly;\n";

    TESTCASE(SANITY_CHECK_deferred_init, 0,
            "let first: MoveOnly;"
            "first = MoveOnly;"
            "let second = first;");

    TESTCASE(SANITY_CHECK_reinitialize, 0,
            "let first = MoveOnly;"
            "let second = first;" // move out
            "first = MoveOnly;" // reinitialize
            "let third = first;");

    TESTCASE(SANITY_CHECK_sometimes_copied, 0,
            "let first = Copyable;"
            "if true { let second = first; }"
            "let third = first;");

    TESTCASE(SANITY_CHECK_copy_out_of_object, 0,
            "let first = (Copyable,);"
            "let second = first.0;");

    TESTCASE(SANITY_CHECK_always_reinitialized, 0,
            "let first = MoveOnly;"
            "if true {"
            "    let second = first;"
            "    first = MoveOnly;"
            "} else {"
            "    let second = first;"
            "    first = MoveOnly;"
            "}"
            "let third = first;");

    TESTCASE(SANITY_CHECK_copy_out_of_pointer, 0,
            "let value = Copyable;"
            "let pointer = &value;"
            "let new_value = *pointer;");

    TESTCASE(use_before_init, kErrUseBeforeInitialization,
            "let first: Copyable;"
            "let second = first;");

    TESTCASE(use_after_move, kErrUseAfterMove,
            "let first = MoveOnly;"
            "let second = first;"
            "let third = first;");

    TESTCASE(sometimes_uninit, kErrUseBeforeInitialization,
            "let first: int;"
            "if true { first = 42; }"
            "let second = first;");

    TESTCASE(sometimes_moved, kErrUseAfterMove,
            "let first = MoveOnly;"
            "if true { let second = first; }"
            "let third = first;");

    TESTCASE(move_out_of_object, kErrMoveOutOfField,
            "let first = (MoveOnly,);"
            "let second = first.0;");

    TESTCASE(sometimes_reinitialized, kErrUseAfterMove,
            "let first = MoveOnly;"
            "if true {"
            "    let second = first;"
            "    first = MoveOnly;"
            "} else {"
            "    let second = first;"
            "}"
            "let third = first;");

    TESTCASE(move_out_of_pointer, kErrMoveOutOfPointer,
            "let value = MoveOnly;"
            "let pointer = &value;"
            "let new_value = *pointer;");

    TESTCASE(move_twice_into_pointer, kErrUseAfterMove,
            "let pointee = MoveOnly;"
            "let pointer = &pointee;"
            "*pointer = pointee;"
            "*pointer = pointee;");

#undef TESTCASE
}

int main(void)
{
#define TESTCASE(A_, B_) \
    CODELINE("struct Struct;") \
    CODELINE("pub trait Constraint {}") \
    CODELINE("pub trait Trait {") \
    CODELINE("    fn method<T"A_">(*self, y: T);") \
    CODELINE("}") \
    CODELINE("impl Trait for Struct {") \
    CODELINE("    fn method<T"B_">(*self, y: T) {}") \
    CODELINE("}")

    test_compiler_status(kErrFalseObligation, "missing_constraint_on_method_arg",
            TESTCASE(": Constraint", ""), "");
    test_compiler_status(kErrFalseObligation, "extra_constraint_on_method_arg",
            TESTCASE("", ": Constraint"), "");
    return 42;
    test_syntax_error();
    test_underscore();
    test_annotations();
    test_enum_error();
    test_name_error();
    test_type_error();
    test_definite_assignment();
    test_closure_error();
    test_arithmetic_error();
    test_tuple_error();
    test_struct_error();
    test_impl_error();
    test_deref_pat();
//    test_list_error();
//    test_map_error();
    test_range_error();
    test_import_error();
    test_uninit_local();
    test_global_const();
    test_match_error();
    test_trait_error();
    test_destructuring();
    test_deferred_init();
    test_projections();
//    test_interpolation();
    test_panic();
    test_divergence();
}
