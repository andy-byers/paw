#include "auxlib.h"
#include "call.h"
#include "lib.h"
#include "opcode.h"
#include "os.h"
#include "rt.h"
#include "test.h"
#include <limits.h>

static struct TestAlloc s_alloc;
typedef uint64_t TypeSet;

static void write_main(char *out, const char *items, const char *text)
{
#define ADD_CHUNK(o, p) \
        memcpy(o, p, strlen(p)); \
        (o) += strlen(p);
    ADD_CHUNK(out, items);
    ADD_CHUNK(out, "pub fn main() {\n");
    ADD_CHUNK(out, text);
    ADD_CHUNK(out, "}\n");
    *out++ = '\0';
#undef ADD_CHUNK
}

static void test_compiler_error(int expect, const char *name, const char *item, const char *text)
{
    char buffer[4096];
    write_main(buffer, item, text);

    paw_Env *P = test_open(NULL, &s_alloc);
    int status = pawL_load_chunk(P, name, buffer);
    check(status == expect);

    test_close(P, &s_alloc);
}

static void test_runtime_error(int expect, const char *name, const char *item, const char *text)
{
    char buffer[4096];
    write_main(buffer, item, text);

    paw_Env *P = test_open(NULL, &s_alloc);
    int status = pawL_load_chunk(P, name, buffer);
    check(status == PAW_OK);

    paw_push_string(P, "main");
    const int g = paw_find_public(P);
    paw_push_public(P, g);

    status = paw_call(P, 0);
    check(status == expect);

    test_close(P, &s_alloc);
}

static void test_name_error(void)
{
    test_compiler_error(PAW_ENAME, "use_before_def_local", "", "let x = x");
    test_compiler_error(PAW_ENAME, "undef_local", "", "x = 1");
    test_compiler_error(PAW_ENAME, "undef_upvalue", "fn f() {x = 1}", " f()");
    test_compiler_error(PAW_ENAME, "undef_field", "struct A {}", "let a = A{}.value");
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
        case PAW_TSTRING:
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
    snprintf(text_buf, sizeof(text_buf), "let x = %s%s",
             op, get_literal(k));

    test_compiler_error(PAW_ETYPE, name_buf, "", text_buf);
}

static void check_unification_errors(void)
{
    for (int k = PAW_TUNIT; k <= PAW_TSTRING; ++k) {
        for (int k2 = PAW_TUNIT; k2 <= PAW_TSTRING; ++k2) {
            if (k == k2) {
                continue;
            }
            char name_buf[256] = {0};
            snprintf(name_buf, sizeof(name_buf), "unification_error(%s, %s)",
                     get_literal(k), get_literal(k2));

            char text_buf[256] = {0};
            snprintf(text_buf, sizeof(text_buf), "let x = %s; let y = %s; x = y",
                     get_literal(k), get_literal(k2));

            test_compiler_error(PAW_ETYPE, name_buf, "", text_buf);
        }
    }
}

static void check_binop_type_error(const char *op, paw_Type k, paw_Type k2)
{
    char name_buf[256] = {0};
    snprintf(name_buf, sizeof(name_buf), "binop_type_error('%s', %s, %s)",
             op, get_literal(k), get_literal(k2));

    char text_buf[256] = {0};
    snprintf(text_buf, sizeof(text_buf), "let x = %s %s %s",
             get_literal(k), op, get_literal(k2));

    test_compiler_error(PAW_ETYPE, name_buf, "", text_buf);
}

static void check_binop_type_errors(const char *op, paw_Type *types)
{
    for (int k = PAW_TUNIT; k <= PAW_TSTRING; ++k) {
        for (int k2 = PAW_TUNIT; k2 <= PAW_TSTRING; ++k2) {
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
    check_unop_type_error("-", PAW_TSTRING);
    check_unop_type_error("~", PAW_TUNIT);
    check_unop_type_error("~", PAW_TBOOL);
    check_unop_type_error("~", PAW_TFLOAT);
    check_unop_type_error("~", PAW_TSTRING);

#define mklist(...) \
    (paw_Type[]) { __VA_ARGS__, -1 }
#define mklist0() \
    (paw_Type[]) { -1 }
    check_binop_type_errors("+", mklist(PAW_TINT, PAW_TFLOAT, PAW_TSTRING));
    check_binop_type_errors("-", mklist(PAW_TINT, PAW_TFLOAT));
    check_binop_type_errors("*", mklist(PAW_TINT, PAW_TFLOAT));
    check_binop_type_errors("%", mklist(PAW_TINT, PAW_TFLOAT));
    check_binop_type_errors("/", mklist(PAW_TINT, PAW_TFLOAT));
    check_binop_type_errors("&", mklist(PAW_TINT));
    check_binop_type_errors("|", mklist(PAW_TINT));
    check_binop_type_errors("^", mklist(PAW_TINT));
    check_binop_type_errors("<", mklist(PAW_TINT, PAW_TFLOAT, PAW_TSTRING));
    check_binop_type_errors(">", mklist(PAW_TINT, PAW_TFLOAT, PAW_TSTRING));
    check_binop_type_errors("<=", mklist(PAW_TINT, PAW_TFLOAT, PAW_TSTRING));
    check_binop_type_errors(">=", mklist(PAW_TINT, PAW_TFLOAT, PAW_TSTRING));
    check_binop_type_errors("==", mklist(PAW_TBOOL, PAW_TINT, PAW_TFLOAT, PAW_TSTRING));
    check_binop_type_errors("!=", mklist(PAW_TBOOL, PAW_TINT, PAW_TFLOAT, PAW_TSTRING));

    test_compiler_error(PAW_ETYPE, "call_unit_variant", "enum E {X}", "let x = E::X()");
    test_compiler_error(PAW_ETYPE, "wrong_constructor_args", "enum E {X(int)}", "let x = E::X(1.0)");
}

static void test_syntax_error(void)
{
    test_compiler_error(PAW_ESYNTAX, "overflow_integer", "", "let d = -9223372036854775808"); // overflows before '-' applied
    test_compiler_error(PAW_ESYNTAX, "binary_digit_range", "", "let b = 0b001201");
    test_compiler_error(PAW_ESYNTAX, "octal_digit_range", "", "let o = 0o385273");
    test_compiler_error(PAW_ESYNTAX, "hex_digit_range", "", "let x = 0x5A2CG3");
    test_compiler_error(PAW_ESYNTAX, "malformed_binary", "", "let b = 0b00$101");
    test_compiler_error(PAW_ESYNTAX, "malformed_octal", "", "let o = 0o37=273");
    test_compiler_error(PAW_ESYNTAX, "malformed_hex", "", "let x = 0y5A2CF3");
    test_compiler_error(PAW_ESYNTAX, "stmt_after_return", "fn f() {return; f()}", "");
    test_compiler_error(PAW_ESYNTAX, "missing_right_paren", "fn f(a: int, b: int, c: int -> int {return (a + b + c)}", "");
    test_compiler_error(PAW_ESYNTAX, "missing_left_paren", "fn fa: int, b: int, c: int) -> int {return (a + b + c)}", "");
    test_compiler_error(PAW_ESYNTAX, "missing_right_curly", "fn f(a: int, b: int, c: int) -> int {return (a + b + c)", "");
    test_compiler_error(PAW_ESYNTAX, "missing_left_curly", "fn f(a: int, b: int, c: int) -> int return (a + b + c)}", "");
    test_compiler_error(PAW_ESYNTAX, "missing_right_angle", "fn f<A, B, C() {}", "");
    test_compiler_error(PAW_ESYNTAX, "missing_left_angle", "fn fA, B, C>() {}", "");
    test_compiler_error(PAW_ESYNTAX, "missing_turbo", "struct A<T> {}", "let a = A<int>{}");
    test_compiler_error(PAW_ESYNTAX, "partial_turbo", "struct A<T> {}", "let a = A:<int>{}");
    test_compiler_error(PAW_ESYNTAX, "missing_left_angle_tubofish", "struct A<T> {}", "let a = A::int>{}");
    test_compiler_error(PAW_ESYNTAX, "missing_right_angle_turbofish", "struct A<T> {}", "let a = A::<int{}");
    test_compiler_error(PAW_ESYNTAX, "square_bracket_generics", "fn f[A, B, C]() {}", "");
    test_compiler_error(PAW_ESYNTAX, "nested_fn", "", "fn f() {}");
    test_compiler_error(PAW_ESYNTAX, "nested_struct", "", "struct S {x: int}");
    test_compiler_error(PAW_ESYNTAX, "nested_enum", "", "enum E {X}");
    test_compiler_error(PAW_ESYNTAX, "toplevel_var", "let v = 1", "");
}

static void test_arithmetic_error(void)
{
    test_runtime_error(PAW_ERUNTIME, "division_by_0_int", "", "let x = 1 / 0");
    test_runtime_error(PAW_ERUNTIME, "division_by_0_float", "", "let x = 1.0 / 0.0");
    test_runtime_error(PAW_ERUNTIME, "negative_left_shift", "", "let x = 1 << -2");
    test_runtime_error(PAW_ERUNTIME, "negative_right_shift", "", "let x = 1 >> -2");
}

static void test_struct_error(void)
{
    test_compiler_error(PAW_ENAME, "struct_missing_field", "struct A {a: int}", "let a = A{}");
    test_compiler_error(PAW_ENAME, "struct_extra_field", "struct A {a: int}", "let a = A{a: 1, b: 2}");
    test_compiler_error(PAW_ENAME, "struct_duplicate_field", "struct A {a: int}", "let a = A{a: 1, a: 1}");
    test_compiler_error(PAW_ENAME, "struct_wrong_field", "struct A {a: int}", "let a = A{b: 2}");
}

static void test_vector_error(void)
{
    test_compiler_error(PAW_ETYPE, "vector_cannot_infer", "", "let a = []");
    test_compiler_error(PAW_ETYPE, "vector_cannot_infer_binop", "", "let a = [] + []");
    test_compiler_error(PAW_ETYPE, "vector_use_before_inference", "", "let a = []; let b = #a");
    test_compiler_error(PAW_ETYPE, "vector_incompatible_types", "", "let a = [1]; a = [2.0]");
    test_compiler_error(PAW_ETYPE, "vector_incompatible_types_2", "", "let a = []; if true {a = [0]} else {a = [true]}");
    test_compiler_error(PAW_ETYPE, "vector_mixed_types", "", "let a = [1, 2, 3, 4, '5']");
    test_compiler_error(PAW_ETYPE, "vector_mixed_nesting", "", "let a = [[[1]], [[2]], [3]]");
}

static void test_map_error(void)
{
    test_compiler_error(PAW_ETYPE, "map_cannot_infer", "", "let a = [:]");
    test_compiler_error(PAW_ETYPE, "map_use_before_inference", "", "let a = [:]; let b = #a");
    test_compiler_error(PAW_ETYPE, "map_incompatible_types", "", "let a = [1: 2]; a = [3: 4.0]");
    test_compiler_error(PAW_ETYPE, "map_incompatible_types_2", "", "let a = [:]; if true {a = [0: 0]} else {a = [1: true]}");
    test_compiler_error(PAW_ETYPE, "map_mixed_types", "", "let a = [1: 2, 3: 4, 5: '6']");
    test_compiler_error(PAW_ETYPE, "map_mixed_nesting", "", "let a = [1: [1: 1], 2: [2: 2], 3: [3: [3: 3]]]");
    test_compiler_error(PAW_ETYPE, "map_nonhashable_literal_key", "", "let map = [[1]: 1]");
    test_compiler_error(PAW_ETYPE, "map_nonhashable_type_key", "", "let map: [[int]: int] = [:]");
}

int main(void)
{
    test_name_error();
    test_syntax_error();
    test_type_error();
    test_arithmetic_error();
    test_struct_error();
    test_vector_error();
    test_map_error();
}
