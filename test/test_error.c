#include "auxlib.h"
#include "lib.h"
#include "os.h"
#include "opcode.h"
#include "test.h"
#include <limits.h>

static struct TestAlloc s_alloc;

typedef uint64_t TypeSet;

static void test_case(int expect, const char *name, const char *text)
{
    (void)name;
    paw_Env *P = test_open(NULL, &s_alloc);
    int status = pawL_load_chunk(P, name, text);
    if (expect == PAW_ESYNTAX) {
        CHECK(status == PAW_ESYNTAX);
    } else {
        CHECK(status == PAW_OK);
        status = paw_call(P, 0);
    }
    test_close(P, &s_alloc);
}

static void test_name_error(void)
{
    test_case(PAW_ENAME, "use_before_def_global", "let x = x");
    test_case(PAW_ENAME, "use_before_def_local", "{let x = x}");

    test_case(PAW_ENAME, "undef_global", "x = 1");
    test_case(PAW_ENAME, "undef_local", "{x = 1}");
    test_case(PAW_ENAME, "undef_upvalue", "(fn() {x = 1})()");
}

static _Bool has_type(TypeSet ts, int kind)
{
    return ts & (1 << kind);
}

static const char *get_literal(int kind)
{
    switch (kind) {
        case PAW_TNULL:
            return "null";
        case PAW_TINTEGER:
            return "123";
        case PAW_TFLOAT:
            return "1.0";
        case PAW_TBOOLEAN:
            return "true";
        case PAW_TSTRING:
            return "'abc'";
        case PAW_TARRAY:
            return "[]";
        case PAW_TMAP:
            return "{}";
        default:
            CHECK(0);
            return NULL;
    }
}

static void check_unop_type_error(const char *op, TypeSet ts)
{
    for (int k = PAW_TNULL; k <= PAW_TUSERDATA; ++k) {
        if (has_type(ts, k)) {
            char name_buf[256] = {0};
            snprintf(name_buf, sizeof(name_buf), "unop_type_error('%s', %s)",
                     op, get_literal(k));

            char text_buf[256] = {0};
            snprintf(text_buf, sizeof(text_buf), "let x = %s%s",
                     op, get_literal(k));

            test_case(PAW_ETYPE, name_buf, text_buf);
        }
    }
}

static void check_binop_type_error_(const char *op, TypeSet ts, TypeSet ts2)
{
    for (int k = PAW_TNULL; k <= PAW_TUSERDATA; ++k) {
        for (int k2 = PAW_TNULL; k2 <= PAW_TUSERDATA; ++k2) {
            if (has_type(ts, k) && has_type(ts2, k2)) {
                char name_buf[256] = {0};
                snprintf(name_buf, sizeof(name_buf), "binop_type_error('%s', %s, %s)",
                         op, get_literal(k), get_literal(k2));

                char text_buf[256] = {0};
                snprintf(text_buf, sizeof(text_buf), "let x = %s %s %s",
                         get_literal(k), op, get_literal(k2));

                test_case(PAW_ETYPE, name_buf, text_buf);
            }
        }
    }
}

static void check_binop_type_error(const char *op, TypeSet ts, TypeSet ts2)
{
    check_binop_type_error_(op, ts, ts2);
    check_binop_type_error_(op, ts2, ts);
}

#define NULL_ (1 << PAW_TNULL)
#define CONTAINER (1 << PAW_TARRAY | 1 << PAW_TMAP)
#define NON_ARITHMETIC (1 << PAW_TNULL | 1 << PAW_TSTRING | 1 << PAW_TARRAY | 1 << PAW_TMAP)
#define ARITHMETIC (1 << PAW_TINTEGER | 1 << PAW_TFLOAT)
#define FLOATING_POINT (1 << PAW_TFLOAT)

static void test_type_error(void)
{
    // Unary operators
    check_unop_type_error("-", NON_ARITHMETIC);
    check_unop_type_error("~", NON_ARITHMETIC | FLOATING_POINT);

    // Binary operators. The left-hand side might be a valid type for the operator, but the right-hand
    // side will never be.
    check_binop_type_error("+", NON_ARITHMETIC | ARITHMETIC, NON_ARITHMETIC);
    check_binop_type_error("-", NON_ARITHMETIC | ARITHMETIC, NON_ARITHMETIC);
    check_binop_type_error("*", NON_ARITHMETIC | ARITHMETIC, NON_ARITHMETIC);
    check_binop_type_error("%", NON_ARITHMETIC | ARITHMETIC, NON_ARITHMETIC);
    check_binop_type_error("/", NON_ARITHMETIC | ARITHMETIC, NON_ARITHMETIC);
    check_binop_type_error("//", NON_ARITHMETIC | ARITHMETIC, NON_ARITHMETIC);
    check_binop_type_error("++", NON_ARITHMETIC | ARITHMETIC, ARITHMETIC | CONTAINER | NULL_);
    check_binop_type_error("&", NON_ARITHMETIC | ARITHMETIC, NON_ARITHMETIC | FLOATING_POINT);
    check_binop_type_error("|", NON_ARITHMETIC | ARITHMETIC, NON_ARITHMETIC | FLOATING_POINT);
    check_binop_type_error("^", NON_ARITHMETIC | ARITHMETIC, NON_ARITHMETIC | FLOATING_POINT);
}

static void too_many_constants(void)
{
    struct TestAlloc a = {0};
    paw_Env *P = test_open(test_alloc, &a);

    Buffer buf;
    pawL_init_buffer(P, &buf);
    pawL_add_string(P, &buf, "{ let x = [");
    for (int i = 0; i < (1 << 16) + 1; ++i) {
        pawL_add_fstring(P, &buf, "%d, ", i);
    }
    // Adding 'null' to an integer causes a type error, but we should get
    // a syntax error during compilation, before the 'null' is parsed.
    pawL_add_string(P, &buf, "]}\n");
    pawL_push_result(P, &buf);

    const char *source = paw_string(P, -1);
    test_case(PAW_ESYNTAX, "too_many_constants", source);
}

static void too_many_instructions(void)
{
    struct TestAlloc a = {0};
    paw_Env *P = test_open(test_alloc, &a);

    Buffer buf;
    pawL_init_buffer(P, &buf);
    pawL_add_string(P, &buf, "{ let x = ");
    for (int i = 0; i < (1 << 16); ++i) {
        pawL_add_fstring(P, &buf, "%d + ", i);
    }
    // Adding 'null' to an integer causes a type error, but we should get
    // a syntax error during compilation, before the 'null' is parsed.
    pawL_add_string(P, &buf, "null}");
    pawL_push_result(P, &buf);

    const char *source = paw_string(P, -1);
    test_case(PAW_ESYNTAX, "too_many_instructions", source);
}

static void too_many_locals(void)
{
    struct TestAlloc a = {0};
    paw_Env *P = test_open(test_alloc, &a);

    Buffer buf;
    pawL_init_buffer(P, &buf);
    pawL_add_string(P, &buf, "{\n");
    for (int i = 0; i < (1 << 16) + 1; ++i) {
        pawL_add_string(P, &buf, "let x\n");
    }
    pawL_add_string(P, &buf, "}");
    pawL_push_result(P, &buf);

    const char *source = paw_string(P, -1);
    test_case(PAW_ESYNTAX, "too_many_locals", source);
}

static void too_far_to_jump(void)
{
    struct TestAlloc a = {0};
    paw_Env *P = test_open(test_alloc, &a);

    Buffer buf;
    pawL_init_buffer(P, &buf);
    pawL_add_string(P, &buf, "if false { let x = 1 ");
    // Perform a series of OP_PUSHCONST (1 byte op, 2 byte constant index) and
    // OP_ADD (1 byte op). This should be enough to cause a syntax error.
    for (int i = 0; i < JUMP_MAX / 4 + 1; ++i) {
        pawL_add_string(P, &buf, "+ 1 ");
    }
    pawL_add_string(P, &buf, "}");
    pawL_push_result(P, &buf);

    const char *source = paw_string(P, -1);
    test_case(PAW_ESYNTAX, "too_far_to_jump", source);
}

static void too_far_to_loop(void)
{
    struct TestAlloc a = {0};
    paw_Env *P = test_open(test_alloc, &a);

    Buffer buf;
    pawL_init_buffer(P, &buf);
    pawL_add_string(P, &buf, "for i = 0,1 { let x = 1 ");
    for (int i = 0; i < JUMP_MAX / 4 + 1; ++i) {
        pawL_add_string(P, &buf, "+ 1 ");
    }
    pawL_add_string(P, &buf, "}");
    pawL_push_result(P, &buf);

    const char *source = paw_string(P, -1);
    test_case(PAW_ESYNTAX, "too_far_to_loop", source);
}

static void test_syntax_error(void)
{
    test_case(PAW_ESYNTAX, "overflow_integer", "-9223372036854775808");
    test_case(PAW_ESYNTAX, "stmt_after_return", "fn f() {return; f()}");
    test_case(PAW_ESYNTAX, "missing_right_paren", "fn f(a, b, c {return [a + b + c]}");
    test_case(PAW_ESYNTAX, "missing_left_paren", "fn fa, b, c) {return [a + b + c]}");
    test_case(PAW_ESYNTAX, "missing_right_curly", "fn f(a, b, c) {return [a + b + c]");
    test_case(PAW_ESYNTAX, "missing_left_curly", "fn f(a, b, c) return [a + b + c]}");
    test_case(PAW_ESYNTAX, "missing_right_bracket", "fn f(a, b, c) {return [a + b + c}");
    test_case(PAW_ESYNTAX, "missing_left_bracket", "fn f(a, b, c) {return a + b + c]}");

    // The following tests are generated, since they require a lot of text.
    too_many_locals();
    too_many_instructions();
    too_many_constants();
    too_far_to_jump();
    too_far_to_loop();
}

int main(void)
{
    test_name_error();
    test_syntax_error();
    test_type_error();

    test_case(PAW_ESYNTAX, "missing_left_paren", "fn fa, b, c) {return [a + b + c]}");
    test_case(PAW_ESYNTAX, "missing_right_paren", "fn f(a, b, c {return [a + b + c]}");
    test_case(PAW_ESYNTAX, "missing_right_curly", "fn f(a, b, c) {return [a + b + c]");
    test_case(PAW_ESYNTAX, "missing_left_curly", "fn f(a, b, c) return [a + b + c]}");
    test_case(PAW_ESYNTAX, "missing_right_bracket", "fn f(a, b, c) {return [a + b + c}");
    test_case(PAW_ESYNTAX, "missing_left_bracket", "fn f(a, b, c) {return a + b + c]}");
}
