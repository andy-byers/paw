#include "auxlib.h"
#include "lib.h"
#include "opcode.h"
#include "os.h"
#include "test.h"
#include <limits.h>

static struct TestAlloc s_alloc;
static char s_error_msg[1024];

typedef uint64_t TypeSet;

static void test_case(int expect, const char *name, const char *text)
{
    (void)name;
    paw_Env *P = test_open(NULL, &s_alloc);
    int status = pawL_load_chunk(P, name, text);
    if (expect == PAW_ESYNTAX) {
        check(status == PAW_ESYNTAX);
    } else {
        check(status == PAW_OK);
        status = paw_call(P, 0);
    }
    if (status != PAW_OK) {
        // Copy the error message to a static buffer so that the paw_Env
        // can be closed.
        check(paw_get_count(P) > 0);
        const char *msg = paw_string(P, -1);
        const size_t len = paw_length(P, -1);
        check(len < paw_lengthof(s_error_msg));
        memcpy(s_error_msg, msg, len);
        s_error_msg[len] = '\0';
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
            check(0);
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

#define codeline(s) s "\n"

static void test_line_numbers(void)
{
    // cause errors by dividing by 0
    const char *code = 
        /*  1 */ codeline("return fn(a, b, c, d, e, f, g, h, i, j, k, l) {")
        /*  2 */ codeline("    let x = 1/a")
        /*  3 */ codeline("    let func")
        /*  4 */ codeline("    {")
        /*  5 */ codeline("        -- line comment")
        /*  6 */ codeline("        let x = 1 + 1/b + 1")
        /*  7 */ codeline("        -* block")
        /*  8 */ codeline("           comment *-")
        /*  9 */ codeline("        func = fn() { let x = 1/c")
        /* 10 */ codeline("            -- another line comment")
        /* 11 */ codeline("            let x = (1+1) / d * 1")
        /* 12 */ codeline("            let x = 1 + 1 + 1 +")
        /* 13 */ codeline("                    1 + 1 + 1 +")
        /* 14 */ codeline("                    1 + 1 + 1/e")
        /* 15 */ codeline("            let x = [1/f,")
        /* 16 */ codeline("                1 +")
        /* 17 */ codeline("                1 +")
        /* 18 */ codeline("                1/g,")
        /* 19 */ codeline("            ]")
        /* 20 */ codeline("            let x = {'a': 1/h,")
        /* 21 */ codeline("                1/i: 1,")
        /* 22 */ codeline("                'b': 1 +")
        /* 23 */ codeline("                     1 +")
        /* 24 */ codeline("                     1/j,")
        /* 25 */ codeline("            }")
        /* 26 */ codeline("        }")
        /* 27 */ codeline("    }")
        /* 28 */ codeline("    let x = 1/k")
        /* 29 */ codeline("    func()")
        /* 30 */ codeline("return 1/l}");
    const int answers[] = {2, 6, 9, 11, 14, 15, 18, 20, 21, 24, 28, 30};
    const size_t n = paw_countof(answers);
    paw_Env *P = test_open(NULL, &s_alloc);
    check(PAW_OK == pawL_load_chunk(P, "faulty", code));
    check(PAW_OK == paw_call(P, 0));
    pawL_check_type(P, -1, PAW_TFUNCTION);
    paw_push_value(P, -1); // copy function
    for (size_t i = 0; i < n; ++i) {
        // push arguments and call the function. Exactly 1 argument is 0
        // on each iteration, the other arguments are 1.
        for (size_t j = 0; j < n; ++j) {
            paw_push_int(P, i != j);
        }
        check(PAW_OK != paw_call(P, n));

        // Extract the line number from the error message.
        const char *message = pawL_check_string(P, -1);
        const char *a, *b;
        check((a = strchr(message, ':')) &&
              (b = strchr(a + 1, ':')));
        char buffer[32];
        memcpy(buffer, a + 1, cast_size(b - a - 1));
        buffer[b - a - 1] = '\0';
        const int line = strtol(buffer, NULL, 10);
        printf("%zu: line %d\n", i, line);
        check(line == answers[i]);
        paw_pop(P, n + 1);
    }
    test_close(P, &s_alloc);
}

int main(void)
{
    test_name_error();
    test_syntax_error();
    test_type_error();
    test_line_numbers();

    test_case(PAW_ESYNTAX, "missing_left_paren", "fn fa, b, c) {return [a + b + c]}");
    test_case(PAW_ESYNTAX, "missing_right_paren", "fn f(a, b, c {return [a + b + c]}");
    test_case(PAW_ESYNTAX, "missing_right_curly", "fn f(a, b, c) {return [a + b + c]");
    test_case(PAW_ESYNTAX, "missing_left_curly", "fn f(a, b, c) return [a + b + c]}");
    test_case(PAW_ESYNTAX, "missing_right_bracket", "fn f(a, b, c) {return [a + b + c}");
    test_case(PAW_ESYNTAX, "missing_left_bracket", "fn f(a, b, c) {return a + b + c]}");
}
