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
    test_case(PAW_ENAME, "use_before_def_local", "let x = x");
    test_case(PAW_ENAME, "undef_local", "x = 1");
    test_case(PAW_ENAME, "undef_upvalue", "fn f() {x = 1} f()");
}

static _Bool has_type(TypeSet ts, int kind)
{
    return ts & (1 << kind);
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

    test_case(PAW_ETYPE, name_buf, text_buf);
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

            test_case(PAW_ETYPE, name_buf, text_buf);
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

    test_case(PAW_ETYPE, name_buf, text_buf);
}

static void check_binop_type_errors(const char *op, paw_Type *types)
{
    for (int k = PAW_TUNIT; k <= PAW_TSTRING; ++k) {
        for (int k2 = PAW_TUNIT; k2 <= PAW_TSTRING; ++k2) {
            for (int t = *types; t >= 0; t = *++types) {
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
    check_unop_type_error("#", PAW_TSTRING);
    check_unop_type_error("!", PAW_TUNIT);
    check_unop_type_error("-", PAW_TUNIT);
    check_unop_type_error("-", PAW_TBOOL);
    check_unop_type_error("-", PAW_TSTRING);
    check_unop_type_error("~", PAW_TUNIT);
    check_unop_type_error("~", PAW_TBOOL);
    check_unop_type_error("~", PAW_TFLOAT);
    check_unop_type_error("~", PAW_TSTRING);

#define mklist(...) (paw_Type[]){__VA_ARGS__, -1}
#define mklist0() (paw_Type[]){-1}
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
    pawL_add_string(P, &buf, "0 }");
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
        pawL_add_string(P, &buf, "let x = 0\n");
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
    test_case(PAW_ESYNTAX, "missing_right_paren", "fn f(a: int, b: int, c: int -> int {return (a + b + c)}");
    test_case(PAW_ESYNTAX, "missing_left_paren", "fn fa: int, b: int, c: int) -> int {return (a + b + c)}");
    test_case(PAW_ESYNTAX, "missing_right_curly", "fn f(a: int, b: int, c: int) -> int {return (a + b + c)");
    test_case(PAW_ESYNTAX, "missing_left_curly", "fn f(a: int, b: int, c: int) -> int return (a + b + c)}");
    test_case(PAW_ESYNTAX, "missing_right_bracket", "fn f[A, B, C() {}");
    test_case(PAW_ESYNTAX, "missing_left_bracket", "fn fA, B, C]() {}");

    // The following tests are generated, since they require a lot of text.
    too_many_locals();
    too_many_instructions();
    too_many_constants();
    too_far_to_jump();
    too_far_to_loop();
}

#if 0
#define codeline(s) s "\n"

static void test_line_numbers(void)
{
    // cause errors by dividing by 0
    const char *code = 
        /*  1 */ codeline("return fn(a: int, b: int, c: int, d: int, e: int, f: int, g: int, h: int, i: int, j: int, k: int, l: int) {")
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
#endif // 0

int main(void)
{
    test_name_error();
    test_syntax_error();
    test_type_error();
    //test_line_numbers();

    test_case(PAW_ESYNTAX, "missing_left_paren", "fn fa, b, c) {return [a + b + c]}");
    test_case(PAW_ESYNTAX, "missing_right_paren", "fn f(a, b, c {return [a + b + c]}");
    test_case(PAW_ESYNTAX, "missing_right_curly", "fn f(a, b, c) {return [a + b + c]");
    test_case(PAW_ESYNTAX, "missing_left_curly", "fn f(a, b, c) return [a + b + c]}");
    test_case(PAW_ESYNTAX, "missing_right_bracket", "fn f(a, b, c) {return [a + b + c}");
    test_case(PAW_ESYNTAX, "missing_left_bracket", "fn f(a, b, c) {return a + b + c]}");
}
