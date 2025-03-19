// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// TODO: print unicode escapes as hexadecimal

#include "error.h"

#define THROW_ERROR(Kind_, Builder_) do { \
    Buffer b; \
    pawL_init_buffer(ENV(C), &b); \
    Builder_ \
    pawL_push_result(ENV(C), &b); \
    pawC_throw(ENV(C), Kind_); \
} while (0)

struct ErrorInfo {
    enum ErrorKind kind;
    struct SourceLoc loc;
    String const *modname;
    String const *primary;
    String const *detail;
};

static String const *format(struct Compiler *C, char const *fmt, ...)
{
    Buffer b;
    paw_Env *P = ENV(C);
    pawL_init_buffer(P, &b);

    va_list arg;
    va_start(arg, fmt);
    pawL_add_vfstring(P, &b, fmt, arg);
    va_end(arg);

    String const *str = pawP_scan_nstring(C, C->strings, b.data, b.size);
    pawL_discard_result(P, &b);
    return str;
}

_Noreturn static void throw(struct Compiler *C, enum ErrorKind kind, String const *modname, struct SourceLoc loc, String const *primary, String const *detail)
{
    Buffer b;
    paw_Env *P = ENV(C);
    pawL_init_buffer(P, &b);

    pawL_add_fstring(P, &b, "%s:%d:%d: %s", modname->text,
            loc.line, loc.column, primary->text);
    if (detail != NULL)
        pawL_add_fstring(P, &b, " (%s)", detail->text);

    pawL_push_result(P, &b);
    pawC_throw(P, kind);
}

_Noreturn void pawErr_invalid_escape(struct Compiler *C, String const *modname, struct SourceLoc loc, char c)
{
    throw(C, E_INVALID_ESCAPE, modname, loc,
            format(C, "invalid escape '\\%c'", c),
            NULL);
}

_Noreturn void pawErr_invalid_unicode_escape(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *codepoint)
{
    throw(C, E_INVALID_UNICODE_ESCAPE, modname, loc,
            format(C, "invalid unicode escape '\\u%s'", codepoint),
            NULL);
}

_Noreturn void pawErr_invalid_unicode_codepoint(struct Compiler *C, String const *modname, struct SourceLoc loc, int codepoint)
{
    throw(C, E_INVALID_UNICODE_CODEPOINT, modname, loc,
            format(C, "invalid unicode codepoint '\\u%d'", codepoint),
            NULL);
}

_Noreturn void pawErr_expected_integer_digit(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *base)
{
    throw(C, E_EXPECTED_INTEGER_DIGIT, modname, loc,
            format(C, "expected at least 1 digit in %s integer", base),
            NULL);
}

_Noreturn void pawErr_unexpected_integer_char(struct Compiler *C, String const *modname, struct SourceLoc loc, char c, char const *base)
{
    throw(C, E_UNEXPECTED_INTEGER_CHAR, modname, loc,
            format(C, "unexpected '%c' in %s integer", c, base),
            NULL);
}

_Noreturn void pawErr_integer_out_of_range(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *text)
{
    throw(C, E_INTEGER_OUT_OF_RANGE, modname, loc,
            format(C, "integer '%s' out of range", text),
            NULL);
}

_Noreturn void pawErr_invalid_integer(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *base, char const *text)
{
    throw(C, E_INVALID_INTEGER, modname, loc,
            format(C, "invalid %s integer '%s'", text),
            NULL);
}

_Noreturn void pawErr_invalid_float(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *text)
{
    throw(C, E_INVALID_FLOAT, modname, loc,
            format(C, "invalid float '%s'", text),
            NULL);
}

_Noreturn void pawErr_too_many_lines(struct Compiler *C, String const *modname, struct SourceLoc loc, int limit)
{
    throw(C, E_INVALID_FLOAT, modname, loc,
            format(C, "too many lines in source file"),
            format(C, "limit is %d", limit));
}

_Noreturn void pawErr_too_many_columns(struct Compiler *C, String const *modname, struct SourceLoc loc, int limit)
{
    throw(C, E_INVALID_FLOAT, modname, loc,
            format(C, "too many columns in source file"),
            format(C, "limit is %d", limit));
}

_Noreturn void pawErr_name_too_long(struct Compiler *C, String const *modname, struct SourceLoc loc, int length, int limit)
{
    throw(C, E_INVALID_FLOAT, modname, loc,
            format(C, "name is too long"),
            format(C, "length is %d but limit is %d", length, limit));
}

_Noreturn void pawErr_unexpected_symbol(struct Compiler *C, String const *modname, struct SourceLoc loc)
{
    throw(C, E_EXPECTED_SYMBOL, modname, loc,
            format(C, "unexpected symbol"), // TODO: print symbol and what was expected
            NULL);
}

_Noreturn void pawErr_expected_delimiter(struct Compiler *C, String const *modname, struct SourceLoc loc, char right, char left, struct SourceLoc open)
{
    throw(C, E_EXPECTED_DELIMITER, modname, loc,
            format(C, "expected '%c' to match '%c' at %d:%d", right, left, open.line, open.column),
            NULL);
}

_Noreturn void pawErr_expected_semicolon(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *what)
{
    throw(C, E_EXPECTED_SEMICOLON, modname, loc,
            format(C, "expected semicolon after %s", what),
            NULL);
}

_Noreturn void pawErr_unexpected_visibility_qualifier(struct Compiler *C, String const *modname, struct SourceLoc loc)
{
    throw(C, E_UNEXPECTED_VISIBILITY_QUALIFIER, modname, loc,
            format(C, "visibility qualifier is not allowed here"),
            NULL);
}

_Noreturn void pawErr_unexpected_annotation(struct Compiler *C, String const *modname, struct SourceLoc loc)
{
    throw(C, E_UNEXPECTED_ANNOTATION, modname, loc,
            format(C, "annotation is not allowed here"),
            NULL);
}

_Noreturn void pawErr_empty_type_list(struct Compiler *C, String const *modname, struct SourceLoc loc)
{
    throw(C, E_EMPTY_TYPE_LIST, modname, loc,
            format(C, "expected at least 1 type"),
            NULL);
}

_Noreturn void pawErr_nonliteral_pattern(struct Compiler *C, String const *modname, struct SourceLoc loc)
{
    throw(C, E_NONLITERAL_PATTERN, modname, loc,
            format(C, "expected literal pattern"),
            NULL);
}

_Noreturn void pawErr_negative_minimum_integer(struct Compiler *C, String const *modname, struct SourceLoc loc)
{
    throw(C, E_NEGATIVE_MINIMUM_INTEGER, modname, loc,
            format(C, "operator '-' applied to minimum integer %I", PAW_INT_MIN),
            format(C, "results in signed integer overflow"));
}

_Noreturn void pawErr_invalid_literal_negation(struct Compiler *C, String const *modname, struct SourceLoc loc)
{
    throw(C, E_INVALID_LITERAL_NEGATION, modname, loc,
            format(C, "operator '-' applied to non-numeric operand"),
            NULL);
}

_Noreturn void pawErr_invalid_selector(struct Compiler *C, String const *modname, struct SourceLoc loc)
{
    throw(C, E_INVALID_SELECTOR, modname, loc,
            format(C, "expected integer or name after '.'"),
            NULL);
}

_Noreturn void pawErr_expected_basic_type(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *what)
{
    throw(C, E_EXPECTED_BASIC_TYPE, modname, loc,
            format(C, "expected basic type but found '%s'", what),
            NULL);
}

_Noreturn void pawErr_expected_expression(struct Compiler *C, String const *modname, struct SourceLoc loc)
{
    throw(C, E_EXPECTED_EXPRESSION, modname, loc,
            format(C, "expected expression"),
            NULL);
}

_Noreturn void pawErr_empty_enumeration(struct Compiler *C, String const *modname, struct SourceLoc loc)
{
    throw(C, E_EMPTY_ENUMERATION, modname, loc,
            format(C, "enumeration has no variants"),
            NULL);
}

_Noreturn void pawErr_empty_struct_body(struct Compiler *C, String const *modname, struct SourceLoc loc)
{
    throw(C, E_EMPTY_STRUCT_BODY, modname, loc,
            format(C, "expected at least 1 field or method in structure body"),
            format(C, "omit curly braces to define unit structure"));
}

_Noreturn void pawErr_empty_variant_field_list(struct Compiler *C, String const *modname, struct SourceLoc loc)
{
    throw(C, E_EMPTY_STRUCT_BODY, modname, loc,
            format(C, "expected at least 1 variant field between parenthesis"),
            format(C, "remove parenthesis to construct unit variant"));
}


_Noreturn void pawErr_function_type_decl(struct Compiler *C, String const *modname, struct SourceLoc loc)
{
    throw(C, E_FUNCTION_TYPE_DECL, modname, loc,
            format(C, "function types are not allowed in 'type' declarations"),
            NULL);
}

_Noreturn void pawErr_expected_type_annotation(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *what, char const *name)
{
    throw(C, E_FUNCTION_TYPE_DECL, modname, loc,
            format(C, "expected type annotation on %s '%s'", what, name),
            NULL);
}

_Noreturn void pawErr_return_outside_function(struct Compiler *C, String const *modname, struct SourceLoc loc)
{
    throw(C, E_RETURN_OUTSIDE_FUNCTION, modname, loc,
            format(C, "return outside function body"),
            NULL);
}

_Noreturn void pawErr_chain_outside_function(struct Compiler *C, String const *modname, struct SourceLoc loc)
{
    throw(C, E_CHAIN_OUTSIDE_FUNCTION, modname, loc,
            format(C, "operator '?' encountered outside function body"),
            NULL);
}

_Noreturn void pawErr_jump_outside_loop(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *kind)
{
    throw(C, E_JUMP_OUTSIDE_LOOP, modname, loc,
            format(C, "'%s' outside loop", kind),
            format(C, "limit is %d"));
}

_Noreturn void pawErr_too_many_elements(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *what, int limit)
{
    throw(C, E_TOO_MANY_ELEMENTS, modname, loc,
            format(C, "too many %s"),
            format(C, "limit is %d", limit));
}

_Noreturn void pawErr_expected_toplevel_item(struct Compiler *C, String const *modname, struct SourceLoc loc)
{
    throw(C, E_EXPECTED_TOPLEVEL_ITEM, modname, loc,
            format(C, "expected toplevel item"),
            NULL);
}

_Noreturn void pawErr_expected_colon_after_map_key(struct Compiler *C, String const *modname, struct SourceLoc loc)
{
    throw(C, E_EXPECTED_COLON_AFTER_MAP_KEY, modname, loc,
            format(C, "expected ':' after map key"),
            NULL);
}

_Noreturn void pawErr_colon_after_list_element(struct Compiler *C, String const *modname, struct SourceLoc loc)
{
    throw(C, E_COLON_AFTER_LIST_ELEMENT, modname, loc,
            format(C, "unexpected ':' after list element"),
            NULL);
}

_Noreturn void pawErr_colons_after_underscore(struct Compiler *C, String const *modname, struct SourceLoc loc)
{
    throw(C, E_COLONS_AFTER_UNDERSCORE, modname, loc,
            format(C, "unexpected '::' after '_'"),
            NULL);
}

_Noreturn void pawErr_expected_self_parameter(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *name)
{
    throw(C, E_EXPECTED_SELF_PARAMETER, modname, loc,
            format(C, "expected parameter named 'self' but found '%s'", name),
            NULL);
}

_Noreturn void pawErr_unexpected_underscore(struct Compiler *C, String const *modname, struct SourceLoc loc)
{
    throw(C, E_UNEXPECTED_UNDERSCORE, modname, loc,
            format(C, "'_' is not allowed here"),
            NULL);
}

_Noreturn void pawErr_expected_comma_separator(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *what)
{
    throw(C, E_EXPECTED_COMMA_SEPARATOR, modname, loc,
            format(C, "expected ',' to separate %s from other items", what),
            NULL);
}
_Noreturn void pawErr_nonliteral_annotation_value(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *name)
{
    throw(C, E_NONLITERAL_ANNOTATION_VALUE, modname, loc,
            format(C, "value for '%s' annotation must be a literal", name),
            NULL);
}
_Noreturn void pawErr_nonprimitive_annotation_value(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *name)
{
    throw(C, E_NONPRIMITIVE_ANNOTATION_VALUE, modname, loc,
            format(C, "value for '%s' annotation must be a primitive type", name),
            NULL);
}


