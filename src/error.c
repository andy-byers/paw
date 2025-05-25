// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "error.h"
#include "ir_type.h"

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

_Noreturn void pawErr_unicode_escape_too_long(struct Compiler *C, String const *modname, struct SourceLoc loc)
{
    throw(C, E_UNICODE_ESCAPE_TOO_LONG, modname, loc,
            format(C, "must contain less than or equal to 6 hex digits"),
            NULL);
}

_Noreturn void pawErr_invalid_unicode_escape(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *codepoint)
{
    throw(C, E_INVALID_UNICODE_ESCAPE, modname, loc,
            format(C, "invalid unicode escape '\\u{%s}'", codepoint),
            NULL);
}

_Noreturn void pawErr_invalid_unicode_codepoint(struct Compiler *C, String const *modname, struct SourceLoc loc, int codepoint)
{
    throw(C, E_INVALID_UNICODE_CODEPOINT, modname, loc,
            format(C, "invalid unicode codepoint '\\u%X'", codepoint),
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
            format(C, "invalid integer '%s'", text),
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
    throw(C, E_TOO_MANY_LINES, modname, loc,
            format(C, "too many lines in source file"),
            format(C, "limit is %d", limit));
}

_Noreturn void pawErr_too_many_columns(struct Compiler *C, String const *modname, struct SourceLoc loc, int limit)
{
    throw(C, E_TOO_MANY_COLUMNS, modname, loc,
            format(C, "too many columns in source file"),
            format(C, "limit is %d", limit));
}

_Noreturn void pawErr_name_too_long(struct Compiler *C, String const *modname, struct SourceLoc loc, int length, int limit)
{
    throw(C, E_NAME_TOO_LONG, modname, loc,
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
    throw(C, E_EMPTY_VARIANT_FIELD_LIST, modname, loc,
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

_Noreturn void pawErr_duplicate_annotation(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *name)
{
    throw(C, E_DUPLICATE_ANNOTATION, modname, loc,
            format(C, "duplicate annotation '%s'", name),
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

_Noreturn void pawErr_module_not_found(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *name)
{
    throw(C, E_MODULE_NOT_FOUND, modname, loc,
            format(C, "module '%s' not found", name),
            NULL);
}

_Noreturn void pawErr_invalid_assignment_target(struct Compiler *C, String const *modname, struct SourceLoc loc)
{
    throw(C, E_INVALID_ASSIGNMENT_TARGET, modname, loc,
            format(C, "invalid target for assignment"),
            NULL);
}

_Noreturn void pawErr_duplicate_item(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *what, char const *name)
{
    throw(C, E_DUPLICATE_ITEM, modname, loc,
            format(C, "duplicate %s '%s'", what, name),
            NULL);
}

_Noreturn void pawErr_unknown_type(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *type)
{
    throw(C, E_UNKNOWN_TYPE, modname, loc,
            format(C, "unknown type '%s'", type),
            NULL);
}

_Noreturn void pawErr_unknown_trait(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *name)
{
    throw(C, E_UNKNOWN_TRAIT, modname, loc,
            format(C, "unknown trait '%s'", name),
            NULL);
}

_Noreturn void pawErr_unknown_item(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *mod, char const *item)
{
    throw(C, E_UNKNOWN_ITEM, modname, loc,
            format(C, "unknown item '%s::%s'", mod, item),
            NULL);
}

_Noreturn void pawErr_extern_function_body(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *name)
{
    throw(C, E_EXTERN_FUNCTION_BODY, modname, loc,
            format(C, "unexpected body on extern function '%s'", name),
            NULL);
}

_Noreturn void pawErr_missing_function_body(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *name)
{
    throw(C, E_MISSING_FUNCTION_BODY, modname, loc,
            format(C, "missing body for function '%s'", name),
            NULL);
}

_Noreturn void pawErr_item_visibility(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *mod, char const *item)
{
    throw(C, E_ITEM_VISIBILITY, modname, loc,
            format(C, "item '%s::%s' cannot be accessed from the current module", mod, item),
            NULL);
}

_Noreturn void pawErr_associated_item_visibility(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *name, char const *type)
{
    throw(C, E_ASSOCIATED_ITEM_VISIBILITY, modname, loc,
            format(C, "item '%s' cannot be accessed from outside a method on type '%s'", name, type),
            NULL);
}

_Noreturn void pawErr_missing_trait_bounds(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *name)
{
    throw(C, E_MISSING_TRAIT_BOUNDS, modname, loc,
            format(C, "generic type '%s' missing trait bounds", name),
            NULL);
}

_Noreturn void pawErr_unsatisfied_trait_bounds(struct Compiler *C, String const *modname, struct SourceLoc loc)
{
    throw(C, E_UNSATISFIED_TRAIT_BOUNDS, modname, loc,
            format(C, "trait bounds not satisfied"),
            NULL);
}

_Noreturn void pawErr_incompatible_types(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *lhs, char const *rhs)
{
    throw(C, E_INCOMPATIBLE_TYPES, modname, loc,
            format(C, "incompatible types '%s' and '%s'", lhs, rhs),
            NULL);
}

_Noreturn void pawErr_cannot_infer(struct Compiler *C, String const *modname, struct SourceLoc loc)
{
    throw(C, E_CANNOT_INFER, modname, loc,
            format(C, "unable to infer type"),
            NULL);
}

_Noreturn void pawErr_cyclic_type(struct Compiler *C, String const *modname, struct SourceLoc loc)
{
    throw(C, E_CYCLIC_TYPE, modname, loc,
            format(C, "encountered cyclic type"),
            NULL);
}

_Noreturn void pawErr_missing_extern_value(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *name)
{
    throw(C, E_MISSING_EXTERN_VALUE, modname, loc,
            format(C, "missing extern value '%s'", name),
            NULL);
}

_Noreturn void pawErr_unexpected_module_name(struct Compiler *C, String const *modname, struct SourceLoc loc)
{
    throw(C, E_UNEXPECTED_MODULE_NAME, modname, loc,
            format(C, "unexpected module name"),
            NULL);
}

_Noreturn void pawErr_transitive_import(struct Compiler *C, String const *modname, struct SourceLoc loc)
{
    throw(C, E_TRANSITIVE_IMPORT, modname, loc,
            format(C, "transitive imports are not supported"),
            NULL);
}

_Noreturn void pawErr_incorrect_type_arity(struct Compiler *C, String const *modname, struct SourceLoc loc, int want, int have)
{
    throw(C, E_INCORRECT_TYPE_ARITY, modname, loc,
            format(C, "%s types", have < want ? "not enough" : "too many"),
            format(C, "expected %d but have %d", want, have));
}

_Noreturn void pawErr_unexpected_type_arguments(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *what, char const *name)
{
    throw(C, E_UNEXPECTED_TYPE_ARGUMENTS, modname, loc,
            format(C, "unexpected type arguments on %s '%s'", what, name),
            NULL);
}

_Noreturn void pawErr_expected_trait(struct Compiler *C, String const *modname, struct SourceLoc loc)
{
    throw(C, E_EXPECTED_TRAIT, modname, loc,
            format(C, "expected trait"),
            NULL);
}

_Noreturn void pawErr_unexpected_trait(struct Compiler *C, String const *modname, struct SourceLoc loc)
{
    throw(C, E_UNEXPECTED_TRAIT, modname, loc,
            format(C, "unexpected trait"),
            NULL);
}

_Noreturn void pawErr_incorrect_item_class(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *want, char const *have)
{
    throw(C, E_INCORRECT_ITEM_CLASS, modname, loc,
            format(C, "expected %s but found %s", want, have),
            NULL);
}

_Noreturn void pawErr_extra_segment(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *name)
{
    throw(C, E_EXTRA_SEGMENT, modname, loc,
            format(C, "extraneous '::%s'", name),
            NULL);
}

_Noreturn void pawErr_missing_trait_method(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *name)
{
    throw(C, E_MISSING_TRAIT_METHOD, modname, loc,
            format(C, "trait method '%s' not implemented", name),
            NULL);
}

_Noreturn void pawErr_trait_method_visibility_mismatch(struct Compiler *C, String const *modname, struct SourceLoc loc, paw_Bool expected_pub, char const *name)
{
    throw(C, E_TRAIT_METHOD_VISIBILITY_MISMATCH, modname, loc,
            format(C, "visibility mismatch (expected %s visibility on method '%s')", expected_pub ? "public" : "private", name),
            NULL);
}

_Noreturn void pawErr_missing_variant_args(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *cons)
{
    throw(C, E_MISSING_VARIANT_ARGS, modname, loc,
            format(C, "missing argument(s) for variant constructor '%s'", cons),
            NULL);
}

_Noreturn void pawErr_reserved_identifier(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *name)
{
    throw(C, E_RESERVED_IDENTIFIER, modname, loc,
            format(C, "'%s' is a reserved identifier", name),
            NULL);
}

_Noreturn void pawErr_unknown_path(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *path)
{
    throw(C, E_UNKNOWN_PATH, modname, loc,
            format(C, "unknown path '%s'", path),
            NULL);
}

_Noreturn void pawErr_type_used_as_value(struct Compiler *C, String const *modname, struct SourceLoc loc)
{
    throw(C, E_TYPE_USED_AS_VALUE, modname, loc,
            format(C, ""),
            NULL);
}

_Noreturn void pawErr_enum_used_as_value(struct Compiler *C, String const *modname, struct SourceLoc loc)
{
    throw(C, E_ENUM_USED_AS_VALUE, modname, loc,
            format(C, ""),
            NULL);
}

_Noreturn void pawErr_missing_fields(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *type)
{
    throw(C, E_MISSING_FIELDS, modname, loc,
            format(C, "missing fields on initializer for struct '%s'", type),
            NULL);
}

_Noreturn void pawErr_expected_value(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *type)
{
    throw(C, E_EXPECTED_VALUE, modname, loc,
            format(C, "expected value but found type '%s'", type),
            NULL);
}

_Noreturn void pawErr_invalid_chain_operand(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *type)
{
    throw(C, E_INVALID_CHAIN_OPERAND, modname, loc,
            format(C, "invalid operand type '%s' for chain operator", type),
            NULL);
}

_Noreturn void pawErr_invalid_unary_operand(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *type, char const *op)
{
    throw(C, E_INVALID_UNARY_OPERAND, modname, loc,
            format(C, "invalid operand type '%s' for unary operator '%s'", type, op),
            NULL);
}

_Noreturn void pawErr_invalid_binary_operand(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *type, char const *op)
{
    throw(C, E_INVALID_BINARY_OPERAND, modname, loc,
            format(C, "invalid operand type '%s' for binary operator '%s'", type, op),
            NULL);
}

_Noreturn void pawErr_cannot_constant_evaluate(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *what)
{
    throw(C, E_CANNOT_CONSTANT_EVALUATE, modname, loc,
            format(C, "%s cannot be constant evaluated", what),
            NULL);
}

_Noreturn void pawErr_nonprimitive_constant(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *type)
{
    throw(C, E_NONPRIMITIVE_CONSTANT, modname, loc,
            format(C, "expected primitive constant but found '%s'", type),
            NULL);
}

_Noreturn void pawErr_expected_element_selector(struct Compiler *C, String const *modname, struct SourceLoc loc)
{
    throw(C, E_EXPECTED_ELEMENT_SELECTOR, modname, loc,
            format(C, "expected integer element selector"),
            NULL);
}

_Noreturn void pawErr_element_selector_out_of_range(struct Compiler *C, String const *modname, struct SourceLoc loc, int elem, int count)
{
    throw(C, E_ELEMENT_SELECTOR_OUT_OF_RANGE, modname, loc,
            format(C, "element selector %d is out of range for %d-tuple", elem, count),
            NULL);
}

_Noreturn void pawErr_expected_adt(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *type)
{
    throw(C, E_EXPECTED_ADT, modname, loc,
            format(C, "expected ADT but found '%s'", type),
            NULL);
}

_Noreturn void pawErr_expected_field_selector(struct Compiler *C, String const *modname, struct SourceLoc loc)
{
    throw(C, E_EXPECTED_FIELD_SELECTOR, modname, loc,
            format(C, "expected field selector"),
            NULL);
}

_Noreturn void pawErr_unknown_method(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *method, char const *type)
{
    throw(C, E_UNKNOWN_METHOD, modname, loc,
            format(C, "unknown method '%s' for type '%s'", method, type),
            NULL);
}

_Noreturn void pawErr_unknown_associated_item(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *method, char const *type)
{
    throw(C, E_UNKNOWN_ASSOCIATED_ITEM, modname, loc,
            format(C, "unknown associated item '%s' for type '%s'", method, type),
            NULL);
}


_Noreturn void pawErr_not_a_method(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *name)
{
    throw(C, E_NOT_A_METHOD, modname, loc,
            format(C, "'%s' is not a method", name),
            NULL);
}

_Noreturn void pawErr_not_callable(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *type)
{
    throw(C, E_NOT_CALLABLE, modname, loc,
            format(C, "type '%s' is not callable", type),
            NULL);
}

_Noreturn void pawErr_incorrect_arity(struct Compiler *C, String const *modname, struct SourceLoc loc, int want, int have)
{
    throw(C, E_INCORRECT_ARITY, modname, loc,
            format(C, "%s arguments", have < want ? "not enough" : "too many"),
            format(C, "expected %d but have %d", want, have));
}

_Noreturn void pawErr_duplicate_field(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *name, char const *type)
{
    throw(C, E_DUPLICATE_FIELD, modname, loc,
            format(C, "duplicate field '%s' in '%s'", name, type),
            NULL);
}

_Noreturn void pawErr_duplicate_binding(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *name)
{
    throw(C, E_DUPLICATE_BINDING, modname, loc,
            format(C, "duplicate binding '%s'", name),
            NULL);
}

_Noreturn void pawErr_expected_struct(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *type)
{
    throw(C, E_EXPECTED_STRUCT, modname, loc,
            format(C, "expected struct but found '%s'", type),
            NULL);
}

_Noreturn void pawErr_unit_struct_with_braces(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *type)
{
    throw(C, E_UNIT_STRUCT_WITH_BRACES, modname, loc,
            format(C, "unexpected braces on unit struct '%s'", type),
            format(C, "omit braces to construct unit struct"));
}

_Noreturn void pawErr_unit_variant_with_parenthesis(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *type)
{
    throw(C, E_UNIT_VARIANT_WITH_PARENTHESIS, modname, loc,
            format(C, "unexpected parenthesis on unit variant '%s'", type),
            format(C, "omit parenthesis to construct unit variant"));
}

_Noreturn void pawErr_missing_field(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *name, char const *type)
{
    throw(C, E_MISSING_FIELD, modname, loc,
            format(C, "missing initializer for field '%s' on struct '%s'", name, type),
            NULL);
}

_Noreturn void pawErr_unknown_field(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *name, char const *type)
{
    throw(C, E_UNKNOWN_FIELD, modname, loc,
            format(C, "unknown field '%s' on struct '%s'", name, type),
            NULL);
}

_Noreturn void pawErr_invalid_index_target(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *type)
{
    throw(C, E_INVALID_INDEX_TARGET, modname, loc,
            format(C, "invalid target '%s' for index operator", type),
            NULL);
}

_Noreturn void pawErr_missing_binding_in_alternative(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *name)
{
    throw(C, E_MISSING_BINDING_IN_ALTERNATIVE, modname, loc,
            format(C, "missing binding '%s' in alternative pattern", name),
            NULL);
}

_Noreturn void pawErr_expected_divergence(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *fn)
{
    throw(C, E_EXPECTED_DIVERGENCE, modname, loc,
            format(C, "expected '%s' to diverge based on return type of '!'", fn),
            NULL);
}

_Noreturn void pawErr_invalid_inclusive_range(struct Compiler *C, String const *modname, struct SourceLoc loc)
{
    throw(C, E_INVALID_INCLUSIVE_RANGE, modname, loc,
            format(C, "type of range cannot be inclusive"),
            NULL);
}

_Noreturn void pawErr_global_constant_cycle(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *name)
{
    throw(C, E_GLOBAL_CONSTANT_CYCLE, modname, loc,
            format(C, "cycle detected between global constants"),
            format(C, "involves constant '%s'", name));
}

_Noreturn void pawErr_uninitialized_destructuring(struct Compiler *C, String const *modname, struct SourceLoc loc)
{
    throw(C, E_UNINITIALIZED_DESTRUCTURING, modname, loc,
            format(C, "variables using deferred initialization cannot use destructuring"),
            NULL);
}

_Noreturn void pawErr_modified_constant(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *name)
{
    throw(C, E_MODIFIED_CONSTANT, modname, loc,
            format(C, "attempt to modify constant '%s'", name),
            NULL);
}

_Noreturn void pawErr_too_many_upvalues(struct Compiler *C, String const *modname, struct SourceLoc loc, int limit)
{
    throw(C, E_TOO_MANY_UPVALUES, modname, loc,
            format(C, "too many upvalues in function"),
            format(C, "limit is %d", limit));
}

_Noreturn void pawErr_initialized_extern_constant(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *name)
{
    throw(C, E_INITIALIZED_EXTERN_CONSTANT, modname, loc,
            format(C, "unexpected initializer for 'extern' constant '%s'", name),
            NULL);
}

_Noreturn void pawErr_uninitialized_constant(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *name)
{
    throw(C, E_UNINITIALIZED_CONSTANT, modname, loc,
            format(C, "missing initializer for constant '%s'", name),
            NULL);
}

_Noreturn void pawErr_infinite_size_object(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *name)
{
    throw(C, E_INFINITE_SIZE_OBJECT, modname, loc,
            format(C, "object '%s' has infinite size", name),
            NULL);
}

_Noreturn void pawErr_object_too_large(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *name)
{
    throw(C, E_OBJECT_TOO_LARGE, modname, loc,
            format(C, "object '%s' is too large", name),
            NULL);
}

_Noreturn void pawErr_nonexhaustive_pattern_match(struct Compiler *C, String const *modname, struct SourceLoc loc)
{
    throw(C, E_NONEXHAUSTIVE_PATTERN_MATCH, modname, loc,
            format(C, "nonexhaustive pattern match"),
            NULL);
}

_Noreturn void pawErr_use_before_initialization(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *name)
{
    throw(C, E_USE_BEFORE_INITIALIZATION, modname, loc,
            format(C, "use of variable '%s' before initialization", name),
            NULL);
}

_Noreturn void pawErr_constant_divide_by_zero(struct Compiler *C, String const *modname, struct SourceLoc loc)
{
    throw(C, E_CONSTANT_DIVIDE_BY_ZERO, modname, loc,
            format(C, "constant division by 0"),
            NULL);
}

_Noreturn void pawErr_constant_negative_shift_count(struct Compiler *C, String const *modname, struct SourceLoc loc)
{
    throw(C, E_CONSTANT_NEGATIVE_SHIFT_COUNT, modname, loc,
            format(C, "constant negative shift count"),
            NULL);
}

_Noreturn void pawErr_too_many_variables(struct Compiler *C, String const *modname, struct SourceLoc loc, int limit)
{
    throw(C, E_TOO_MANY_VARIABLES, modname, loc,
            format(C, "too many variables"),
            format(C, "limit is %d", limit));
}


_Noreturn void pawErr_too_far_to_jump(struct Compiler *C, String const *modname, struct SourceLoc loc, int limit)
{
    throw(C, E_TOO_FAR_TO_JUMP, modname, loc,
            format(C, "too far to jump"),
            format(C, "limit is %d instructions", limit));
}


_Noreturn void pawErr_too_many_constants(struct Compiler *C, String const *modname, struct SourceLoc loc, int limit)
{
    throw(C, E_TOO_MANY_CONSTANTS, modname, loc,
            format(C, "too many constants"),
            format(C, "limit is %d", limit));
}


