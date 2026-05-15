// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "error.h"
#include "auxlib.h"
#include "ir_type.h"

#include <stdio.h> // snprintf
#include <inttypes.h> // PRIu64

struct ErrorInfo {
    enum ErrorKind kind;
    struct SourceSpan span;
    Str const *modname;
    Str const *primary;
    Str const *detail;
};

static Str const *format(struct Compiler *C, char const *fmt, ...)
{
    Buffer b;
    paw_Env *P = ENV(C);
    pawL_init_buffer(P, &b);

    va_list arg;
    va_start(arg, fmt);
    pawL_add_vfstring(P, &b, fmt, arg);
    va_end(arg);

    return pawL_buffer_finish(P, &b);
}

_Noreturn static void throw(struct Compiler *C, enum ErrorKind kind, Str const *modname, struct SourceSpan span, Str const *primary, Str const *detail)
{
    paw_Env *P = ENV(C);
    if (P->current_errmsg == NULL) {
        Buffer b;
        pawL_init_buffer(P, &b);

        pawL_add_fstring(P, &b, "%s:%d:%d: %s", modname->text,
                span.range.start.line, span.range.start.column, primary->text);
        if (detail != NULL)
            pawL_add_fstring(P, &b, " (%s)", detail->text);

        P->current_errmsg = pawL_buffer_finish(P, &b);
    }
    pawC_throw(P, (int)kind);
}

_Noreturn void pawErr_unsupported(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *feature)
{
    throw(C, E_UNSUPPORTED, modname, span,
            format(C, "feature \"%s\" is not supported", feature),
            NULL);
}

_Noreturn void pawErr_empty_char(struct Compiler *C, Str const *modname, struct SourceSpan span)
{
    throw(C, E_EMPTY_CHAR, modname, span,
            format(C, "character literal is empty"),
            NULL);
}

_Noreturn void pawErr_char_too_long(struct Compiler *C, Str const *modname, struct SourceSpan span)
{
    throw(C, E_CHAR_TOO_LONG, modname, span,
            format(C, "character literal is too long"),
            NULL);
}

_Noreturn void pawErr_unterminated_char(struct Compiler *C, Str const *modname, struct SourceSpan span)
{
    throw(C, E_UNTERMINATED_CHAR, modname, span,
            format(C, "unterminated character literal"),
            NULL);
}

_Noreturn void pawErr_unterminated_string(struct Compiler *C, Str const *modname, struct SourceSpan span)
{
    throw(C, E_UNTERMINATED_STRING, modname, span,
            format(C, "unterminated string literal"),
            NULL);
}

_Noreturn void pawErr_invalid_unicode_codepoint(struct Compiler *C, Str const *modname, struct SourceSpan span, unsigned codepoint)
{
    throw(C, E_INVALID_UNICODE_CODEPOINT, modname, span,
            format(C, "invalid unicode codepoint \"0x%X\"", codepoint),
            NULL);
}

_Noreturn void pawErr_expected_integer_digit(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *base)
{
    throw(C, E_EXPECTED_INTEGER_DIGIT, modname, span,
            format(C, "expected at least 1 digit in %s integer", base),
            NULL);
}

_Noreturn void pawErr_unexpected_integer_char(struct Compiler *C, Str const *modname, struct SourceSpan span, char c, char const *base)
{
    throw(C, E_UNEXPECTED_INTEGER_CHAR, modname, span,
            format(C, "unexpected '%c' in %s integer", c, base),
            NULL);
}

_Noreturn void pawErr_integer_too_big_to_parse(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *text)
{
    throw(C, E_INTEGER_OUT_OF_RANGE, modname, span,
            format(C, "integer \"%s\" out of range", text),
            NULL);
}

_Noreturn void pawErr_integer_out_of_range(struct Compiler *C, Str const *modname, struct SourceSpan span, paw_Uint u)
{
    char buffer[64];
    snprintf(buffer, sizeof(buffer), "%" PRIu64, u);

    pawErr_integer_too_big_to_parse(C, modname, span, buffer);
}

_Noreturn void pawErr_invalid_integer(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *base, char const *text)
{
    throw(C, E_INVALID_INTEGER, modname, span,
            format(C, "invalid integer \"%s\"", text),
            NULL);
}

_Noreturn void pawErr_invalid_float(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *text)
{
    throw(C, E_INVALID_FLOAT, modname, span,
            format(C, "invalid float \"%s\"", text),
            NULL);
}

_Noreturn void pawErr_null_before_eof(struct Compiler *C, Str const *modname, struct SourceSpan span, int length)
{
    throw(C, E_NULL_BEFORE_EOF, modname, span,
            format(C, "encountered '\\0' before end of file"),
            NULL);
}

_Noreturn void pawErr_unexpected_symbol(struct Compiler *C, Str const *modname, struct SourceSpan span)
{
    throw(C, E_EXPECTED_SYMBOL, modname, span,
            format(C, "unexpected symbol"), // TODO: print symbol and what was expected
            NULL);
}

_Noreturn void pawErr_expected_delimiter(struct Compiler *C, Str const *modname, struct SourceSpan span, char right, char left, struct SourceLoc open)
{
    throw(C, E_EXPECTED_DELIMITER, modname, span,
            format(C, "expected '%c' to match '%c' at %d:%d", right, left, open.line, open.column),
            NULL);
}

_Noreturn void pawErr_expected_semicolon(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *what)
{
    throw(C, E_EXPECTED_SEMICOLON, modname, span,
            format(C, "expected semicolon after %s", what),
            NULL);
}

_Noreturn void pawErr_empty_type_list(struct Compiler *C, Str const *modname, struct SourceSpan span)
{
    throw(C, E_EMPTY_TYPE_LIST, modname, span,
            format(C, "expected at least 1 type"),
            NULL);
}

_Noreturn void pawErr_multiple_applicable_items(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *path)
{
    throw(C, E_MULTIPLE_APPLICABLE_ITEMS, modname, span,
            format(C, "multiple applicable items for path \"%s\"", path),
            NULL);
}

_Noreturn void pawErr_nonliteral_pattern(struct Compiler *C, Str const *modname, struct SourceSpan span)
{
    throw(C, E_NONLITERAL_PATTERN, modname, span,
            format(C, "expected literal pattern"),
            NULL);
}

_Noreturn void pawErr_negative_minimum_integer(struct Compiler *C, Str const *modname, struct SourceSpan span)
{
    throw(C, E_NEGATIVE_MINIMUM_INTEGER, modname, span,
            format(C, "operator '-' applied to minimum integer %I", PAW_INT_MIN),
            format(C, "results in signed integer overflow"));
}

_Noreturn void pawErr_invalid_literal_negation(struct Compiler *C, Str const *modname, struct SourceSpan span)
{
    throw(C, E_INVALID_LITERAL_NEGATION, modname, span,
            format(C, "operator '-' applied to non-numeric operand"),
            NULL);
}

_Noreturn void pawErr_invalid_selector(struct Compiler *C, Str const *modname, struct SourceSpan span)
{
    throw(C, E_INVALID_SELECTOR, modname, span,
            format(C, "expected integer or name after '.'"),
            NULL);
}

_Noreturn void pawErr_expected_basic_type(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *what)
{
    throw(C, E_EXPECTED_BASIC_TYPE, modname, span,
            format(C, "expected basic type but found \"%s\"", what),
            NULL);
}

_Noreturn void pawErr_expected_expression(struct Compiler *C, Str const *modname, struct SourceSpan span)
{
    throw(C, E_EXPECTED_EXPRESSION, modname, span,
            format(C, "expected expression"),
            NULL);
}

_Noreturn void pawErr_empty_enumeration(struct Compiler *C, Str const *modname, struct SourceSpan span)
{
    throw(C, E_EMPTY_ENUMERATION, modname, span,
            format(C, "enumeration has no variants"),
            NULL);
}

_Noreturn void pawErr_empty_struct_body(struct Compiler *C, Str const *modname, struct SourceSpan span)
{
    throw(C, E_EMPTY_STRUCT_BODY, modname, span,
            format(C, "expected at least 1 field or method in structure body"),
            format(C, "omit curly braces to define unit structure"));
}

_Noreturn void pawErr_empty_variant_field_list(struct Compiler *C, Str const *modname, struct SourceSpan span)
{
    throw(C, E_EMPTY_VARIANT_FIELD_LIST, modname, span,
            format(C, "expected at least 1 variant field between parenthesis"),
            format(C, "remove parenthesis to construct unit variant"));
}

_Noreturn void pawErr_function_type_decl(struct Compiler *C, Str const *modname, struct SourceSpan span)
{
    throw(C, E_FUNCTION_TYPE_DECL, modname, span,
            format(C, "function types are not allowed in \"type\" declarations"),
            NULL);
}

_Noreturn void pawErr_invalid_annotation_type(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *name)
{
    throw(C, E_INVALID_ANNOTATION_TYPE, modname, span,
            format(C, "invalid value type for annotation \"%s\"", name),
            NULL);
}

_Noreturn void pawErr_expected_type_annotation(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *what, char const *name)
{
    throw(C, E_FUNCTION_TYPE_DECL, modname, span,
            format(C, "expected type annotation on %s \"%s\"", what, name),
            NULL);
}

_Noreturn void pawErr_return_outside_function(struct Compiler *C, Str const *modname, struct SourceSpan span)
{
    throw(C, E_RETURN_OUTSIDE_FUNCTION, modname, span,
            format(C, "return outside function body"),
            NULL);
}

_Noreturn void pawErr_chain_outside_function(struct Compiler *C, Str const *modname, struct SourceSpan span)
{
    throw(C, E_CHAIN_OUTSIDE_FUNCTION, modname, span,
            format(C, "operator '?' encountered outside function body"),
            NULL);
}

_Noreturn void pawErr_jump_outside_loop(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *kind)
{
    throw(C, E_JUMP_OUTSIDE_LOOP, modname, span,
            format(C, "\"%s\" outside loop", kind),
            format(C, "limit is %d"));
}

_Noreturn void pawErr_too_many_elements(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *what, int limit)
{
    throw(C, E_TOO_MANY_ELEMENTS, modname, span,
            format(C, "too many %s"),
            format(C, "limit is %d", limit));
}

_Noreturn void pawErr_expected_toplevel_item(struct Compiler *C, Str const *modname, struct SourceSpan span)
{
    throw(C, E_EXPECTED_TOPLEVEL_ITEM, modname, span,
            format(C, "expected toplevel item"),
            NULL);
}

_Noreturn void pawErr_expected_colon_after_map_key(struct Compiler *C, Str const *modname, struct SourceSpan span)
{
    throw(C, E_EXPECTED_COLON_AFTER_MAP_KEY, modname, span,
            format(C, "expected ':' after map key"),
            NULL);
}

_Noreturn void pawErr_colon_after_list_element(struct Compiler *C, Str const *modname, struct SourceSpan span)
{
    throw(C, E_COLON_AFTER_LIST_ELEMENT, modname, span,
            format(C, "unexpected ':' after list element"),
            NULL);
}

_Noreturn void pawErr_expected_self_parameter(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *name)
{
    throw(C, E_EXPECTED_SELF_PARAMETER, modname, span,
            format(C, "expected parameter named \"self\" but found \"%s\"", name),
            NULL);
}

_Noreturn void pawErr_unexpected_underscore(struct Compiler *C, Str const *modname, struct SourceSpan span)
{
    throw(C, E_UNEXPECTED_UNDERSCORE, modname, span,
            format(C, "'_' is not allowed here"),
            NULL);
}

_Noreturn void pawErr_expected_comma_separator(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *what)
{
    throw(C, E_EXPECTED_COMMA_SEPARATOR, modname, span,
            format(C, "expected ',' to separate %s from other items", what),
            NULL);
}

_Noreturn void pawErr_duplicate_annotation(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *name)
{
    throw(C, E_DUPLICATE_ANNOTATION, modname, span,
            format(C, "duplicate annotation \"%s\"", name),
            NULL);
}

_Noreturn void pawErr_nonliteral_annotation_value(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *name)
{
    throw(C, E_NONLITERAL_ANNOTATION_VALUE, modname, span,
            format(C, "value for \"%s\" annotation must be a literal", name),
            NULL);
}

_Noreturn void pawErr_nonprimitive_annotation_value(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *name)
{
    throw(C, E_NONPRIMITIVE_ANNOTATION_VALUE, modname, span,
            format(C, "value for \"%s\" annotation must be a primitive type", name),
            NULL);
}

_Noreturn void pawErr_invalid_glob(struct Compiler *C, Str const *modname, struct SourceSpan span)
{
    throw(C, E_INVALID_GLOB, modname, span,
            format(C, "'*' must be the last path segment and cannot have an \"as\" alias"),
            NULL);
}

_Noreturn void pawErr_module_not_found(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *name)
{
    throw(C, E_MODULE_NOT_FOUND, modname, span,
            format(C, "module \"%s\" not found", name),
            NULL);
}

_Noreturn void pawErr_invalid_assignment_target(struct Compiler *C, Str const *modname, struct SourceSpan span)
{
    throw(C, E_INVALID_ASSIGNMENT_TARGET, modname, span,
            format(C, "invalid target for assignment"),
            NULL);
}

_Noreturn void pawErr_ambiguous_path(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *path)
{
    throw(C, E_AMBIGUOUS_PATH, modname, span,
            format(C, "path \"%s\" is ambiguous", path),
            NULL);
}

_Noreturn void pawErr_duplicate_item(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *what, char const *name)
{
    throw(C, E_DUPLICATE_ITEM, modname, span,
            format(C, "duplicate %s \"%s\"", what, name),
            NULL);
}

_Noreturn void pawErr_extern_function_body(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *name)
{
    throw(C, E_EXTERN_FUNCTION_BODY, modname, span,
            format(C, "unexpected body on extern function \"%s\"", name),
            NULL);
}

_Noreturn void pawErr_missing_function_body(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *name)
{
    throw(C, E_MISSING_FUNCTION_BODY, modname, span,
            format(C, "missing body for function \"%s\"", name),
            NULL);
}

_Noreturn void pawErr_item_visibility(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *mod, char const *item)
{
    throw(C, E_ITEM_VISIBILITY, modname, span,
            format(C, "item \"%s::%s\" cannot be accessed from the current module", mod, item),
            NULL);
}

_Noreturn void pawErr_associated_item_visibility(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *name, char const *type)
{
    throw(C, E_ASSOCIATED_ITEM_VISIBILITY, modname, span,
            format(C, "item \"%s\" cannot be accessed from outside a method on type \"%s\"", name, type),
            NULL);
}

_Noreturn void pawErr_invalid_glob_target(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *path)
{
    throw(C, E_ASSOCIATED_ITEM_VISIBILITY, modname, span,
            format(C, "invalid glob import \"%s\"", path),
            format(C, "kind of item cannot be glob imported"));
}

_Noreturn void pawErr_missing_trait_bounds(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *name)
{
    throw(C, E_MISSING_TRAIT_BOUNDS, modname, span,
            format(C, "type parameter \"%s\" missing trait bounds", name),
            NULL);
}

_Noreturn void pawErr_trait_bounds_on_alias_generic(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *name)
{
    throw(C, E_TRAIT_BOUNDS_ON_ALIAS_GENERIC, modname, span,
            format(C, "trait bounds not allowed on type parameter \"%s\" for type alias", name),
            NULL);
}

_Noreturn void pawErr_unsatisfied_trait_bounds(struct Compiler *C, Str const *modname, struct SourceSpan span)
{
    throw(C, E_UNSATISFIED_TRAIT_BOUNDS, modname, span,
            format(C, "trait bounds not satisfied"),
            NULL);
}

_Noreturn void pawErr_incompatible_types(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *lhs, char const *rhs)
{
    throw(C, E_INCOMPATIBLE_TYPES, modname, span,
            format(C, "incompatible types \"%s\" and \"%s\"", lhs, rhs),
            NULL);
}

_Noreturn void pawErr_cannot_infer(struct Compiler *C, Str const *modname, struct SourceSpan span)
{
    throw(C, E_CANNOT_INFER, modname, span,
            format(C, "unable to infer type"),
            NULL);
}

_Noreturn void pawErr_cyclic_type(struct Compiler *C, Str const *modname, struct SourceSpan span)
{
    throw(C, E_CYCLIC_TYPE, modname, span,
            format(C, "encountered cyclic type"),
            NULL);
}

_Noreturn void pawErr_missing_extern_value(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *name)
{
    throw(C, E_MISSING_EXTERN_VALUE, modname, span,
            format(C, "missing extern value \"%s\"", name),
            NULL);
}

_Noreturn void pawErr_unexpected_module_name(struct Compiler *C, Str const *modname, struct SourceSpan span)
{
    throw(C, E_UNEXPECTED_MODULE_NAME, modname, span,
            format(C, "unexpected module name"),
            NULL);
}

_Noreturn void pawErr_transitive_import(struct Compiler *C, Str const *modname, struct SourceSpan span)
{
    throw(C, E_TRANSITIVE_IMPORT, modname, span,
            format(C, "transitive imports are not supported"),
            NULL);
}

_Noreturn void pawErr_incorrect_type_arity(struct Compiler *C, Str const *modname, struct SourceSpan span, int want, int have)
{
    throw(C, E_INCORRECT_TYPE_ARITY, modname, span,
            format(C, "%s types", have < want ? "not enough" : "too many"),
            format(C, "expected %d but have %d", want, have));
}

_Noreturn void pawErr_expected_type_arguments(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *what, char const *name)
{
    throw(C, E_EXPECTED_TYPE_ARGUMENTS, modname, span,
            format(C, "expected type arguments on %s \"%s\"", what, name),
            NULL);
}

_Noreturn void pawErr_unexpected_type_arguments(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *what, char const *name)
{
    throw(C, E_UNEXPECTED_TYPE_ARGUMENTS, modname, span,
            format(C, "unexpected type arguments on %s \"%s\"", what, name),
            NULL);
}

_Noreturn void pawErr_expected_trait(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *path)
{
    throw(C, E_EXPECTED_TRAIT, modname, span,
            format(C, "expected trait but found type \"%s\"", path),
            NULL);
}

_Noreturn void pawErr_unexpected_trait(struct Compiler *C, Str const *modname, struct SourceSpan span)
{
    throw(C, E_UNEXPECTED_TRAIT, modname, span,
            format(C, "unexpected trait"),
            NULL);
}

_Noreturn void pawErr_incorrect_item_class(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *want, char const *have)
{
    throw(C, E_INCORRECT_ITEM_CLASS, modname, span,
            format(C, "expected %s but found %s", want, have),
            NULL);
}

_Noreturn void pawErr_extra_segment(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *name)
{
    throw(C, E_EXTRA_SEGMENT, modname, span,
            format(C, "extraneous \"::%s\"", name),
            NULL);
}

_Noreturn void pawErr_missing_trait_method(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *name)
{
    throw(C, E_MISSING_TRAIT_METHOD, modname, span,
            format(C, "trait method \"%s\" not implemented", name),
            NULL);
}

_Noreturn void pawErr_trait_method_visibility_mismatch(struct Compiler *C, Str const *modname, struct SourceSpan span, paw_Bool expected_pub, char const *name)
{
    throw(C, E_TRAIT_METHOD_VISIBILITY_MISMATCH, modname, span,
            format(C, "visibility mismatch (expected %s visibility on method \"%s\")", expected_pub ? "public" : "private", name),
            NULL);
}

_Noreturn void pawErr_missing_variant_args(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *cons)
{
    throw(C, E_MISSING_VARIANT_ARGS, modname, span,
            format(C, "missing argument(s) for variant constructor \"%s\"", cons),
            NULL);
}

_Noreturn void pawErr_reserved_identifier(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *name)
{
    throw(C, E_RESERVED_IDENTIFIER, modname, span,
            format(C, "\"%s\" is a reserved identifier", name),
            NULL);
}

_Noreturn void pawErr_unexpected_type(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *type)
{
    throw(C, E_UNEXPECTED_TYPE, modname, span,
            format(C, "expected value but found type \"%s\"", type),
            NULL);
}

_Noreturn void pawErr_unknown_path(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *path)
{
    throw(C, E_UNKNOWN_PATH, modname, span,
            format(C, "unknown path \"%s\"", path),
            NULL);
}

_Noreturn void pawErr_missing_fields(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *type)
{
    throw(C, E_MISSING_FIELDS, modname, span,
            format(C, "missing fields on initializer for struct \"%s\"", type),
            NULL);
}

_Noreturn void pawErr_expected_value(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *type)
{
    throw(C, E_EXPECTED_VALUE, modname, span,
            format(C, "expected value but found type \"%s\"", type),
            NULL);
}

_Noreturn void pawErr_invalid_chain_operand(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *type)
{
    throw(C, E_INVALID_CHAIN_OPERAND, modname, span,
            format(C, "invalid operand type \"%s\" for chain operator", type),
            NULL);
}

_Noreturn void pawErr_invalid_unary_operand(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *type, char const *op)
{
    throw(C, E_INVALID_UNARY_OPERAND, modname, span,
            format(C, "invalid operand type \"%s\" for unary operator \"%s\"", type, op),
            NULL);
}

_Noreturn void pawErr_invalid_binary_operand(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *type, char const *op)
{
    throw(C, E_INVALID_BINARY_OPERAND, modname, span,
            format(C, "invalid operand type \"%s\" for binary operator \"%s\"", type, op),
            NULL);
}

_Noreturn void pawErr_cannot_constant_evaluate(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *what)
{
    throw(C, E_CANNOT_CONSTANT_EVALUATE, modname, span,
            format(C, "%s cannot be constant evaluated", what),
            NULL);
}

_Noreturn void pawErr_nonprimitive_constant(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *type)
{
    throw(C, E_NONPRIMITIVE_CONSTANT, modname, span,
            format(C, "expected primitive constant but found \"%s\"", type),
            NULL);
}

_Noreturn void pawErr_expected_element_selector(struct Compiler *C, Str const *modname, struct SourceSpan span)
{
    throw(C, E_EXPECTED_ELEMENT_SELECTOR, modname, span,
            format(C, "expected integer element selector"),
            NULL);
}

_Noreturn void pawErr_element_selector_out_of_range(struct Compiler *C, Str const *modname, struct SourceSpan span, int elem, int count)
{
    throw(C, E_ELEMENT_SELECTOR_OUT_OF_RANGE, modname, span,
            format(C, "element selector %d is out of range for %d-tuple", elem, count),
            NULL);
}

_Noreturn void pawErr_expected_adt(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *type)
{
    throw(C, E_EXPECTED_ADT, modname, span,
            format(C, "expected ADT but found \"%s\"", type),
            NULL);
}

_Noreturn void pawErr_expected_field_selector(struct Compiler *C, Str const *modname, struct SourceSpan span)
{
    throw(C, E_EXPECTED_FIELD_SELECTOR, modname, span,
            format(C, "expected field selector"),
            NULL);
}

_Noreturn void pawErr_unknown_method(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *method, char const *type)
{
    throw(C, E_UNKNOWN_METHOD, modname, span,
            format(C, "unknown method \"%s\" for type \"%s\"", method, type),
            NULL);
}

_Noreturn void pawErr_unknown_associated_item(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *method, char const *type)
{
    throw(C, E_UNKNOWN_ASSOCIATED_ITEM, modname, span,
            format(C, "unknown associated item \"%s\" for type \"%s\"", method, type),
            NULL);
}


_Noreturn void pawErr_not_a_method(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *name)
{
    throw(C, E_NOT_A_METHOD, modname, span,
            format(C, "\"%s\" is not a method", name),
            NULL);
}

_Noreturn void pawErr_not_callable(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *type)
{
    throw(C, E_NOT_CALLABLE, modname, span,
            format(C, "type \"%s\" is not callable", type),
            NULL);
}

_Noreturn void pawErr_incorrect_arity(struct Compiler *C, Str const *modname, struct SourceSpan span, int want, int have)
{
    throw(C, E_INCORRECT_ARITY, modname, span,
            format(C, "%s arguments", have < want ? "not enough" : "too many"),
            format(C, "expected %d but have %d", want, have));
}

_Noreturn void pawErr_duplicate_field(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *name, char const *type)
{
    throw(C, E_DUPLICATE_FIELD, modname, span,
            format(C, "duplicate field \"%s\" in \"%s\"", name, type),
            NULL);
}

_Noreturn void pawErr_duplicate_binding(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *name)
{
    throw(C, E_DUPLICATE_BINDING, modname, span,
            format(C, "duplicate binding \"%s\"", name),
            NULL);
}

_Noreturn void pawErr_expected_struct(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *type)
{
    throw(C, E_EXPECTED_STRUCT, modname, span,
            format(C, "expected struct but found \"%s\"", type),
            NULL);
}

_Noreturn void pawErr_unit_struct_with_braces(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *type)
{
    throw(C, E_UNIT_STRUCT_WITH_BRACES, modname, span,
            format(C, "unexpected braces on unit struct \"%s\"", type),
            format(C, "omit braces to construct unit struct"));
}

_Noreturn void pawErr_unit_variant_with_parenthesis(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *type)
{
    throw(C, E_UNIT_VARIANT_WITH_PARENTHESIS, modname, span,
            format(C, "unexpected parenthesis on unit variant \"%s\"", type),
            format(C, "omit parenthesis to construct unit variant"));
}

_Noreturn void pawErr_missing_field(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *name, char const *type)
{
    throw(C, E_MISSING_FIELD, modname, span,
            format(C, "missing initializer for field \"%s\" on struct \"%s\"", name, type),
            NULL);
}

_Noreturn void pawErr_unknown_field(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *name, char const *type)
{
    throw(C, E_UNKNOWN_FIELD, modname, span,
            format(C, "unknown field \"%s\" on struct \"%s\"", name, type),
            NULL);
}

_Noreturn void pawErr_invalid_index_target(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *type)
{
    throw(C, E_INVALID_INDEX_TARGET, modname, span,
            format(C, "invalid target \"%s\" for index operator", type),
            NULL);
}

_Noreturn void pawErr_missing_binding_in_alternative(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *name)
{
    throw(C, E_MISSING_BINDING_IN_ALTERNATIVE, modname, span,
            format(C, "missing binding \"%s\" in alternative pattern", name),
            NULL);
}

_Noreturn void pawErr_expected_divergence(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *fn)
{
    throw(C, E_EXPECTED_DIVERGENCE, modname, span,
            format(C, "expected \"%s\" to diverge based on return type of '!'", fn),
            NULL);
}

_Noreturn void pawErr_invalid_inclusive_range(struct Compiler *C, Str const *modname, struct SourceSpan span)
{
    throw(C, E_INVALID_INCLUSIVE_RANGE, modname, span,
            format(C, "type of range cannot be inclusive"),
            NULL);
}

_Noreturn void pawErr_global_constant_cycle(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *name)
{
    throw(C, E_GLOBAL_CONSTANT_CYCLE, modname, span,
            format(C, "cycle detected between global constants"),
            format(C, "involves constant \"%s\"", name));
}

_Noreturn void pawErr_uninitialized_destructuring(struct Compiler *C, Str const *modname, struct SourceSpan span)
{
    throw(C, E_UNINITIALIZED_DESTRUCTURING, modname, span,
            format(C, "variables using deferred initialization cannot use destructuring"),
            NULL);
}

_Noreturn void pawErr_modified_constant(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *name)
{
    throw(C, E_MODIFIED_CONSTANT, modname, span,
            format(C, "attempt to modify constant \"%s\"", name),
            NULL);
}

_Noreturn void pawErr_too_many_upvalues(struct Compiler *C, Str const *modname, struct SourceSpan span, int limit)
{
    throw(C, E_TOO_MANY_UPVALUES, modname, span,
            format(C, "too many upvalues in function"),
            format(C, "limit is %d", limit));
}

_Noreturn void pawErr_initialized_extern_constant(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *name)
{
    throw(C, E_INITIALIZED_EXTERN_CONSTANT, modname, span,
            format(C, "unexpected initializer for \"extern\" constant \"%s\"", name),
            NULL);
}

_Noreturn void pawErr_uninitialized_constant(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *name)
{
    throw(C, E_UNINITIALIZED_CONSTANT, modname, span,
            format(C, "missing initializer for constant \"%s\"", name),
            NULL);
}

_Noreturn void pawErr_infinite_size_object(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *name)
{
    throw(C, E_INFINITE_SIZE_OBJECT, modname, span,
            format(C, "object \"%s\" has infinite size", name),
            NULL);
}

_Noreturn void pawErr_object_too_large(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *name)
{
    throw(C, E_OBJECT_TOO_LARGE, modname, span,
            format(C, "object \"%s\" is too large", name),
            NULL);
}

_Noreturn void pawErr_nonexhaustive_pattern_match(struct Compiler *C, Str const *modname, struct SourceSpan span)
{
    throw(C, E_NONEXHAUSTIVE_PATTERN_MATCH, modname, span,
            format(C, "nonexhaustive pattern match"),
            NULL);
}

_Noreturn void pawErr_use_before_initialization(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *name)
{
    throw(C, E_USE_BEFORE_INITIALIZATION, modname, span,
            format(C, "use of variable \"%s\" before initialization", name),
            NULL);
}

_Noreturn void pawErr_constant_divide_by_zero(struct Compiler *C, Str const *modname, struct SourceSpan span)
{
    throw(C, E_CONSTANT_DIVIDE_BY_ZERO, modname, span,
            format(C, "constant division by 0"),
            NULL);
}

_Noreturn void pawErr_constant_negative_shift_count(struct Compiler *C, Str const *modname, struct SourceSpan span)
{
    throw(C, E_CONSTANT_NEGATIVE_SHIFT_COUNT, modname, span,
            format(C, "constant negative shift count"),
            NULL);
}

_Noreturn void pawErr_too_many_variables(struct Compiler *C, Str const *modname, struct SourceSpan span, int limit)
{
    throw(C, E_TOO_MANY_VARIABLES, modname, span,
            format(C, "too many variables"),
            format(C, "limit is %d", limit));
}


_Noreturn void pawErr_too_far_to_jump(struct Compiler *C, Str const *modname, struct SourceSpan span, int limit)
{
    throw(C, E_TOO_FAR_TO_JUMP, modname, span,
            format(C, "too far to jump"),
            format(C, "limit is %d instructions", limit));
}


_Noreturn void pawErr_too_many_constants(struct Compiler *C, Str const *modname, struct SourceSpan span, int limit)
{
    throw(C, E_TOO_MANY_CONSTANTS, modname, span,
            format(C, "too many constants"),
            format(C, "limit is %d", limit));
}

_Noreturn void pawErr_captured_inout_arg(struct Compiler *C, Str const *modname, struct SourceSpan span)
{
    throw(C, E_CAPTURED_INOUT_ARG, modname, span,
            format(C, "inout argument cannot be captured"),
            NULL);
}





void pawErr_start(paw_Env *P)
{
    P->error = (ErrorHandler){.P = P};
}

void pawErr_set_module_name(paw_Env *P, Str const *name)
{
    P->error.modname = name;
}

void pawErr_set_source_loc(paw_Env *P, struct SourceSpan span)
{
    P->error.span = span;
}

static Str const *format_(paw_Env *P, char const *fmt, va_list arg)
{
    Buffer b;
    pawL_init_buffer(P, &b);
    pawL_add_vfstring(P, &b, fmt, arg);
    return pawL_buffer_finish(P, &b);
}

void pawErr_set_message(paw_Env *P, char const *fmt, ...)
{
    ErrorHandler *e = &P->error;

    va_list arg;
    va_start(arg, fmt);
    e->message = format_(e->P, fmt, arg);
    va_end(arg);
}

void pawErr_set_hint(paw_Env *P, char const *fmt, ...)
{
    ErrorHandler *e = &P->error;

    va_list arg;
    va_start(arg, fmt);
    e->hint = format_(e->P, fmt, arg);
    va_end(arg);
}

void pawErr_finish(paw_Env *P)
{
    ErrorHandler *e = &P->error;

    Buffer b;
    pawL_init_buffer(P, &b);

    if (e->modname != NULL)
        pawL_add_fstring(P, &b, "%s", e->modname->text);

//    if (e->span.line > 0) {
//        if (b.size > 0) pawL_add_char(P, &b, ':'); // separator
//        pawL_add_fstring(P, &b, "%d:%d", e->span.line, e->span.column);
//    }
    if (e->message != NULL) {
        if (b.size > 0) pawL_add_char(P, &b, ' '); // separator
        pawL_add_fstring(P, &b, "%s", e->message->text);
    }
    if (e->hint != NULL) {
        if (b.size > 0) pawL_add_char(P, &b, ' '); // separator
        pawL_add_fstring(P, &b, "(%s)", e->hint->text);
    }

    P->current_errmsg = pawL_buffer_finish(P, &b);
}

_Noreturn void pawErr_generic_error(paw_Env *P, Str const *modname, struct SourceSpan span, char const *fmt, ...)
{
    pawErr_start(P);
    pawErr_set_module_name(P, modname);
    pawErr_set_source_loc(P, span);

    va_list arg;
    va_start(arg, fmt);
    pawErr_set_message(P, fmt, arg);
    va_end(arg);

    pawErr_finish(P);
    pawC_throw(P, -1);
}

_Noreturn void pawErr_exceeded_limit(paw_Env *P, Str const *modname, struct SourceSpan span, char const *what, paw_Int limit)
{
    pawErr_start(P);
    pawErr_set_module_name(P, modname);
    pawErr_set_source_loc(P, span);
    pawErr_set_message(P, "too many %s", what);
    pawErr_set_hint(P, "expected at most %I", limit);
    pawErr_finish(P);
    pawC_throw(P, -1);
}


static char const *ref_kind_name(enum SpanRefKind kind)
{
    switch (kind) {
        case SPAN_REF_TRAIT_SELF:
            return "trait self type";
        case SPAN_REF_FOR_LOOP:
            return "for loop";
        case SPAN_REF_QUESTION_MARK:
            return "question mark operator";
        case SPAN_REF_RANGE:
            return "constructor for range";
    }
}

static paw_Bool maybe_deref_span(paw_Env *P, struct SourceSpan *span_ptr, enum SpanRefKind *ref_kind_ptr)
{
    if (span_ptr->kind == SRC_SPAN_REF) {
        for (;;) {
            if (ref_kind_ptr != NULL) *ref_kind_ptr = SourceSpan_ref_kind(*span_ptr);
            *span_ptr = pawSrc_follow_ref(P->C, SourceSpan_ref_value(*span_ptr));
            if (span_ptr->kind != SRC_SPAN_REF)
                return PAW_TRUE;
        }
    }
    return PAW_FALSE;
}

static void add_location(paw_Env *P, struct SourceLoc loc, Buffer *buffer)
{
    pawL_add_fstring(P, buffer, "%d:%d",
            loc.line, loc.column);
}

static void add_span_start_location(paw_Env *P, struct SourceSpan span, Buffer *buffer)
{
    enum SpanRefKind ref_kind;
    paw_Bool is_ref = maybe_deref_span(P, &span, &ref_kind);
    if (is_ref) {
        pawL_add_fstring(P, buffer, "code generated from %s at ",
                ref_kind_name(ref_kind));
    }
    struct SourceLoc const start = SourceSpan_range_start(span);
    add_location(P, start, buffer);
}

static void add_error_header(paw_Env *P, Str const *modname, struct SourceSpan span, Buffer *buffer)
{
    enum SpanRefKind ref_kind;
    paw_Bool const is_ref = maybe_deref_span(P, &span, &ref_kind);
    struct SourceLoc const start = SourceSpan_range_start(span);
    pawL_add_fstring(P, buffer, "%s:", modname->text);
    add_location(P, start, buffer);

    if (is_ref)
        pawL_add_fstring(P, buffer, " (generated for %s)",
                ref_kind_name(ref_kind));

    L_ADD_LITERAL(P, buffer, ": ");
}

static Str const *name_of_base(paw_Env *P, int base)
{
    switch (base) {
        case 2:
            return SCAN_STR(P->C, "binary");
        case 8:
            return SCAN_STR(P->C, "octal");
        case 10:
            return SCAN_STR(P->C, "decimal");
        case 16:
            return SCAN_STR(P->C, "hexadecimal");
        default:
            return pawP_format_string(P->C, "base-%d", base);
    }
}










static void FormatTooManyLinesError(paw_Env *P, struct TooManyLinesError *error, Buffer *buffer)
{
    pawL_add_fstring(P, buffer,
            "%s: too many lines in file \"%s\" (expected at most %d)",
            error->modname->text, error->filename->text, error->max_lines);
}

static void FormatTooManyColumnsError(paw_Env *P, struct TooManyColumnsError *error, Buffer *buffer)
{
    pawL_add_fstring(P, buffer,
            "%s: too many columns in file \"%s\" (expected at most %d)",
            error->modname->text, error->filename->text, error->max_columns);
}

static void FormatNameTooLongError(paw_Env *P, struct NameTooLongError *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer,
            "name too long (expected at most %d characters)",
            error->max_chars);
}

static void FormatInvalidStrLiteralError(paw_Env *P, struct InvalidStrLiteralError *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "invalid string literal (%s)",
            error->reason->text);
}

static void FormatEmptyStrLiteralError(paw_Env *P, struct EmptyStrLiteralError *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "empty string literal");
}

static void FormatStrLiteralTooLongError(paw_Env *P, struct StrLiteralTooLongError *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "string literal too long");
}

static void FormatUnterminatedStrLiteralError(paw_Env *P, struct UnterminatedStrLiteralError *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "unterminated string literal");
}

static void FormatInvalidCharLiteralError(paw_Env *P, struct InvalidCharLiteralError *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "invalid character literal (%s)",
            error->reason->text);
}

static void FormatEmptyCharLiteralError(paw_Env *P, struct EmptyCharLiteralError *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "empty character literal");
}

static void FormatCharLiteralTooLongError(paw_Env *P, struct CharLiteralTooLongError *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "character literal too long");
}

static void FormatUnterminatedCharLiteralError(paw_Env *P, struct UnterminatedCharLiteralError *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "unterminated character literal");
}

static void FormatUnknownEscapeCharError(paw_Env *P, struct UnknownEscapeCharError *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "unknown escape character");
}

static void FormatInvalidCharInHexEscapeError(paw_Env *P, struct InvalidCharInHexEscapeError *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "invalid character found in hex escape");
}

static void FormatInvalidUnicodeEscapeError(paw_Env *P, struct InvalidUnicodeEscapeError *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
}

static void FormatInvalidUnicodeCodepointError(paw_Env *P, struct InvalidUnicodeCodepointError *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
}

static void FormatUnicodeEscapeTooLongError(paw_Env *P, struct UnicodeEscapeTooLongError *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
}

static void FormatInvalidCharInUnicodeEscapeError(paw_Env *P, struct InvalidCharInUnicodeEscapeError *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "invalid character found in unicode escape");
}

static void FormatEmptyUnicodeEscapeError(paw_Env *P, struct EmptyUnicodeEscapeError *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
}

static void FormatUnterminatedUnicodeEscapeError(paw_Env *P, struct UnterminatedUnicodeEscapeError *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
}

static void FormatIntegerTooBigToParseError(paw_Env *P, struct IntegerTooBigToParseError *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "%s integer too big to parse",
            name_of_base(P, error->base)->text);
}

static void FormatInvalidIntegerLiteralError(paw_Env *P, struct InvalidIntegerLiteralError *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
}

static void FormatExpectedIntegerDigitError(paw_Env *P, struct ExpectedIntegerDigitError *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
}

static void FormatInvalidCharInIntegerError(paw_Env *P, struct InvalidCharInIntegerError *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "unexpected '%c' in %s integer",
            error->invalid, name_of_base(P, error->base)->text);
}

static void FormatInvalidFloatLiteralError(paw_Env *P, struct InvalidFloatLiteralError *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "invalid float literal (%s)",
            error->reason->text);
}

static void FormatExpectedSymbolError(paw_Env *P, struct ExpectedSymbolError *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "expected symbol \"%s\" but found \"%s\"",
            error->expected->text, error->have->text);
}

static void FormatExpectedDelimiterError(paw_Env *P, struct ExpectedDelimiterError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "expected '%c' to match '%c' at ",
            error->close, error->open);
    add_location(P, error->open_loc, buffer);
}

static void FormatExpectedSemicolonError(paw_Env *P, struct ExpectedSemicolonError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "expected semicolon after \"%s\"",
            error->what->text);
}

static void FormatEmptyTypeListError(paw_Env *P, struct EmptyTypeListError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
}

static void FormatNonliteralPatternError(paw_Env *P, struct NonliteralPatternError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
}

static void FormatIntegerOutOfRangeError(paw_Env *P, struct IntegerOutOfRangeError *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer,
            "integer %U out of range of type \"int\" (maximum value is %I)",
            error->uint64, PAW_INT_MAX);
}

static void FormatNegativeIntegerOutOfRangeError(paw_Env *P, struct NegativeIntegerOutOfRangeError *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer,
            "integer -%U out of range of type \"int\" (minimum value is %I)",
            error->uint64, PAW_INT_MIN);
}

static void FormatInvalidLiteralNegationError(paw_Env *P, struct InvalidLiteralNegationError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer,
            "encountered operator \"-\" applied to non-integral literal");
}

static void FormatInvalidSelectorError(paw_Env *P, struct InvalidSelectorError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "invalid selector");
}

static void FormatExpectedBasicTypeError(paw_Env *P, struct ExpectedBasicTypeError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "expected basic type");
}

static void FormatExpectedExpressionError(paw_Env *P, struct ExpectedExpressionError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "expected expression");
}

static void FormatEmptyEnumerationError(paw_Env *P, struct EmptyEnumerationError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "empty enumeration");
}

static void FormatEmptyStructBodyError(paw_Env *P, struct EmptyStructBodyError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "empty struct body (omit braces to define a unit structure)");
}

static void FormatEmptyVariantFieldListError(paw_Env *P, struct EmptyVariantFieldListError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer,
            "empty variant field list (omit parenthesis to define a variant without fields)");
}

static void FormatFunctionTypeDeclError(paw_Env *P, struct FunctionTypeDeclError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "function type cannot be aliased");
}

static void FormatInvalidAnnotationTypeError(paw_Env *P, struct InvalidAnnotationTypeError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer,
            "invalid value type for annotation \"%s\"",
            error->name->text);
}

static void FormatExpectedTypeAnnotationError(paw_Env *P, struct ExpectedTypeAnnotationError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer,
            "expected type annotation on %s \"%s\"",
            error->what->text, error->name->text);
}

static void FormatReturnOutsideFunctionError(paw_Env *P, struct ReturnOutsideFunctionError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "encountered \"return\" outside function body");
}

static void FormatChainOutsideFunctionError(paw_Env *P, struct ChainOutsideFunctionError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "encountered \"?\" operator outside function body");
}

static void FormatJumpOutsideLoopError(paw_Env *P, struct JumpOutsideLoopError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "encountered \"%s\" outside loop body",
            error->what->text);
}

static void FormatPathTooLongError(paw_Env *P, struct PathTooLongError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer,
            "too many path segments (expected at most %d)",
            error->max_segments);
}

static void FormatExpectedToplevelItemError(paw_Env *P, struct ExpectedToplevelItemError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
}

static void FormatInvalidSelfPtrShorthandError(paw_Env *P, struct InvalidSelfPtrShorthandError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer,
            "\"*%s\" can only appear before \"self\" or a type",
            error->is_mut ? "mut" : "");
}

static void FormatUnexpectedUnderscoreError(paw_Env *P, struct UnexpectedUnderscoreError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
}

static void FormatExpectedCommaSeparatorError(paw_Env *P, struct ExpectedCommaSeparatorError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
}

static void FormatDuplicateAnnotationError(paw_Env *P, struct DuplicateAnnotationError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "encountered duplicate of annotation at ");
    add_span_start_location(P, error->previous, buffer);
}

static void FormatNonliteralAnnotationValueError(paw_Env *P, struct NonliteralAnnotationValueError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer,
            "annotation value for \"%s\" is not a literal expression",
            error->name->text);
}

static void FormatNonprimitiveAnnotationValueError(paw_Env *P, struct NonprimitiveAnnotationValueError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer,
            "annotation value for \"%s\" is not a primitive value",
            error->name->text);
}

static void FormatUseOfReservedIdentifierError(paw_Env *P, struct UseOfReservedIdentifierError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "use of reserved identifier \"%s\"", error->name->text);
}

static void FormatVisibilityQualifierNotAllowedError(paw_Env *P, struct VisibilityQualifierNotAllowedError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "visibility qualifier not allowed here");
}

static void FormatTooManyTupleElementsError(paw_Env *P, struct TooManyTupleElementsError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "too many elements in tuple (limit is %d)",
            error->max_elements);
}

static void FormatLimitExceededError(paw_Env *P, struct LimitExceededError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "too many %s (limit is %d)",
            error->what->text, error->limit);
}

static void FormatInvalidGlobImportError(paw_Env *P, struct InvalidGlobImportError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "invalid glob import");
}

static void FormatInvalidAssignmentTargetError(paw_Env *P, struct InvalidAssignmentTargetError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "invalid target for assignment");
}

static void FormatUnexpectedSymbolError(paw_Env *P, struct UnexpectedSymbolError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "unexpected symbol");
}

static void FormatUnsupportedError(paw_Env *P, struct UnsupportedError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "unsupported");
}


// import errors
static void FormatModuleNotFoundError(paw_Env *P, struct ModuleNotFoundError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "modulenotfound");
}

// collection errors
static void FormatAmbiguousPathError(paw_Env *P, struct AmbiguousPathError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "ambiguous path \"%s\"",
            error->path->text);
}

static void FormatDuplicateItemError(paw_Env *P, struct DuplicateItemError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "duplicate %s \"%s\"",
            error->what->text, error->item_name->text);
}

static void FormatExternFunctionBodyError(paw_Env *P, struct ExternFunctionBodyError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer,
            "encountered body on function \"%s\" declared with \"extern\" annotation",
            error->fn_name->text);
}

static void FormatMissingFunctionBodyError(paw_Env *P, struct MissingFunctionBodyError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "missing body on function");
}

static void FormatItemVisibilityError(paw_Env *P, struct ItemVisibilityError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "non-public item can only be accessed by other items in the same module");
}

static void FormatAssociatedItemVisibilityError(paw_Env *P, struct AssociatedItemVisibilityError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer,
            "non-public associated item \"%s\" (defined on type \"%s\") "
            "can only be accessed by other items in the same module",
            error->field_name->text, error->parent_name->text);
}

static void FormatInvalidGlobTargetError(paw_Env *P, struct InvalidGlobTargetError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "invalid glob target \"%s\"",
            error->path->text);
}


static void FormatMissingTraitBoundsError(paw_Env *P, struct MissingTraitBoundsError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "type parameter \"%s\" missing trait bounds",
            error->name->text);
}

static void FormatTraitBoundsOnAliasGenericError(paw_Env *P, struct TraitBoundsOnAliasGenericError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "trait bounds not allowed on type parameter \"%s\" for type alias",
            error->name);
}

static void FormatFalseObligationError(paw_Env *P, struct FalseObligationError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer,
            "obligation \"%s\" was proven false",
            error->obligation->text);
}

static void FormatUnsatisfiedObligationError(paw_Env *P, struct UnsatisfiedObligationError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer,
            "unsatisfiable obligation \"%s\"",
            error->example->text);
    if (error->num_unsolved > 1)
        pawL_add_fstring(P, buffer,
                " (and %d others)",
                error->num_unsolved - 1);
}

static void FormatIncompatibleTypesError(paw_Env *P, struct IncompatibleTypesError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "incompatible types \"%s\" and \"%s\"",
            error->lhs->text, error->rhs->text);
}

static void FormatCannotInferError(paw_Env *P, struct CannotInferError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "unable to infer type");
}

static void FormatCyclicTypeError(paw_Env *P, struct CyclicTypeError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "encountered cyclic type");
}

static void FormatMissingExternValueError(paw_Env *P, struct MissingExternValueError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "missing extern value \"%s\"",
            error->name->text);
}

static void FormatUnexpectedModuleNameError(paw_Env *P, struct UnexpectedModuleNameError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "unexpected module name");
}

static void FormatTransitiveImportError(paw_Env *P, struct TransitiveImportError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "transitive imports are not supported");
}

static void FormatIncorrectTypeArityError(paw_Env *P, struct IncorrectTypeArityError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "%s types (expected %d but have %d)",
            error->have < error->want ? "not enough" : "too many",
            error->want, error->have);
}

static void FormatExpectedTypeArgumentsError(paw_Env *P, struct ExpectedTypeArgumentsError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "expected type arguments on %s \"%s\"",
            error->what->text, error->name->text);
}

static void FormatUnexpectedTypeArgumentsError(paw_Env *P, struct UnexpectedTypeArgumentsError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "unexpected type arguments on %s \"%s\"",
            error->what->text, error->name->text);
}

static void FormatExpectedTraitError(paw_Env *P, struct ExpectedTraitError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "expected trait but found type \"%s\"",
            error->path->text);
}

static void FormatUnexpectedTraitError(paw_Env *P, struct UnexpectedTraitError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "unexpected trait");
}

static void FormatIncorrectItemClassError(paw_Env *P, struct IncorrectItemClassError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "expected %s but found %s",
            error->want->text, error->have->text);
}

static void FormatExpectedTraitPathError(paw_Env *P, struct ExpectedTraitPathError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "expected trait path");
}

static void FormatExtraSegmentError(paw_Env *P, struct ExtraSegmentError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "extraneous \"::%s\"",
            error->name->text);
}

static void FormatDuplicateAssocItemError(paw_Env *P, struct DuplicateAssocItemError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer,
            "duplicate associated item \"%s\" in ",
            error->item->text);
}

static void FormatTraitImplAssocItemNotCompatibleError(paw_Env *P, struct TraitImplAssocItemNotCompatibleError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer,
            "assocaited type \"%s\" not compatible with corresponding item"
            "in trait definition \"%s\"",
            error->item->text, error->trait->text);
}

static void FormatTraitImplUnknownAssocItemError(paw_Env *P, struct TraitImplUnknownAssocItemError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer,
            "assocaited type \"%s\" not found in trait definition \"%s\"",
            error->item->text, error->trait->text);
}

static void FormatTraitImplMissingAssocItemError(paw_Env *P, struct TraitImplMissingAssocItemError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer,
            "missing associated item \"%s\" in implementation of trait \"%s\"",
            error->item->text, error->trait->text);
}

static void FormatMultipleApplicableItemsError(paw_Env *P, struct MultipleApplicableItemsError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "multiple applicable items");
}

static void FormatMissingVariantArgsError(paw_Env *P, struct MissingVariantArgsError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "missing argument(s) for variant constructor \"%s\"",
            error->cons->text);
}

static void FormatUnexpectedTypeError(paw_Env *P, struct UnexpectedTypeError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "expected value but found type \"%s\"",
            error->type->text);
}

static void FormatUnknownPathError(paw_Env *P, struct UnknownPathError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "unknown path \"%s\"",
            error->path->text);
}

static void FormatMissingFieldsError(paw_Env *P, struct MissingFieldsError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "missing fields on initializer for struct \"%s\"",
            error->type->text);
}

static void FormatExpectedValueError(paw_Env *P, struct ExpectedValueError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "expected value but found type \"%s\"",
            error->type->text);
}

static void FormatInvalidChainOperandError(paw_Env *P, struct InvalidChainOperandError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "invalid operand type \"%s\" for chain operator",
            error->type->text);
}

static void FormatInvalidUnaryOperandError(paw_Env *P, struct InvalidUnaryOperandError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "invalid operand type \"%s\" for unary operator \"%s\"",
            error->type->text, error->op->text);
}

static void FormatInvalidBinaryOperandError(paw_Env *P, struct InvalidBinaryOperandError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "invalid operand type \"%s\" for binary operator \"%s\"",
            error->type->text, error->op->text);
}

static void FormatCannotConstantEvaluateError(paw_Env *P, struct CannotConstantEvaluateError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "%s cannot be constant evaluated",
            error->what);
}

static void FormatNonprimitiveConstantError(paw_Env *P, struct NonprimitiveConstantError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "expected primitive constant but found \"%s\"",
            error->type);
}

static void FormatExpectedElementSelectorError(paw_Env *P, struct ExpectedElementSelectorError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "expected integer element selector");
}

static void FormatElementSelectorOutOfRangeError(paw_Env *P, struct ElementSelectorOutOfRangeError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "element selector %d is out of range for %d-tuple",
            error->index, error->count);
}

static void FormatExpectedAdtError(paw_Env *P, struct ExpectedAdtError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "expected ADT but found \"%s\"",
            error->type->text);
}

static void FormatExpectedFieldSelectorError(paw_Env *P, struct ExpectedFieldSelectorError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "expected field selector");
}

static void FormatUnknownMethodError(paw_Env *P, struct UnknownMethodError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "unknown method \"%s\" for type \"%s\"",
            error->method->text, error->type->text);
}

static void FormatUnknownAssociatedItemError(paw_Env *P, struct UnknownAssociatedItemError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "unknown associated item \"%s\" for type \"%s\"",
            error->item->text, error->type->text);
}


static void FormatNotAMethodError(paw_Env *P, struct NotAMethodError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "\"%s\" is not a method",
            error->name->text);
}

static void FormatNotCallableError(paw_Env *P, struct NotCallableError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "type \"%s\" is not callable",
            error->type->text);
}

static void FormatIncorrectArityError(paw_Env *P, struct IncorrectArityError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "%s arguments (expected %d but have %d)",
            error->have < error->want ? "not enough" : "too many",
            error->want, error->have);
}

static void FormatDuplicateNameError(paw_Env *P, struct DuplicateNameError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "duplicate %s name \"%s\"",
            error->what->text, error->name->text);
}

static void FormatDuplicateBindingError(paw_Env *P, struct DuplicateBindingError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "duplicate binding \"%s\"",
            error->name->text);
}

static void FormatExpectedStructError(paw_Env *P, struct ExpectedStructError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "expected struct but found \"%s\"",
            error->type->text);
}

static void FormatUnitStructWithBracesError(paw_Env *P, struct UnitStructWithBracesError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer,
            "unexpected braces on unit struct \"%s\" (omit "
            "braces to construct structure with no fields)",
            error->type->text);
}

static void FormatUnitVariantWithParenthesisError(paw_Env *P, struct UnitVariantWithParenthesisError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer,
            "unexpected parenthesis on unit variant \"%s\" (omit "
            "parenthesis to construct variant with no fields)",
            error->type->text);
}

static void FormatMissingFieldError(paw_Env *P, struct MissingFieldError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "missing initializer for field \"%s\" on struct \"%s\"",
            error->name->text, error->type->text);
}

static void FormatUnknownFieldError(paw_Env *P, struct UnknownFieldError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "unknown field \"%s\" on struct \"%s\"",
            error->name->text, error->type->text);
}

static void FormatInvalidIndexTargetError(paw_Env *P, struct InvalidIndexTargetError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "invalid target \"%s\" for index operator",
            error->type->text);
}

static void FormatMissingBindingInAlternativeError(paw_Env *P, struct MissingBindingInAlternativeError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "missing binding \"%s\" in alternative pattern",
            error->name->text);
}

static void FormatExpectedDivergenceError(paw_Env *P, struct ExpectedDivergenceError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "expected \"%s\" to diverge based on return type of \"!\"",
            error->fn->text);
}

static void FormatBlanketInherentImplError(paw_Env *P, struct BlanketInherentImplError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "type parameter cannot be `Self` type of inherent implementation");
}

static void FormatTypeContainsSelfError(paw_Env *P, struct TypeContainsSelfError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "recursive type requires indirection");
}

static void FormatMultipleApplicableTraitsError(paw_Env *P, struct MultipleApplicableTraitsError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "multiple applicable traits");
}

static void FormatMoveOutOfFieldError(paw_Env *P, struct MoveOutOfFieldError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer,
            "field of type `%s` cannot be moved out of its containing "
            "object (implement `Copy` for `%s` to copy the field)",
            error->type->text, error->type->text);
}

static void FormatMoveOutOfElementError(paw_Env *P, struct MoveOutOfElementError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer,
            "element of type `%s` cannot be moved out of its containing "
            "sequence (implement `Copy` for `%s` to copy the element)",
            error->type->text, error->type->text);
}

static void FormatMoveOutOfPointerError(paw_Env *P, struct MoveOutOfPointerError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer,
            "value of type `%s` cannot be moved out from behind a "
            "pointer (implement `Copy` for `%s` to copy the value)",
            error->type->text, error->type->text);
}

static void FormatInvalidInclusiveRangeError(paw_Env *P, struct InvalidInclusiveRangeError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "type of range cannot be inclusive");
}

static void FormatGlobalConstantCycleError(paw_Env *P, struct GlobalConstantCycleError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "cycle detected between global constants (involves constant \"%s\")",
            error->name->text);
}

static void FormatUninitializedDestructuringError(paw_Env *P, struct UninitializedDestructuringError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "variables using deferred initialization cannot use destructuring");
}

static void FormatModifiedConstantError(paw_Env *P, struct ModifiedConstantError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "attempt to modify constant \"%s\"",
            error->name->text);
}

static void FormatTooManyUpvaluesError(paw_Env *P, struct TooManyUpvaluesError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "too many upvalues in function (limit is %d)",
            error->limit);
}

static void FormatInitializedExternConstantError(paw_Env *P, struct InitializedExternConstantError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "unexpected initializer for \"extern\" constant \"%s\"",
            error->name->text);
}

static void FormatUninitializedConstantError(paw_Env *P, struct UninitializedConstantError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "missing initializer for constant \"%s\"",
            error->name->text);
}

static void FormatInfiniteSizeObjectError(paw_Env *P, struct InfiniteSizeObjectError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "object \"%s\" has infinite size",
            error->name->text);
}

static void FormatObjectTooLargeError(paw_Env *P, struct ObjectTooLargeError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "object \"%s\" is too large",
            error->name->text);
}

static void FormatNonexhaustivePatternMatchError(paw_Env *P, struct NonexhaustivePatternMatchError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "nonexhaustive pattern match");
}

static void FormatUseBeforeInitializationError(paw_Env *P, struct UseBeforeInitializationError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "use of variable \"%s\" before initialization",
            error->name->text);
}

static void FormatConstantDivideByZeroError(paw_Env *P, struct ConstantDivideByZeroError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "constant division by 0");
}

static void FormatConstantNegativeShiftCountError(paw_Env *P, struct ConstantNegativeShiftCountError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "constant negative shift count");
}

static void FormatTooManyVariablesError(paw_Env *P, struct TooManyVariablesError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "too many variables (limit is %d)",
            error->limit);
}


static void FormatTooFarToJumpError(paw_Env *P, struct TooFarToJumpError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "too far to jump (limit is %d instructions)",
            error->limit);
}


static void FormatTooManyConstantsError(paw_Env *P, struct TooManyConstantsError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "too many constants (limit is %d)",
            error->limit);
}

_Noreturn void pawErr_throw(struct Compiler *C, enum ErrorKind kind, void *payload)
{
    Buffer buffer;
    paw_Env *P = ENV(C);
    pawL_init_buffer(P, &buffer);

    switch (kind) {
#define X(Name_, _2, _3) case kErr##Name_: \
            Format##Name_##Error(P, (struct Name_##Error *)payload, &buffer); \
            break;
        ERR_ERROR_LIST(X)
#undef X

        // TODO: remove later
        case E_UNSUPPORTED: case E_EMPTY_CHAR: case E_CHAR_TOO_LONG: case E_UNTERMINATED_CHAR: case E_UNTERMINATED_STRING: case E_INVALID_UNICODE_CODEPOINT: case E_EXPECTED_INTEGER_DIGIT: case E_UNEXPECTED_INTEGER_CHAR: case E_INTEGER_OUT_OF_RANGE: case E_INVALID_INTEGER: case E_INVALID_FLOAT: case E_NULL_BEFORE_EOF: case E_EXPECTED_SYMBOL: case E_EXPECTED_DELIMITER: case E_EXPECTED_SEMICOLON: case E_EMPTY_TYPE_LIST: case E_NONLITERAL_PATTERN: case E_NEGATIVE_MINIMUM_INTEGER: case E_INVALID_LITERAL_NEGATION: case E_INVALID_SELECTOR: case E_EXPECTED_BASIC_TYPE: case E_EXPECTED_EXPRESSION: case E_EMPTY_ENUMERATION: case E_EMPTY_STRUCT_BODY: case E_EMPTY_VARIANT_FIELD_LIST: case E_FUNCTION_TYPE_DECL: case E_INVALID_ANNOTATION_TYPE: case E_EXPECTED_TYPE_ANNOTATION: case E_RETURN_OUTSIDE_FUNCTION: case E_CHAIN_OUTSIDE_FUNCTION: case E_JUMP_OUTSIDE_LOOP: case E_TOO_MANY_ELEMENTS: case E_EXPECTED_TOPLEVEL_ITEM: case E_COLON_AFTER_LIST_ELEMENT: case E_EXPECTED_COLON_AFTER_MAP_KEY: case E_COLONS_AFTER_UNDERSCORE: case E_EXPECTED_SELF_PARAMETER: case E_UNEXPECTED_UNDERSCORE: case E_EXPECTED_COMMA_SEPARATOR: case E_DUPLICATE_ANNOTATION: case E_NONLITERAL_ANNOTATION_VALUE: case E_NONPRIMITIVE_ANNOTATION_VALUE: case E_INVALID_GLOB: case E_ITEM_VISIBILITY: case E_ASSOCIATED_ITEM_VISIBILITY: case E_INVALID_GLOB_TARGET: case E_AMBIGUOUS_PATH: case E_DUPLICATE_ITEM: case E_UNKNOWN_PATH: case E_MULTIPLE_APPLICABLE_ITEMS: case E_UNEXPECTED_MODULE_NAME: case E_TRANSITIVE_IMPORT: case E_INCORRECT_TYPE_ARITY: case E_EXPECTED_TYPE_ARGUMENTS: case E_UNEXPECTED_TYPE_ARGUMENTS: case E_EXPECTED_TRAIT: case E_UNEXPECTED_TRAIT: case E_INCORRECT_ITEM_CLASS: case E_EXTRA_SEGMENT: case E_INVALID_ASSIGNMENT_TARGET: case E_MODULE_NOT_FOUND: case E_EXTERN_FUNCTION_BODY: case E_MISSING_FUNCTION_BODY: case E_MISSING_TRAIT_BOUNDS: case E_TRAIT_BOUNDS_ON_ALIAS_GENERIC: case E_UNSATISFIED_TRAIT_BOUNDS: case E_INCOMPATIBLE_TYPES: case E_CANNOT_INFER: case E_CYCLIC_TYPE: case E_MISSING_EXTERN_VALUE: case E_MISSING_TRAIT_METHOD: case E_TRAIT_METHOD_VISIBILITY_MISMATCH: case E_MISSING_VARIANT_ARGS: case E_RESERVED_IDENTIFIER: case E_UNEXPECTED_TYPE: case E_MISSING_FIELDS: case E_EXPECTED_VALUE: case E_INVALID_CHAIN_OPERAND: case E_INVALID_UNARY_OPERAND: case E_INVALID_BINARY_OPERAND: case E_CANNOT_CONSTANT_EVALUATE: case E_NONPRIMITIVE_CONSTANT: case E_EXPECTED_ELEMENT_SELECTOR: case E_ELEMENT_SELECTOR_OUT_OF_RANGE: case E_EXPECTED_ADT: case E_EXPECTED_FIELD_SELECTOR: case E_MISSING_FIELD: case E_UNKNOWN_FIELD: case E_UNKNOWN_METHOD: case E_UNKNOWN_ASSOCIATED_ITEM: case E_NOT_A_METHOD: case E_NOT_CALLABLE: case E_INCORRECT_ARITY: case E_DUPLICATE_BINDING: case E_DUPLICATE_FIELD: case E_EXPECTED_STRUCT: case E_UNIT_STRUCT_WITH_BRACES: case E_UNIT_VARIANT_WITH_PARENTHESIS: case E_INVALID_INDEX_TARGET: case E_MISSING_BINDING_IN_ALTERNATIVE: case E_INVALID_INCLUSIVE_RANGE: case E_EXPECTED_DIVERGENCE: case E_GLOBAL_CONSTANT_CYCLE: case E_UNINITIALIZED_DESTRUCTURING: case E_MODIFIED_CONSTANT: case E_TOO_MANY_UPVALUES: case E_INITIALIZED_EXTERN_CONSTANT: case E_UNINITIALIZED_CONSTANT: case E_INFINITE_SIZE_OBJECT: case E_OBJECT_TOO_LARGE: case E_NONEXHAUSTIVE_PATTERN_MATCH: case E_USE_BEFORE_INITIALIZATION: case E_CONSTANT_DIVIDE_BY_ZERO: case E_CONSTANT_NEGATIVE_SHIFT_COUNT: case E_TOO_MANY_VARIABLES: case E_TOO_FAR_TO_JUMP: case E_TOO_MANY_CONSTANTS: case E_CAPTURED_INOUT_ARG:
            __builtin_trap();
    }

    P->current_errmsg = pawL_buffer_finish(P, &buffer);
    pawC_throw(P, (int)kind);
}

enum ErrorCategory pawErr_error_category(enum ErrorKind kind)
{
    switch (kind) {
#define X(Name_, Category_, _3) case kErr##Name_: \
            return Category_;
        ERR_ERROR_LIST(X)
#undef X

        // TODO: remove later
        case E_UNSUPPORTED: case E_EMPTY_CHAR: case E_CHAR_TOO_LONG: case E_UNTERMINATED_CHAR: case E_UNTERMINATED_STRING: case E_INVALID_UNICODE_CODEPOINT: case E_EXPECTED_INTEGER_DIGIT: case E_UNEXPECTED_INTEGER_CHAR: case E_INTEGER_OUT_OF_RANGE: case E_INVALID_INTEGER: case E_INVALID_FLOAT: case E_NULL_BEFORE_EOF: case E_EXPECTED_SYMBOL: case E_EXPECTED_DELIMITER: case E_EXPECTED_SEMICOLON: case E_EMPTY_TYPE_LIST: case E_NONLITERAL_PATTERN: case E_NEGATIVE_MINIMUM_INTEGER: case E_INVALID_LITERAL_NEGATION: case E_INVALID_SELECTOR: case E_EXPECTED_BASIC_TYPE: case E_EXPECTED_EXPRESSION: case E_EMPTY_ENUMERATION: case E_EMPTY_STRUCT_BODY: case E_EMPTY_VARIANT_FIELD_LIST: case E_FUNCTION_TYPE_DECL: case E_INVALID_ANNOTATION_TYPE: case E_EXPECTED_TYPE_ANNOTATION: case E_RETURN_OUTSIDE_FUNCTION: case E_CHAIN_OUTSIDE_FUNCTION: case E_JUMP_OUTSIDE_LOOP: case E_TOO_MANY_ELEMENTS: case E_EXPECTED_TOPLEVEL_ITEM: case E_COLON_AFTER_LIST_ELEMENT: case E_EXPECTED_COLON_AFTER_MAP_KEY: case E_COLONS_AFTER_UNDERSCORE: case E_EXPECTED_SELF_PARAMETER: case E_UNEXPECTED_UNDERSCORE: case E_EXPECTED_COMMA_SEPARATOR: case E_DUPLICATE_ANNOTATION: case E_NONLITERAL_ANNOTATION_VALUE: case E_NONPRIMITIVE_ANNOTATION_VALUE: case E_INVALID_GLOB: case E_ITEM_VISIBILITY: case E_ASSOCIATED_ITEM_VISIBILITY: case E_INVALID_GLOB_TARGET: case E_AMBIGUOUS_PATH: case E_DUPLICATE_ITEM: case E_UNKNOWN_PATH: case E_MULTIPLE_APPLICABLE_ITEMS: case E_UNEXPECTED_MODULE_NAME: case E_TRANSITIVE_IMPORT: case E_INCORRECT_TYPE_ARITY: case E_EXPECTED_TYPE_ARGUMENTS: case E_UNEXPECTED_TYPE_ARGUMENTS: case E_EXPECTED_TRAIT: case E_UNEXPECTED_TRAIT: case E_INCORRECT_ITEM_CLASS: case E_EXTRA_SEGMENT: case E_INVALID_ASSIGNMENT_TARGET: case E_MODULE_NOT_FOUND: case E_EXTERN_FUNCTION_BODY: case E_MISSING_FUNCTION_BODY: case E_MISSING_TRAIT_BOUNDS: case E_TRAIT_BOUNDS_ON_ALIAS_GENERIC: case E_UNSATISFIED_TRAIT_BOUNDS: case E_INCOMPATIBLE_TYPES: case E_CANNOT_INFER: case E_CYCLIC_TYPE: case E_MISSING_EXTERN_VALUE: case E_MISSING_TRAIT_METHOD: case E_TRAIT_METHOD_VISIBILITY_MISMATCH: case E_MISSING_VARIANT_ARGS: case E_RESERVED_IDENTIFIER: case E_UNEXPECTED_TYPE: case E_MISSING_FIELDS: case E_EXPECTED_VALUE: case E_INVALID_CHAIN_OPERAND: case E_INVALID_UNARY_OPERAND: case E_INVALID_BINARY_OPERAND: case E_CANNOT_CONSTANT_EVALUATE: case E_NONPRIMITIVE_CONSTANT: case E_EXPECTED_ELEMENT_SELECTOR: case E_ELEMENT_SELECTOR_OUT_OF_RANGE: case E_EXPECTED_ADT: case E_EXPECTED_FIELD_SELECTOR: case E_MISSING_FIELD: case E_UNKNOWN_FIELD: case E_UNKNOWN_METHOD: case E_UNKNOWN_ASSOCIATED_ITEM: case E_NOT_A_METHOD: case E_NOT_CALLABLE: case E_INCORRECT_ARITY: case E_DUPLICATE_BINDING: case E_DUPLICATE_FIELD: case E_EXPECTED_STRUCT: case E_UNIT_STRUCT_WITH_BRACES: case E_UNIT_VARIANT_WITH_PARENTHESIS: case E_INVALID_INDEX_TARGET: case E_MISSING_BINDING_IN_ALTERNATIVE: case E_INVALID_INCLUSIVE_RANGE: case E_EXPECTED_DIVERGENCE: case E_GLOBAL_CONSTANT_CYCLE: case E_UNINITIALIZED_DESTRUCTURING: case E_MODIFIED_CONSTANT: case E_TOO_MANY_UPVALUES: case E_INITIALIZED_EXTERN_CONSTANT: case E_UNINITIALIZED_CONSTANT: case E_INFINITE_SIZE_OBJECT: case E_OBJECT_TOO_LARGE: case E_NONEXHAUSTIVE_PATTERN_MATCH: case E_USE_BEFORE_INITIALIZATION: case E_CONSTANT_DIVIDE_BY_ZERO: case E_CONSTANT_NEGATIVE_SHIFT_COUNT: case E_TOO_MANY_VARIABLES: case E_TOO_FAR_TO_JUMP: case E_TOO_MANY_CONSTANTS: case E_CAPTURED_INOUT_ARG:
            __builtin_trap();
    }
}

