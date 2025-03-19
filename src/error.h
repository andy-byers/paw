// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_ERROR_H
#define PAW_ERROR_H

#include "compile.h"

enum ErrorKind {
    E_NONE = 0x100,

    // lexical errors
    E_INVALID_ESCAPE,
    E_INVALID_UNICODE_ESCAPE,
    E_INVALID_UNICODE_CODEPOINT,
    E_EXPECTED_INTEGER_DIGIT,
    E_UNEXPECTED_INTEGER_CHAR,
    E_INTEGER_OUT_OF_RANGE,
    E_INVALID_INTEGER,
    E_INVALID_FLOAT,
    E_TOO_MANY_LINES,
    E_TOO_MANY_COLUMNS,
    E_NAME_TOO_LONG,

    // parser errors
    E_EXPECTED_SYMBOL,
    E_EXPECTED_DELIMITER,
    E_EXPECTED_SEMICOLON,
    E_UNEXPECTED_VISIBILITY_QUALIFIER,
    E_UNEXPECTED_ANNOTATION,
    E_EMPTY_TYPE_LIST,
    E_NONLITERAL_PATTERN,
    E_NEGATIVE_MINIMUM_INTEGER,
    E_INVALID_LITERAL_NEGATION,
    E_INVALID_SELECTOR,
    E_EXPECTED_BASIC_TYPE,
    E_EXPECTED_EXPRESSION,
    E_EMPTY_ENUMERATION,
    E_EMPTY_STRUCT_BODY,
    E_EMPTY_VARIANT_FIELD_LIST,
    E_FUNCTION_TYPE_DECL,
    E_EXPECTED_TYPE_ANNOTATION,
    E_RETURN_OUTSIDE_FUNCTION,
    E_CHAIN_OUTSIDE_FUNCTION,
    E_JUMP_OUTSIDE_LOOP,
    E_TOO_MANY_ELEMENTS,
    E_EXPECTED_TOPLEVEL_ITEM,
    E_COLON_AFTER_LIST_ELEMENT,
    E_EXPECTED_COLON_AFTER_MAP_KEY,
    E_COLONS_AFTER_UNDERSCORE,
    E_EXPECTED_SELF_PARAMETER,
    E_UNEXPECTED_UNDERSCORE,
    E_EXPECTED_COMMA_SEPARATOR,
    E_NONLITERAL_ANNOTATION_VALUE,
    E_NONPRIMITIVE_ANNOTATION_VALUE,
};

// lexical errors
_Noreturn void pawErr_invalid_escape(struct Compiler *C, String const *modname, struct SourceLoc loc, char c);
_Noreturn void pawErr_invalid_unicode_escape(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *codepoint);
_Noreturn void pawErr_invalid_unicode_codepoint(struct Compiler *C, String const *modname, struct SourceLoc loc, int codepoint);
_Noreturn void pawErr_expected_integer_digit(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *base);
_Noreturn void pawErr_unexpected_integer_char(struct Compiler *C, String const *modname, struct SourceLoc loc, char c, char const *base);
_Noreturn void pawErr_integer_out_of_range(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *text);
_Noreturn void pawErr_invalid_integer(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *base, char const *text);
_Noreturn void pawErr_invalid_float(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *text);
_Noreturn void pawErr_too_many_lines(struct Compiler *C, String const *modname, struct SourceLoc loc, int limit);
_Noreturn void pawErr_too_many_columns(struct Compiler *C, String const *modname, struct SourceLoc loc, int limit);
_Noreturn void pawErr_name_too_long(struct Compiler *C, String const *modname, struct SourceLoc loc, int length, int limit);

// parser errors
_Noreturn void pawErr_unexpected_symbol(struct Compiler *C, String const *modname, struct SourceLoc loc);
_Noreturn void pawErr_expected_delimiter(struct Compiler *C, String const *modname, struct SourceLoc loc, char right, char left, struct SourceLoc open);
_Noreturn void pawErr_expected_semicolon(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *what);
_Noreturn void pawErr_unexpected_visibility_qualifier(struct Compiler *C, String const *modname, struct SourceLoc loc);
_Noreturn void pawErr_unexpected_annotation(struct Compiler *C, String const *modname, struct SourceLoc loc);
_Noreturn void pawErr_empty_type_list(struct Compiler *C, String const *modname, struct SourceLoc loc);
_Noreturn void pawErr_nonliteral_pattern(struct Compiler *C, String const *modname, struct SourceLoc loc);
_Noreturn void pawErr_negative_minimum_integer(struct Compiler *C, String const *modname, struct SourceLoc loc);
_Noreturn void pawErr_invalid_literal_negation(struct Compiler *C, String const *modname, struct SourceLoc loc);
_Noreturn void pawErr_invalid_selector(struct Compiler *C, String const *modname, struct SourceLoc loc);
_Noreturn void pawErr_expected_basic_type(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *what);
_Noreturn void pawErr_expected_expression(struct Compiler *C, String const *modname, struct SourceLoc loc);
_Noreturn void pawErr_empty_enumeration(struct Compiler *C, String const *modname, struct SourceLoc loc);
_Noreturn void pawErr_empty_struct_body(struct Compiler *C, String const *modname, struct SourceLoc loc);
_Noreturn void pawErr_empty_variant_field_list(struct Compiler *C, String const *modname, struct SourceLoc loc);
_Noreturn void pawErr_function_type_decl(struct Compiler *C, String const *modname, struct SourceLoc loc);
_Noreturn void pawErr_expected_type_annotation(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *what, char const *name);
_Noreturn void pawErr_too_many_elements(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *what, int limit);
_Noreturn void pawErr_expected_toplevel_item(struct Compiler *C, String const *modname, struct SourceLoc loc);
_Noreturn void pawErr_expected_colon_after_map_key(struct Compiler *C, String const *modname, struct SourceLoc loc);
_Noreturn void pawErr_colon_after_list_element(struct Compiler *C, String const *modname, struct SourceLoc loc);
_Noreturn void pawErr_colons_after_underscore(struct Compiler *C, String const *modname, struct SourceLoc loc);
_Noreturn void pawErr_expected_self_parameter(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *name);
_Noreturn void pawErr_unexpected_underscore(struct Compiler *C, String const *modname, struct SourceLoc loc);
_Noreturn void pawErr_expected_comma_separator(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *what);
_Noreturn void pawErr_nonliteral_annotation_value(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *name);
_Noreturn void pawErr_nonprimitive_annotation_value(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *name);
_Noreturn void pawErr_return_outside_function(struct Compiler *C, String const *modname, struct SourceLoc loc);
_Noreturn void pawErr_chain_outside_function(struct Compiler *C, String const *modname, struct SourceLoc loc);
_Noreturn void pawErr_jump_outside_loop(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *what);

#endif // PAW_ERROR_H
