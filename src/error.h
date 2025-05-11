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
    E_DUPLICATE_ANNOTATION,
    E_NONLITERAL_ANNOTATION_VALUE,
    E_NONPRIMITIVE_ANNOTATION_VALUE,

    // AST lowering errors
    E_INVALID_ASSIGNMENT_TARGET,

    // import errors
    E_MODULE_NOT_FOUND,

    // collection errors
    E_DUPLICATE_ITEM,
    E_UNKNOWN_TYPE,
    E_UNKNOWN_TRAIT,
    E_UNKNOWN_ITEM,
    E_EXTERN_FUNCTION_BODY,
    E_MISSING_FUNCTION_BODY,
    E_ITEM_VISIBILITY,
    E_ASSOCIATED_ITEM_VISIBILITY,

    // type errors
    E_MISSING_TRAIT_BOUNDS,
    E_UNSATISFIED_TRAIT_BOUNDS,
    E_INCOMPATIBLE_TYPES,
    E_CANNOT_INFER,
    E_CYCLIC_TYPE,

    // compiler errors
    E_MISSING_EXTERN_VALUE,

    // lookup errors
    E_UNEXPECTED_MODULE_NAME,
    E_TRANSITIVE_IMPORT,
    E_INCORRECT_TYPE_ARITY,
    E_UNEXPECTED_TYPE_ARGUMENTS,
    E_EXPECTED_TRAIT,
    E_UNEXPECTED_TRAIT,
    E_INCORRECT_ITEM_CLASS,
    E_EXTRA_SEGMENT,

    // trait errors
    E_MISSING_TRAIT_METHOD,
    E_TRAIT_METHOD_VISIBILITY_MISMATCH,

    // resolver errors
    E_MISSING_VARIANT_ARGS,
    E_RESERVED_IDENTIFIER,
    E_UNKNOWN_PATH,
    E_TYPE_USED_AS_VALUE,
    E_ENUM_USED_AS_VALUE,
    E_MISSING_FIELDS,
    E_EXPECTED_VALUE,
    E_INVALID_CHAIN_OPERAND,
    E_INVALID_UNARY_OPERAND,
    E_INVALID_BINARY_OPERAND,
    E_CANNOT_CONSTANT_EVALUATE,
    E_NONPRIMITIVE_CONSTANT,
    E_EXPECTED_ELEMENT_SELECTOR,
    E_ELEMENT_SELECTOR_OUT_OF_RANGE,
    E_EXPECTED_ADT,
    E_EXPECTED_FIELD_SELECTOR,
    E_MISSING_FIELD,
    E_UNKNOWN_FIELD,
    E_UNKNOWN_METHOD,
    E_UNKNOWN_ASSOCIATED_ITEM,
    E_NOT_A_METHOD,
    E_NOT_CALLABLE,
    E_INCORRECT_ARITY,
    E_DUPLICATE_BINDING,
    E_DUPLICATE_FIELD,
    E_EXPECTED_STRUCT,
    E_UNIT_STRUCT_WITH_BRACES,
    E_UNIT_VARIANT_WITH_PARENTHESIS,
    E_INVALID_INDEX_TARGET,
    E_INVALID_SLICE_TARGET,
    E_MISSING_BINDING_IN_ALTERNATIVE,

    // HIR lowering errors
    E_GLOBAL_CONSTANT_CYCLE,
    E_UNINITIALIZED_DESTRUCTURING,
    E_MODIFIED_CONSTANT,
    E_TOO_MANY_UPVALUES,
    E_INITIALIZED_EXTERN_CONSTANT,
    E_UNINITIALIZED_CONSTANT,

    // unboxing errors
    E_INFINITE_SIZE_OBJECT,
    E_OBJECT_TOO_LARGE,

    // exhaustiveness checking errors
    E_NONEXHAUSTIVE_PATTERN_MATCH,

    // SSA construction errors
    E_USE_BEFORE_INITIALIZATION,

    // constant propagation errors
    E_CONSTANT_DIVIDE_BY_ZERO,
    E_CONSTANT_NEGATIVE_SHIFT_COUNT,

    // register allocation errors
    E_TOO_MANY_VARIABLES,

    // code generation errors
    E_TOO_FAR_TO_JUMP,
    E_TOO_MANY_CONSTANTS,
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
_Noreturn void pawErr_duplicate_annotation(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *name);
_Noreturn void pawErr_nonliteral_annotation_value(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *name);
_Noreturn void pawErr_nonprimitive_annotation_value(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *name);
_Noreturn void pawErr_return_outside_function(struct Compiler *C, String const *modname, struct SourceLoc loc);
_Noreturn void pawErr_chain_outside_function(struct Compiler *C, String const *modname, struct SourceLoc loc);
_Noreturn void pawErr_jump_outside_loop(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *what);

// import errors
_Noreturn void pawErr_module_not_found(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *what);

// AST lowering errors
_Noreturn void pawErr_invalid_assignment_target(struct Compiler *C, String const *modname, struct SourceLoc loc);

// collection errors
_Noreturn void pawErr_duplicate_item(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *what, char const *name);
_Noreturn void pawErr_unknown_type(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *type);
_Noreturn void pawErr_unknown_trait(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *name);
_Noreturn void pawErr_unknown_item(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *mod, char const *item);
_Noreturn void pawErr_extern_function_body(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *name);
_Noreturn void pawErr_missing_function_body(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *name);
_Noreturn void pawErr_item_visibility(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *mod, char const *item);
_Noreturn void pawErr_associated_item_visibility(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *name, char const *type);

// type errors
_Noreturn void pawErr_missing_trait_bounds(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *name);
_Noreturn void pawErr_unsatisfied_trait_bounds(struct Compiler *C, String const *modname, struct SourceLoc loc);
_Noreturn void pawErr_incompatible_types(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *lhs, char const *rhs);
_Noreturn void pawErr_cannot_infer(struct Compiler *C, String const *modname, struct SourceLoc loc);
_Noreturn void pawErr_cyclic_type(struct Compiler *C, String const *modname, struct SourceLoc loc);

// compiler errors
_Noreturn void pawErr_missing_extern_value(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *name);

// lookup errors
_Noreturn void pawErr_unexpected_module_name(struct Compiler *C, String const *modname, struct SourceLoc loc);
_Noreturn void pawErr_transitive_import(struct Compiler *C, String const *modname, struct SourceLoc loc);
_Noreturn void pawErr_incorrect_type_arity(struct Compiler *C, String const *modname, struct SourceLoc loc, int want, int have);
_Noreturn void pawErr_unexpected_type_arguments(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *what, char const *name);
_Noreturn void pawErr_expected_trait(struct Compiler *C, String const *modname, struct SourceLoc loc);
_Noreturn void pawErr_unexpected_trait(struct Compiler *C, String const *modname, struct SourceLoc loc);
_Noreturn void pawErr_incorrect_item_class(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *want, char const *have);
_Noreturn void pawErr_extra_segment(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *name);

// trait errors
_Noreturn void pawErr_missing_trait_method(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *name);
_Noreturn void pawErr_trait_method_visibility_mismatch(struct Compiler *C, String const *modname, struct SourceLoc loc, paw_Bool expected_pub, char const *name);

// resolver errors
_Noreturn void pawErr_missing_variant_args(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *cons);
_Noreturn void pawErr_reserved_identifier(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *name);
_Noreturn void pawErr_unknown_path(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *path);
_Noreturn void pawErr_unknown_field(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *name, char const *type);
_Noreturn void pawErr_missing_fields(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *type);
_Noreturn void pawErr_expected_value(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *type);
_Noreturn void pawErr_invalid_chain_operand(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *type);
_Noreturn void pawErr_invalid_unary_operand(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *type, char const *op);
_Noreturn void pawErr_invalid_binary_operand(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *type, char const *op);
_Noreturn void pawErr_cannot_constant_evaluate(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *type);
_Noreturn void pawErr_nonprimitive_constant(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *type);
_Noreturn void pawErr_expected_element_selector(struct Compiler *C, String const *modname, struct SourceLoc loc);
_Noreturn void pawErr_element_selector_out_of_range(struct Compiler *C, String const *modname, struct SourceLoc loc, int elem, int count);
_Noreturn void pawErr_expected_adt(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *type);
_Noreturn void pawErr_expected_field_selector(struct Compiler *C, String const *modname, struct SourceLoc loc);
_Noreturn void pawErr_unknown_field(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *name, char const *type);
_Noreturn void pawErr_unknown_method(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *method, char const *type);
_Noreturn void pawErr_unknown_associated_item(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *method, char const *type);
_Noreturn void pawErr_not_a_method(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *type);
_Noreturn void pawErr_not_callable(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *type);
_Noreturn void pawErr_incorrect_arity(struct Compiler *C, String const *modname, struct SourceLoc loc, int want, int have);
_Noreturn void pawErr_duplicate_field(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *name, char const *type);
_Noreturn void pawErr_duplicate_binding(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *name);
_Noreturn void pawErr_expected_struct(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *type);
_Noreturn void pawErr_unit_struct_with_braces(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *type);
_Noreturn void pawErr_unit_variant_with_parenthesis(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *type);
_Noreturn void pawErr_missing_field(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *name, char const *type);
_Noreturn void pawErr_unknown_field(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *name, char const *type);
_Noreturn void pawErr_invalid_index_target(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *type);
_Noreturn void pawErr_invalid_slice_target(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *type);
_Noreturn void pawErr_missing_binding_in_alternative(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *type);
_Noreturn void pawErr_expected_option_return(struct Compiler *C, String const *modname, struct SourceLoc loc);
_Noreturn void pawErr_expected_result_return(struct Compiler *C, String const *modname, struct SourceLoc loc);

// HIR lowering errors
_Noreturn void pawErr_global_constant_cycle(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *name);
_Noreturn void pawErr_uninitialized_destructuring(struct Compiler *C, String const *modname, struct SourceLoc loc);
_Noreturn void pawErr_modified_constant(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *name);
_Noreturn void pawErr_too_many_upvalues(struct Compiler *C, String const *modname, struct SourceLoc loc, int limit);
_Noreturn void pawErr_initialized_extern_constant(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *name);
_Noreturn void pawErr_uninitialized_constant(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *name);

// unboxing errors
_Noreturn void pawErr_infinite_size_object(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *name);
_Noreturn void pawErr_object_too_large(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *name);

// exhaustiveness checking errors
_Noreturn void pawErr_nonexhaustive_pattern_match(struct Compiler *C, String const *modname, struct SourceLoc loc);

// SSA construction errors
_Noreturn void pawErr_use_before_initialization(struct Compiler *C, String const *modname, struct SourceLoc loc, char const *name);

// constant propagation errors
_Noreturn void pawErr_constant_divide_by_zero(struct Compiler *C, String const *modname, struct SourceLoc loc);
_Noreturn void pawErr_constant_negative_shift_count(struct Compiler *C, String const *modname, struct SourceLoc loc);

// register allocation errors
_Noreturn void pawErr_too_many_variables(struct Compiler *C, String const *modname, struct SourceLoc loc, int limit);

// code generation errors
_Noreturn void pawErr_too_far_to_jump(struct Compiler *C, String const *modname, struct SourceLoc loc, int limit);
_Noreturn void pawErr_too_many_constants(struct Compiler *C, String const *modname, struct SourceLoc loc, int limit);

#endif // PAW_ERROR_H
