// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// TODO: remove the old error handling code and use the "THROW_ERROR" API
// TODO: eventually report multiple errors per compilation?
// TODO: produce better error messages: print the line of source code where the error occurred and use span range to highlight target of message if applicable
// TODO: remove unused errors and add dedicated errors for situations pawErr_generic_error is currently being used

#ifndef PAW_ERROR_H
#define PAW_ERROR_H

#include "core.h"
#include "source.h"
#include "str.h"

#define ERR_ERROR_LIST(X) \
    X(TooManyLines, ERR_CATEGORY_SYNTAX, \
        Str const *modname; \
        Str const *filename; \
        int max_lines; \
    ) \
    X(TooManyColumns, ERR_CATEGORY_SYNTAX, \
        Str const *modname; \
        Str const *filename; \
        int max_columns; \
    ) \
    X(NameTooLong, ERR_CATEGORY_SYNTAX, \
        Str const *modname; \
        struct SourceSpan span; \
        int max_chars; \
    ) \
    X(InvalidStrLiteral, ERR_CATEGORY_SYNTAX, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *reason; \
    ) \
    X(EmptyStrLiteral, ERR_CATEGORY_SYNTAX, \
        Str const *modname; \
        struct SourceSpan span; \
    ) \
    X(StrLiteralTooLong, ERR_CATEGORY_SYNTAX, \
        Str const *modname; \
        struct SourceSpan span; \
    ) \
    X(UnterminatedStrLiteral, ERR_CATEGORY_SYNTAX, \
        Str const *modname; \
        struct SourceSpan span; \
    ) \
    X(InvalidCharLiteral, ERR_CATEGORY_SYNTAX, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *reason; \
    ) \
    X(EmptyCharLiteral, ERR_CATEGORY_SYNTAX, \
        Str const *modname; \
        struct SourceSpan span; \
    ) \
    X(CharLiteralTooLong, ERR_CATEGORY_SYNTAX, \
        Str const *modname; \
        struct SourceSpan span; \
    ) \
    X(UnterminatedCharLiteral, ERR_CATEGORY_SYNTAX, \
        Str const *modname; \
        struct SourceSpan span; \
    ) \
    X(UnknownEscapeChar, ERR_CATEGORY_SYNTAX, \
        Str const *modname; \
        struct SourceSpan span; \
    ) \
    X(InvalidCharInHexEscape, ERR_CATEGORY_SYNTAX, \
        Str const *modname; \
        struct SourceSpan span; \
    ) \
    X(InvalidUnicodeEscape, ERR_CATEGORY_SYNTAX, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *reason; \
    ) \
    X(InvalidUnicodeCodepoint, ERR_CATEGORY_SYNTAX, \
        Str const *modname; \
        struct SourceSpan span; \
        unsigned codepoint; \
    ) \
    X(UnicodeEscapeTooLong, ERR_CATEGORY_SYNTAX, \
        Str const *modname; \
        struct SourceSpan span; \
    ) \
    X(InvalidCharInUnicodeEscape, ERR_CATEGORY_SYNTAX, \
        Str const *modname; \
        struct SourceSpan span; \
    ) \
    X(EmptyUnicodeEscape, ERR_CATEGORY_SYNTAX, \
        Str const *modname; \
        struct SourceSpan span; \
    ) \
    X(UnterminatedUnicodeEscape, ERR_CATEGORY_SYNTAX, \
        Str const *modname; \
        struct SourceSpan span; \
    ) \
    X(IntegerTooBigToParse, ERR_CATEGORY_SYNTAX, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *text; \
        int base; \
    ) \
    X(InvalidIntegerLiteral, ERR_CATEGORY_SYNTAX, \
        Str const *modname; \
        struct SourceSpan span; \
        int base; \
    ) \
    X(ExpectedIntegerDigit, ERR_CATEGORY_SYNTAX, \
        Str const *modname; \
        struct SourceSpan span; \
        int base; \
    ) \
    X(InvalidCharInInteger, ERR_CATEGORY_SYNTAX, \
        Str const *modname; \
        struct SourceSpan span; \
        int base; \
        char invalid; \
    ) \
    X(InvalidFloatLiteral, ERR_CATEGORY_SYNTAX, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *reason; \
    ) \
    X(ExpectedSymbol, ERR_CATEGORY_SYNTAX, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *expected; \
        Str const *have; \
    ) \
    X(ExpectedDelimiter, ERR_CATEGORY_SYNTAX, \
        Str const *modname; \
        struct SourceSpan span; \
        struct SourceLoc open_loc; \
        char open; \
        char close; \
    ) \
    X(ExpectedSemicolon, ERR_CATEGORY_SYNTAX, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *what; \
    ) \
    X(EmptyTypeList, ERR_CATEGORY_SYNTAX, \
        Str const *modname; \
        struct SourceSpan span; \
    ) \
    X(NonliteralPattern, ERR_CATEGORY_SYNTAX, \
        Str const *modname; \
        struct SourceSpan span; \
    ) \
    X(IntegerOutOfRange, ERR_CATEGORY_SYNTAX, \
        Str const *modname; \
        struct SourceSpan span; \
        paw_Uint uint64; \
    ) \
    X(NegativeIntegerOutOfRange, ERR_CATEGORY_SYNTAX, \
        Str const *modname; \
        struct SourceSpan span; \
        paw_Uint uint64; \
    ) \
    X(InvalidLiteralNegation, ERR_CATEGORY_SYNTAX, \
        Str const *modname; \
        struct SourceSpan span; \
    ) \
    X(InvalidSelector, ERR_CATEGORY_SYNTAX, \
        Str const *modname; \
        struct SourceSpan span; \
    ) \
    X(ExpectedBasicType, ERR_CATEGORY_SYNTAX, \
        Str const *modname; \
        struct SourceSpan span; \
    ) \
    X(ExpectedExpression, ERR_CATEGORY_SYNTAX, \
        Str const *modname; \
        struct SourceSpan span; \
    ) \
    X(EmptyEnumeration, ERR_CATEGORY_SYNTAX, \
        Str const *modname; \
        struct SourceSpan span; \
    ) \
    X(EmptyStructBody, ERR_CATEGORY_SYNTAX, \
        Str const *modname; \
        struct SourceSpan span; \
    ) \
    X(EmptyVariantFieldList, ERR_CATEGORY_SYNTAX, \
        Str const *modname; \
        struct SourceSpan span; \
    ) \
    X(FunctionTypeDecl, ERR_CATEGORY_SYNTAX, \
        Str const *modname; \
        struct SourceSpan span; \
    ) \
    X(InvalidAnnotationType, ERR_CATEGORY_SYNTAX, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *name; \
    ) \
    X(ExpectedTypeAnnotation, ERR_CATEGORY_SYNTAX, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *what; \
        Str const *name; \
    ) \
    X(ReturnOutsideFunction, ERR_CATEGORY_SYNTAX, \
        Str const *modname; \
        struct SourceSpan span; \
    ) \
    X(ChainOutsideFunction, ERR_CATEGORY_SYNTAX, \
        Str const *modname; \
        struct SourceSpan span; \
    ) \
    X(JumpOutsideLoop, ERR_CATEGORY_SYNTAX, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *what; \
    ) \
    X(PathTooLong, ERR_CATEGORY_SYNTAX, \
        Str const *modname; \
        struct SourceSpan span; \
        int max_segments; \
    ) \
    X(ExpectedToplevelItem, ERR_CATEGORY_SYNTAX, \
        Str const *modname; \
        struct SourceSpan span; \
    ) \
    X(InvalidSelfPtrShorthand, ERR_CATEGORY_SYNTAX, \
        Str const *modname; \
        struct SourceSpan span; \
        paw_Bool is_mut; \
    ) \
    X(UnexpectedUnderscore, ERR_CATEGORY_SYNTAX, \
        Str const *modname; \
        struct SourceSpan span; \
    ) \
    X(ExpectedCommaSeparator, ERR_CATEGORY_SYNTAX, \
        Str const *modname; \
        struct SourceSpan span; \
    ) \
    X(DuplicateAnnotation, ERR_CATEGORY_SYNTAX, \
        Str const *modname; \
        struct SourceSpan span; \
        struct SourceSpan previous; \
    ) \
    X(NonliteralAnnotationValue, ERR_CATEGORY_SYNTAX, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *name; \
    ) \
    X(NonprimitiveAnnotationValue, ERR_CATEGORY_SYNTAX, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *name; \
    ) \
    X(TooManyTupleElements, ERR_CATEGORY_SYNTAX, \
        Str const *modname; \
        struct SourceSpan span; \
        int max_elements; \
    ) \
    X(VisibilityQualifierNotAllowed, ERR_CATEGORY_SYNTAX, \
        Str const *modname; \
        struct SourceSpan span; \
    ) \
    X(UseOfReservedIdentifier, ERR_CATEGORY_SYNTAX, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *name; \
    ) \
    X(LimitExceeded, ERR_CATEGORY_SYNTAX, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *what; \
        int limit; \
    ) \
    X(InvalidGlobImport, ERR_CATEGORY_SYNTAX, \
        Str const *modname; \
        struct SourceSpan span; \
    ) \
    X(InvalidAssignmentTarget, ERR_CATEGORY_SYNTAX, \
        Str const *modname; \
        struct SourceSpan span; \
    ) \
    X(UnexpectedSymbol, ERR_CATEGORY_SYNTAX, \
        Str const *modname; \
        struct SourceSpan span; \
    ) \
    X(Unsupported, ERR_CATEGORY_SYNTAX, \
        Str const *modname; \
        struct SourceSpan span; \
    ) \
    X(ModuleNotFound, ERR_CATEGORY_SYNTAX, \
        Str const *modname; \
        struct SourceSpan span; \
    ) \
    X(AmbiguousPath, ERR_CATEGORY_SYNTAX, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *path; \
    ) \
    X(DuplicateItem, ERR_CATEGORY_SYNTAX, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *item_name; \
        Str const *what; \
    ) \
    X(ExternFunctionBody, ERR_CATEGORY_SYNTAX, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *fn_name; \
    ) \
    X(MissingFunctionBody, ERR_CATEGORY_SYNTAX, \
        Str const *modname; \
        struct SourceSpan span; \
    ) \
    X(ItemVisibility, ERR_CATEGORY_SYNTAX, \
        Str const *modname; \
        struct SourceSpan span; \
    ) \
    X(AssociatedItemVisibility, ERR_CATEGORY_SYNTAX, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *parent_name; \
        Str const *field_name; \
    ) \
    X(InvalidGlobTarget, ERR_CATEGORY_SYNTAX, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *path; \
    ) \
    X(ExtraSegment, ERR_CATEGORY_NAME, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *name; \
    ) \
    X(DuplicateAssocItem, ERR_CATEGORY_NAME, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *item; \
    ) \
    X(TraitImplAssocItemNotCompatible, ERR_CATEGORY_NAME, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *item; \
        Str const *trait; \
    ) \
    X(TraitImplUnknownAssocItem, ERR_CATEGORY_NAME, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *item; \
        Str const *trait; \
    ) \
    X(TraitImplMissingAssocItem, ERR_CATEGORY_NAME, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *item; \
        Str const *trait; \
    ) \
    X(MultipleApplicableItems, ERR_CATEGORY_NAME, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *name; \
    ) \
    X(MissingVariantArgs, ERR_CATEGORY_NAME, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *cons; \
    ) \
    X(UnexpectedType, ERR_CATEGORY_NAME, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *type; \
    ) \
    X(UnknownPath, ERR_CATEGORY_NAME, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *path; \
    ) \
    X(UnknownField, ERR_CATEGORY_NAME, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *name; \
        Str const *type; \
    ) \
    X(MissingFields, ERR_CATEGORY_NAME, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *type; \
    ) \
    X(ExpectedValue, ERR_CATEGORY_NAME, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *type; \
    ) \
    X(MissingTraitBounds, ERR_CATEGORY_TYPE, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *name; \
    ) \
    X(TraitBoundsOnAliasGeneric, ERR_CATEGORY_TYPE, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *name; \
    ) \
    X(FalseObligation, ERR_CATEGORY_TYPE, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *obligation; \
    ) \
    X(UnsatisfiedObligation, ERR_CATEGORY_TYPE, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *example; \
        int num_unsolved; \
    ) \
    X(IncompatibleTypes, ERR_CATEGORY_TYPE, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *lhs; \
        Str const *rhs; \
    ) \
    X(CannotInfer, ERR_CATEGORY_TYPE, \
        Str const *modname; \
        struct SourceSpan span; \
    ) \
    X(CyclicType, ERR_CATEGORY_TYPE, \
        Str const *modname; \
        struct SourceSpan span; \
    ) \
    X(MissingExternValue, ERR_CATEGORY_TYPE, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *name; \
    ) \
    X(UnexpectedModuleName, ERR_CATEGORY_TYPE, \
        Str const *modname; \
        struct SourceSpan span; \
    ) \
    X(TransitiveImport, ERR_CATEGORY_TYPE, \
        Str const *modname; \
        struct SourceSpan span; \
    ) \
    X(IncorrectTypeArity, ERR_CATEGORY_TYPE, \
        Str const *modname; \
        struct SourceSpan span; \
        int have; \
        int want; \
    ) \
    X(ExpectedTypeArguments, ERR_CATEGORY_TYPE, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *what; \
        Str const *name; \
    ) \
    X(UnexpectedTypeArguments, ERR_CATEGORY_TYPE, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *what; \
        Str const *name; \
    ) \
    X(ExpectedTrait, ERR_CATEGORY_TYPE, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *path; \
    ) \
    X(UnexpectedTrait, ERR_CATEGORY_TYPE, \
        Str const *modname; \
        struct SourceSpan span; \
    ) \
    X(IncorrectItemClass, ERR_CATEGORY_TYPE, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *have; \
        Str const *want; \
    ) \
    X(InvalidChainOperand, ERR_CATEGORY_TYPE, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *type; \
    ) \
    X(InvalidUnaryOperand, ERR_CATEGORY_TYPE, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *type; \
        Str const *op; \
    ) \
    X(InvalidBinaryOperand, ERR_CATEGORY_TYPE, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *type; \
        Str const *op; \
    ) \
    X(CannotConstantEvaluate, ERR_CATEGORY_TYPE, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *what; \
    ) \
    X(NonprimitiveConstant, ERR_CATEGORY_TYPE, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *type; \
    ) \
    X(ExpectedElementSelector, ERR_CATEGORY_TYPE, \
        Str const *modname; \
        struct SourceSpan span; \
    ) \
    X(ElementSelectorOutOfRange, ERR_CATEGORY_TYPE, \
        Str const *modname; \
        struct SourceSpan span; \
        int index; \
        int count; \
    ) \
    X(ExpectedAdt, ERR_CATEGORY_TYPE, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *type; \
    ) \
    X(ExpectedFieldSelector, ERR_CATEGORY_TYPE, \
        Str const *modname; \
        struct SourceSpan span; \
    ) \
    X(UnknownMethod, ERR_CATEGORY_TYPE, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *method; \
        Str const *type; \
    ) \
    X(UnknownAssociatedItem, ERR_CATEGORY_TYPE, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *item; \
        Str const *type; \
    ) \
    X(NotAMethod, ERR_CATEGORY_TYPE, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *name; \
    ) \
    X(NotCallable, ERR_CATEGORY_TYPE, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *type; \
    ) \
    X(IncorrectArity, ERR_CATEGORY_TYPE, \
        Str const *modname; \
        struct SourceSpan span; \
        int have; \
        int want; \
    ) \
    X(DuplicateName, ERR_CATEGORY_TYPE, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *what; \
        Str const *name; \
    ) \
    X(DuplicateBinding, ERR_CATEGORY_TYPE, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *name; \
    ) \
    X(ExpectedStruct, ERR_CATEGORY_TYPE, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *type; \
    ) \
    X(UnitStructWithBraces, ERR_CATEGORY_TYPE, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *type; \
    ) \
    X(UnitVariantWithParenthesis, ERR_CATEGORY_TYPE, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *type; \
    ) \
    X(MissingField, ERR_CATEGORY_TYPE, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *name; \
        Str const *type; \
    ) \
    X(InvalidIndexTarget, ERR_CATEGORY_TYPE, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *type; \
    ) \
    X(InvalidInclusiveRange, ERR_CATEGORY_TYPE, \
        Str const *modname; \
        struct SourceSpan span; \
    ) \
    X(ExpectedDivergence, ERR_CATEGORY_TYPE, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *fn; \
    ) \
    X(BlanketInherentImpl, ERR_CATEGORY_TYPE, \
        Str const *modname; \
        struct SourceSpan span; \
    ) \
    X(TypeContainsSelf, ERR_CATEGORY_TYPE, \
        Str const *modname; \
        struct SourceSpan span; \
    ) \
    X(MultipleApplicableTraits, ERR_CATEGORY_TYPE, \
        Str const *modname; \
        struct SourceSpan span; \
    ) \
    X(MoveOutOfPointer, ERR_CATEGORY_TYPE, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *type; \
    ) \
    X(MoveOutOfElement, ERR_CATEGORY_TYPE, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *type; \
    ) \
    X(MoveOutOfField, ERR_CATEGORY_TYPE, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *type; \
    ) \
    X(MissingBindingInAlternative, ERR_CATEGORY_PATTERN, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *name; \
    ) \
    X(NonexhaustivePatternMatch, ERR_CATEGORY_PATTERN, \
        Str const *modname; \
        struct SourceSpan span; \
    ) \
    X(GlobalConstantCycle, ERR_CATEGORY_CODEGEN, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *name; \
    ) \
    X(UninitializedDestructuring, ERR_CATEGORY_CODEGEN, \
        Str const *modname; \
        struct SourceSpan span; \
    ) \
    X(ModifiedConstant, ERR_CATEGORY_CODEGEN, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *name; \
    ) \
    X(TooManyUpvalues, ERR_CATEGORY_CODEGEN, \
        Str const *modname; \
        struct SourceSpan span; \
        int limit; \
    ) \
    X(InitializedExternConstant, ERR_CATEGORY_CODEGEN, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *name; \
    ) \
    X(UninitializedConstant, ERR_CATEGORY_CODEGEN, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *name; \
    ) \
    X(InfiniteSizeObject, ERR_CATEGORY_CODEGEN, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *name; \
    ) \
    X(ObjectTooLarge, ERR_CATEGORY_CODEGEN, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *name; \
    ) \
    X(UseBeforeInitialization, ERR_CATEGORY_CODEGEN, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *name; \
    ) \
    X(ConstantDivideByZero, ERR_CATEGORY_CODEGEN, \
        Str const *modname; \
        struct SourceSpan span; \
    ) \
    X(ConstantNegativeShiftCount, ERR_CATEGORY_CODEGEN, \
        Str const *modname; \
        struct SourceSpan span; \
    ) \
    X(TooManyVariables, ERR_CATEGORY_CODEGEN, \
        Str const *modname; \
        struct SourceSpan span; \
        int limit; \
    ) \
    X(TooFarToJump, ERR_CATEGORY_CODEGEN, \
        Str const *modname; \
        struct SourceSpan span; \
        int limit; \
    ) \
    X(TooManyConstants, ERR_CATEGORY_CODEGEN, \
        Str const *modname; \
        struct SourceSpan span; \
        int limit; \
    )

enum ErrorCategory {
    ERR_CATEGORY_SYNTAX, // Lexing and parsing
    ERR_CATEGORY_NAME, // Name resolution
    ERR_CATEGORY_TYPE, // Typesystem
    ERR_CATEGORY_TRAIT, // Trait solver
    ERR_CATEGORY_PATTERN, // Pattern matching
    ERR_CATEGORY_MIDDLE, // Middle end semantic analysis
    ERR_CATEGORY_RESERVED, // Reserved for expansion
    ERR_CATEGORY_KFOLD, // Constant evaluation
    ERR_CATEGORY_CODEGEN, // Code generation
    ERR_CATEGORY_ICE, // Internal errors
};

enum ErrorKind {
#define X(Name_, Category_, Fields_) kErr##Name_,
    ERR_ERROR_LIST(X)
#undef X

    // TODO: remove old enumerators
    E_UNSUPPORTED, E_EMPTY_CHAR, E_CHAR_TOO_LONG, E_UNTERMINATED_CHAR, E_UNTERMINATED_STRING, E_INVALID_UNICODE_CODEPOINT, E_EXPECTED_INTEGER_DIGIT, E_UNEXPECTED_INTEGER_CHAR, E_INTEGER_OUT_OF_RANGE, E_INVALID_INTEGER, E_INVALID_FLOAT, E_NULL_BEFORE_EOF, E_EXPECTED_SYMBOL, E_EXPECTED_DELIMITER, E_EXPECTED_SEMICOLON, E_EMPTY_TYPE_LIST, E_NONLITERAL_PATTERN, E_NEGATIVE_MINIMUM_INTEGER, E_INVALID_LITERAL_NEGATION, E_INVALID_SELECTOR, E_EXPECTED_BASIC_TYPE, E_EXPECTED_EXPRESSION, E_EMPTY_ENUMERATION, E_EMPTY_STRUCT_BODY, E_EMPTY_VARIANT_FIELD_LIST, E_FUNCTION_TYPE_DECL, E_INVALID_ANNOTATION_TYPE, E_EXPECTED_TYPE_ANNOTATION, E_RETURN_OUTSIDE_FUNCTION, E_CHAIN_OUTSIDE_FUNCTION, E_JUMP_OUTSIDE_LOOP, E_TOO_MANY_ELEMENTS, E_EXPECTED_TOPLEVEL_ITEM, E_COLON_AFTER_LIST_ELEMENT, E_EXPECTED_COLON_AFTER_MAP_KEY, E_COLONS_AFTER_UNDERSCORE, E_EXPECTED_SELF_PARAMETER, E_UNEXPECTED_UNDERSCORE, E_EXPECTED_COMMA_SEPARATOR, E_DUPLICATE_ANNOTATION, E_NONLITERAL_ANNOTATION_VALUE, E_NONPRIMITIVE_ANNOTATION_VALUE, E_INVALID_GLOB, E_ITEM_VISIBILITY, E_ASSOCIATED_ITEM_VISIBILITY, E_INVALID_GLOB_TARGET, E_AMBIGUOUS_PATH, E_DUPLICATE_ITEM, E_UNKNOWN_PATH, E_MULTIPLE_APPLICABLE_ITEMS, E_UNEXPECTED_MODULE_NAME, E_TRANSITIVE_IMPORT, E_INCORRECT_TYPE_ARITY, E_EXPECTED_TYPE_ARGUMENTS, E_UNEXPECTED_TYPE_ARGUMENTS, E_EXPECTED_TRAIT, E_UNEXPECTED_TRAIT, E_INCORRECT_ITEM_CLASS, E_EXTRA_SEGMENT, E_INVALID_ASSIGNMENT_TARGET, E_MODULE_NOT_FOUND, E_EXTERN_FUNCTION_BODY, E_MISSING_FUNCTION_BODY, E_MISSING_TRAIT_BOUNDS, E_TRAIT_BOUNDS_ON_ALIAS_GENERIC, E_UNSATISFIED_TRAIT_BOUNDS, E_INCOMPATIBLE_TYPES, E_CANNOT_INFER, E_CYCLIC_TYPE, E_MISSING_EXTERN_VALUE, E_MISSING_TRAIT_METHOD, E_TRAIT_METHOD_VISIBILITY_MISMATCH, E_MISSING_VARIANT_ARGS, E_RESERVED_IDENTIFIER, E_UNEXPECTED_TYPE, E_MISSING_FIELDS, E_EXPECTED_VALUE, E_INVALID_CHAIN_OPERAND, E_INVALID_UNARY_OPERAND, E_INVALID_BINARY_OPERAND, E_CANNOT_CONSTANT_EVALUATE, E_NONPRIMITIVE_CONSTANT, E_EXPECTED_ELEMENT_SELECTOR, E_ELEMENT_SELECTOR_OUT_OF_RANGE, E_EXPECTED_ADT, E_EXPECTED_FIELD_SELECTOR, E_MISSING_FIELD, E_UNKNOWN_FIELD, E_UNKNOWN_METHOD, E_UNKNOWN_ASSOCIATED_ITEM, E_NOT_A_METHOD, E_NOT_CALLABLE, E_INCORRECT_ARITY, E_DUPLICATE_BINDING, E_DUPLICATE_FIELD, E_EXPECTED_STRUCT, E_UNIT_STRUCT_WITH_BRACES, E_UNIT_VARIANT_WITH_PARENTHESIS, E_INVALID_INDEX_TARGET, E_MISSING_BINDING_IN_ALTERNATIVE, E_INVALID_INCLUSIVE_RANGE, E_EXPECTED_DIVERGENCE, E_GLOBAL_CONSTANT_CYCLE, E_UNINITIALIZED_DESTRUCTURING, E_MODIFIED_CONSTANT, E_TOO_MANY_UPVALUES, E_INITIALIZED_EXTERN_CONSTANT, E_UNINITIALIZED_CONSTANT, E_INFINITE_SIZE_OBJECT, E_OBJECT_TOO_LARGE, E_NONEXHAUSTIVE_PATTERN_MATCH, E_USE_BEFORE_INITIALIZATION, E_CONSTANT_DIVIDE_BY_ZERO, E_CONSTANT_NEGATIVE_SHIFT_COUNT, E_TOO_MANY_VARIABLES, E_TOO_FAR_TO_JUMP, E_TOO_MANY_CONSTANTS, E_CAPTURED_INOUT_ARG,
};

// Define error structures
#define X(Name_, Category_, Fields_) struct Name_##Error {Fields_};
    ERR_ERROR_LIST(X)
#undef X

#define THROW_ERROR(C_, Kind_, ...) pawErr_throw(C_, \
        kErr##Kind_, &(struct Kind_##Error) {__VA_ARGS__})
_Noreturn void pawErr_throw(struct Compiler *C, enum ErrorKind kind, void *payload);

enum ErrorCategory pawErr_error_category(enum ErrorKind kind);




// TODO: remove everything below this line except for the "endif"

// general errors
_Noreturn void pawErr_unsupported(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *feature);

// lexical errors
_Noreturn void pawErr_empty_char(struct Compiler *C, Str const *modname, struct SourceSpan span);
_Noreturn void pawErr_char_too_long(struct Compiler *C, Str const *modname, struct SourceSpan span);
_Noreturn void pawErr_unterminated_char(struct Compiler *C, Str const *modname, struct SourceSpan span);
_Noreturn void pawErr_unterminated_string(struct Compiler *C, Str const *modname, struct SourceSpan span);
_Noreturn void pawErr_invalid_escape(struct Compiler *C, Str const *modname, struct SourceSpan span, char c);
_Noreturn void pawErr_invalid_hex_escape(struct Compiler *C, Str const *modname, struct SourceSpan span);
_Noreturn void pawErr_hex_escape_too_short(struct Compiler *C, Str const *modname, struct SourceSpan span);
_Noreturn void pawErr_unicode_escape_too_long(struct Compiler *C, Str const *modname, struct SourceSpan span);
_Noreturn void pawErr_unterminated_unicode_escape(struct Compiler *C, Str const *modname, struct SourceSpan span);
_Noreturn void pawErr_invalid_unicode_escape(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *codepoint);
_Noreturn void pawErr_invalid_unicode_codepoint(struct Compiler *C, Str const *modname, struct SourceSpan span, unsigned codepoint);
_Noreturn void pawErr_expected_integer_digit(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *base);
_Noreturn void pawErr_unexpected_integer_char(struct Compiler *C, Str const *modname, struct SourceSpan span, char c, char const *base);
_Noreturn void pawErr_integer_too_big_to_parse(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *text);
_Noreturn void pawErr_integer_out_of_range(struct Compiler *C, Str const *modname, struct SourceSpan span, paw_Uint u);
_Noreturn void pawErr_invalid_integer(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *base, char const *text);
_Noreturn void pawErr_invalid_float(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *text);
_Noreturn void pawErr_too_many_lines(struct Compiler *C, Str const *modname, struct SourceSpan span, int limit);
_Noreturn void pawErr_too_many_columns(struct Compiler *C, Str const *modname, struct SourceSpan span, int limit);
_Noreturn void pawErr_name_too_long(struct Compiler *C, Str const *modname, struct SourceSpan span, int length, int limit);
_Noreturn void pawErr_null_before_eof(struct Compiler *C, Str const *modname, struct SourceSpan span, int length);

// parser errors
_Noreturn void pawErr_unexpected_symbol(struct Compiler *C, Str const *modname, struct SourceSpan span);
_Noreturn void pawErr_expected_delimiter(struct Compiler *C, Str const *modname, struct SourceSpan span, char right, char left, struct SourceLoc open);
_Noreturn void pawErr_expected_semicolon(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *what);
_Noreturn void pawErr_empty_type_list(struct Compiler *C, Str const *modname, struct SourceSpan span);
_Noreturn void pawErr_nonliteral_pattern(struct Compiler *C, Str const *modname, struct SourceSpan span);
_Noreturn void pawErr_negative_minimum_integer(struct Compiler *C, Str const *modname, struct SourceSpan span);
_Noreturn void pawErr_invalid_literal_negation(struct Compiler *C, Str const *modname, struct SourceSpan span);
_Noreturn void pawErr_invalid_selector(struct Compiler *C, Str const *modname, struct SourceSpan span);
_Noreturn void pawErr_expected_basic_type(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *what);
_Noreturn void pawErr_expected_expression(struct Compiler *C, Str const *modname, struct SourceSpan span);
_Noreturn void pawErr_empty_enumeration(struct Compiler *C, Str const *modname, struct SourceSpan span);
_Noreturn void pawErr_empty_struct_body(struct Compiler *C, Str const *modname, struct SourceSpan span);
_Noreturn void pawErr_empty_variant_field_list(struct Compiler *C, Str const *modname, struct SourceSpan span);
_Noreturn void pawErr_function_type_decl(struct Compiler *C, Str const *modname, struct SourceSpan span);
_Noreturn void pawErr_invalid_annotation_type(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *name);
_Noreturn void pawErr_expected_type_annotation(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *what, char const *name);
_Noreturn void pawErr_too_many_elements(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *what, int limit);
_Noreturn void pawErr_expected_toplevel_item(struct Compiler *C, Str const *modname, struct SourceSpan span);
_Noreturn void pawErr_expected_colon_after_map_key(struct Compiler *C, Str const *modname, struct SourceSpan span);
_Noreturn void pawErr_colon_after_list_element(struct Compiler *C, Str const *modname, struct SourceSpan span);
_Noreturn void pawErr_colons_after_underscore(struct Compiler *C, Str const *modname, struct SourceSpan span);
_Noreturn void pawErr_expected_self_parameter(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *name);
_Noreturn void pawErr_unexpected_underscore(struct Compiler *C, Str const *modname, struct SourceSpan span);
_Noreturn void pawErr_expected_comma_separator(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *what);
_Noreturn void pawErr_duplicate_annotation(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *name);
_Noreturn void pawErr_nonliteral_annotation_value(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *name);
_Noreturn void pawErr_nonprimitive_annotation_value(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *name);
_Noreturn void pawErr_invalid_glob(struct Compiler *C, Str const *modname, struct SourceSpan span);
_Noreturn void pawErr_return_outside_function(struct Compiler *C, Str const *modname, struct SourceSpan span);
_Noreturn void pawErr_chain_outside_function(struct Compiler *C, Str const *modname, struct SourceSpan span);
_Noreturn void pawErr_jump_outside_loop(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *what);

// import errors
_Noreturn void pawErr_module_not_found(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *what);

// AST lowering errors
_Noreturn void pawErr_invalid_assignment_target(struct Compiler *C, Str const *modname, struct SourceSpan span);

// collection errors
_Noreturn void pawErr_ambiguous_path(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *path);
_Noreturn void pawErr_duplicate_item(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *what, char const *name);
_Noreturn void pawErr_extern_function_body(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *name);
_Noreturn void pawErr_missing_function_body(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *name);
_Noreturn void pawErr_item_visibility(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *mod, char const *item);
_Noreturn void pawErr_associated_item_visibility(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *name, char const *type);
_Noreturn void pawErr_invalid_glob_target(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *path);

// type errors
_Noreturn void pawErr_missing_trait_bounds(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *name);
_Noreturn void pawErr_trait_bounds_on_alias_generic(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *name);
_Noreturn void pawErr_unsatisfied_trait_bounds(struct Compiler *C, Str const *modname, struct SourceSpan span);
_Noreturn void pawErr_incompatible_types(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *lhs, char const *rhs);
_Noreturn void pawErr_cannot_infer(struct Compiler *C, Str const *modname, struct SourceSpan span);
_Noreturn void pawErr_cyclic_type(struct Compiler *C, Str const *modname, struct SourceSpan span);

// compiler errors
_Noreturn void pawErr_missing_extern_value(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *name);

// lookup errors
_Noreturn void pawErr_unexpected_module_name(struct Compiler *C, Str const *modname, struct SourceSpan span);
_Noreturn void pawErr_transitive_import(struct Compiler *C, Str const *modname, struct SourceSpan span);
_Noreturn void pawErr_incorrect_type_arity(struct Compiler *C, Str const *modname, struct SourceSpan span, int want, int have);
_Noreturn void pawErr_expected_type_arguments(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *what, char const *name);
_Noreturn void pawErr_unexpected_type_arguments(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *what, char const *name);
_Noreturn void pawErr_expected_trait(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *path);
_Noreturn void pawErr_unexpected_trait(struct Compiler *C, Str const *modname, struct SourceSpan span);
_Noreturn void pawErr_incorrect_item_class(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *want, char const *have);
_Noreturn void pawErr_extra_segment(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *name);

// trait errors
_Noreturn void pawErr_missing_trait_method(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *name);
_Noreturn void pawErr_trait_method_visibility_mismatch(struct Compiler *C, Str const *modname, struct SourceSpan span, paw_Bool expected_pub, char const *name);

// TODO: new resolver errors
_Noreturn void pawErr_multiple_applicable_items(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *path);

// resolver errors
_Noreturn void pawErr_missing_variant_args(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *cons);
_Noreturn void pawErr_reserved_identifier(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *name);
_Noreturn void pawErr_unexpected_type(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *type);
_Noreturn void pawErr_unknown_path(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *path);
_Noreturn void pawErr_unknown_field(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *name, char const *type);
_Noreturn void pawErr_missing_fields(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *type);
_Noreturn void pawErr_expected_value(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *type);
_Noreturn void pawErr_invalid_chain_operand(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *type);
_Noreturn void pawErr_invalid_unary_operand(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *type, char const *op);
_Noreturn void pawErr_invalid_binary_operand(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *type, char const *op);
_Noreturn void pawErr_cannot_constant_evaluate(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *type);
_Noreturn void pawErr_nonprimitive_constant(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *type);
_Noreturn void pawErr_expected_element_selector(struct Compiler *C, Str const *modname, struct SourceSpan span);
_Noreturn void pawErr_element_selector_out_of_range(struct Compiler *C, Str const *modname, struct SourceSpan span, int elem, int count);
_Noreturn void pawErr_expected_adt(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *type);
_Noreturn void pawErr_expected_field_selector(struct Compiler *C, Str const *modname, struct SourceSpan span);
_Noreturn void pawErr_unknown_field(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *name, char const *type);
_Noreturn void pawErr_unknown_method(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *method, char const *type);
_Noreturn void pawErr_unknown_associated_item(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *method, char const *type);
_Noreturn void pawErr_not_a_method(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *type);
_Noreturn void pawErr_not_callable(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *type);
_Noreturn void pawErr_incorrect_arity(struct Compiler *C, Str const *modname, struct SourceSpan span, int want, int have);
_Noreturn void pawErr_duplicate_field(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *name, char const *type);
_Noreturn void pawErr_duplicate_binding(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *name);
_Noreturn void pawErr_expected_struct(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *type);
_Noreturn void pawErr_unit_struct_with_braces(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *type);
_Noreturn void pawErr_unit_variant_with_parenthesis(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *type);
_Noreturn void pawErr_missing_field(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *name, char const *type);
_Noreturn void pawErr_unknown_field(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *name, char const *type);
_Noreturn void pawErr_invalid_index_target(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *type);
_Noreturn void pawErr_missing_binding_in_alternative(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *name);
_Noreturn void pawErr_invalid_inclusive_range(struct Compiler *C, Str const *modname, struct SourceSpan span);
_Noreturn void pawErr_expected_divergence(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *fn);

// HIR lowering errors
_Noreturn void pawErr_global_constant_cycle(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *name);
_Noreturn void pawErr_uninitialized_destructuring(struct Compiler *C, Str const *modname, struct SourceSpan span);
_Noreturn void pawErr_modified_constant(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *name);
_Noreturn void pawErr_too_many_upvalues(struct Compiler *C, Str const *modname, struct SourceSpan span, int limit);
_Noreturn void pawErr_initialized_extern_constant(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *name);
_Noreturn void pawErr_uninitialized_constant(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *name);

// unboxing errors
_Noreturn void pawErr_infinite_size_object(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *name);
_Noreturn void pawErr_object_too_large(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *name);

// exhaustiveness checking errors
_Noreturn void pawErr_nonexhaustive_pattern_match(struct Compiler *C, Str const *modname, struct SourceSpan span);

// SSA construction errors
_Noreturn void pawErr_use_before_initialization(struct Compiler *C, Str const *modname, struct SourceSpan span, char const *name);

// constant propagation errors
_Noreturn void pawErr_constant_divide_by_zero(struct Compiler *C, Str const *modname, struct SourceSpan span);
_Noreturn void pawErr_constant_negative_shift_count(struct Compiler *C, Str const *modname, struct SourceSpan span);

// register allocation errors
_Noreturn void pawErr_too_many_variables(struct Compiler *C, Str const *modname, struct SourceSpan span, int limit);

// code generation errors
_Noreturn void pawErr_too_far_to_jump(struct Compiler *C, Str const *modname, struct SourceSpan span, int limit);
_Noreturn void pawErr_too_many_constants(struct Compiler *C, Str const *modname, struct SourceSpan span, int limit);

_Noreturn void pawErr_captured_inout_arg(struct Compiler *C, Str const *modname, struct SourceSpan span);

typedef struct ErrorHandler {
    paw_Env *P;
    Str const *modname;
    Str const *message;
    Str const *hint;
    struct SourceSpan span;
} ErrorHandler;

void pawErr_start(paw_Env *P);
void pawErr_set_module_name(paw_Env *P, Str const *name);
void pawErr_set_source_loc(paw_Env *P, struct SourceSpan span);
void pawErr_set_message(paw_Env *P, char const *fmt, ...);
void pawErr_set_hint(paw_Env *P, char const *fmt, ...);
void pawErr_finish(paw_Env *P);

_Noreturn void pawErr_generic_error(paw_Env *P, Str const *modname, struct SourceSpan span, char const *fmt, ...);

// Convenience functions for throwing common errors
_Noreturn void pawErr_exceeded_limit(paw_Env *P, Str const *modname, struct SourceSpan span, char const *what, paw_Int limit);

#endif // PAW_ERROR_H
