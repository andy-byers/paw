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
    X(None, ERR_CATEGORY_NONE, ) \
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
    X(HexEscapeTooShort, ERR_CATEGORY_SYNTAX, \
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
    X(ModuleNotFound, ERR_CATEGORY_SYNTAX, \
        Str const *modname; \
        struct SourceSpan span; \
    ) \
    X(AmbiguousPath, ERR_CATEGORY_SYNTAX, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *path; \
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
    X(ExpectedTraitPath, ERR_CATEGORY_SYNTAX, \
        Str const *modname; \
        struct SourceSpan span; \
    ) \
    X(ExpectedType, ERR_CATEGORY_SYNTAX, \
        Str const *modname; \
        struct SourceSpan span; \
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
        Str const *missing_modname; \
        struct SourceSpan missing_span; \
        struct SourceSpan impl_span; \
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
        Str const *kind; \
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
    X(InvalidImplTarget, ERR_CATEGORY_NAME, \
        Str const *modname; \
        struct SourceSpan span; \
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
    X(CannotInferConst, ERR_CATEGORY_TYPE, \
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
    X(TraitNotImplemented, ERR_CATEGORY_TYPE, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *trait; \
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
    X(GlobalConstantCycle, ERR_CATEGORY_MIDDLE, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *name; \
    ) \
    X(UninitializedDestructuring, ERR_CATEGORY_MIDDLE, \
        Str const *modname; \
        struct SourceSpan span; \
    ) \
    X(ModifiedConstant, ERR_CATEGORY_MIDDLE, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *name; \
    ) \
    X(TooManyUpvalues, ERR_CATEGORY_MIDDLE, \
        Str const *modname; \
        struct SourceSpan span; \
        int limit; \
    ) \
    X(InitializedExternConstant, ERR_CATEGORY_MIDDLE, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *name; \
    ) \
    X(UninitializedConstant, ERR_CATEGORY_MIDDLE, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *name; \
    ) \
    X(InfiniteSizeObject, ERR_CATEGORY_MIDDLE, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *name; \
    ) \
    X(ObjectTooLarge, ERR_CATEGORY_MIDDLE, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *name; \
    ) \
    X(UseBeforeInitialization, ERR_CATEGORY_MIDDLE, \
        Str const *modname; \
        struct SourceSpan local_span; \
        struct SourceSpan use_span; \
        Str const *name; \
    ) \
    X(UseAfterMove, ERR_CATEGORY_MIDDLE, \
        Str const *modname; \
        struct SourceSpan local_span; \
        struct SourceSpan use_span; \
        Str const *name; \
    ) \
    X(FalseConstObligation, ERR_CATEGORY_MIDDLE, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *name; \
    ) \
    X(UnsatisfiedConstObligation, ERR_CATEGORY_MIDDLE, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *name; \
    ) \
    X(ConstantDivideByZero, ERR_CATEGORY_KFOLD, \
        Str const *modname; \
        struct SourceSpan span; \
    ) \
    X(ConstantOverflow, ERR_CATEGORY_KFOLD, \
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
    ) \
    X(Unsupported, ERR_CATEGORY_ICE, \
        Str const *modname; \
        struct SourceSpan span; \
    ) \
    X(Internal, ERR_CATEGORY_ICE, \
        Str const *modname; \
        struct SourceSpan span; \
        Str const *message; \
    )

enum ErrorCategory {
    ERR_CATEGORY_NONE,
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
};

// Define error structures
#define X(Name_, Category_, Fields_) struct Name_##Error {Fields_};
    ERR_ERROR_LIST(X)
#undef X

#define THROW_ERROR(C_, Kind_, ...) pawErr_throw(C_, \
        kErr##Kind_, &(struct Kind_##Error) {__VA_ARGS__})
EXTERN_C _Noreturn void pawErr_throw(struct Compiler *C, enum ErrorKind kind, void *payload);

enum ErrorCategory pawErr_error_category(enum ErrorKind kind);




// TODO: remove everything below this line except for the "endif"

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
