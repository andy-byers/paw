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

static void describe_span_start(paw_Env *P, struct SourceSpan span, Buffer *buffer)
{
    enum SpanRefKind ref_kind;
    paw_Bool is_ref = maybe_deref_span(P, &span, &ref_kind);
    if (is_ref) {
        pawL_add_fstring(P, buffer, "in code generated from %s on ",
                ref_kind_name(ref_kind));
    } else {
        L_ADD_LITERAL(P, buffer, "on ");
    }
    struct SourceLoc const start = SourceSpan_range_start(span);
    pawL_add_fstring(P, buffer, "line %d", start.line);
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
            "%s: too many lines in file `%s` (expected at most %d)",
            error->modname->text, error->filename->text, error->max_lines);
}

static void FormatTooManyColumnsError(paw_Env *P, struct TooManyColumnsError *error, Buffer *buffer)
{
    pawL_add_fstring(P, buffer,
            "%s: too many columns in file `%s` (expected at most %d)",
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
    pawL_add_fstring(P, buffer, "expected symbol `%s` but found `%s`",
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
    pawL_add_fstring(P, buffer, "expected semicolon after `%s`",
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
            "integer %U out of range of type `int` (maximum value is %I)",
            error->uint64, PAW_INT_MAX);
}

static void FormatNegativeIntegerOutOfRangeError(paw_Env *P, struct NegativeIntegerOutOfRangeError *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer,
            "integer -%U out of range of type `int` (minimum value is %I)",
            error->uint64, PAW_INT_MIN);
}

static void FormatInvalidLiteralNegationError(paw_Env *P, struct InvalidLiteralNegationError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer,
            "encountered operator `-` applied to non-integral literal");
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
            "invalid value type for annotation `%s`",
            error->name->text);
}

static void FormatExpectedTypeAnnotationError(paw_Env *P, struct ExpectedTypeAnnotationError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer,
            "expected type annotationon on %s `%s`",
            error->what->text, error->name->text);
}

static void FormatReturnOutsideFunctionError(paw_Env *P, struct ReturnOutsideFunctionError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "encountered `return` outside function body");
}

static void FormatChainOutsideFunctionError(paw_Env *P, struct ChainOutsideFunctionError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "encountered `?` operator outside function body");
}

static void FormatJumpOutsideLoopError(paw_Env *P, struct JumpOutsideLoopError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "encountered `%s` outside loop body",
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
            "`*%s` can only appear before `self` or a type",
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
            "annotation value for `%s` is not a literal expression",
            error->name->text);
}

static void FormatNonprimitiveAnnotationValueError(paw_Env *P, struct NonprimitiveAnnotationValueError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer,
            "annotation value for `%s` is not a primitive value",
            error->name->text);
}

static void FormatUseOfReservedIdentifierError(paw_Env *P, struct UseOfReservedIdentifierError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "use of reserved identifier `%s`", error->name->text);
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
    pawL_add_fstring(P, buffer, "ambiguous path `%s`",
            error->path->text);
}

static void FormatDuplicateItemError(paw_Env *P, struct DuplicateItemError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "duplicate %s `%s`",
            error->what->text, error->item_name->text);
}

static void FormatExternFunctionBodyError(paw_Env *P, struct ExternFunctionBodyError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer,
            "encountered body on function `%s` declared with `extern` annotation",
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
            "non-public associated item `%s` (defined on type `%s`) "
            "can only be accessed by other items in the same module",
            error->field_name->text, error->parent_name->text);
}

static void FormatInvalidGlobTargetError(paw_Env *P, struct InvalidGlobTargetError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "invalid glob target `%s`",
            error->path->text);
}


static void FormatMissingTraitBoundsError(paw_Env *P, struct MissingTraitBoundsError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "type parameter `%s` missing trait bounds",
            error->name->text);
}

static void FormatTraitBoundsOnAliasGenericError(paw_Env *P, struct TraitBoundsOnAliasGenericError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "trait bounds not allowed on type parameter `%s` for type alias",
            error->name);
}

static void FormatFalseObligationError(paw_Env *P, struct FalseObligationError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer,
            "obligation `%s` was disproven",
            error->obligation->text);
}

static void FormatUnsatisfiedObligationError(paw_Env *P, struct UnsatisfiedObligationError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer,
            "unsatisfiable obligation `%s`",
            error->example->text);
    if (error->num_unsolved > 1)
        pawL_add_fstring(P, buffer,
                " (and %d others)",
                error->num_unsolved - 1);
}

static void FormatIncompatibleTypesError(paw_Env *P, struct IncompatibleTypesError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "incompatible types `%s` and `%s`",
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
    pawL_add_fstring(P, buffer, "missing extern value `%s`",
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
    pawL_add_fstring(P, buffer, "expected type arguments on %s `%s`",
            error->what->text, error->name->text);
}

static void FormatUnexpectedTypeArgumentsError(paw_Env *P, struct UnexpectedTypeArgumentsError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "unexpected type arguments on %s `%s`",
            error->what->text, error->name->text);
}

static void FormatExpectedTraitError(paw_Env *P, struct ExpectedTraitError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "expected trait but found type `%s`",
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
    pawL_add_fstring(P, buffer, "extraneous `::%s`",
            error->name->text);
}

static void FormatDuplicateAssocItemError(paw_Env *P, struct DuplicateAssocItemError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer,
            "duplicate associated item `%s` in ",
            error->item->text);
}

static void FormatTraitImplAssocItemNotCompatibleError(paw_Env *P, struct TraitImplAssocItemNotCompatibleError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer,
            "assocaited type `%s` not compatible with corresponding item"
            "in trait definition `%s`",
            error->item->text, error->trait->text);
}

static void FormatTraitImplUnknownAssocItemError(paw_Env *P, struct TraitImplUnknownAssocItemError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer,
            "assocaited type `%s` not found in trait definition `%s`",
            error->item->text, error->trait->text);
}

static void FormatTraitImplMissingAssocItemError(paw_Env *P, struct TraitImplMissingAssocItemError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->impl_span, buffer);
    pawL_add_fstring(P, buffer,
            "missing associated item `%s` in implementation of trait `%s` "
            "(declaration of `%s` is ",
            error->item->text, error->trait->text, error->item->text);
    describe_span_start(P, error->missing_span, buffer);
    pawL_add_fstring(P, buffer, " of ");
    if (pawS_eq(error->missing_modname, error->modname)) {
        L_ADD_LITERAL(P, buffer, "this module");
    } else {
        pawL_add_fstring(P, buffer, "module `%s`",
                error->missing_modname->text);
    }
    pawL_add_char(P, buffer, ')');
}

static void FormatMultipleApplicableItemsError(paw_Env *P, struct MultipleApplicableItemsError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "multiple applicable items");
}

static void FormatMissingVariantArgsError(paw_Env *P, struct MissingVariantArgsError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "missing argument(s) for variant constructor `%s`",
            error->cons->text);
}

static void FormatUnexpectedTypeError(paw_Env *P, struct UnexpectedTypeError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "expected value but found type `%s`",
            error->type->text);
}

static void FormatUnknownPathError(paw_Env *P, struct UnknownPathError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "unknown path `%s`",
            error->path->text);
}

static void FormatMissingFieldsError(paw_Env *P, struct MissingFieldsError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "missing fields on initializer for struct `%s`",
            error->type->text);
}

static void FormatExpectedValueError(paw_Env *P, struct ExpectedValueError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "expected value but found type `%s`",
            error->type->text);
}

static void FormatInvalidImplTargetError(paw_Env *P, struct InvalidImplTargetError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "invalid `Self` for impl block");
}

static void FormatInvalidChainOperandError(paw_Env *P, struct InvalidChainOperandError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "invalid operand type `%s` for chain operator",
            error->type->text);
}

static void FormatInvalidUnaryOperandError(paw_Env *P, struct InvalidUnaryOperandError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "invalid operand type `%s` for unary operator `%s`",
            error->type->text, error->op->text);
}

static void FormatInvalidBinaryOperandError(paw_Env *P, struct InvalidBinaryOperandError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "invalid operand type `%s` for binary operator `%s`",
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
    pawL_add_fstring(P, buffer, "expected primitive constant but found `%s`",
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
    pawL_add_fstring(P, buffer, "expected ADT but found `%s`",
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
    pawL_add_fstring(P, buffer, "unknown method `%s` for type `%s`",
            error->method->text, error->type->text);
}

static void FormatUnknownAssociatedItemError(paw_Env *P, struct UnknownAssociatedItemError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "unknown associated item `%s` for type `%s`",
            error->item->text, error->type->text);
}


static void FormatNotAMethodError(paw_Env *P, struct NotAMethodError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "`%s` is not a method",
            error->name->text);
}

static void FormatNotCallableError(paw_Env *P, struct NotCallableError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "type `%s` is not callable",
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
    pawL_add_fstring(P, buffer, "duplicate %s name `%s`",
            error->what->text, error->name->text);
}

static void FormatDuplicateBindingError(paw_Env *P, struct DuplicateBindingError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "duplicate binding `%s`",
            error->name->text);
}

static void FormatExpectedStructError(paw_Env *P, struct ExpectedStructError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "expected struct but found `%s`",
            error->type->text);
}

static void FormatUnitStructWithBracesError(paw_Env *P, struct UnitStructWithBracesError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer,
            "unexpected braces on unit struct `%s` (omit "
            "braces to construct structure with no fields)",
            error->type->text);
}

static void FormatUnitVariantWithParenthesisError(paw_Env *P, struct UnitVariantWithParenthesisError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer,
            "unexpected parenthesis on unit variant `%s` (omit "
            "parenthesis to construct variant with no fields)",
            error->type->text);
}

static void FormatMissingFieldError(paw_Env *P, struct MissingFieldError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "missing initializer for field `%s` on struct `%s`",
            error->name->text, error->type->text);
}

static void FormatUnknownFieldError(paw_Env *P, struct UnknownFieldError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "unknown field `%s` on struct `%s`",
            error->name->text, error->type->text);
}

static void FormatInvalidIndexTargetError(paw_Env *P, struct InvalidIndexTargetError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "invalid target `%s` for index operator",
            error->type->text);
}

static void FormatMissingBindingInAlternativeError(paw_Env *P, struct MissingBindingInAlternativeError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "missing binding `%s` in alternative pattern",
            error->name->text);
}

static void FormatExpectedDivergenceError(paw_Env *P, struct ExpectedDivergenceError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "expected `%s` to diverge based on return type of `!`",
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
    pawL_add_fstring(P, buffer, "cycle detected between global constants (involves constant `%s`)",
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
    pawL_add_fstring(P, buffer, "attempt to modify constant `%s`",
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
    pawL_add_fstring(P, buffer, "unexpected initializer for `extern` constant `%s`",
            error->name->text);
}

static void FormatUninitializedConstantError(paw_Env *P, struct UninitializedConstantError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "missing initializer for constant `%s`",
            error->name->text);
}

static void FormatInfiniteSizeObjectError(paw_Env *P, struct InfiniteSizeObjectError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "object `%s` has infinite size",
            error->name->text);
}

static void FormatObjectTooLargeError(paw_Env *P, struct ObjectTooLargeError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "object `%s` is too large",
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
    pawL_add_fstring(P, buffer, "use of variable `%s` before initialization",
            error->name->text);
}

static void FormatUseAfterMoveError(paw_Env *P, struct UseAfterMoveError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "use of variable `%s` after it was moved",
            error->name->text);
}

static void FormatFalseConstObligationError(paw_Env *P, struct FalseConstObligationError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "const obligation for `%s` was disproven",
            error->name->text);
}

static void FormatUnsatisfiedConstObligationError(paw_Env *P, struct UnsatisfiedConstObligationError const *error, Buffer *buffer)
{
    add_error_header(P, error->modname, error->span, buffer);
    pawL_add_fstring(P, buffer, "unsatisfiable const obligation for `%s`",
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
    }
}

