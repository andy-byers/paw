#include "lex.h"
#include "string.h"
#include "test.h"

const char *tokenRepr(enum TokenKind kind)
{
    switch (kind) {
        case kTokenParenL:
            return "(";
        case kTokenParenR:
            return ")";
        case kTokenBracketL:
            return "[";
        case kTokenBracketR:
            return "]";
        case kTokenCurlyL:
            return "{";
        case kTokenCurlyR:
            return "}";
        case kTokenMinus:
            return "-";
        case kTokenPlus:
            return "+";
        case kTokenStar:
            return "*";
        case kTokenSlash:
            return "/";
        case kTokenLess:
            return "<";
        case kTokenGreater:
            return ">";
        case kTokenAmper:
            return "&";
        case kTokenEquals:
            return "=";
        case kTokenCaret:
            return "^";
        case kTokenPercent:
            return "%";
        case kTokenBang:
            return "!";
        case kTokenComma:
            return ",";
        case kTokenQuestion:
            return "?";
        case kTokenTilde:
            return "~";
        case kTokenPipe:
            return "|";
        case kTokenSemi:
            return ";";
        default:
            return "";
    }
}

static struct StringTable sTable;
static struct Lex sLex = {.table = &sTable};

static void testTokenize(const char *text, const struct Token *result)
{
    initLexer(&sLex, text, (Size)strlen(text));
    for (Size i = 0;; ++i) {
        nextToken(&sLex);
        const struct Token t = sLex.token;
        CHECK(t.kind == result[i].kind);
        switch (t.kind) {
            case kTokenInteger:
            case kTokenFloat:
            case kTokenString:
            case kTokenName:
                // TODO: Check token values
            default:
                break;
        }
        if (t.kind >= kTokenEnd) {
            break;
        }
    }
}

static void testValidTokenize(const char *text)
{
    initLexer(&sLex, text, (Size)strlen(text));
    for (;;) {
        nextToken(&sLex);
        const struct Token t = sLex.token;
        CHECK(t.kind != kTokenError);
        if (t.kind == kTokenEnd) {
            break;
        }
    }
}

static void testInvalidTokenize(const char *text)
{
    initLexer(&sLex, text, (Size)strlen(text));
    for (;;) {
        nextToken(&sLex);
        const struct Token t = sLex.token;
        CHECK(t.kind != kTokenEnd);
        if (t.kind == kTokenError) {
            break;
        }
    }
}

static struct Token makeFloat(const char *text)
{
    return (struct Token){
        .kind = kTokenFloat,
        .real = strtod(text, NULL)};
}

static struct Token makeInteger(const char *text, Size base)
{
    return (struct Token){
        .kind = kTokenInteger,
        .integer = strtoll(text, NULL, base),
    };
}

static struct Token makeStringOrName(const char *text, Size size, Bool isString)
{
    static struct StringTable sTable;
    static Bool sIsInit = FALSE;
    if (!sIsInit) {
        initStrings(&sTable);
        sIsInit = TRUE;
    }
    return (struct Token){
        .string = addString(&sTable, text, size),
        .kind = isString ? kTokenString : kTokenName,
    };
}

#define MAKE_TOKEN(tokKind) \
    (struct Token) { .kind = (tokKind) }
#define MAKE_STRING(text) makeStringOrName(text, (Size)strlen(text), TRUE)
#define MAKE_NAME(text) makeStringOrName(text, (Size)strlen(text), FALSE)
#define MAKE_SINGLE(kind) MAKE_TOKEN(kind)
#define MAKE_END() MAKE_TOKEN(kTokenEnd)
#define MAKE_ERROR() MAKE_TOKEN(kTokenError)
#define MAKE_INT(text) makeInteger(text, 10)
#define MAKE_INT_BASE2(text) makeInteger(text, 2)
#define MAKE_INT_BASE8(text) makeInteger(text, 8)
#define MAKE_INT_BASE16(text) makeInteger(text, 16)
#define MAKE_REAL(text) makeFloat(text)

static void testValidInt(Size base, const char *text)
{
    testTokenize(text, (struct Token[]){
                           makeInteger(text, base),
                           MAKE_END(),
                       });
}

static void testValidFloat(const char *text)
{
    testTokenize(text, (struct Token[]){
                           MAKE_REAL(text),
                           MAKE_END(),
                       });
}

static void testValidString(const char *text)
{
    testTokenize(text, (struct Token[]){
                           MAKE_STRING(text),
                           MAKE_END(),
                       });
}

static void testParenList(const char *text)
{
    testTokenize(text, (struct Token[]){
                           MAKE_SINGLE(kTokenParenL),
                           MAKE_STRING(text),
                           MAKE_SINGLE(kTokenParenR),
                           MAKE_END(),
                       });
}

int main(void)
{
    // Initialize the string table used by the tests.
    initStrings(&sTable);

    // Valid line comments
    testValidTokenize("//");
    testValidTokenize("// comment");
    testValidTokenize("// a //b c//d//e");
    testValidTokenize("1//");
    testValidTokenize("1// comment");
    testValidTokenize("1// a //b c//d//e");
    testValidTokenize("1 //");
    testValidTokenize("1 // comment");
    testValidTokenize("1 // a //b c//d//e");
    testValidTokenize("1 // c1\n2 // c2\n3 // c3");

    // Valid block comments
    testValidTokenize("/**/");
    testValidTokenize("/*       */");
    testValidTokenize("/*comment*/");
    testValidTokenize("/*/*comment*/*/");
    testValidTokenize("/*a/*b*c/d/*e*f/*g*/");

    // Invalid block comments
    testInvalidTokenize("/*");
    testInvalidTokenize("/*       ");
    testInvalidTokenize("/*comment* /");
    testInvalidTokenize("/*comment");

    // Multiple comments in a row
    testValidTokenize("(/*1*/ /* 2 *//*3*/)//Line comment");

    // Valid integers
    testValidInt(16, "0X1");
    testValidInt(16, "0x1");
    testValidInt(16, "0x0");
    testValidInt(16, "0x01");
    testValidInt(16, "0x0123456789");
    testValidInt(16, "0xabcdef");
    testValidInt(16, "0xABCDEF");
    testValidInt(16, "0x0123456789abcdef");
    testValidInt(16, "0x0123456789ABCDEF");
    testValidInt(10, "0");
    testValidInt(10, "1");
    testValidInt(10, "1234567890");
    testValidInt(8, "0O0");
    testValidInt(8, "0o0");
    testValidInt(8, "0o1");
    testValidInt(8, "0o01");
    testValidInt(8, "0o01234567");
    testValidInt(2, "0B0");
    testValidInt(2, "0b0");
    testValidInt(2, "0b01010011");

    // Invalid integers
    testInvalidTokenize("0xg");
    // 'g' is not a hex digit, so this string will be interpreted as a hex number followed by
    // a name. The parsing phase should recognize this as a syntax error.
    // testTokenize("0x0123456789abcdefg", (struct Token[]){
    //            MAKE_INT_BASE16("0x0123456789abcdef"),
    //            MAKE_TOKEN(kTokenName, 0, "g"),
    //            MAKE_END(),
    //        });
    testInvalidTokenize("01");
    testInvalidTokenize("0x");
    testInvalidTokenize("0o");
    testInvalidTokenize("0b");
    testInvalidTokenize("0xx1");
    testInvalidTokenize("0oo1");
    testInvalidTokenize("0bb1");
    testInvalidTokenize("0o8");
    testInvalidTokenize("0o012345678");
    testInvalidTokenize("0b2");
    testInvalidTokenize("0b012");

    // Valid real numbers
    testValidFloat("1.0");
    testValidFloat("1.");
    testValidFloat("1.e1");
    testValidFloat("1e1");
    testValidFloat("1.0e1");
    testValidFloat("1.0E1");
    testValidFloat("1.0e+1");
    testValidFloat("1.0e-1");
    testValidFloat("123456789.0");
    testValidFloat("1.234567890");
    testValidFloat("123456789.0e+123");
    testValidFloat("1.234567890e-123");

    // Invalid real numbers
    testInvalidTokenize("1.0e");
    testInvalidTokenize("1.e");
    testInvalidTokenize(".1"); // Integral part is required
    testInvalidTokenize(".1e1");
    testInvalidTokenize(".1e");
    testInvalidTokenize("1e");
    testInvalidTokenize("1e+");
    testInvalidTokenize("1e-");
    testInvalidTokenize("1e++1");
    testInvalidTokenize("1e--1");
    testInvalidTokenize("1e+-1");
    testInvalidTokenize("1..0");
    testInvalidTokenize("1.0.0");
    testInvalidTokenize("1.01.0");
    testInvalidTokenize("1.0.0e1");

    // Valid strings
    testValidString("\"abc\"");
    testValidString("\"\\t\\r\\n\\f\\v\\\"\"");
    testValidString("\"\\\\\"");

    // Invalid strings
    testInvalidTokenize("\"abc");
    testInvalidTokenize("abc\""); // Name followed by quotes
    testInvalidTokenize("\"\\\"");
    testInvalidTokenize("\"\\");

    // Parenthesized list
    testTokenize("(var, 123, 1.0, \"abc\", true)", //
                 (struct Token[]){
                     MAKE_SINGLE(kTokenParenL),
                     MAKE_NAME("var"),
                     MAKE_SINGLE(kTokenComma),
                     MAKE_INT("123"),
                     MAKE_SINGLE(kTokenComma),
                     MAKE_REAL("1.0"),
                     MAKE_SINGLE(kTokenComma),
                     MAKE_STRING("\"abc\""),
                     MAKE_SINGLE(kTokenComma),
                     MAKE_NAME("true"),
                     MAKE_SINGLE(kTokenParenR),
                     MAKE_END(),
                 });

    testValidTokenize("const a = 1;");
    testValidTokenize("let a = 1;");
    testValidTokenize("return a;");
    testValidTokenize("continue;");
    testValidTokenize("break;");
    testValidTokenize("if a{return 1;}else{return 2;}");

    freeLexer(&sLex);
    return 0;
}
