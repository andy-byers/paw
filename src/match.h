#ifndef PAW_MATCH_H
#define PAW_MATCH_H

#include "ast.h"

typedef struct AstMatchVar {
    EXPR_HEADER;
    AstIdent *ident;
} AstMatchVar;

typedef struct AstMatchVariant {
    EXPR_HEADER;
    AstIdent *ident;
} AstMatchVariant;

typedef struct AstMatchEnum {
    EXPR_HEADER;
    AstIdent *ident;
} AstMatchEnum;

typedef struct AstMatchField {
    EXPR_HEADER;
    AstIdent *name;
    AstMatchVar *var;
} AstMatchField;

typedef struct AstMatchStruct {
    EXPR_HEADER;
    AstMatchField *fields;
} AstMatchStruct;

typedef struct AstMatchExpr {
    EXPR_HEADER;
    AstExpr *var;
    AstExprList *arms;
} AstMatchExpr;

#endif // PAW_MATCH_H
