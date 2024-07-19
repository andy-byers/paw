#ifndef PAW_UNIFY_H
#define PAW_UNIFY_H

#include "lex.h"
#include "type.h"

typedef struct AstType AstType; // type for unifier
typedef struct Unifier Unifier; // unification context
typedef struct UnificationTable UnificationTable; // unification table

struct Unifier {
    struct Ast *ast;
    UnificationTable *table;
    Lex *lex;
    int depth;
};

AstType *pawU_normalize(UnificationTable *table, AstType *a);

// Impose the constraint that types 'a' and 'b' are equal
void pawU_unify(Unifier *U, AstType *a, AstType *b);

// Create a new type variable
AstType *pawU_new_unknown(Unifier *U);

// Inference context handling
void pawU_enter_binder(Unifier *U);
void pawU_leave_binder(Unifier *U);

#endif // PAW_UNIFY_H
