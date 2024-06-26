#ifndef PAW_UNIFY_H
#define PAW_UNIFY_H

#include "lex.h"
#include "type.h"

typedef struct AstType AstType; // type for unifier
typedef struct Unifier Unifier; // unification context
typedef struct UniTable UniTable; // unification table

typedef AstType *(*Unify)(Unifier *, AstType *, AstType *);

struct Unifier {
    UniTable *table;
    Unify unify;
    Lex *lex;
    int depth;
};

AstType *pawU_normalize(UniTable *table, AstType *a);

// Impose the constraint that type variables 'a' and 'b' are equal
void pawU_unify(Unifier *U, AstType *a, AstType *b);

// Create a new type variable
AstType *pawU_new_unknown(Unifier *U, DefId id);

// Generics context handling
void pawU_enter_binder(Unifier *U, UniTable *table);
UniTable *pawU_leave_binder(Unifier *U);

// TODO: Don't leak unification tables! Being lazy right now

#endif // PAW_UNIFY_H
