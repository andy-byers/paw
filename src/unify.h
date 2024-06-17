#ifndef PAW_UNIFY_H
#define PAW_UNIFY_H

#include "lex.h"
#include "type.h"

typedef struct Type Type; // type for unifier
typedef struct Unifier Unifier; // unification context
typedef struct UniTable UniTable; // unification table

typedef Type *(*Unify)(Unifier *, Type *, Type *);

struct Unifier {
    UniTable *table;
    Unify unify;
    Lex *lex;
    int depth;
};

Type *pawU_normalize(UniTable *table, Type *a);

// Impose the constraint that type variables 'a' and 'b' are equal
void pawU_unify(Unifier *U, Type *a, Type *b);

// Create a new type variable
Type *pawU_new_unknown(Unifier *U, DefId id);

// Generics context handling
void pawU_enter_binder(Unifier *U, UniTable *table);
UniTable *pawU_leave_binder(Unifier *U);

// TODO: Don't leak unification tables! Being lazy right now

#endif // PAW_UNIFY_H
