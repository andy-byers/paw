#ifndef PAW_UNIFY_H
#define PAW_UNIFY_H

#include "lex.h"

typedef struct Unifier Unifier; // unification context
typedef struct UnificationTable UnificationTable; // unification table

struct Unifier {
    struct Ast *ast;
    struct Hir *hir;
    UnificationTable *table;
    struct Resolver *R;
    paw_Env *P;
    int depth;
};

struct HirType *pawU_normalize(UnificationTable *table, struct HirType *a);

// Impose the constraint that types 'a' and 'b' are equal
void pawU_unify(Unifier *U, struct HirType *a, struct HirType *b);

// Create a new type variable
struct HirType *pawU_new_unknown(Unifier *U);

// Inference context handling
void pawU_enter_binder(Unifier *U);
void pawU_leave_binder(Unifier *U);

#endif // PAW_UNIFY_H
