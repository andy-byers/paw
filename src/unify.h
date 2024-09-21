#ifndef PAW_UNIFY_H
#define PAW_UNIFY_H

#include "lex.h"

struct Unifier;
struct HirType;

typedef struct UnificationTable UnificationTable;
typedef int (*Unify)(struct Unifier *, struct HirType *, struct HirType *);

struct Unifier {
    struct Hir *hir;
    Unify action;
    UnificationTable *table;
    struct Compiler *C;
    int depth;
};

struct HirType *pawU_normalize(UnificationTable *table, struct HirType *a);

// Check if 'a' and 'b' are equal without side effects (besides normalization)
paw_Bool pawU_equals(struct Unifier *U, struct HirType *a, struct HirType *b);

// Impose the constraint that types 'a' and 'b' are equal
void pawU_unify(struct Unifier *U, struct HirType *a, struct HirType *b);

// Create a new type variable
struct HirType *pawU_new_unknown(struct Unifier *U);

// Inference context handling
void pawU_enter_binder(struct Unifier *U);
void pawU_leave_binder(struct Unifier *U);

#endif // PAW_UNIFY_H
