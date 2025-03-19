#ifndef PAW_UNIFY_H
#define PAW_UNIFY_H

#include "ir_type.h"

struct Unifier;
struct IrType;
struct IrTypeList;

typedef struct UnificationTable UnificationTable;
typedef int (*Unify)(struct Unifier *, struct IrType *, struct IrType *);

struct Unifier {
    Unify action;
    UnificationTable *table;
    struct Compiler *C;
    int depth;
};

struct IrType *pawU_normalize(UnificationTable *table, struct IrType *a);

// Check if 'a' and 'b' are equal without side effects (besides normalization)
paw_Bool pawU_equals(struct Unifier *U, struct IrType *a, struct IrType *b);

// Impose the constraint that types 'a' and 'b' are equal
void pawU_unify(struct Unifier *U, struct IrType *a, struct IrType *b);

static void pawU_unify_lists(struct Unifier *U, struct IrTypeList *a, struct IrTypeList *b)
{
    struct IrType **pa, **pb;
    K_LIST_ZIP (a, pa, b, pb) {
        pawU_unify(U, *pa, *pb);
    }
}

// Create a new type variable
struct IrType *pawU_new_unknown(struct Unifier *U, struct IrTypeList *bounds);
struct IrTypeList *pawU_new_unknowns(struct Unifier *U, struct IrTypeList *types);

// Inference context handling
void pawU_enter_binder(struct Unifier *U);
void pawU_leave_binder(struct Unifier *U);

paw_Bool pawU_list_equals(struct Unifier *U, struct IrTypeList *lhs, struct IrTypeList *rhs);

// Return true if 'a' is more generic than or equal to 'b', false otherwise
paw_Bool pawU_is_compat(struct Unifier *U, struct IrType *a, struct IrType *b);

#endif // PAW_UNIFY_H
