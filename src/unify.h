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
    Str const *modname;
    struct Compiler *C;
    int depth;
};

struct IrType *pawU_normalize(struct Unifier *U, struct IrType *a);

struct IrObligations *pawU_steal_obligations(struct Unifier *U);

// Check if 'a' and 'b' are equal without side effects (besides normalization)
paw_Bool pawU_equals(struct Unifier *U, struct IrType *a, struct IrType *b);

// Impose the constraint that types 'a' and 'b' are equal
int pawU_unify(struct Unifier *U, struct IrType *a, struct IrType *b);

// Create a new type variable
struct IrType *pawU_new_unknown(struct Unifier *U, struct SourceLoc loc, struct IrTypeList *bounds);

// TODO: get rid of this function and create lists of unknowns wherever they are needed and add source locations
struct IrTypeList *pawU_new_unknowns(struct Unifier *U, struct SourceLoc loc, struct IrTypeList *types);

// Inference context handling
void pawU_enter_binder(struct Unifier *U, Str const *modname);
void pawU_leave_binder(struct Unifier *U);

int pawU_current_position(struct Unifier *U);
void pawU_undo_unifications(struct Unifier *U, int position);
void pawU_discard_variables(struct Unifier *U);


static inline void pawU_unify_unchecked(struct Unifier *U, IrType *a, IrType *b)
{
    int const unused = pawU_unify(U, a, b);
    paw_assert(unused == 0); PAW_UNUSED(unused);
}

#endif // PAW_UNIFY_H
