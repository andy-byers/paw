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

struct IrType *pawU_normalize(UnificationTable *table, struct IrType *a);

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

// Return true if 'a' is more generic than or equal to 'b', false otherwise
paw_Bool pawU_is_compat(struct Unifier *U, struct IrType *a, struct IrType *b);

#endif // PAW_UNIFY_H
