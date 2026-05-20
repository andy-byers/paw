#ifndef PAW_UNIFY_H
#define PAW_UNIFY_H

#include "ir_type.h"

struct Unifier;
struct IrType;
struct IrTypeList;

typedef struct UnificationTable UnificationTable;
typedef int (*Unify)(struct Unifier *, struct IrType *, struct IrType *);
typedef int (*UnifyTrait)(struct Compiler *, struct IrTrait *, struct IrTrait *);

struct Unifier {
    UnificationTable *table;
    Str const *modname;
    struct Compiler *C;
    int depth;
};

struct IrType *pawU_normalize(struct Unifier *U, struct IrType *a);
struct IrType *pawU_normalize_projections(struct Unifier *U, struct IrType *type);
struct IrConst *pawU_normalize_const(struct Unifier *U, IrConst *k);

// Impose the constraint that types 'a' and 'b' are equal
int pawU_unify(struct Unifier *U, struct IrType *a, struct IrType *b);

// Create a new type variable
struct IrType *pawU_new_unknown(struct Unifier *U, struct SourceSpan span);

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
