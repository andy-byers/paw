#ifndef PAW_UNIFY_H
#define PAW_UNIFY_H

#include "lex.h"

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

// Create a new type variable
struct IrType *pawU_new_unknown(struct Unifier *U, int line);

// Inference context handling
void pawU_enter_binder(struct Unifier *U);
void pawU_leave_binder(struct Unifier *U);

paw_Bool pawU_list_equals(struct Unifier *U, struct IrTypeList *lhs, struct IrTypeList *rhs);

#endif // PAW_UNIFY_H
