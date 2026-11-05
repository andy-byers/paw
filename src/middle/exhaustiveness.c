// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// exhaustiveness.c: Exhaustiveness check for pattern matching, based off of
//     https://github.com/yorickpeterse/pattern-matching-in-rust.

#include "match.h"

#include "common/map.h"
#include "compiler/compiler.h"
#include "compiler/error.h"
#include "hir/hir.h"
#include "ir/type.h"

#define USEFULNESS_ERROR(U_, Kind_, ...) THROW_ERROR((U_)->C, \
        Kind_, .modname = (U_)->modname, __VA_ARGS__)

struct Usefulness {
    struct Allocator a;
    struct SourceSpan span;
    struct MatchVars *vars;
    struct Compiler *C;
    struct Pool *pool;
    struct Hir *hir;
    Str const *modname;
    paw_Env *P;
    int var_id;
    int line;
};

struct Column {
    struct MatchVar var;
    struct HirPat *pat;
};

struct Row {
    struct ColumnList *columns;
    struct HirExpr *guard;
    struct MatchBody body;
};

struct RawCase {
    struct Constructor cons;
    struct MatchVars *vars;
    struct RowList *rows;
};

DEFINE_LIST(RawCaseList, struct RawCase)
DEFINE_LIST(ColumnList, struct Column)
DEFINE_LIST(RowList, struct Row)

static struct Decision *new_decision(struct Usefulness *U, enum DecisionKind kind)
{
    struct Decision *result = compiler_alloc(U->C, NULL, 0, sizeof(struct Decision));
    *result = (struct Decision){
        .kind = kind,
    };
    return result;
}

static struct Decision *new_failure(struct Usefulness *U)
{
    return new_decision(U, DECISION_FAILURE);
}

static struct Decision *new_success(struct Usefulness *U, struct MatchBody body)
{
    struct Decision *result = new_decision(U, DECISION_SUCCESS);
    result->success.body = body;
    return result;
}

static struct Decision *new_guard(struct Usefulness *U, struct HirExpr *cond, struct MatchBody body, struct Decision *rest)
{
    struct Decision *result = new_decision(U, DECISION_GUARD);
    result->guard.cond = cond;
    result->guard.body = body;
    result->guard.rest = rest;
    return result;
}

static struct Decision *new_multi(struct Usefulness *U, struct MatchVar test, struct CaseList *cases, struct Decision *rest)
{
    struct Decision *result = new_decision(U, DECISION_MULTIWAY);
    result->multi.test = test;
    result->multi.cases = cases;
    result->multi.rest = rest;
    return result;
}

static int constructor_index(struct Constructor cons)
{
    if (cons.kind != CONS_VARIANT)
        return 0;
    return cons.variant.index;
}

static struct MatchVar new_variable(struct Usefulness *U, struct SourceSpan span, IrType *type)
{
    MatchVars_push(U->vars, (struct MatchVar){
        .id = U->var_id++,
        .type = type,
        .span = span,
    });
    return MatchVars_last(U->vars);
}

static struct MatchBody new_body(struct Usefulness *U, struct HirExpr *result)
{
    return (struct MatchBody){
        .bindings = BindingList_new(ALLOCATOR_OF(U)),
        .result = result,
    };
}

static void extend_row_list(struct Usefulness *U, struct RowList *target, struct RowList *rows)
{
    struct Row const *prow;
    RowList_reserve(target, rows->count);
    LIST_FOREACH (rows, prow) {
        RowList_push(target, *prow);
    }
}

static struct MatchBody copy_body(struct Usefulness *U, struct MatchBody body)
{
    struct BindingList *bindings = BindingList_new(ALLOCATOR_OF(U));
    BindingList_reserve(bindings, body.bindings->count);

    struct Binding const *pb;
    LIST_FOREACH (body.bindings, pb) {
        BindingList_push(bindings, *pb);
    }

    return (struct MatchBody){
        .bindings = bindings,
        .result = body.result,
    };
}

static struct Row copy_row(struct Usefulness *U, struct Row row)
{
    struct ColumnList *cols = ColumnList_new(ALLOCATOR_OF(U));
    ColumnList_reserve(cols, row.columns->count);

    struct Column const *pcol;
    LIST_FOREACH (row.columns, pcol) {
        ColumnList_push(cols, *pcol);
    }

    return (struct Row){
        .columns = cols,
        .guard = row.guard,
        .body = copy_body(U, row.body),
    };
}

static struct HirPatList *join_lists(struct Usefulness *U, struct HirPatList *lhs, struct HirPatList *rhs)
{
    struct HirPat *const *ppat;
    LIST_FOREACH (rhs, ppat) {
        HirPatList_push(lhs, *ppat);
    }
    return lhs;
}

static struct Row remove_first_row(struct RowList *rows)
{
    return RowList_remove(rows, 0);
}

static int find_or_pattern(struct Row row)
{
    int index;
    struct Column const *pcol;
    LIST_ENUMERATE (row.columns, index, pcol) {
        if (HirIsOrPat(pcol->pat))
            return index;
    }
    return -1;
}

static paw_Bool contains_or_pattern(struct RowList const *rows)
{
    struct Row const *prow;
    LIST_FOREACH (rows, prow) {
        if (find_or_pattern(*prow) >= 0)
            return PAW_TRUE;
    }
    return PAW_FALSE;
}

// Translated from @yorickpeterse/pattern-matching-in-rust
// OR patterns need to be expanded here, so that each alternative gets its own copy of the
// row. Each alternative may bind variables in different positions in the pattern, so they
// need to be handled separately.
static void expand_or_patterns(struct Usefulness *U, struct RowList *rows)
{
    if (!contains_or_pattern(rows))
        return;

    struct RowList *new_rows = RowList_new(ALLOCATOR_OF(U));
    paw_Bool found = PAW_TRUE;

    while (found) {
        found = PAW_FALSE;

        while (rows->count > 0) {
            struct Row const row = remove_first_row(rows);
            int const icol = find_or_pattern(row);
            if (icol >= 0) {
                found = PAW_TRUE;

                struct Column const col = ColumnList_get(row.columns, icol);
                struct HirOrPat const *or = HirGetOrPat(col.pat);

                struct HirPat *const *ppat;
                LIST_FOREACH (or->pats, ppat) {
                    struct Row const new_row = copy_row(U, row);
                    ColumnList_set(new_row.columns, icol, (struct Column){
                        .var = col.var,
                        .pat = *ppat,
                    });
                    RowList_push(new_rows, new_row);
                }
            } else {
                RowList_push(new_rows, row);
            }
        }

        struct RowList const temp = *rows;
        *rows = *new_rows;
        *new_rows = temp;
    }
}

static struct MatchVars *variables_for_types(struct Usefulness *U, struct SourceSpan span, IrTypeList *types)
{
    struct MatchVars *result = MatchVars_new(ALLOCATOR_OF(U));
    if (types == NULL) return result;

    IrType *const *ptype;
    LIST_FOREACH (types, ptype) {
        struct MatchVar var = new_variable(U, span, *ptype);
        MatchVars_push(result, var);
    }
    return result;
}

static int auto_deref_pat(struct HirPat **ppat)
{
    int deref = 0;
    for (; HirIsDerefPat(*ppat); ++deref)
        *ppat = HirGetDerefPat(*ppat)->pointee;
    return deref;
}

static void move_bindings_to_right(struct Usefulness *U, struct Row *row)
{
    // filter list of columns while building list of variable declarations
    struct ColumnList *columns = ColumnList_new(ALLOCATOR_OF(U));
    struct Column *pcol;
    LIST_FOREACH (row->columns, pcol) {
        if (HirIsDerefPat(pcol->pat)) {
            struct HirPat *pointee = pcol->pat;
            pcol->var.deref = auto_deref_pat(&pointee);
            if (HirIsBindingPat(pointee)) {
                struct HirBindingPat const *p = HirGetBindingPat(pointee);
                BindingList_push(row->body.bindings, (struct Binding){
                    .name = p->ident.name,
                    .var = pcol->var,
                    .ref = p->is_ref,
                    .id = p->id,
                });
            } else if (!HirIsWildcardPat(pointee)) {
                ColumnList_push(columns, *pcol);
            }
        } else if (HirIsBindingPat(pcol->pat)) {
            struct HirBindingPat const *p = HirGetBindingPat(pcol->pat);
            BindingList_push(row->body.bindings, (struct Binding){
                .name = p->ident.name,
                .var = pcol->var,
                .ref = p->is_ref,
                .id = p->id,
            });
        } else if (!HirIsWildcardPat(pcol->pat)) {
            ColumnList_push(columns, *pcol);
        }
    }
    row->columns = columns;
}

static paw_Bool remove_column(struct Row row, struct MatchVar var, struct Column *presult)
{
    int index;
    struct Column *pcol;
    struct ColumnList *cols = row.columns;
    LIST_ENUMERATE (row.columns, index, pcol) {
        if (pcol->var.id == var.id) {
            *presult = ColumnList_remove(cols, index);
            return PAW_FALSE;
        }
    }
    return PAW_TRUE;
}

static struct Decision *compile_rows(struct Usefulness *U, struct RowList *rows);

static struct CaseList *compile_cases(struct Usefulness *U, struct RawCaseList *raw_cases)
{
    struct RawCase *prc;
    struct CaseList *result = CaseList_new(ALLOCATOR_OF(U));
    LIST_FOREACH(raw_cases, prc) {
        struct Decision *dec = compile_rows(U, prc->rows);
        CaseList_push(result, (struct MatchCase){
            .cons = prc->cons,
            .vars = prc->vars,
            .dec = dec,
        });
    }
    return result;
}

static struct HirPatList *struct_pat_fields(struct Usefulness *U, struct HirStructPat *p)
{
    struct HirPat *const *pfield;
    struct HirPatList *fields = HirPatList_new(ALLOCATOR_OF(U->hir));
    LIST_FOREACH(p->fields, pfield) {
        struct HirFieldPat *field = HirGetFieldPat(*pfield);
        HirPatList_push(fields, field->pat);
    }
    return fields;
}

static struct CaseList *compile_constructor_cases(struct Usefulness *U, struct RowList *rows, struct MatchVar branch_var, struct RawCaseList *cases)
{
    struct Row const *prow;
    LIST_FOREACH (rows, prow) {
        struct Column col;
        if (remove_column(*prow, branch_var, &col)) {
            struct RawCase *prc;
            LIST_FOREACH (cases, prc) {
                RowList_push(prc->rows, copy_row(U, *prow));
            }
            continue;
        }

        struct HirPat *pat = col.pat;
        struct Row const r = copy_row(U, *prow);
        col.var.deref = auto_deref_pat(&pat);

        // pattern matrix specialization
        int index = 0;
        HirPatList *fields;
        if (HirIsVariantPat(pat)) {
            fields = HirGetVariantPat(pat)->fields;
            index = HirGetVariantPat(pat)->index;
        } else if (HirIsStructPat(pat)) {
            fields = struct_pat_fields(U, HirGetStructPat(pat));
        } else if (HirIsTuplePat(pat)) {
            fields = HirGetTuplePat(pat)->elems;
        } else {
            struct HirExpr *expr = HirGetLiteralPat(pat)->expr;
            index = HirGetLiteralExpr(expr)->b;
            fields = &(HirPatList){0};
        }

        struct RawCase *rc = &LIST_AT(cases, index);
        paw_assert(rc->vars->count == fields->count);
        struct MatchVar const *pv;
        struct HirPat *const *pp;
        LIST_ZIP (fields, pp, rc->vars, pv) {
            ColumnList_push(r.columns, (struct Column){
                .var = *pv,
                .pat = *pp,
            });
        }
        RowList_push(rc->rows, r);
    }

    return compile_cases(U, cases);
}

struct LiteralResult {
    struct CaseList *cases;
    struct Decision *fallback;
};

DEFINE_MAP(CaseMap, P_VALUE_HASH, P_VALUE_EQUALS, IrValue, int)

static struct LiteralResult compile_literal_cases(struct Usefulness *U, struct RowList *rows, struct MatchVar branch_var)
{
    CaseMap *tested = CaseMap_new(ALLOCATOR_OF(U));
    struct RawCaseList *raw_cases = RawCaseList_new(ALLOCATOR_OF(U));
    struct RowList *fallback = RowList_new(ALLOCATOR_OF(U));

    struct Row const *prow;
    LIST_FOREACH (rows, prow) {
        struct Column col;
        if (remove_column(*prow, branch_var, &col)) {
            // This row had a wildcard or binding in place of 'branch_var', meaning
            // it needs to be considered in all other cases.
            RowList_push(fallback, copy_row(U, *prow));
            struct RawCase const *prc;
            LIST_FOREACH (raw_cases, prc) {
                RowList_push(prc->rows, copy_row(U, *prow));
            }
            continue;
        }

        struct HirPat *pat = col.pat;
        struct Row const r = copy_row(U, *prow);
        col.var.deref = auto_deref_pat(&pat);

        struct HirLiteralPat *p = HirGetLiteralPat(pat);
        struct HirLiteralExpr *e = HirGetLiteralExpr(p->expr);
        struct Constructor cons = {0};

        switch (e->lit_kind) {
            case HIR_LIT_TUPLE:
                paw_assert(e->tuple.elems->count == 0);
                cons.kind = CONS_TUPLE;
                cons.tuple.elems = IrTypeList_new(ALLOCATOR_OF(U));
                break;
            case HIR_LIT_BOOL:
                cons.kind = CONS_BOOL;
                cons.value.b = e->b;
                break;
            case HIR_LIT_CHAR:
                cons.kind = CONS_CHAR;
                cons.value.c = e->c;
                break;
            case HIR_LIT_INT:
                cons.kind = CONS_INT;
                cons.value.i64 = e->i.value;
                break;
            case HIR_LIT_FLOAT:
                // normalize float values
                if (e->f.value == 0.0)
                    e->f.value = 0.0;
                cons.kind = CONS_FLOAT;
                cons.value.f64 = e->f.value;
                break;
            default:
                paw_assert(e->lit_kind == HIR_LIT_STR);
                cons.kind = CONS_STR;
                cons.value.s = e->s;
                break;
        }

        IrValue const key = cons.value;
        int *pindex = CaseMap_get(tested, key);
        if (pindex != NULL) {
            struct RawCase rc = RawCaseList_get(raw_cases, *pindex);
            RowList_push(rc.rows, r);
            continue;
        }
        CaseMap_insert(tested, key, raw_cases->count);
        struct RowList *rows = RowList_new(ALLOCATOR_OF(U));
        extend_row_list(U, rows, fallback);
        RowList_push(rows, r);
        RawCaseList_push(raw_cases, (struct RawCase){
            .vars = MatchVars_new(ALLOCATOR_OF(U)),
            .cons = cons,
            .rows = rows,
        });
    }

    CaseMap_delete(tested);
    return (struct LiteralResult){
        .cases = compile_cases(U, raw_cases),
        .fallback = compile_rows(U, fallback),
    };
}

// TODO: choose a better branch column
static struct Column find_branch_col(struct Usefulness *U, struct ColumnList *cols)
{
    PAW_UNUSED(U);
    return ColumnList_first(cols);
}

static IrTypeList *collect_field_types(struct Usefulness *U, struct HirDecl *decl, IrType *type)
{
    struct HirAdtDecl *d = HirGetAdtDecl(decl);
    IrTypeList *result = IrTypeList_new(ALLOCATOR_OF(U));
    struct HirVariantDecl *v = HirGetVariantDecl(LIST_FIRST(d->variants));
    IrTypeList_reserve(result, v->fields->count);

    struct HirDecl *const *pfield;
    LIST_FOREACH (v->fields, pfield) {
        IrType *next = GET_NODE_TYPE(U->C, *pfield);
        next = instantiate_struct_field(U->C, type, next);
        IrTypeList_push(result, next);
    }
    return result;
}

struct RawCaseList *cases_for_struct(struct Usefulness *U, struct MatchVar var)
{
    IrType *struct_type = ir_remove_indirection(U->C, var.type);
    struct HirDecl *decl = hir_get_decl(U->hir, IR_TYPE_DID(struct_type));

    IrTypeList *fields = collect_field_types(U, decl, struct_type);
    struct MatchVars *subvars = variables_for_types(U, var.span, fields);
    struct Constructor cons = {
        .kind = CONS_STRUCT,
        .struct_.type = struct_type,
    };

    struct RawCaseList *result = RawCaseList_new(ALLOCATOR_OF(U));
    RawCaseList_push(result, (struct RawCase){
        .rows = RowList_new(ALLOCATOR_OF(U)),
        .vars = subvars,
        .cons = cons,
    });
    return result;
}

struct RawCaseList *cases_for_tuple(struct Usefulness *U, struct MatchVar var)
{
    IrType *tuple_type = ir_remove_indirection(U->C, var.type);
    struct RawCaseList *result = RawCaseList_new(ALLOCATOR_OF(U));
    IrTypeList *elems = IrIsTuple(tuple_type)
        ? IrGetTuple(tuple_type)->elems // tuple
        : IrTypeList_new(ALLOCATOR_OF(U->C)); // unit
    struct MatchVars *subvars = variables_for_types(U, var.span, elems);
    struct Constructor cons = {
        .kind = CONS_TUPLE,
        .tuple.elems = elems,
    };

    RawCaseList_push(result, (struct RawCase){
        .rows = RowList_new(ALLOCATOR_OF(U)),
        .vars = subvars,
        .cons = cons,
    });
    return result;
}

struct RawCaseList *cases_for_variant(struct Usefulness *U, struct MatchVar var)
{
    IrType *adt_type = ir_remove_indirection(U->C, var.type);
    struct HirDecl *decl = hir_get_decl(U->hir, IR_TYPE_DID(adt_type));
    struct HirDeclList *variants = HirGetAdtDecl(decl)->variants;
    paw_assert(!HirGetAdtDecl(decl)->is_struct);

    struct RawCaseList *result = RawCaseList_new(ALLOCATOR_OF(U));
    RawCaseList_reserve(result, variants->count);

    int index;
    struct HirDecl *const *pvariant;
    LIST_ENUMERATE (variants, index, pvariant) {
        IrType *type = GET_NODE_TYPE(U->C, *pvariant);
        type = instantiate_struct_field(U->C, adt_type, type);
        struct MatchVars *subvars = variables_for_types(U,
                var.span, ir_fn_params(U->C, type));
        struct Constructor const cons = {
            .kind = CONS_VARIANT,
            .variant.type = type,
            .variant.index = index,
        };

        RawCaseList_push(result, (struct RawCase){
            .rows = RowList_new(ALLOCATOR_OF(U)),
            .vars = subvars,
            .cons = cons,
        });
    }
    return result;
}

enum BranchMode {
    BRANCH_VARIANT,
    BRANCH_STRUCT,
    BRANCH_TUPLE,
    BRANCH_LITERAL,
};

static enum BranchMode branch_mode(struct Usefulness *U, IrType *type)
{
    type = ir_remove_indirection(U->C, type);
    if (IrIsTuple(type) || IrIsUnit(type))
        return BRANCH_TUPLE;
    enum BuiltinKind const code = pawP_type2code(U->C, type);
    if (IS_BASIC_TYPE(code))
        return BRANCH_LITERAL;
    struct HirDecl *decl = hir_get_decl(U->hir, IR_TYPE_DID(type));
    if (!HirGetAdtDecl(decl)->is_struct)
        return BRANCH_VARIANT;
    return BRANCH_STRUCT;
}

static struct RawCase bool_case(struct Usefulness *U, struct SourceSpan span, paw_Bool b)
{
    return (struct RawCase){
        .cons = (struct Constructor){.kind = CONS_BOOL, .value.b = b},
        .vars = variables_for_types(U, span, NULL),
        .rows = RowList_new(ALLOCATOR_OF(U)),
    };
}

static struct Decision *compile_rows(struct Usefulness *U, struct RowList *rows)
{
    if (rows->count == 0)
        USEFULNESS_ERROR(U, NonexhaustivePatternMatch, U->span);

    expand_or_patterns(U, rows);
    for (int i = 0; i < rows->count; ++i) {
        move_bindings_to_right(U, &LIST_AT(rows, i));
    }
    struct Row first_row = RowList_first(rows);
    if (first_row.columns->count == 0) {
        remove_first_row(rows);
        struct Row const row = copy_row(U, first_row);
        if (row.guard == NULL)
            return new_success(U, row.body);
        struct Decision *rest = compile_rows(U, rows);
        return new_guard(U, row.guard, row.body, rest);
    }

    struct Column branch_col = find_branch_col(U, first_row.columns);
    switch (branch_mode(U, branch_col.var.type)) {
        case BRANCH_VARIANT: {
            RawCaseList *raw_cases = cases_for_variant(U, branch_col.var);
            CaseList *cases = compile_constructor_cases(U, rows, branch_col.var, raw_cases);
            return new_multi(U, branch_col.var, cases, NULL);
        }
        case BRANCH_STRUCT: {
            RawCaseList *raw_cases = cases_for_struct(U, branch_col.var);
            CaseList *cases = compile_constructor_cases(U, rows, branch_col.var, raw_cases);
            return new_multi(U, branch_col.var, cases, NULL);
        }
        case BRANCH_TUPLE: {
            RawCaseList *raw_cases = cases_for_tuple(U, branch_col.var);
            CaseList *cases = compile_constructor_cases(U, rows, branch_col.var, raw_cases);
            return new_multi(U, branch_col.var, cases, NULL);
        }
        case BRANCH_LITERAL: {
            if (pawP_type2code(U->C, branch_col.var.type) == BUILTIN_BOOL) {
                RawCaseList *raw_cases = RawCaseList_new(ALLOCATOR_OF(U));
                RawCaseList_push(raw_cases, bool_case(U, branch_col.var.span, PAW_FALSE));
                RawCaseList_push(raw_cases, bool_case(U, branch_col.var.span, PAW_TRUE));
                CaseList *cases = compile_constructor_cases(U, rows, branch_col.var, raw_cases);
                return new_multi(U, branch_col.var, cases, NULL);
            } else {
                struct LiteralResult result = compile_literal_cases(U, rows, branch_col.var);
                return new_multi(U, branch_col.var, result.cases, result.fallback);
            }
        }
    }
}

struct Decision *pawP_check_exhaustiveness(struct Hir *hir, struct Pool *pool, Str const *modname, struct HirMatchExpr *match, struct MatchVars *vars)
{
    struct Compiler *C = hir->C;

    struct Usefulness U = {
        .a = POOL_ALLOCATOR(pool),
        .modname = modname,
        .span = match->span,
        .pool = pool,
        .vars = vars,
        .hir = hir,
        .P = ENV(C),
        .C = C,
    };

    struct RowList *rows = RowList_new(ALLOCATOR_OF(&U));
    struct SourceSpan const span = match->target->hdr.span;
    struct MatchVar const var = new_variable(&U, span, GET_NODE_TYPE(C, match->target));

    struct HirExpr *const *parm;
    LIST_FOREACH (match->arms, parm) {
        struct HirMatchArm const *arm = HirGetMatchArm(*parm);
        struct ColumnList *cols = ColumnList_new(ALLOCATOR_OF(&U));
        ColumnList_push(cols, (struct Column){
            .var = var,
            .pat = arm->pat,
        });
        RowList_push(rows, (struct Row){
            .body = new_body(&U, arm->result),
            .guard = arm->guard,
            .columns = cols,
        });
    }

    return compile_rows(&U, rows);
}
