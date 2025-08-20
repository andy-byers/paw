// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// exhaustiveness.c: Exhaustiveness check for pattern matching, based off of
//     https://github.com/yorickpeterse/pattern-matching-in-rust.

#include "compile.h"
#include "error.h"
#include "hir.h"
#include "ir_type.h"
#include "map.h"
#include "match.h"

#define USEFULNESS_ERROR(U_, Kind_, ...) pawErr_##Kind_((U_)->C, (U_)->modname, __VA_ARGS__)

struct Usefulness {
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

DEFINE_LIST(struct Usefulness, RawCaseList, struct RawCase)
DEFINE_LIST(struct Usefulness, ColumnList, struct Column)
DEFINE_LIST(struct Usefulness, RowList, struct Row)

static struct Decision *new_decision(struct Usefulness *U, enum DecisionKind kind)
{
    struct Decision *result = P_ALLOC(U->C, NULL, 0, sizeof(struct Decision));
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
    MatchVars_push(U->C, U->vars, (struct MatchVar){
        .id = U->var_id++,
        .type = type,
        .span = span,
    });
    return MatchVars_last(U->vars);
}

static struct MatchBody new_body(struct Usefulness *U, struct HirExpr *result)
{
    return (struct MatchBody){
        .bindings = BindingList_new_from(U->C, U->pool),
        .result = result,
    };
}

static void extend_row_list(struct Usefulness *U, struct RowList *target, struct RowList *rows)
{
    struct Row const *prow;
    RowList_reserve(U, target, rows->count);
    K_LIST_FOREACH (rows, prow) {
        RowList_push(U, target, *prow);
    }
}

static struct MatchBody copy_body(struct Usefulness *U, struct MatchBody body)
{
    struct BindingList *bindings = BindingList_new_from(U->C, U->pool);
    BindingList_reserve(U->C, bindings, body.bindings->count);

    struct Binding const *pb;
    K_LIST_FOREACH (body.bindings, pb) {
        BindingList_push(U->C, bindings, *pb);
    }

    return (struct MatchBody){
        .bindings = bindings,
        .result = body.result,
    };
}

static struct Row copy_row(struct Usefulness *U, struct Row row)
{
    struct ColumnList *cols = ColumnList_new(U);
    ColumnList_reserve(U, cols, row.columns->count);

    struct Column const *pcol;
    K_LIST_FOREACH (row.columns, pcol) {
        ColumnList_push(U, cols, *pcol);
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
    K_LIST_FOREACH (rhs, ppat) {
        HirPatList_push(U->hir, lhs, *ppat);
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
    K_LIST_ENUMERATE (row.columns, index, pcol) {
        if (HirIsOrPat(pcol->pat))
            return index;
    }
    return -1;
}

static paw_Bool contains_or_pattern(struct RowList const *rows)
{
    struct Row const *prow;
    K_LIST_FOREACH (rows, prow) {
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

    struct RowList *new_rows = RowList_new(U);
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
                K_LIST_FOREACH (or->pats, ppat) {
                    struct Row const new_row = copy_row(U, row);
                    ColumnList_set(new_row.columns, icol, (struct Column){
                        .var = col.var,
                        .pat = *ppat,
                    });
                    RowList_push(U, new_rows, new_row);
                }
            } else {
                RowList_push(U, new_rows, row);
            }
        }

        struct RowList const temp = *rows;
        *rows = *new_rows;
        *new_rows = temp;
    }
}

static struct MatchVars *variables_for_types(struct Usefulness *U, struct SourceSpan span, IrTypeList *types)
{
    struct MatchVars *result = MatchVars_new_from(U->C, U->pool);
    if (types == NULL)
        return result;

    IrType *const *ptype;
    K_LIST_FOREACH (types, ptype) {
        struct MatchVar const var = new_variable(U, span, *ptype);
        MatchVars_push(U->C, result, var);
    }
    return result;
}

#if 0

#include <stdio.h>

static void print_path(struct Compiler*C,struct HirPath *path)
{
    pawHir_dump_path(C, path);
    printf("%s",paw_str(C->P,-1));
    paw_pop(C->P,1);
}

static void print_pat(struct Compiler*C,struct HirPat *pat);
static void print_pat_list(struct Compiler*C,struct HirPatList *pats)
{
    for (int i = 0; i < pats->count; ++i) {
        if (i > 0) printf(", ");
        print_pat(C, pats->data[i]);
    }
}

static void print_pat(struct Compiler*C,struct HirPat *pat)
{
    switch (HIR_KINDOF(pat)) {
        case kHirVariantPat:
            print_path(C,pat->variant.path);
            if (pat->variant.fields->count > 0) {
                printf("(");
                print_pat_list(C, pat->variant.fields);
                printf(")");
            }
            break;
        case kHirStructPat:
            print_path(C,pat->struct_.path);
            if (pat->struct_.fields->count > 0) {
                printf("{");
                print_pat_list(C, pat->struct_.fields);
                printf("}");
            }
            break;
        case kHirBindingPat:
            printf("%s", pat->bind.name->text);
            break;
        case kHirFieldPat:
            printf("%s: ", pat->field.name->text);
            print_pat(C, pat->field.pat);
            break;
        case kHirLiteralPat:
            if (HirIsLiteralExpr(pat->lit.expr)) {
                struct HirLiteralExpr *e = HirGetLiteralExpr(pat->lit.expr);
                if (e->lit_kind == kHirLitBasic && e->basic.t == BUILTIN_INT) {
                    printf("%lld", (long long)e->basic.value.i);
                    break;
                }
            }
            printf("<lit>");
            break;
        case kHirWildcardPat:
            printf("_");
            break;
        default:
            printf("?");
    }
}

static void print_col(struct Compiler*C,struct Column *col)
{
    print_pat(C, col->pat);
    printf("\n");
}

#endif // 0

static void move_bindings_to_right(struct Usefulness *U, struct Row *row)
{
    // filter list of columns while building list of variable declarations
    struct ColumnList *columns = ColumnList_new(U);
    struct Column const *pcol;
    K_LIST_FOREACH (row->columns, pcol) {
        if (HirIsBindingPat(pcol->pat)) {
            struct HirBindingPat const *p = HirGetBindingPat(pcol->pat);
            BindingList_push(U->C, row->body.bindings, (struct Binding){
                .name = p->ident.name,
                .var = pcol->var,
                .id = p->id,
            });
        } else if (!HirIsWildcardPat(pcol->pat)) {
            ColumnList_push(U, columns, *pcol);
        }
    }
    row->columns = columns;
}

static paw_Bool remove_column(struct Usefulness *U, struct Row row, struct MatchVar var, struct Column *presult)
{
    int index;
    struct Column *pcol;
    struct ColumnList *cols = row.columns;
    K_LIST_ENUMERATE (row.columns, index, pcol) {
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
    struct CaseList *result = CaseList_new_from(U->C, U->pool);
    K_LIST_FOREACH(raw_cases, prc) {
        struct Decision *dec = compile_rows(U, prc->rows);
        CaseList_push(U->C, result, (struct MatchCase){
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
    struct HirPatList *fields = HirPatList_new(U->hir);
    K_LIST_FOREACH(p->fields, pfield) {
        struct HirFieldPat *field = HirGetFieldPat(*pfield);
        HirPatList_push(U->hir, fields, field->pat);
    }
    return fields;
}

static struct CaseList *compile_constructor_cases(struct Usefulness *U, struct RowList *rows, struct MatchVar branch_var, struct RawCaseList *cases)
{
    struct Row const *prow;
    K_LIST_FOREACH (rows, prow) {
        struct Column col;
        if (remove_column(U, *prow, branch_var, &col)) {
            struct RawCase *prc;
            K_LIST_FOREACH (cases, prc) {
                RowList_push(U, prc->rows, copy_row(U, *prow));
            }
            continue;
        }

        struct HirPat *pat = col.pat;
        struct Row const r = copy_row(U, *prow);

        // pattern matrix specialization
        int index = 0;
        struct HirPatList *fields;
        if (HirIsVariantPat(pat)) {
            fields = HirGetVariantPat(pat)->fields;
            index = HirGetVariantPat(pat)->index;
        } else if (HirIsStructPat(pat)) {
            fields = struct_pat_fields(U, HirGetStructPat(pat));
        } else {
            fields = HirGetTuplePat(pat)->elems;
        }

        struct RawCase *rc = &K_LIST_AT(cases, index);
        paw_assert(rc->vars->count == fields->count);
        struct MatchVar const *pv;
        struct HirPat *const *pp;
        K_LIST_ZIP (fields, pp, rc->vars, pv) {
            ColumnList_push(U, r.columns, (struct Column){
                .var = *pv,
                .pat = *pp,
            });
        }
        RowList_push(U, rc->rows, r);
    }

    return compile_cases(U, cases);
}

struct LiteralResult {
    struct CaseList *cases;
    struct Decision *fallback;
};

DEFINE_MAP(struct Usefulness, CaseMap, pawP_alloc, P_VALUE_HASH, P_VALUE_EQUALS, Value, int)

static struct LiteralResult compile_literal_cases(struct Usefulness *U, struct RowList *rows, struct MatchVar branch_var)
{
    CaseMap *tested = CaseMap_new(U);
    struct RawCaseList *raw_cases = RawCaseList_new(U);
    struct RowList *fallback = RowList_new(U);

    struct Row const *prow;
    K_LIST_FOREACH (rows, prow) {
        struct Column col;
        if (remove_column(U, *prow, branch_var, &col)) {
            // This row had a wildcard or binding in place of 'branch_var', meaning
            // it needs to be considered in all other cases.
            RowList_push(U, fallback, copy_row(U, *prow));
            struct RawCase const *prc;
            K_LIST_FOREACH (raw_cases, prc) {
                RowList_push(U, prc->rows, copy_row(U, *prow));
            }
            continue;
        }

        struct HirPat *pat = col.pat;
        struct Row const r = copy_row(U, *prow);

        struct HirLiteralPat *p = HirGetLiteralPat(pat);
        struct HirLiteralExpr *e = HirGetLiteralExpr(p->expr);
        struct Constructor cons = {0};

        switch (e->basic.code) {
            case BUILTIN_UNIT:
                cons.kind = CONS_TUPLE;
                cons.tuple.elems = IrTypeList_new_from(U->C, U->pool);
                break;
            case BUILTIN_BOOL:
                cons.kind = CONS_BOOL;
                cons.value = e->basic.value;
                break;
            case BUILTIN_CHAR:
                cons.kind = CONS_CHAR;
                cons.value = e->basic.value;
                break;
            case BUILTIN_INT:
                cons.kind = CONS_INT;
                cons.value = e->basic.value;
                break;
            case BUILTIN_FLOAT:
                // normalize float keys
                if (V_FLOAT(e->basic.value) == 0.0)
                    V_SET_FLOAT(&e->basic.value, 0.0);
                cons.kind = CONS_FLOAT;
                cons.value = e->basic.value;
                break;
            default:
                paw_assert(e->basic.code == BUILTIN_STR);
                cons.kind = CONS_STR;
                cons.value = e->basic.value;
                break;
        }

        Value const key = e->basic.value;
        int *pindex = CaseMap_get(U, tested, key);
        if (pindex != NULL) {
            struct RawCase rc = RawCaseList_get(raw_cases, *pindex);
            RowList_push(U, rc.rows, r);
            continue;
        }
        CaseMap_insert(U, tested, key, raw_cases->count);
        struct RowList *rows = RowList_new(U);
        extend_row_list(U, rows, fallback);
        RowList_push(U, rows, r);
        RawCaseList_push(U, raw_cases, (struct RawCase){
            .vars = MatchVars_new_from(U->C, U->pool),
            .cons = cons,
            .rows = rows,
        });
    }

    CaseMap_delete(U, tested);
    return (struct LiteralResult){
        .cases = compile_cases(U, raw_cases),
        .fallback = compile_rows(U, fallback),
    };
}

// TODO: choose a better branch column
static struct Column find_branch_col(struct Usefulness *U, struct ColumnList *cols)
{
    return ColumnList_first(cols);
}

static IrTypeList *collect_field_types(struct Usefulness *U, struct HirDecl *decl, IrType *type)
{
    struct HirAdtDecl *d = HirGetAdtDecl(decl);
    IrTypeList *result = IrTypeList_new_from(U->C, U->pool);
    struct HirVariantDecl *v = HirGetVariantDecl(K_LIST_FIRST(d->variants));
    IrTypeList_reserve(U->C, result, v->fields->count);

    struct HirDecl *const *pfield;
    K_LIST_FOREACH (v->fields, pfield) {
        IrType *next = GET_NODE_TYPE(U->C, *pfield);
        next = pawP_instantiate_field(U->C, type, next);
        IrTypeList_push(U->C, result, next);
    }
    return result;
}

struct RawCaseList *cases_for_struct(struct Usefulness *U, struct MatchVar var)
{
    struct HirDecl *decl = pawHir_get_decl(U->hir, IR_TYPE_DID(var.type));

    IrTypeList *fields = collect_field_types(U, decl, var.type);
    struct MatchVars *subvars = variables_for_types(U, var.span, fields);
    struct Constructor cons = {
        .kind = CONS_STRUCT,
        .struct_.type = var.type,
    };

    struct RawCaseList *result = RawCaseList_new(U);
    RawCaseList_push(U, result, (struct RawCase){
        .rows = RowList_new(U),
        .vars = subvars,
        .cons = cons,
    });
    return result;
}

struct RawCaseList *cases_for_tuple(struct Usefulness *U, struct MatchVar var)
{
    struct RawCaseList *result = RawCaseList_new(U);
    IrTypeList *elems = IrIsTuple(var.type)
        ? IrGetTuple(var.type)->elems // tuple
        : IrTypeList_new(U->C); // unit
    struct MatchVars *subvars = variables_for_types(U, var.span, elems);
    struct Constructor cons = {
        .kind = CONS_TUPLE,
        .tuple.elems = elems,
    };

    RawCaseList_push(U, result, (struct RawCase){
        .rows = RowList_new(U),
        .vars = subvars,
        .cons = cons,
    });
    return result;
}

struct RawCaseList *cases_for_variant(struct Usefulness *U, struct MatchVar var)
{
    struct HirDecl *decl = pawHir_get_decl(U->hir, IR_TYPE_DID(var.type));
    struct HirDeclList *variants = HirGetAdtDecl(decl)->variants;
    paw_assert(!HirGetAdtDecl(decl)->is_struct);

    struct RawCaseList *result = RawCaseList_new(U);
    RawCaseList_reserve(U, result, variants->count);

    int index;
    struct HirDecl *const *pvariant;
    K_LIST_ENUMERATE (variants, index, pvariant) {
        IrType *type = GET_NODE_TYPE(U->C, *pvariant);
        type = pawP_instantiate_field(U->C, var.type, type);
        struct MatchVars *subvars = variables_for_types(U,
                var.span, IR_FPTR(type)->params);
        struct Constructor const cons = {
            .kind = CONS_VARIANT,
            .variant.type = type,
            .variant.index = index,
        };

        RawCaseList_push(U, result, (struct RawCase){
            .rows = RowList_new(U),
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

static enum BranchMode branch_mode(struct Usefulness *U, struct MatchVar var)
{
    enum BuiltinKind code = pawP_type2code(U->C, var.type);
    if (!IrIsAdt(var.type) || code == BUILTIN_UNIT)
        return BRANCH_TUPLE;
    if (IS_BASIC_TYPE(code))
        return BRANCH_LITERAL;
    struct HirDecl *decl = pawHir_get_decl(U->hir, IR_TYPE_DID(var.type));
    if (!HirGetAdtDecl(decl)->is_struct)
        return BRANCH_VARIANT;
    return BRANCH_STRUCT;
}

static struct Decision *compile_rows(struct Usefulness *U, struct RowList *rows)
{
    if (rows->count == 0)
        USEFULNESS_ERROR(U, nonexhaustive_pattern_match, U->span.start);

    expand_or_patterns(U, rows);
    for (int i = 0; i < rows->count; ++i) {
        move_bindings_to_right(U, &K_LIST_AT(rows, i));
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
    switch (branch_mode(U, branch_col.var)) {
        case BRANCH_VARIANT: {
            struct RawCaseList *raw_cases = cases_for_variant(U, branch_col.var);
            struct CaseList *cases = compile_constructor_cases(U, rows, branch_col.var, raw_cases);
            return new_multi(U, branch_col.var, cases, NULL);
        }
        case BRANCH_STRUCT: {
            struct RawCaseList *raw_cases = cases_for_struct(U, branch_col.var);
            struct CaseList *cases = compile_constructor_cases(U, rows, branch_col.var, raw_cases);
            return new_multi(U, branch_col.var, cases, NULL);
        }
        case BRANCH_TUPLE: {
            struct RawCaseList *raw_cases = cases_for_tuple(U, branch_col.var);
            struct CaseList *cases = compile_constructor_cases(U, rows, branch_col.var, raw_cases);
            return new_multi(U, branch_col.var, cases, NULL);
        }
        case BRANCH_LITERAL: {
            struct LiteralResult result = compile_literal_cases(U, rows, branch_col.var);
            return new_multi(U, branch_col.var, result.cases, result.fallback);
        }
    }
}

struct Decision *pawP_check_exhaustiveness(struct Hir *hir, struct Pool *pool, Str const *modname, struct HirMatchExpr *match, struct MatchVars *vars)
{
    struct Compiler *C = hir->C;

    struct Usefulness U = {
        .modname = modname,
        .span = match->span,
        .pool = pool,
        .vars = vars,
        .hir = hir,
        .P = ENV(C),
        .C = C,
    };

    struct RowList *rows = RowList_new(&U);
    struct SourceSpan span = match->target->hdr.span;
    struct MatchVar var = new_variable(&U, span, GET_NODE_TYPE(C, match->target));

    struct HirExpr *const *parm;
    K_LIST_FOREACH (match->arms, parm) {
        struct HirMatchArm *arm = HirGetMatchArm(*parm);
        struct ColumnList *cols = ColumnList_new(&U);
        ColumnList_push(&U, cols, (struct Column){
            .var = var,
            .pat = arm->pat,
        });
        RowList_push(&U, rows, (struct Row){
            .body = new_body(&U, arm->result),
            .guard = arm->guard,
            .columns = cols,
        });
    }

    return compile_rows(&U, rows);
}
