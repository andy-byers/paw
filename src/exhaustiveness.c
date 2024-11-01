// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// exhaustiveness.c: Exhaustiveness check for pattern matching, mostly based
//     off of https://github.com/yorickpeterse/pattern-matching-in-rust.

#include "compile.h"
#include "hir.h"
#include "map.h"
#include "match.h"

struct Usefulness {
    struct Compiler *C;
    struct Pool *pool;
    paw_Env *P;
    int var_id;
    int line;
};

struct Column {
    struct MatchVar *var;
    struct HirPat *pat;
};

struct Row {
    struct ColumnList *columns;
    struct MatchBody *body;
};

struct RawCase {
    struct Constructor *cons;
    struct VariableList *vars;
    struct RowList *rows;
};

DEFINE_LIST(struct Usefulness, decision_list_, DecisionList, struct Decision)
DEFINE_LIST(struct Usefulness, column_list_, ColumnList, struct Column)
DEFINE_LIST(struct Usefulness, row_list_, RowList, struct Row)
DEFINE_LIST(struct Usefulness, raw_case_list_, RawCaseList, struct RawCase)

static struct RawCase *new_raw_case(struct Usefulness *U, struct Constructor *cons, struct VariableList *vars, struct RowList *rows)
{
    struct RawCase *c = pawK_pool_alloc(ENV(U), U->pool, sizeof(struct RawCase));
    *c = (struct RawCase){
        .cons = cons,
        .vars = vars,
        .rows = rows,
    };
    return c;
}

static struct MatchCase *new_case(struct Usefulness *U, struct Constructor *cons, struct VariableList *vars, struct Decision *dec)
{
    struct MatchCase *c = pawK_pool_alloc(ENV(U), U->pool, sizeof(struct MatchCase));
    *c = (struct MatchCase){
        .cons = cons,
        .vars = vars,
        .dec = dec,
    };
    return c;
}

static struct Decision *new_decision(struct Usefulness *U, enum DecisionKind kind)
{
    struct Decision *result = pawK_pool_alloc(ENV(U), U->pool, sizeof(struct Decision));
    *result = (struct Decision){
        .kind = kind,
    };
    return result;
}

static struct Decision *new_failure(struct Usefulness *U)
{
    return new_decision(U, DECISION_FAILURE);
}

static struct Decision *new_success(struct Usefulness *U, struct MatchBody *body)
{
    struct Decision *result = new_decision(U, DECISION_SUCCESS);
    result->success.body = body;
    return result;
}

static struct Decision *new_guard(struct Usefulness *U, struct HirExpr *cond, struct MatchBody *body, struct Decision *rest)
{
    struct Decision *result = new_decision(U, DECISION_GUARD);
    result->guard.cond = cond;
    result->guard.body = body;
    result->guard.rest = rest;
    return result;
}

static struct Decision *new_multi(struct Usefulness *U, struct MatchVar *test, struct CaseList *cases, struct Decision *rest)
{
    struct Decision *result = new_decision(U, DECISION_MULTIWAY);
    result->multi.test = test;
    result->multi.cases = cases;
    result->multi.rest = rest;
    return result;
}

static struct Constructor *new_constructor(struct Usefulness *U, enum ConstructorKind kind)
{
    struct Constructor *cons = pawK_pool_alloc(ENV(U), U->pool, sizeof(struct Constructor));
    *cons = (struct Constructor){
        .kind = kind,
    };
    return cons;
}

static int constructor_index(const struct Constructor *cons)
{
    if (cons->kind != CONS_VARIANT) return 0;
    return cons->variant.index;
}

static struct MatchVar *new_variable(struct Usefulness *U, struct HirType *type)
{
    struct MatchVar *var = pawK_pool_alloc(ENV(U), U->pool, sizeof(struct MatchVar));
    *var = (struct MatchVar){
        .id = U->var_id++,
        .type = type,
    };
    return var;
}

static struct MatchBody *new_body(struct Usefulness *U, struct HirBlock *block)
{
    struct MatchBody *result = pawK_pool_alloc(ENV(U), U->pool, sizeof(struct MatchBody));
    *result = (struct MatchBody){
        .bindings = variable_list_new(U->C),
        .block = block,
    };
    return result;
}

static struct Column *new_column(struct Usefulness *U, struct MatchVar *var, struct HirPat *pat)
{
    struct Column *column = pawK_pool_alloc(ENV(U), U->pool, sizeof(struct Column));
    *column = (struct Column){
        .var = var,
        .pat = pat,
    };
    return column;
}

static struct Row *new_row(struct Usefulness *U, struct ColumnList *columns, struct MatchBody *body)
{
    struct Row *row = pawK_pool_alloc(ENV(U), U->pool, sizeof(struct Row));
    *row = (struct Row){
        .columns = columns,
        .body = body,
    };
    return row;
}

static void extend_row_list(struct Usefulness *U, struct RowList *target, struct RowList *rows)
{
    for (int i = 0; i < rows->count; ++i) {
        row_list_push(U, target, K_LIST_GET(rows, i));
    }
}

static struct HirPatList *join_lists(struct Usefulness *U, struct HirPatList *lhs, struct HirPatList *rhs)
{
    for (int i = 0; i < rhs->count; ++i) {
        pawHir_pat_list_push(U->C, lhs, K_LIST_GET(rhs, i));
    }
    return lhs;
}

static struct HirPatList *flatten_or(struct Usefulness *U, struct HirPat *pat)
{
    if (HirIsOrPat(pat)) return HirGetOrPat(pat)->pats;
    struct HirPatList *pats = pawHir_pat_list_new(U->C);
    pawHir_pat_list_push(U->C, pats, pat);
    return pats;
}

static struct VariableList *variables_for_types(struct Usefulness *U, struct HirTypeList *types)
{
    struct VariableList *result = variable_list_new(U->C);
    if (types == NULL) return result;

    for (int i = 0; i < types->count; ++i) {
        struct MatchVar *var = new_variable(U, K_LIST_GET(types, i));
        variable_list_push(U->C, result, var);
        var->index = i;
    }
    return result;
}

#if 0

#include"stdio.h"

static void print_path(struct Compiler*C,struct HirPath *path)
{
    pawHir_dump_path(C, path);
    printf("%s",paw_string(C->P,-1));
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
                if (e->lit_kind == kHirLitBasic && e->basic.t == PAW_TINT) {
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
    struct ColumnList *columns = column_list_new(U);
    for (int icol = 0; icol < row->columns->count; ++icol) {
        struct Column *col = K_LIST_GET(row->columns, icol);
        if (HirIsBindingPat(col->pat)) {
            K_LIST_APPEND(U->C, row->body->bindings, col->var);
        } else if (!HirIsWildcardPat(col->pat)){
            K_LIST_APPEND(U->C, columns, col);
        }
    }
    row->columns = columns;
}

static struct Column *remove_column(struct Usefulness *U, struct Row *row, struct MatchVar *var)
{
    struct ColumnList *cols = row->columns;
    for (int i = 0; i < cols->count; ++i) {
        struct Column *col = cols->data[i];
        if (col->var->id == var->id) {
            struct Column *result = K_LIST_GET(cols, i);
            const size_t rest = CAST_SIZE(cols->count - i - 1);
            memmove(cols->data + i, cols->data + i + 1,
                    rest * sizeof(cols->data[0]));
            --cols->count;
            return result;
        }
    }
    return NULL;
}

static struct Decision *compile_rows(struct Usefulness *U, struct RowList *rows);

static struct ColumnList *unpack_tuple_fields(struct Usefulness *U, struct HirPatList *pats)
{
    struct ColumnList *cols = column_list_new(U);
    for (int i = 0; i < pats->count; ++i) {
        struct HirPat *pat = K_LIST_GET(pats, i);
        struct MatchVar *var = new_variable(U, HIR_TYPEOF(pat));
        K_LIST_APPEND(U->C, cols, new_column(U, var, pat));
    }
    return cols;
}

static struct ColumnList *unpack_variant_fields(struct Usefulness *U, struct HirPatList *pats, struct HirType *type)
{
    struct ColumnList *cols = column_list_new(U);
    for (int i = 0; i < pats->count; ++i) {
        struct HirPat *pat = K_LIST_GET(pats, i);
        struct MatchVar *var = new_variable(U, HIR_TYPEOF(pat));
        K_LIST_APPEND(U->C, cols, new_column(U, var, pat));
    }
    return cols;
}

static struct CaseList *compile_cases(struct Usefulness *U, struct RawCaseList *raw_cases)
{
    struct CaseList *result = case_list_new(U->C);
    for (int i = 0; i < raw_cases->count; ++i) {
        struct RawCase *rc = K_LIST_GET(raw_cases, i);
        struct Decision *dec = compile_rows(U, rc->rows);
        struct MatchCase *cs = new_case(U, rc->cons, rc->vars, dec);
        case_list_push(U->C, result, cs);
    }
    return result;
}

static struct HirPatList *struct_pat_fields(struct Usefulness *U, struct HirStructPat *p)
{
    struct HirPatList *fields = pawHir_pat_list_new(U->C);
    for (int i = 0; i < p->fields->count; ++i) {
        struct HirFieldPat *field = HirGetFieldPat(K_LIST_GET(p->fields, i));
        pawHir_pat_list_push(U->C, fields, field->pat);
    }
    return fields;
}

static struct CaseList *compile_constructor_cases(struct Usefulness *U, struct RowList *rows, struct MatchVar *branch_var, struct RawCaseList *cases)
{
    for (int irow = 0; irow < rows->count; ++irow) {
        struct Row *row = K_LIST_GET(rows, irow);
        struct Column *col = remove_column(U, row, branch_var);
        if (col == NULL) {
            for (int i = 0; i < cases->count; ++i) {
                struct RawCase *cs = K_LIST_GET(cases, i);
                row_list_push(U, cs->rows, row);
            }
            continue;
        }

        struct HirPatList *pats = flatten_or(U, col->pat);
        for (int i = 0; i < pats->count; ++i) {
            struct HirPat *pat = K_LIST_GET(pats, i);

            // pattern matrix specialization
            int index = 0;
            struct HirPatList *fields;
            if (HirIsVariantPat(pat)) {
                fields = HirGetVariantPat(pat)->fields;
                index = HirGetVariantPat(pat)->index;
            } else if (HirIsStructPat(pat)){
                fields = struct_pat_fields(U, HirGetStructPat(pat));
            } else {
                fields = HirGetTuplePat(pat)->elems;
            }

            struct RawCase *cs = K_LIST_GET(cases, index);
            paw_assert(cs->vars->count == fields->count);
            for (int i = 0; i < fields->count; ++i) {
                struct Column *c = new_column(U, K_LIST_GET(cs->vars, i),
                        K_LIST_GET(fields, i));
                column_list_push(U, row->columns, c);
            }
            row = new_row(U, row->columns, row->body);
            row_list_push(U, cs->rows, row);
        }
    }

    return compile_cases(U, cases);
}

struct LiteralResult {
    struct CaseList *cases;
    struct Decision *fallback;
};

static struct LiteralResult compile_literal_cases(struct Usefulness *U, struct RowList *rows, struct MatchVar *branch_var)
{
    paw_Env *P = ENV(U);
    paw_new_map(P, 0);

    Map *tested = V_MAP(P->top.p[-1]);
    struct RawCaseList *raw_cases = raw_case_list_new(U);
    struct RowList *fallback = row_list_new(U);

    for (int irow = 0; irow < rows->count; ++irow) {
        struct Row *row = K_LIST_GET(rows, irow);

        struct Column *col = remove_column(U, row, branch_var);
        if (col == NULL) {
            // This row had a wildcard or binding in place of 'branch_var', meaning
            // it needs to be considered in all other cases.
            row_list_push(U, fallback, row);
            for (int i = 0; i < raw_cases->count; ++i) {
                struct RawCase *rc = K_LIST_GET(raw_cases, i);
                row_list_push(U, rc->rows, row);
            }
            continue;
        }

        struct HirPatList *pats = flatten_or(U, col->pat);
        for (int i = 0; i < pats->count; ++i) {
            struct HirPat *pat = K_LIST_GET(pats, i);

            struct HirLiteralPat *p = HirGetLiteralPat(pat);
            struct HirLiteralExpr *e = HirGetLiteralExpr(p->expr);
            const Value key = e->basic.value;
            struct Constructor *cons;

            switch (e->basic.t) {
                case PAW_TUNIT:
                    cons = new_constructor(U, CONS_TUPLE);
                    cons->tuple.elems = pawHir_type_list_new(U->C);
                    break;
                case PAW_TBOOL:
                    cons = new_constructor(U, CONS_BOOL);
                    break;
                case PAW_TINT:
                    cons = new_constructor(U, CONS_INT);
                    break;
                case PAW_TFLOAT:
                    cons = new_constructor(U, CONS_FLOAT);
                    break;
                default:
                    paw_assert(e->basic.t == PAW_TSTR);
                    cons = new_constructor(U, CONS_STR);
                    break;
            }

            Value *pv = pawH_get(tested, key);
            if (pv != NULL) {
                struct RawCase *rc = K_LIST_GET(raw_cases, pv->i);
                row_list_push(U, rc->rows, row);
                continue;
            }
            pawH_insert(P, tested, key, I2V(raw_cases->count));
            struct RowList *rows = row_list_new(U);
            extend_row_list(U, rows, fallback);
            row_list_push(U, rows, row);
            struct RawCase *rc = new_raw_case(U, cons, variable_list_new(U->C), rows);
            raw_case_list_push(U, raw_cases, rc);
        }
    }

    pawC_pop(P); // pop 'tested'

    return (struct LiteralResult){
        .cases = compile_cases(U, raw_cases),
        .fallback = compile_rows(U, fallback),
    };
}

static struct Column *find_branch_col(struct Usefulness *U, struct ColumnList *cols)
{
    return K_LIST_GET(cols, 0);
}

static struct HirTypeList *collect_field_types(struct Usefulness *U, struct HirDecl *decl, struct HirType *type)
{
    struct HirAdtDecl *d = HirGetAdtDecl(decl);
    struct HirTypeList *result = pawHir_type_list_new(U->C);
    for (int i = 0; i < d->fields->count; ++i) {
        struct HirDecl *field = K_LIST_GET(d->fields, i);
        struct HirType *next = pawP_instantiate_field(U->C, type, field);
        K_LIST_APPEND(U->C, result, next);
    }
    return result;
}

struct RawCaseList *cases_for_struct(struct Usefulness *U, struct MatchVar *var)
{
    struct HirDecl *decl = pawHir_get_decl(U->C, HIR_TYPE_DID(var->type));

    struct HirTypeList *fields = collect_field_types(U, decl, var->type);
    struct VariableList *subvars = variables_for_types(U, fields);
    struct Constructor *cons = new_constructor(U, CONS_STRUCT);
    cons->struct_.type = var->type;

    struct RawCase *cs = new_raw_case(U, cons, subvars, row_list_new(U));
    struct RawCaseList *result = raw_case_list_new(U);
    raw_case_list_push(U, result, cs);
    return result;
}

struct RawCaseList *cases_for_tuple(struct Usefulness *U, struct MatchVar *var)
{
    struct HirTypeList *elems = HirGetTupleType(var->type)->elems;
    struct VariableList *subvars = variables_for_types(U, elems);
    struct Constructor *cons = new_constructor(U, CONS_TUPLE);
    cons->tuple.elems = elems;

    struct RawCase *cs = new_raw_case(U, cons, subvars, row_list_new(U));
    struct RawCaseList *result = raw_case_list_new(U);
    raw_case_list_push(U, result, cs);
    return result;
}

struct RawCaseList *cases_for_variant(struct Usefulness *U, struct MatchVar *var)
{
    struct HirDecl *decl = pawHir_get_decl(U->C, HIR_TYPE_DID(var->type));
    struct HirDeclList *fields = HirGetAdtDecl(decl)->fields;
    paw_assert(!HirGetAdtDecl(decl)->is_struct);

    struct RawCaseList *result = raw_case_list_new(U);
    for (int i = 0; i < fields->count; ++i) {
        struct HirDecl *field = K_LIST_GET(fields, i);
        struct HirType *type = pawP_instantiate_field(U->C, var->type, field);
        struct VariableList *subvars = variables_for_types(U, HIR_FPTR(type)->params);
        struct Constructor *cons = new_constructor(U, CONS_VARIANT);
        cons->variant.type = type;
        cons->variant.index = i;

        struct RawCase *cs = new_raw_case(U, cons, subvars, row_list_new(U));
        raw_case_list_push(U, result, cs);
    }
    return result;
}

enum BranchMode {
    BRANCH_VARIANT,
    BRANCH_STRUCT,
    BRANCH_TUPLE,
    BRANCH_LITERAL,
};

static enum BranchMode branch_mode(struct Usefulness *U, struct MatchVar *var)
{
    if (!HirIsAdt(var->type)) {
        paw_assert(HirIsTupleType(var->type));
        return BRANCH_TUPLE;
    }
    struct HirDecl *decl = pawHir_get_decl(U->C, HIR_TYPE_DID(var->type));
    if (!HirGetAdtDecl(decl)->is_struct) return BRANCH_VARIANT;
    if (HirGetAdtDecl(decl)->did <= PAW_TSTR) return BRANCH_LITERAL;
    return BRANCH_STRUCT;
}

static struct Decision *compile_rows(struct Usefulness *U, struct RowList *rows)
{
    if (rows->count == 0) {
        TYPE_ERROR(U, "non-exhaustive pattern match");
    }
    for (int i = 0; i < rows->count; ++i) {
        move_bindings_to_right(U, K_LIST_GET(rows, i));
    }
    struct Row *first_row = K_LIST_GET(rows, 0);
    if (first_row->columns->count == 0) {
        struct Decision *result = new_decision(U, DECISION_SUCCESS);
        result->success.body = first_row->body;
        return result;
    }

    struct Column *branch_col = find_branch_col(U, first_row->columns);
    switch (branch_mode(U, branch_col->var)) {
        case BRANCH_VARIANT: {
            struct RawCaseList *raw_cases = cases_for_variant(U, branch_col->var);
            struct CaseList *cases = compile_constructor_cases(U, rows, branch_col->var, raw_cases);
            return new_multi(U, branch_col->var, cases, NULL);
        }
        case BRANCH_STRUCT: {
            // TODO: implement struct patterns
            struct RawCaseList *raw_cases = cases_for_struct(U, branch_col->var);
            struct CaseList *cases = compile_constructor_cases(U, rows, branch_col->var, raw_cases);
            return new_multi(U, branch_col->var, cases, NULL);
        }
        case BRANCH_TUPLE: {
            struct RawCaseList *raw_cases = cases_for_tuple(U, branch_col->var);
            struct CaseList *cases = compile_constructor_cases(U, rows, branch_col->var, raw_cases);
            return new_multi(U, branch_col->var, cases, NULL);
        }
        case BRANCH_LITERAL: {
            struct LiteralResult result = compile_literal_cases(U, rows, branch_col->var);
            return new_multi(U, branch_col->var, result.cases, result.fallback);
        }
    }
}

void pawP_check_exhaustiveness(struct Compiler *C, struct HirMatchStmt *match)
{
    paw_Env *P = ENV(C);
    struct Usefulness U = {
        .pool = C->pool,
        .P = P,
        .C = C,
    };

    struct RowList *rows = row_list_new(&U);
    struct MatchVar *var = new_variable(&U, HIR_TYPEOF(match->target));
    for (int i = 0; i < match->arms->count; ++i) {
        struct HirMatchArm *arm = HirGetMatchArm(K_LIST_GET(match->arms, i));
        struct ColumnList *cols = column_list_new(&U);
        K_LIST_APPEND(C, cols, new_column(&U, var, arm->guard));
        K_LIST_APPEND(C, rows, new_row(&U, cols, new_body(&U, arm->result)));
    }

    struct Decision *tree = compile_rows(&U, rows);
    pawH_insert(P, C->matches, I2V(match->hid), P2V(tree));
}
