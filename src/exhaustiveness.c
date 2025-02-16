// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// exhaustiveness.c: Exhaustiveness check for pattern matching, based off of
//     https://github.com/yorickpeterse/pattern-matching-in-rust.

#include "compile.h"
#include "hir.h"
#include "ir_type.h"
#include "map.h"
#include "match.h"

struct Usefulness {
    struct VariableList *vars;
    struct Compiler *C;
    struct Pool *pool;
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
    struct VariableList *vars;
    struct RowList *rows;
};

DEFINE_LIST(struct Usefulness, decision_list_, DecisionList, struct Decision *)
DEFINE_LIST(struct Usefulness, raw_case_list_, RawCaseList, struct RawCase)
DEFINE_LIST(struct Usefulness, column_list_, ColumnList, struct Column)
DEFINE_LIST(struct Usefulness, row_list_, RowList, struct Row)

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
    if (cons.kind != CONS_VARIANT) return 0;
    return cons.variant.index;
}

static struct MatchVar new_variable(struct Usefulness *U, struct IrType *type)
{
    K_LIST_PUSH(U->C, U->vars, ((struct MatchVar){
        .id = U->var_id++,
        .type = type,
    }));
    return K_LIST_LAST(U->vars);
}

static struct MatchBody new_body(struct Usefulness *U, struct HirExpr *result)
{
    return (struct MatchBody){
        .bindings = binding_list_new(U->C),
        .result = result,
    };
}

static void extend_row_list(struct Usefulness *U, struct RowList *target, struct RowList *rows)
{
    for (int i = 0; i < rows->count; ++i) {
        K_LIST_PUSH(U, target, K_LIST_GET(rows, i));
    }
}

static struct MatchBody copy_body(struct Usefulness *U, struct MatchBody body)
{
    struct BindingList *bindings = binding_list_new(U->C);
    for (int i = 0; i < body.bindings->count; ++i) {
        struct Binding b = K_LIST_GET(body.bindings, i);
        K_LIST_PUSH(U->C, bindings, b);
    }
    return (struct MatchBody){
        .bindings = bindings,
        .result = body.result,
    };
}

static struct Row copy_row(struct Usefulness *U, struct Row row)
{
    struct ColumnList *cols = column_list_new(U);
    for (int i = 0; i < row.columns->count; ++i) {
        K_LIST_PUSH(U->C, cols, K_LIST_GET(row.columns, i));
    }
    return (struct Row){
        .columns = cols,
        .guard = row.guard,
        .body = copy_body(U, row.body),
    };
}

static struct HirPatList *join_lists(struct Usefulness *U, struct HirPatList *lhs, struct HirPatList *rhs)
{
    for (int i = 0; i < rhs->count; ++i) {
        K_LIST_PUSH(U->C, lhs, K_LIST_GET(rhs, i));
    }
    return lhs;
}

static struct HirPatList *flatten_or(struct Usefulness *U, struct HirPat *pat)
{
    if (HirIsOrPat(pat)) return HirGetOrPat(pat)->pats;
    struct HirPatList *pats = pawHir_pat_list_new(U->C);
    K_LIST_PUSH(U->C, pats, pat);
    return pats;
}

static struct Row remove_first_row(struct Usefulness *U, struct RowList *rows)
{
    struct Row first_row = K_LIST_FIRST(rows);
    memmove(rows->data, rows->data + 1, CAST_SIZE(rows->count - 1) * sizeof(rows->data[0]));
    --rows->count;
    return first_row;
}

static int find_or_pattern(struct Row row)
{
    for (int icol = 0; icol < row.columns->count; ++icol) {
        struct Column col = K_LIST_GET(row.columns, icol);
        if (HirIsOrPat(col.pat)) return icol;
    }
    return -1;
}

static paw_Bool contains_or_pattern(const struct RowList *rows)
{
    for (int irow = 0; irow < rows->count; ++irow) {
        struct Row row = K_LIST_GET(rows, irow);
        if (find_or_pattern(row) >= 0) return PAW_TRUE;
    }
    return PAW_FALSE;
}

// Translated from @yorickpeterse/pattern-matching-in-rust
// OR patterns need to be expanded here, so that each alternative gets its own copy of the
// row. Each alternative may bind variables in different positions in the pattern, so they
// need to be handled separately.
static void expand_or_patterns(struct Usefulness *U, struct RowList *rows) {
    if (!contains_or_pattern(rows)) return;

    struct RowList *new_rows = row_list_new(U);
    paw_Bool found = PAW_TRUE;

    while (found) {
        found = PAW_FALSE;

        while (rows->count > 0) {
            struct Row row = remove_first_row(U, rows);
            const int icol = find_or_pattern(row);
            if (icol >= 0) {
                found = PAW_TRUE;

                struct Column col = K_LIST_GET(row.columns, icol);
                struct HirOrPat *or = HirGetOrPat(col.pat);

                for (int ipat = 0; ipat < or->pats->count; ++ipat) {
                    struct HirPat *p = K_LIST_GET(or->pats, ipat);
                    struct Row new_row = copy_row(U, row);
                    K_LIST_SET(new_row.columns, icol, ((struct Column){
                                    .var = col.var,
                                    .pat = p,
                                }));
                    K_LIST_PUSH(U->C, new_rows, new_row);
                }
            } else {
                K_LIST_PUSH(U->C, new_rows, row);
            }
        }

        struct RowList temp = *rows;
        *rows = *new_rows;
        *new_rows = temp;
    }
}

static struct VariableList *variables_for_types(struct Usefulness *U, struct IrTypeList *types)
{
    struct VariableList *result = variable_list_new(U->C);
    if (types == NULL) return result;

    for (int i = 0; i < types->count; ++i) {
        struct MatchVar var = new_variable(U, K_LIST_GET(types, i));
        K_LIST_PUSH(U->C, result, var);
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
    struct ColumnList *columns = column_list_new(U);
    for (int icol = 0; icol < row->columns->count; ++icol) {
        struct Column col = K_LIST_GET(row->columns, icol);
        if (HirIsBindingPat(col.pat)) {
            struct HirBindingPat *p = HirGetBindingPat(col.pat);
            K_LIST_PUSH(U->C, row->body.bindings, ((struct Binding){
                            .name = p->name,
                            .var = col.var,
                        }));
        } else if (!HirIsWildcardPat(col.pat)) {
            K_LIST_PUSH(U->C, columns, col);
        }
    }
    row->columns = columns;
}

static paw_Bool remove_column(struct Usefulness *U, struct Row row, struct MatchVar var, struct Column *pcol)
{
    struct ColumnList *cols = row.columns;
    for (int i = 0; i < cols->count; ++i) {
        struct Column col = K_LIST_GET(cols, i);
        if (col.var.id == var.id) {
            *pcol = col; // memmove trashes memory
            const size_t rest = CAST_SIZE(cols->count - i - 1);
            memmove(cols->data + i, cols->data + i + 1,
                    rest * sizeof(cols->data[0]));
            --cols->count;
            return PAW_FALSE;
        }
    }
    return PAW_TRUE;
}

static struct Decision *compile_rows(struct Usefulness *U, struct RowList *rows);

static struct ColumnList *unpack_tuple_fields(struct Usefulness *U, struct HirPatList *pats)
{
    struct ColumnList *cols = column_list_new(U);
    for (int i = 0; i < pats->count; ++i) {
        struct HirPat *pat = K_LIST_GET(pats, i);
        struct MatchVar var = new_variable(U, GET_NODE_TYPE(U->C, pat));
        K_LIST_PUSH(U->C, cols, ((struct Column){.var = var, .pat = pat}));
    }
    return cols;
}

static struct ColumnList *unpack_variant_fields(struct Usefulness *U, struct HirPatList *pats, struct IrType *type)
{
    struct ColumnList *cols = column_list_new(U);
    for (int i = 0; i < pats->count; ++i) {
        struct HirPat *pat = K_LIST_GET(pats, i);
        struct MatchVar var = new_variable(U, GET_NODE_TYPE(U->C, pat));
        K_LIST_PUSH(U->C, cols, ((struct Column){.var = var, .pat = pat}));
    }
    return cols;
}

static struct CaseList *compile_cases(struct Usefulness *U, struct RawCaseList *raw_cases)
{
    struct CaseList *result = case_list_new(U->C);
    for (int i = 0; i < raw_cases->count; ++i) {
        struct RawCase *rc = &K_LIST_GET(raw_cases, i);
        struct Decision *dec = compile_rows(U, rc->rows);
        K_LIST_PUSH(U->C, result, ((struct MatchCase){
                        .cons = rc->cons,
                        .vars = rc->vars,
                        .dec = dec,
                    }));
    }
    return result;
}

static struct HirPatList *struct_pat_fields(struct Usefulness *U, struct HirStructPat *p)
{
    struct HirPatList *fields = pawHir_pat_list_new(U->C);
    for (int i = 0; i < p->fields->count; ++i) {
        struct HirFieldPat *field = HirGetFieldPat(K_LIST_GET(p->fields, i));
        K_LIST_PUSH(U->C, fields, field->pat);
    }
    return fields;
}

static struct CaseList *compile_constructor_cases(struct Usefulness *U, struct RowList *rows, struct MatchVar branch_var, struct RawCaseList *cases)
{
    for (int irow = 0; irow < rows->count; ++irow) {
        struct Row row = K_LIST_GET(rows, irow);
        struct Column col;
        if (remove_column(U, row, branch_var, &col)) {
            for (int i = 0; i < cases->count; ++i) {
                struct RawCase *rc = &K_LIST_GET(cases, i);
                K_LIST_PUSH(U, rc->rows, copy_row(U, row));
            }
            continue;
        }

        struct HirPat *pat = col.pat;
        struct Row r = copy_row(U, row);

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

        struct RawCase *rc = &K_LIST_GET(cases, index);
        paw_assert(rc->vars->count == fields->count);
        for (int i = 0; i < fields->count; ++i) {
            K_LIST_PUSH(U, r.columns, ((struct Column){
                            .var = K_LIST_GET(rc->vars, i),
                            .pat = K_LIST_GET(fields, i),
                        }));
        }
        K_LIST_PUSH(U, rc->rows, r);
    }

    return compile_cases(U, cases);
}

struct LiteralResult {
    struct CaseList *cases;
    struct Decision *fallback;
};

static struct LiteralResult compile_literal_cases(struct Usefulness *U, struct RowList *rows, struct MatchVar branch_var)
{
    paw_Env *P = ENV(U);
    paw_new_map(P, 0);

    Map *tested = V_MAP(P->top.p[-1]);
    struct RawCaseList *raw_cases = raw_case_list_new(U);
    struct RowList *fallback = row_list_new(U);

    for (int irow = 0; irow < rows->count; ++irow) {
        struct Row row = K_LIST_GET(rows, irow);

        struct Column col;
        if (remove_column(U, row, branch_var, &col)) {
            // This row had a wildcard or binding in place of 'branch_var', meaning
            // it needs to be considered in all other cases.
            K_LIST_PUSH(U, fallback, row);
            for (int i = 0; i < raw_cases->count; ++i) {
                struct RawCase rc = K_LIST_GET(raw_cases, i);
                K_LIST_PUSH(U, rc.rows, row);
            }
            continue;
        }

        struct HirPat *pat = col.pat;
        struct Row r = copy_row(U, row);

        struct HirLiteralPat *p = HirGetLiteralPat(pat);
        struct HirLiteralExpr *e = HirGetLiteralExpr(p->expr);
        const Value key = e->basic.value;
        struct Constructor cons = {0};

        switch (e->basic.t) {
            case BUILTIN_UNIT:
                cons.kind = CONS_TUPLE;
                cons.tuple.elems = pawIr_type_list_new(U->C);
                break;
            case BUILTIN_BOOL:
                cons.kind = CONS_BOOL;
                cons.value = e->basic.value;
                break;
            case BUILTIN_INT:
                cons.kind = CONS_INT;
                cons.value = e->basic.value;
                break;
            case BUILTIN_FLOAT:
                cons.kind = CONS_FLOAT;
                cons.value = e->basic.value;
                break;
            default:
                paw_assert(e->basic.t == BUILTIN_STR);
                cons.kind = CONS_STR;
                cons.value = e->basic.value;
                break;
        }

        Value *pv = pawH_get(tested, key);
        if (pv != NULL) {
            struct RawCase rc = K_LIST_GET(raw_cases, pv->i);
            K_LIST_PUSH(U, rc.rows, r);
            continue;
        }
        pawH_insert(P, tested, key, I2V(raw_cases->count));
        struct RowList *rows = row_list_new(U);
        extend_row_list(U, rows, fallback);
        K_LIST_PUSH(U, rows, r);
        K_LIST_PUSH(U->C, raw_cases, ((struct RawCase){
                        .vars = variable_list_new(U->C),
                        .cons = cons,
                        .rows = rows,
                    }));
    }

    pawC_pop(P); // pop 'tested'

    return (struct LiteralResult){
        .cases = compile_cases(U, raw_cases),
        .fallback = compile_rows(U, fallback),
    };
}

static struct Column find_branch_col(struct Usefulness *U, struct ColumnList *cols)
{
    return K_LIST_GET(cols, 0);
}

static struct IrTypeList *collect_field_types(struct Usefulness *U, struct HirDecl *decl, struct IrType *type)
{
    struct HirAdtDecl *d = HirGetAdtDecl(decl);
    struct IrTypeList *result = pawIr_type_list_new(U->C);
    for (int i = 0; i < d->fields->count; ++i) {
        struct HirDecl *field = K_LIST_GET(d->fields, i);
        struct IrType *next = pawP_instantiate_field(U->C, type, field);
        K_LIST_PUSH(U->C, result, next);
    }
    return result;
}

struct RawCaseList *cases_for_struct(struct Usefulness *U, struct MatchVar var)
{
    struct HirDecl *decl = pawHir_get_decl(U->C, IR_TYPE_DID(var.type));

    struct IrTypeList *fields = collect_field_types(U, decl, var.type);
    struct VariableList *subvars = variables_for_types(U, fields);
    struct Constructor cons = {
        .kind = CONS_STRUCT,
        .struct_.type = var.type,
    };

    struct RawCaseList *result = raw_case_list_new(U);
    K_LIST_PUSH(U->C, result, ((struct RawCase){
                    .rows = row_list_new(U),
                    .vars = subvars,
                    .cons = cons,
                }));
    return result;
}

struct RawCaseList *cases_for_tuple(struct Usefulness *U, struct MatchVar var)
{
    struct IrTypeList *elems = IrGetTuple(var.type)->elems;
    struct VariableList *subvars = variables_for_types(U, elems);
    struct Constructor cons = {
        .kind = CONS_TUPLE,
        .tuple.elems = elems,
    };

    struct RawCaseList *result = raw_case_list_new(U);
    K_LIST_PUSH(U, result, ((struct RawCase){
                    .rows = row_list_new(U),
                    .vars = subvars,
                    .cons = cons,
                }));
    return result;
}

struct RawCaseList *cases_for_variant(struct Usefulness *U, struct MatchVar var)
{
    struct HirDecl *decl = pawHir_get_decl(U->C, IR_TYPE_DID(var.type));
    struct HirDeclList *fields = HirGetAdtDecl(decl)->fields;
    paw_assert(!HirGetAdtDecl(decl)->is_struct);

    struct RawCaseList *result = raw_case_list_new(U);
    for (int i = 0; i < fields->count; ++i) {
        struct HirDecl *field = K_LIST_GET(fields, i);
        struct IrType *type = pawP_instantiate_field(U->C, var.type, field);
        struct VariableList *subvars = variables_for_types(U, IR_FPTR(type)->params);
        struct Constructor cons = {
            .kind = CONS_VARIANT,
            .variant.type = type,
            .variant.index = i,
        };

        K_LIST_PUSH(U->C, result, ((struct RawCase){
                        .rows = row_list_new(U),
                        .vars = subvars,
                        .cons = cons,
                    }));
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
    if (!IrIsAdt(var.type)) {
        paw_assert(IrIsTuple(var.type));
        return BRANCH_TUPLE;
    }
    enum BuiltinKind code = pawP_type2code(U->C, var.type);
    if (IS_BASIC_TYPE(code)) return BRANCH_LITERAL;
    struct HirDecl *decl = pawHir_get_decl(U->C, IR_TYPE_DID(var.type));
    if (!HirGetAdtDecl(decl)->is_struct) return BRANCH_VARIANT;
    return BRANCH_STRUCT;
}

static struct Decision *compile_rows(struct Usefulness *U, struct RowList *rows)
{
    if (rows->count == 0) {
        TYPE_ERROR(U, "non-exhaustive pattern match");
    }
    expand_or_patterns(U, rows);
    for (int i = 0; i < rows->count; ++i) {
        move_bindings_to_right(U, &K_LIST_GET(rows, i));
    }
    struct Row first_row = K_LIST_GET(rows, 0);
    if (first_row.columns->count == 0) {
        remove_first_row(U, rows);
        if (first_row.guard == NULL) return new_success(U, first_row.body);
        return new_guard(U, first_row.guard, first_row.body, compile_rows(U, rows));
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

struct Decision *pawP_check_exhaustiveness(struct Compiler *C, struct HirMatchExpr *match, struct VariableList *vars)
{
    paw_Env *P = ENV(C);
    struct Usefulness U = {
        .pool = C->pool,
        .vars = vars,
        .P = P,
        .C = C,
    };

    struct RowList *rows = row_list_new(&U);
    struct MatchVar var = new_variable(&U, GET_NODE_TYPE(C, match->target));
    for (int i = 0; i < match->arms->count; ++i) {
        struct HirMatchArm *arm = HirGetMatchArm(K_LIST_GET(match->arms, i));
        struct ColumnList *cols = column_list_new(&U);
        K_LIST_PUSH(C, cols, ((struct Column){.var = var, .pat = arm->pat}));
        K_LIST_PUSH(C, rows, ((struct Row){
                        .body = new_body(&U, arm->result),
                        .guard = arm->guard,
                        .columns = cols,
                    }));
    }

    return compile_rows(&U, rows);
}
