// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "auxlib.h"
#include "compile.h"
#include "hir.h"
#include "ir_type.h"
#include "match.h"

#if defined(PAW_DEBUG_EXTRA)

struct Printer {
    struct Compiler *C;
    Buffer *buf;
    paw_Env *P;
    int indent;
};

#define PRINT_LITERAL(P, lit) L_ADD_LITERAL(ENV(P), (P)->buf, lit)
#define PRINT_STRING(P, str) pawL_add_nstring(ENV(P), (P)->buf, (str)->text, (str)->length)
#define PRINT_FORMAT(P, ...) pawL_add_fstring(ENV(P), (P)->buf, __VA_ARGS__)
#define PRINT_CHAR(P, c) pawL_add_char(ENV(P), (P)->buf, c)

static void print_indentation(struct Printer *P)
{
    for (int i = 0; i < P->indent; ++i) {
        PRINT_LITERAL(P, ". ");
    }
}

static void print_bindings(struct Printer *P, struct BindingList *bindings)
{
    for (int i = 0; i < bindings->count; ++i) {
        print_indentation(P);
        struct Binding b = BindingList_get(bindings, i);
        PRINT_FORMAT(P, "Binding(#%d, %s: %s),\n", b.var.id,
                     b.name->text, pawIr_print_type(P->C, b.var.type));
    }
}

static void print_body(struct Printer *P, struct MatchBody body)
{
    PRINT_LITERAL(P, "Body(\n");
    ++P->indent;

    print_bindings(P, body.bindings);

    print_indentation(P);
    PRINT_FORMAT(P, "Result(hid=%d),\n", body.result->hdr.hid);

    --P->indent;
    print_indentation(P);
    PRINT_LITERAL(P, ")\n");
}

static void print_decision(struct Printer *P, struct Decision *dec);

static void print_var(struct Printer *P, struct MatchVar var)
{
    PRINT_FORMAT(P, "Var(#%d, %s),\n", var.id, pawIr_print_type(P->C, var.type));
}

static void print_vars(struct Printer *P, struct VariableList *vars)
{
    for (int i = 0; i < vars->count; ++i) {
        print_indentation(P);
        print_var(P, VariableList_get(vars, i));
    }
}

static void print_cases(struct Printer *P, struct CaseList *cases)
{
    struct MatchCase *pmc;
    K_LIST_FOREACH (cases, pmc) {
        print_indentation(P);
        PRINT_LITERAL(P, "Case(");
        switch (pmc->cons.kind) {
            case CONS_BOOL:
                PRINT_FORMAT(P, "%s", pmc->cons.value.u ? "true" : "false");
                break;
            case CONS_INT:
                PRINT_FORMAT(P, "%I", pmc->cons.value.i);
                break;
            case CONS_FLOAT:
                PRINT_FORMAT(P, "%f", pmc->cons.value.f);
                break;
            case CONS_STR:
                PRINT_FORMAT(P, "\"%s\"", V_TEXT(pmc->cons.value));
                break;
            case CONS_WILDCARD:
                PRINT_LITERAL(P, "_");
                break;
            case CONS_VARIANT:
                PRINT_FORMAT(P, "#%d", pmc->cons.variant.index);
                break;
            case CONS_TUPLE:
                PRINT_FORMAT(P, "%d-tuple", pmc->cons.tuple.elems->count);
                break;
            default:
                PRINT_LITERAL(P, "?");
        }
        PRINT_LITERAL(P, ") => ");
        if (pmc->vars->count > 0) {
            PRINT_LITERAL(P, "{\n");
            ++P->indent;
            print_vars(P, pmc->vars);
            --P->indent;
            print_indentation(P);
            PRINT_LITERAL(P, "} ");
        }
        print_decision(P, pmc->dec);
        PRINT_LITERAL(P, ",\n");
    }
}

static void print_decision(struct Printer *P, struct Decision *dec)
{
    if (dec == NULL)
        return;
    switch (dec->kind) {
        case DECISION_FAILURE:
            PRINT_LITERAL(P, "Failure");
            break;
        case DECISION_SUCCESS:
            PRINT_LITERAL(P, "Success(\n");
            ++P->indent;
            print_indentation(P);
            print_body(P, dec->success.body);
            --P->indent;
            print_indentation(P);
            PRINT_LITERAL(P, ")");
            break;
        case DECISION_GUARD:
            PRINT_LITERAL(P, "Guard(\n");
            ++P->indent;
            print_indentation(P);
            print_body(P, dec->guard.body);
            print_indentation(P);
            PRINT_LITERAL(P, "Rest => ");
            print_decision(P, dec->guard.rest);
            PRINT_LITERAL(P, "\n");
            --P->indent;
            print_indentation(P);
            PRINT_LITERAL(P, ")");
            break;
        case DECISION_MULTIWAY:
            PRINT_LITERAL(P, "Multiway(\n");
            ++P->indent;
            print_indentation(P);
            PRINT_LITERAL(P, "Test => ");
            print_var(P, dec->multi.test);
            print_cases(P, dec->multi.cases);
            print_indentation(P);
            PRINT_LITERAL(P, "Rest => ");
            print_decision(P, dec->multi.rest);
            PRINT_LITERAL(P, "\n");
            --P->indent;
            print_indentation(P);
            PRINT_LITERAL(P, ")");
    }
}

char const *pawP_print_decision(struct Compiler *C, struct Decision *dec)
{
    Buffer buf;
    paw_Env *P = ENV(C);
    pawL_init_buffer(P, &buf);

    struct Printer printer = {
        .buf = &buf,
        .P = P,
        .C = C,
    };
    print_indentation(&printer);
    print_decision(&printer, dec);

    pawL_push_result(P, &buf);
    return paw_str(P, -1);
}

#endif // PAW_DEBUG_EXTRA
