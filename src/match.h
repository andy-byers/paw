// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_MATCH_H
#define PAW_MATCH_H

#include "compile.h"

struct MatchVar {
    struct IrType *type;
    int id;
};

struct Binding {
    struct MatchVar var;
    String *name;
};

enum ConstructorKind {
    CONS_BOOL,
    CONS_INT,
    CONS_FLOAT,
    CONS_STR,
    CONS_TUPLE,
    CONS_STRUCT,
    CONS_VARIANT,
    CONS_WILDCARD,
    CONS_REST,
};

struct Constructor {
    enum ConstructorKind kind;
    union {
        Value value;

        struct {
            struct IrTypeList *elems;
        } tuple;

        struct {
            struct IrType *type;
            int index;
        } variant;

        struct {
            struct IrType *type;
        } struct_;
    };
};

struct MatchBody {
    struct BindingList *bindings;
    struct HirExpr *result;
};

struct MatchCase {
    struct Constructor cons;
    struct VariableList *vars;
    struct Decision *dec;
};

enum DecisionKind {
    DECISION_GUARD,
    DECISION_MULTIWAY,
    DECISION_SUCCESS,
    DECISION_FAILURE,
};

struct Decision {
    enum DecisionKind kind;
    union {
        struct {
            struct HirExpr *cond;
            struct MatchBody body;
            struct Decision *rest;
        } guard;

        struct {
            struct MatchVar test;
            struct CaseList *cases;
            struct Decision *rest;
        } multi;

        struct {
            struct MatchBody body;
        } success;
    };
};

DEFINE_LIST(struct Compiler, variable_list_, VariableList, struct MatchVar)
DEFINE_LIST(struct Compiler, binding_list_, BindingList, struct Binding)
DEFINE_LIST(struct Compiler, case_list_, CaseList, struct MatchCase)

char const *pawP_print_decision(struct Compiler *C, struct Decision *dec);

#endif // PAW_MATCH_H
