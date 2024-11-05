// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "ir_type.h"
#include "mir.h"

struct MirScope *pawMir_get_scope(struct Mir *mir, MirScopeId id)
{
    return &K_LIST_GET(mir->scopes, id.value);
}

struct Mir *pawMir_new(struct Compiler *C, String *name, struct IrType *type, struct IrType *self, enum FuncKind fn_kind, paw_Bool is_native, paw_Bool is_pub, paw_Bool is_poly)
{
    struct Mir *mir = pawK_pool_alloc(ENV(C), C->pool, sizeof(struct Mir));
    *mir = (struct Mir){
        .scopes = pawMir_scope_list_new(C),
        .upvalues = pawMir_upvalue_list_new(C),
        .children = pawMir_body_list_new(C),
        .blocks = pawMir_block_list_new(C),
        .is_native = is_native,
        .is_poly = is_poly,
        .is_pub = is_pub,
        .fn_kind = fn_kind,
        .name = name,
        .type = type,
        .self = self,
    };
    return mir;
}

struct MirRegister *pawMir_new_register(struct Compiler *C, int value, struct IrType *type)
{
    struct MirRegister *r = pawK_pool_alloc(ENV(C), C->pool, sizeof(struct MirRegister));
    *r = (struct MirRegister){
        .value = value,
        .type = type,
    };
    return r;
}

struct MirInstruction *pawMir_new_instruction(struct Compiler *C, enum MirInstructionKind kind)
{
    struct MirInstruction *instr = pawK_pool_alloc(ENV(C), C->pool, sizeof(struct MirInstruction));
    *instr = (struct MirInstruction){
        .hdr.kind = kind,
    };
    return instr;
}

struct MirTerminator *pawMir_new_terminator(struct Compiler *C, enum MirTerminatorKind kind)
{
    struct MirTerminator *term = pawK_pool_alloc(ENV(C), C->pool, sizeof(struct MirTerminator));
    *term = (struct MirTerminator){
        .hdr.kind = kind,
    };
    return term;
}

struct MirTerminator *pawMir_new_return(struct Compiler *C, struct MirRegister *value)
{
    struct MirTerminator *r = pawMir_new_terminator(C, kMirReturn);
    MirGetReturn(r)->value = value;
    return r;
}

struct MirTerminator *pawMir_new_goto(struct Compiler *C, MirBlockId bid)
{
    struct MirTerminator *r = pawMir_new_terminator(C, kMirGoto);
    MirGetGoto(r)->target = bid;
    return r;
}

struct MirBlock *pawMir_new_block(struct Compiler *C, MirBlockId bid)
{
    struct MirBlock *block = pawK_pool_alloc(ENV(C), C->pool, sizeof(struct MirBlock));
    *block = (struct MirBlock){
        .code = pawMir_instruction_list_new(C),
        .bid = bid,
    };
    return block;
}


static void AcceptLocal(struct MirVisitor *V, struct MirLocal *t)
{
    pawMir_visit_register(V, t->output);
    pawMir_visit_register(V, t->target);
}

static void AcceptGlobal(struct MirVisitor *V, struct MirGlobal *t)
{
    pawMir_visit_register(V, t->output);
}

static void AcceptConstant(struct MirVisitor *V, struct MirConstant *t)
{
    pawMir_visit_register(V, t->output);
}

static void AcceptAggregate(struct MirVisitor *V, struct MirAggregate *t)
{
    pawMir_visit_register(V, t->output);
}

static void AcceptExplode(struct MirVisitor *V, struct MirExplode *t)
{
    pawMir_visit_register(V, t->input);
    pawMir_visit_register_list(V, t->outputs);
}

static void AcceptContainer(struct MirVisitor *V, struct MirContainer *t)
{
    pawMir_visit_register(V, t->output);
}

static void AcceptUpvalue(struct MirVisitor *V, struct MirUpvalue *t)
{
    pawMir_visit_register(V, t->output);
}

static void AcceptSetUpvalue(struct MirVisitor *V, struct MirSetUpvalue *t)
{
    pawMir_visit_register(V, t->value);
}

static void AcceptAllocLocal(struct MirVisitor *V, struct MirAllocLocal *t)
{
    pawMir_visit_register(V, t->output);
}

static void AcceptFreeLocal(struct MirVisitor *V, struct MirFreeLocal *t)
{
    pawMir_visit_register(V, t->reg);
}

static void AcceptEnterScope(struct MirVisitor *V, struct MirEnterScope *t)
{
    PAW_UNUSED(V);
    PAW_UNUSED(t);
}

static void AcceptLeaveScope(struct MirVisitor *V, struct MirLeaveScope *t)
{
    PAW_UNUSED(V);
    PAW_UNUSED(t);
}

static void AcceptAssign(struct MirVisitor *V, struct MirAssign *t)
{
    pawMir_visit_register(V, t->rhs);
}

static void AcceptCall(struct MirVisitor *V, struct MirCall *t)
{
    pawMir_visit_register(V, t->output);
    pawMir_visit_register(V, t->target);
    pawMir_visit_register_list(V, t->args);
}

static void AcceptCast(struct MirVisitor *V, struct MirCast *t)
{
    pawMir_visit_register(V, t->output);
}

static void AcceptClosure(struct MirVisitor *V, struct MirClosure *t)
{
    pawMir_visit_register(V, t->output);
}

static void AcceptGetElement(struct MirVisitor *V, struct MirGetElement *t)
{
    pawMir_visit_register(V, t->output);
    pawMir_visit_register(V, t->object);
    pawMir_visit_register(V, t->key);
}

static void AcceptSetElement(struct MirVisitor *V, struct MirSetElement *t)
{
    pawMir_visit_register(V, t->object);
    pawMir_visit_register(V, t->key);
    pawMir_visit_register(V, t->value);
}

static void AcceptGetRange(struct MirVisitor *V, struct MirGetRange *t)
{
    pawMir_visit_register(V, t->output);
    pawMir_visit_register(V, t->object);
    pawMir_visit_register(V, t->lower);
    pawMir_visit_register(V, t->upper);
}

static void AcceptSetRange(struct MirVisitor *V, struct MirSetRange *t)
{
    pawMir_visit_register(V, t->object);
    pawMir_visit_register(V, t->lower);
    pawMir_visit_register(V, t->upper);
    pawMir_visit_register(V, t->value);
}

static void AcceptGetField(struct MirVisitor *V, struct MirGetField *t)
{
    pawMir_visit_register(V, t->output);
    pawMir_visit_register(V, t->object);
}

static void AcceptSetField(struct MirVisitor *V, struct MirSetField *t)
{
    pawMir_visit_register(V, t->object);
    pawMir_visit_register(V, t->value);
}

static void AcceptUnaryOp(struct MirVisitor *V, struct MirUnaryOp *t)
{
    pawMir_visit_register(V, t->val);
    pawMir_visit_register(V, t->output);
}

static void AcceptBinaryOp(struct MirVisitor *V, struct MirBinaryOp *t)
{
    pawMir_visit_register(V, t->lhs);
    pawMir_visit_register(V, t->rhs);
    pawMir_visit_register(V, t->output);
}

static void AcceptDiscard(struct MirVisitor *V, struct MirDiscard *t)
{
    pawMir_visit_register(V, t->reg);
}


static void AcceptReturn(struct MirVisitor *V, struct MirReturn *t)
{
    if (t->value != NULL) pawMir_visit_register(V, t->value);
}

static void AcceptGoto(struct MirVisitor *V, struct MirGoto *t)
{
    PAW_UNUSED(V);
    PAW_UNUSED(t);
}

static void AcceptForLoop(struct MirVisitor *V, struct MirForLoop *t)
{
    pawMir_visit_register(V, t->var);
    pawMir_visit_register(V, t->iter);
    pawMir_visit_register(V, t->end);
    pawMir_visit_register(V, t->step);
}

static void AcceptBranch(struct MirVisitor *V, struct MirBranch *t)
{
    pawMir_visit_register(V, t->cond);
}

static void AcceptSwitch(struct MirVisitor *V, struct MirSwitch *t)
{
    pawMir_visit_register(V, t->discr);
}

#define VISITOR_CALL(V, name, x) ((V)->Visit##name != NULL ? (V)->Visit##name(V, x) : 1)

#define VISITOR_POSTCALL(V, name, x) ((V)->PostVisit##name != NULL ? (V)->PostVisit##name(V, x) : (void)0)
#define DEFINE_VISITOR_CASES(X) case kMir##X: { \
        struct Mir##X *v = MirGet##X(node); \
        if (VISITOR_CALL(V, X, v)) Accept##X(V, v); \
        VISITOR_POSTCALL(V, X, v); \
    } \
    break;

void pawMir_visit_register(struct MirVisitor *V, struct MirRegister *node)
{
    paw_assert(node != NULL);
    if (V->VisitRegister(V, node)) {
        V->PostVisitRegister(V, node);
    }
}

void pawMir_visit_instruction(struct MirVisitor *V, struct MirInstruction *node)
{
    paw_assert(node != NULL);
    if (!V->VisitInstruction(V, node)) return;

    switch (MIR_KINDOF(node)) {
        MIR_INSTRUCTION_LIST(DEFINE_VISITOR_CASES)
    }

    V->PostVisitInstruction(V, node);
}

void pawMir_visit_terminator(struct MirVisitor *V, struct MirTerminator *node)
{
    paw_assert(node != NULL);
    if (!V->VisitTerminator(V, node)) return;

    switch (MIR_KINDOF(node)) {
        MIR_TERMINATOR_LIST(DEFINE_VISITOR_CASES)
    }

    V->PostVisitTerminator(V, node);
}

void pawMir_visit_block(struct MirVisitor *V, struct MirBlock *b)
{
    paw_assert(b != NULL);
    if (!V->VisitBlock(V, b)) return;

    pawMir_visit_instruction_list(V, b->code);
    pawMir_visit_terminator(V, b->term);

    V->PostVisitBlock(V, b);
}

paw_Bool default_visit_terminator(struct MirVisitor *V, struct MirTerminator *node) {return PAW_TRUE;}
paw_Bool default_visit_instruction(struct MirVisitor *V, struct MirInstruction *node) {return PAW_TRUE;}
paw_Bool default_visit_block(struct MirVisitor *V, struct MirBlock *node) {return PAW_TRUE;}
paw_Bool default_visit_register(struct MirVisitor *V, struct MirRegister *node) {return PAW_TRUE;}
void default_post_visit_terminator(struct MirVisitor *V, struct MirTerminator *node) {}
void default_post_visit_instruction(struct MirVisitor *V, struct MirInstruction *node) {}
void default_post_visit_block(struct MirVisitor *V, struct MirBlock *node) {}
void default_post_visit_register(struct MirVisitor *V, struct MirRegister *node) {}

void pawMir_visitor_init(struct MirVisitor *V, struct Compiler *C, void *ud)
{
    *V = (struct MirVisitor){
        .ud = ud,
        .C = C,

        .VisitBlock = default_visit_block,
        .VisitInstruction = default_visit_instruction,
        .VisitTerminator = default_visit_terminator,
        .VisitRegister = default_visit_register,

        .PostVisitBlock = default_post_visit_block,
        .PostVisitInstruction = default_post_visit_instruction,
        .PostVisitTerminator = default_post_visit_terminator,
        .PostVisitRegister = default_post_visit_register,
    };
}

void pawMir_visit(struct MirVisitor *V, struct Mir *mir)
{
    pawMir_visit_block_list(V, mir->blocks);
    for (int i = 0; i < mir->children->count; ++i) {
        pawMir_visit(V, K_LIST_GET(mir->children, i));
    }
}

#define DEFINE_LIST_VISITOR(name, T) \
    void pawMir_visit_##name##_list(struct MirVisitor *V, struct Mir##T##List *list) { \
        if (list == NULL) return; \
        for (int i = 0; i < list->count; ++i) { \
            pawMir_visit_##name(V, K_LIST_GET(list, i)); \
        } \
    }
DEFINE_LIST_VISITOR(block, Block)
DEFINE_LIST_VISITOR(instruction, Instruction)
DEFINE_LIST_VISITOR(register, Register)
#undef DEFINE_LIST_VISITOR


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

static void dump_instruction(struct Printer *, struct MirInstruction *);
static void dump_terminator(struct Printer *, struct MirTerminator *);

static void indentation(struct Printer *P)
{
    for (int i = 0; i < P->indent; ++i) {
        PRINT_LITERAL(P, "  ");
    }
}

#define DUMP_FMT(P, ...) (indentation(P), pawL_add_fstring(ENV(P), (P)->buf, __VA_ARGS__))
#define DUMP_MSG(P, msg) (indentation(P), pawL_add_string(ENV(P), (P)->buf, msg))

static void dump_instruction_list(struct Printer *P, struct MirInstructionList *list)
{
    for (int i = 0; i < list->count; ++i) {
        dump_instruction(P, list->data[i]);
    }
}

static void dump_register(struct Printer *P, const char *name, struct MirRegister *r)
{
    DUMP_FMT(P, "%s: reg %d (%s)\n", name, r->value, pawIr_print_type(P->C, r->type));
    paw_pop(P->P, 1);
}

static void dump_instruction(struct Printer *P, struct MirInstruction *instr)
{
    const enum MirInstructionKind k = MIR_KINDOF(instr);
    DUMP_MSG(P, kMirInstructionNames[k]);
    PRINT_LITERAL(P, " {\n");
    ++P->indent;

    switch (k) {
        case kMirAllocLocal: {
            struct MirAllocLocal *t = MirGetAllocLocal(instr);
            dump_register(P, "output", t->output);
            DUMP_FMT(P, "name: %s\n", t->name->text);
            break;
        }
        case kMirFreeLocal: {
            struct MirFreeLocal *t = MirGetFreeLocal(instr);
            dump_register(P, "reg", t->reg);
            break;
        }
        case kMirEnterScope: {
            struct MirEnterScope *t = MirGetEnterScope(instr);
            DUMP_FMT(P, "scope_id: %d\n", t->scope_id.value);
            break;
        }
        case kMirLeaveScope: {
            struct MirLeaveScope *t = MirGetLeaveScope(instr);
            DUMP_FMT(P, "scope_id: %d\n", t->scope_id.value);
            break;
        }
        case kMirDiscard: {
            struct MirDiscard *t = MirGetDiscard(instr);
            dump_register(P, "reg", t->reg);
            break;
        }
        case kMirLocal: {
            struct MirLocal *t = MirGetLocal(instr);
            dump_register(P, "output", t->output);
            dump_register(P, "target", t->target);
            break;
        }
        case kMirUpvalue: {
            struct MirUpvalue *t = MirGetUpvalue(instr);
            dump_register(P, "output", t->output);
            DUMP_FMT(P, "index: %d\n", t->index);
            break;
        }
        case kMirGlobal: {
            struct MirGlobal *t = MirGetGlobal(instr);
            struct HirDecl *decl = pawHir_get_decl(P->C, IR_TYPE_DID(t->output->type));
            const char *type = pawIr_print_type(P->C, t->output->type);
            dump_register(P, "output", t->output);
            DUMP_FMT(P, "name: %s\n", decl->hdr.name->text);
            DUMP_FMT(P, "type: %s\n", type);
            --ENV(P->C)->top.p; // pop 'type'
            break;
        }
        case kMirConstant: {
            struct MirConstant *t = MirGetConstant(instr);
            dump_register(P, "output", t->output);
            switch (t->code) {
                case PAW_TBOOL:
                    DUMP_MSG(P, "type: bool\n");
                    DUMP_FMT(P, "value: %s\n", V_TRUE(t->value) ? "true" : "false");
                    break;
                case PAW_TINT:
                    DUMP_MSG(P, "type: int\n");
                    DUMP_FMT(P, "value: %I\n", V_INT(t->value));
                    break;
                case PAW_TFLOAT:
                    DUMP_MSG(P, "type: float\n");
                    DUMP_FMT(P, "value: %f\n", V_FLOAT(t->value));
                    break;
                case PAW_TSTR:
                    DUMP_MSG(P, "type: str\n");
                    DUMP_FMT(P, "value: \"%s\"\n", V_TEXT(t->value));
                    break;
            }
            break;
        }
        case kMirSetUpvalue: {
            struct MirSetUpvalue *t = MirGetSetUpvalue(instr);
            dump_register(P, "value", t->value);
            DUMP_FMT(P, "index: %d\n", t->index);
            break;
        }
        case kMirExplode: {
            struct MirExplode *t = MirGetExplode(instr);
            dump_register(P, "input", t->input);
            for (int i = 0; i < t->outputs->count; ++i) {
                dump_register(P, "output", K_LIST_GET(t->outputs, i));
            }
            break;
        }
        case kMirContainer: {
            struct MirContainer *t = MirGetContainer(instr);
            dump_register(P, "output", t->output);
            break;
        }
        case kMirAggregate: {
            struct MirAggregate *t = MirGetAggregate(instr);
            dump_register(P, "output", t->output);
            break;
        }
        case kMirAssign: {
            struct MirAssign *t = MirGetAssign(instr);
            DUMP_FMT(P, "is_upvalue: %d\n", t->is_upvalue);
            DUMP_FMT(P, "place: %d\n", t->place);
            dump_register(P, "rhs", t->rhs);
            break;
        }
        case kMirCall: {
            struct MirCall *t = MirGetCall(instr);
            dump_register(P, "output", t->output);
            dump_register(P, "target", t->target);
            for (int i = 0; i < t->args->count; ++i) {
                dump_register(P, "arg", K_LIST_GET(t->args, i));
            }
            break;
        }
        case kMirCast: {
            struct MirCast *t = MirGetCast(instr);
            dump_register(P, "output", t->output);
            dump_register(P, "target", t->target);
            switch (t->type) {
                case PAW_TBOOL:
                    DUMP_MSG(P, "type: bool\n");
                    break;
                case PAW_TINT:
                    DUMP_MSG(P, "type: int\n");
                    break;
                case PAW_TFLOAT:
                    DUMP_MSG(P, "type: float\n");
                    break;
                case PAW_TSTR:
                    DUMP_MSG(P, "type: str\n");
                    break;
            }
            break;
        }
        case kMirClosure: {
            struct MirClosure *t = MirGetClosure(instr);
            dump_register(P, "output", t->output);
            break;
        }
        case kMirGetElement: {
            struct MirGetElement *t = MirGetGetElement(instr);
            dump_register(P, "object", t->object);
            dump_register(P, "key", t->key);
            break;
        }
        case kMirSetElement: {
            struct MirSetElement *t = MirGetSetElement(instr);
            dump_register(P, "object", t->object);
            dump_register(P, "key", t->key);
            dump_register(P, "value", t->value);
            break;
        }
        case kMirGetRange: {
            struct MirGetRange *t = MirGetGetRange(instr);
            dump_register(P, "object", t->object);
            dump_register(P, "lower", t->lower);
            dump_register(P, "upper", t->upper);
            break;
        }
        case kMirSetRange: {
            struct MirSetRange *t = MirGetSetRange(instr);
            dump_register(P, "object", t->object);
            dump_register(P, "lower", t->lower);
            dump_register(P, "upper", t->upper);
            dump_register(P, "value", t->value);
            break;
        }
        case kMirGetField: {
            struct MirGetField *t = MirGetGetField(instr);
            dump_register(P, "object", t->object);
            DUMP_FMT(P, "index: %d\n", t->index);
            break;
        }
        case kMirSetField: {
            struct MirSetField *t = MirGetSetField(instr);
            dump_register(P, "object", t->object);
            DUMP_FMT(P, "index: %d\n", t->index);
            dump_register(P, "value", t->value);
            break;
        }
        case kMirUnaryOp: {
            struct MirUnaryOp *t = MirGetUnaryOp(instr);
            DUMP_FMT(P, "op: %s\n", paw_unop_name(t->op));
            dump_register(P, "output", t->output);
            dump_register(P, "val", t->val);
            break;
        }
        case kMirBinaryOp: {
            struct MirBinaryOp *t = MirGetBinaryOp(instr);
            DUMP_FMT(P, "op: %s\n", paw_binop_name(t->op));
            dump_register(P, "output", t->output);
            dump_register(P, "lhs", t->lhs);
            dump_register(P, "rhs", t->rhs);
            break;
        }
    }

    --P->indent;
    DUMP_MSG(P, "}\n");
}

static void dump_terminator(struct Printer *P, struct MirTerminator *term)
{
    if (term == NULL) {
        DUMP_MSG(P, "(null)\n");
        return;
    }
    const enum MirTerminatorKind k = MIR_KINDOF(term);
    DUMP_MSG(P, kMirTerminatorNames[k]);
    PRINT_LITERAL(P, " {\n");
    ++P->indent;

    switch (k) {
        case kMirReturn: {
            struct MirReturn *t = MirGetReturn(term);
            if (t->value != NULL) {
                dump_register(P, "value", t->value);
            } else {
                DUMP_FMT(P, "value: (null)\n");
            }
            break;
        }
        case kMirForLoop: {
            struct MirForLoop *t = MirGetForLoop(term);
            dump_register(P, "iter", t->iter);
            dump_register(P, "end", t->end);
            dump_register(P, "step", t->step);
            DUMP_FMT(P, "for_kind: %d\n", t->for_kind);
            DUMP_FMT(P, "then_arm => BB #%d\n", t->then_arm.value);
            DUMP_FMT(P, "else_arm => BB #%d\n", t->else_arm.value);
            break;
        }
        case kMirBranch: {
            struct MirBranch *t = MirGetBranch(term);
            dump_register(P, "cond", t->cond);
            DUMP_FMT(P, "then_arm => BB #%d\n", t->then_arm.value);
            DUMP_FMT(P, "else_arm => BB #%d\n", t->else_arm.value);
            break;
        }
        case kMirSwitch: {
            struct MirSwitch *t = MirGetSwitch(term);
            dump_register(P, "discr", t->discr);
            for (int i = 0; i < t->arms->count; ++i) {
                struct MirSwitchArm arm = K_LIST_GET(t->arms, i);
                DUMP_FMT(P, "arm %d => BB #%d\n", arm.value, arm.bid.value);
            }
            DUMP_FMT(P, "otherwise => BB #%d\n", t->otherwise.value);
            break;
        }
        case kMirGoto: {
            DUMP_FMT(P, "target: BB #%d\n", MirGetGoto(term)->target.value);
            break;
        }
    }

    --P->indent;
    DUMP_MSG(P, "}\n");
}

static void dump_block(struct Printer *P, struct MirBlock *block)
{
    DUMP_MSG(P, "MirBlock {\n");
    ++P->indent;

    DUMP_FMT(P, "bid: #%d\n", block->bid.value);
    dump_instruction_list(P, block->code);
    dump_terminator(P, block->term);

    --P->indent;
    DUMP_MSG(P, "}\n");
}

static void dump_mir(struct Printer *P, struct Mir *mir)
{
    DUMP_MSG(P, "Mir {\n");
    ++P->indent;

    for (int i = 0; i < mir->blocks->count; ++i) {
        struct MirBlock *bb = K_LIST_GET(mir->blocks, i);
        dump_block(P, bb);
    }

    --P->indent;
    DUMP_MSG(P, "}\n");
}

const char *pawMir_dump(struct Compiler *C, struct Mir *mir)
{
    Buffer buf;
    paw_Env *P = ENV(C);
    pawL_init_buffer(P, &buf);

    dump_mir(&(struct Printer){
                .P = ENV(C),
                .buf = &buf,
                .C = C,
            }, mir);

    for (int i = 0; i < mir->children->count; ++i) {
        struct Mir *child = K_LIST_GET(mir->children, i);
        dump_mir(&(struct Printer){
                    .P = ENV(C),
                    .buf = &buf,
                    .C = C,
                }, child);
    }

    pawL_push_result(P, &buf);
    return paw_string(P, -1);
}

