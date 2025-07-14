
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

// dominance.c: Computes the dominance tree using the Lengauer-Tarjan algorithm
//
// Uses the description in "Advanced Compiler Design and Implementation", by
// Steven S. Muchnick.

#include "mir.h"

#define PRED(D, v) (mir_bb_data((D)->mir, v)->predecessors)
#define IDOM(D, v) CHECK_EXP(0 <= (v).value && (v).value < (D)->idom->count, \
                             &K_LIST_AT((D)->idom, (v).value))

struct DominanceState {
    struct Compiler *C;
    struct MirBlockList *idom;
    struct Mir *mir;
    int N;
};

static MirBlock intersect(struct DominanceState *D, MirBlock x, MirBlock y)
{
    // Walk up the dominator tree until a common parent is found.
    while (!MIR_ID_EQUALS(x, y)) {
        while (x.value > y.value)
            x = *IDOM(D, x);
        while (y.value > x.value)
            y = *IDOM(D, y);
    }
    return x;
}

static void compute_dominance(struct DominanceState *D, MirBlock r)
{
    *IDOM(D, r) = r;

    paw_Bool changed = PAW_TRUE;
    while (changed) {
        changed = PAW_FALSE;
        for (MirBlock b = MIR_BB(1); b.value < D->N; ++b.value) {
            struct MirBlockData *data = mir_bb_data(D->mir, b);
            struct MirBlockList *pred = data->predecessors;
            MirBlock idom = MIR_INVALID_BB;
            for (int i = 0; i < pred->count; ++i) {
                MirBlock const p = MirBlockList_get(pred, i);
                if (!MIR_ID_EXISTS(*IDOM(D, p)))
                    continue;
                if (!MIR_ID_EXISTS(idom)) {
                    idom = p;
                } else {
                    idom = intersect(D, idom, p);
                }
            }
            if (!MIR_ID_EQUALS(*IDOM(D, b), idom)) {
                changed = PAW_TRUE;
                *IDOM(D, b) = idom;
            }
        }
    }

    *IDOM(D, r) = MIR_INVALID_BB;
}

struct MirBlockList *pawMir_compute_dominance_tree(struct Compiler *C, struct Mir *mir)
{
    struct DominanceState D = {
        .N = mir->blocks->count,
        .idom = MirBlockList_new(mir),
        .mir = mir,
        .C = C,
    };

    MirBlockList_reserve(mir, D.idom, D.N);
    for (int i = 0; i < D.N; ++i) {
        MirBlockList_push(mir, D.idom, MIR_INVALID_BB);
    }

    compute_dominance(&D, MIR_ROOT_BB);
    return D.idom;
}

static void push_unique_bb(struct Mir *mir, struct MirBlockList *df, MirBlock b)
{
    MirBlock const *pb;
    K_LIST_FOREACH(df, pb) {
        if (MIR_ID_EQUALS(b, *pb))
            return;
    }
    MirBlockList_push(mir, df, b);
}

struct MirBucketList *pawMir_compute_dominance_frontiers(struct Compiler *C, struct Mir *mir, struct MirBlockList *idom)
{
    int const N = idom->count;
    struct MirBucketList *result = MirBucketList_new(mir);
    MirBucketList_reserve(mir, result, N);
    for (int i = 0; i < N; ++i) {
        struct MirBlockList *df = MirBlockList_new(mir);
        MirBucketList_push(mir, result, df);
    }

    for (MirBlock b = MIR_ROOT_BB; b.value < N; ++b.value) {
        MirBlock const target = MirBlockList_get(idom, b.value);
        struct MirBlockData *data = mir_bb_data(mir, b);
        struct MirBlockList *pred = data->predecessors;
        if (pred->count < 2)
            continue;
        MirBlock const *pp;
        K_LIST_FOREACH (pred, pp) {
            MirBlock runner = *pp;
            while (!MIR_ID_EQUALS(runner, target)) {
                struct MirBlockList *df = MirBucketList_get(result, runner.value);
                push_unique_bb(mir, df, b);
                runner = MirBlockList_get(idom, runner.value);
            }
        }
    }
    return result;
}

#ifdef PAW_DEBUG_EXTRA

#include <stdio.h>

static void debug_idom(struct DominanceState *D, struct MirBlockList *result)
{
    printf("idom = [\n");
    for (int i = 0; i < result->count; ++i) {
        MirBlock const b = MirBlockList_get(result, i);
        printf("  bb%d: bb%d\n", i, b.value);
    }
    printf("]\n");
}

static void debug_df(struct DominanceState *D, struct MirBucketList *result)
{
    printf("df = {\n");
    for (int i = 0; i < result->count; ++i) {
        struct MirBlockList *df = MirBucketList_get(result, i);
        printf("  bb%d: [", i);
        for (int j = 0; j < df->count; ++j) {
            MirBlock const b = MirBlockList_get(df, j);
            if (j > 0)
                printf(", ");
            printf("bb%d", b.value);
        }
        printf("]\n");
    }
    printf("}\n");
}

#endif // PAW_DEBUG_EXTRA
