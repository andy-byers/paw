// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_SOURCE_H
#define PAW_SOURCE_H

#include "auxlib.h"

struct SourceLoc {
    int line;
    int column;
};

enum SpanKind {
    SRC_SPAN_RANGE,
    SRC_SPAN_REF,
};

enum SpanRefKind {
    SPAN_REF_TRAIT_SELF,
    SPAN_REF_QUESTION_MARK,
    SPAN_REF_FOR_LOOP,
    SPAN_REF_RANGE,
    SPAN_REF_FSTRING,
};

typedef struct SpanRef {
    int value;
} SpanRef;

struct SourceSpan {
    enum SpanKind kind;
    union {
        struct {
            struct SourceLoc start;
            struct SourceLoc end;
        } range;

        struct {
            enum SpanRefKind kind;
            SpanRef value;
        } ref;
    };
};

static struct SourceLoc SourceSpan_range_start(struct SourceSpan span)
{
    paw_assert(span.kind == SRC_SPAN_RANGE);
    return span.range.start;
}

static struct SourceLoc SourceSpan_range_end(struct SourceSpan span)
{
    paw_assert(span.kind == SRC_SPAN_RANGE);
    return span.range.end;
}

static enum SpanRefKind SourceSpan_ref_kind(struct SourceSpan span)
{
    paw_assert(span.kind == SRC_SPAN_REF);
    return span.ref.kind;
}

static SpanRef SourceSpan_ref_value(struct SourceSpan span)
{
    paw_assert(span.kind == SRC_SPAN_REF);
    return span.ref.value;
}

static struct SourceSpan SourceSpan_from_range(struct SourceLoc start, struct SourceLoc end)
{
    struct SourceSpan const span = {
        .kind = SRC_SPAN_RANGE,
        .range.start = start,
        .range.end = end,
    };
    return span;
}

static struct SourceSpan SourceSpan_from_ref(SpanRef ref, enum SpanRefKind kind)
{
    struct SourceSpan span = {
        .kind = SRC_SPAN_REF,
        .ref.value = ref,
        .ref.kind = kind,
    };
    return span;
}

static inline void pawSrc_init_location(struct SourceLoc *ploc)
{
    *ploc = (struct SourceLoc){1, 1};
}

void pawSrc_add_location(paw_Env *P, struct Buffer *b, struct SourceLoc loc);

struct Compiler;

SpanRef pawSrc_create_ref(struct Compiler *C, struct SourceSpan referrent);
struct SourceSpan pawSrc_follow_ref(struct Compiler *C, SpanRef ref);

#endif // PAW_SOURCE_H
