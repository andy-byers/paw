// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "source.h"
#include "compile.h"
#include "env.h"

void pawSrc_add_location(paw_Env *P, struct Buffer *b, struct SourceLoc loc)
{
    pawL_add_fstring(P, b, "%d:%d", loc.line, loc.column);
}

SpanRef pawSrc_create_ref(struct Compiler *C, struct SourceSpan referrent)
{
    SpanRef const ref = {C->source_span_refs->count};
    SourceSpanRefs_insert(C, C->source_span_refs, ref, referrent);
    return ref;
}

struct SourceSpan pawSrc_follow_ref(struct Compiler *C, SpanRef ref)
{
    return *SourceSpanRefs_get(C, C->source_span_refs, ref);
}
