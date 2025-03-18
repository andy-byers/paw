
#include "source.h"

void pawSrc_add_location(paw_Env *P, struct Buffer *b, struct SourceLoc loc)
{
    pawL_add_fstring(P, b, "%d:%d", loc.line, loc.column);
}
