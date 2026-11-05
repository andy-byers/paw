
add_library(paw_middle STATIC)
target_sources(paw_middle
    PUBLIC  analysis.h
            eval.h
            layout.h
            match.h
    PRIVATE analysis.c
            decision.c
            dominance.c
            dropgen.c
            eval.c
            exhaustiveness.c
            kfold.c
            kprop.c
            layout.c
            liveness.c
            lower_hir.c
            monomorphize.c)

target_link_libraries(paw_middle
    PRIVATE paw_common
            paw_config
            paw_ir
            paw_mir)
