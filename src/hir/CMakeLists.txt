
add_library(paw_hir STATIC)
target_sources(paw_hir
    PUBLIC  hir.h
    PRIVATE hir.c)

target_link_libraries(paw_hir
    PRIVATE paw_common
            paw_config)
