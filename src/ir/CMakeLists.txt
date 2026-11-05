
add_library(paw_ir STATIC)
target_sources(paw_ir
    PUBLIC  type.h
            value.h
    PRIVATE type.c
            value.c)

target_link_libraries(paw_ir
    PRIVATE paw_common
            paw_config
            paw_sema)
