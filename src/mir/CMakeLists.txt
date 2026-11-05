
add_library(paw_mir STATIC)
target_sources(paw_mir
    PUBLIC  mir.h 
    PRIVATE mir.c)

target_link_libraries(paw_mir
    PRIVATE paw_common
            paw_config)


