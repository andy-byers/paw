
add_library(paw_sema STATIC)
target_sources(paw_sema
    PUBLIC  impl.h
            resolver.h
            solve.h
            trait.h
            unify.h
    PRIVATE collect_items.c
            impl.c
            import.c
            instantiate.c
            lower_ast.c
            lower_type.c
            resolver.c
            solve.c
            trait.c
            typeck.c
            unify.c)

target_link_libraries(paw_sema
    PRIVATE paw_common
            paw_config)
