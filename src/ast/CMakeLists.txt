
add_library(paw_ast STATIC)
target_sources(paw_ast
    PUBLIC  ast.h
    PRIVATE ast.c)

target_link_libraries(paw_ast
    PRIVATE paw_common
            paw_config)


