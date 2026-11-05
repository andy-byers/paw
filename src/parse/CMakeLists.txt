
add_library(paw_parse STATIC)
target_sources(paw_parse
    PUBLIC  lex.h 
            parse.h
    PRIVATE lex.c 
            parse.c)

target_link_libraries(paw_parse
    PRIVATE paw_ast
            paw_common
            paw_config)
