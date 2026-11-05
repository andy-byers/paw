
add_library(paw_common STATIC)
target_sources(paw_common
    PUBLIC  auxlib.h
            list.h
            map.h
            str.h
            util.h
    PRIVATE auxlib.c
            list.c
            map.c
            str.c
            util.c)

target_link_libraries(paw_common
    PRIVATE paw_compiler # TODO: needed b/c data structures need Pool allocator. make them generic on allocator type (add allocator macro param)
            paw_config)

