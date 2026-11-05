
add_library(paw_compiler STATIC)
target_sources(paw_compiler
    PUBLIC  api.h
            error.h
            call.h
            code.h
            compiler.h
            config.h
            debug.h
            env.h
            glue.h
            lib.h
            mangle.h
            mem.h
            os.h
            pool.h
            prefix.h
            source.h
            stats.h
            type_folder.h
    PRIVATE api.c
            call.c
            code.c
            codegen.c
            compiler.c
            debug.c
            env.c
            error.c
            lib.c
            mangle.c
            mem.c
            os.c
            pool.c
            source.c
            stats.c
            type_folder.c)

target_link_libraries(paw_compiler
    PRIVATE paw_codegen
            paw_common
            paw_config
            paw_parse
            paw_hir
            paw_ir
            paw_middle)
