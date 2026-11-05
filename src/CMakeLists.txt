
# Interface library containing configuration options needed by all modules
# in the project.
add_library(paw_config INTERFACE)
target_include_directories(paw_config
    INTERFACE ${PROJECT_SOURCE_DIR}/src)
target_compile_definitions(paw_config
    INTERFACE PAW_PRELUDE_PATH="${PAW_PRELUDE_PATH}"
              PAW_STDLIB_PATH="${PAW_STDLIB_PATH}"
              PAW_CLANG_PATH="${PAW_CLANG_PATH}"
              PAW_BINARY_DIR="${CMAKE_BINARY_DIR}"
              PAW_STRESS=${PAW_STRESS})

if(${CMAKE_C_COMPILER_ID} STREQUAL MSVC)
    target_compile_options(paw_config
            INTERFACE /W2)
else()
    target_compile_options(paw_config
            INTERFACE -Wall -Wextra -Wpedantic -Wsign-conversion
                      -Wno-unused-function -Wno-unused-parameter
                      -Wno-unused-variable)
    target_link_libraries(paw_config
            INTERFACE m)
endif()

add_subdirectory(ast)
add_subdirectory(codegen)
add_subdirectory(common)
add_subdirectory(compiler)
add_subdirectory(hir)
add_subdirectory(ir)
add_subdirectory(middle)
add_subdirectory(mir)
add_subdirectory(parse)
add_subdirectory(sema)

#
# Build the compiler driver
#

add_executable(pawc paw.c)
target_link_libraries(pawc
    PRIVATE paw_ast
            paw_codegen
            paw_config
            paw_hir
            paw_ir
            paw_middle
            paw_mir
            paw_parse
            paw_sema)

