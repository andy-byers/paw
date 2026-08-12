
cmake_minimum_required(VERSION 3.14)
project(paw
    LANGUAGES C CXX
    VERSION 0.0.1)
 
set(CMAKE_C_STANDARD 11) 
set(CMAKE_C_STANDARD_REQUIRED ON) 
 
set(MAIN_PROJECT OFF)
if (CMAKE_CURRENT_SOURCE_DIR STREQUAL CMAKE_SOURCE_DIR)
    set(MAIN_PROJECT ON)
endif()

set(PAW_CLANG_PATH "" CACHE PATH "Path to clang binary")
set(PAW_BINARY_DIR "" CACHE PATH "Location of compilation artifacts")

# TODO: installation support
set(PAW_ROOT_DIR ${CMAKE_BINARY_DIR}/src/codegen)

#find_package(LLVM REQUIRED CONFIG)
#add_definitions(${LLVM_DEFINITIONS})
#llvm_map_components_to_libnames(LLVM_LIBS native passes support)

option(PAW_BuildFuzzers "Build the fuzz targets" OFF)
option(PAW_BuildTests "Build the tests" ${MAIN_PROJECT})

set(PAW_PRELUDE_PATH "${CMAKE_SOURCE_DIR}/std/prelude.paw" CACHE STRING "Path to prelude module")
set(PAW_STDLIB_PATH "${CMAKE_SOURCE_DIR}/std/" CACHE STRING "Path to standard library source files")
set(PAW_STRESS 0 CACHE STRING "Enable expensive checks (supports up to level 2)")
 
add_library(paw_context INTERFACE)
if(${CMAKE_C_COMPILER_ID} STREQUAL MSVC)
    target_compile_options(paw_context
            INTERFACE /W2)
else()
    target_compile_options(paw_context
            INTERFACE -Wall -Wextra -Wpedantic -Wsign-conversion
                      -Wno-unused-function)
    target_link_libraries(paw_context
            INTERFACE m)
endif()

add_subdirectory(src/codegen)

set(PAW_SOURCE_DIR ${PROJECT_SOURCE_DIR})

set(PAW_HEADER_FILES
    ${PAW_SOURCE_DIR}/src/analysis.h
    ${PAW_SOURCE_DIR}/src/api.h
    ${PAW_SOURCE_DIR}/src/ast.h
    ${PAW_SOURCE_DIR}/src/auxlib.h
    ${PAW_SOURCE_DIR}/src/call.h
    ${PAW_SOURCE_DIR}/src/code.h
    ${PAW_SOURCE_DIR}/src/compile.h
    ${PAW_SOURCE_DIR}/src/debug.h
    ${PAW_SOURCE_DIR}/src/env.h
    ${PAW_SOURCE_DIR}/src/error.h
    ${PAW_SOURCE_DIR}/src/eval.h
    ${PAW_SOURCE_DIR}/src/hir.h
    ${PAW_SOURCE_DIR}/src/ir_type.h
    ${PAW_SOURCE_DIR}/src/impl.h
    ${PAW_SOURCE_DIR}/src/layout.h
    ${PAW_SOURCE_DIR}/src/lex.h
    ${PAW_SOURCE_DIR}/src/lib.h
    ${PAW_SOURCE_DIR}/src/mangle.h
    ${PAW_SOURCE_DIR}/src/map.h
    ${PAW_SOURCE_DIR}/src/match.h
    ${PAW_SOURCE_DIR}/src/mem.h
    ${PAW_SOURCE_DIR}/src/mir.h
    ${PAW_SOURCE_DIR}/src/os.h
    ${PAW_SOURCE_DIR}/src/parse.h
    ${PAW_SOURCE_DIR}/src/pool.h
    ${PAW_SOURCE_DIR}/src/prefix.h
    ${PAW_SOURCE_DIR}/src/resolve.h
    ${PAW_SOURCE_DIR}/src/solve.h
    ${PAW_SOURCE_DIR}/src/source.h
    ${PAW_SOURCE_DIR}/src/stats.h
    ${PAW_SOURCE_DIR}/src/str.h
    ${PAW_SOURCE_DIR}/src/trait.h
    ${PAW_SOURCE_DIR}/src/type_folder.h
    ${PAW_SOURCE_DIR}/src/unify.h
    ${PAW_SOURCE_DIR}/src/util.h
    ${PAW_SOURCE_DIR}/src/value.h)

set(PAW_SOURCE_FILES
    ${PAW_SOURCE_DIR}/src/analysis.c
    ${PAW_SOURCE_DIR}/src/api.c
    ${PAW_SOURCE_DIR}/src/ast.c
    ${PAW_SOURCE_DIR}/src/auxlib.c
    ${PAW_SOURCE_DIR}/src/call.c
    ${PAW_SOURCE_DIR}/src/code.c
    ${PAW_SOURCE_DIR}/src/codegen.c
    ${PAW_SOURCE_DIR}/src/collect_items.c
    ${PAW_SOURCE_DIR}/src/compile.c
    ${PAW_SOURCE_DIR}/src/debug.c
    ${PAW_SOURCE_DIR}/src/decision.c
    ${PAW_SOURCE_DIR}/src/dominance.c
    ${PAW_SOURCE_DIR}/src/dropgen.c
    ${PAW_SOURCE_DIR}/src/env.c
    ${PAW_SOURCE_DIR}/src/error.c
    ${PAW_SOURCE_DIR}/src/eval.c
    ${PAW_SOURCE_DIR}/src/exhaustiveness.c
    ${PAW_SOURCE_DIR}/src/hir.c
    ${PAW_SOURCE_DIR}/src/impl.c
    ${PAW_SOURCE_DIR}/src/import.c
    ${PAW_SOURCE_DIR}/src/instantiate.c
    ${PAW_SOURCE_DIR}/src/ir_type.c
    ${PAW_SOURCE_DIR}/src/kfold.c
    ${PAW_SOURCE_DIR}/src/kprop.c
    ${PAW_SOURCE_DIR}/src/layout.c
    ${PAW_SOURCE_DIR}/src/lex.c
    ${PAW_SOURCE_DIR}/src/list.c
    ${PAW_SOURCE_DIR}/src/lib.c
    ${PAW_SOURCE_DIR}/src/liveness.c
    ${PAW_SOURCE_DIR}/src/lower_ast.c
    ${PAW_SOURCE_DIR}/src/lower_hir.c
    ${PAW_SOURCE_DIR}/src/lower_type.c
    ${PAW_SOURCE_DIR}/src/mangle.c
    ${PAW_SOURCE_DIR}/src/map.c
    ${PAW_SOURCE_DIR}/src/mem.c
    ${PAW_SOURCE_DIR}/src/mir.c
    ${PAW_SOURCE_DIR}/src/monomorphize.c
    ${PAW_SOURCE_DIR}/src/os.c
    ${PAW_SOURCE_DIR}/src/parse.c
    ${PAW_SOURCE_DIR}/src/pool.c
    ${PAW_SOURCE_DIR}/src/resolve.c
    ${PAW_SOURCE_DIR}/src/solve.c
    ${PAW_SOURCE_DIR}/src/source.c
    ${PAW_SOURCE_DIR}/src/stats.c
    ${PAW_SOURCE_DIR}/src/str.c
    ${PAW_SOURCE_DIR}/src/trait.c
    ${PAW_SOURCE_DIR}/src/type_folder.c
    ${PAW_SOURCE_DIR}/src/typeck.c
    ${PAW_SOURCE_DIR}/src/unify.c
    ${PAW_SOURCE_DIR}/src/util.c
    ${PAW_SOURCE_DIR}/src/value.c)

# TODO: Paw really needs better organization. At least split off a "compiler" directory to hold the numerous compiler source files
add_library(paw STATIC)
#target_include_directories(paw 
#    PRIVATE ${LLVM_INCLUDE_DIRS})
#target_link_libraries(paw 
#    PRIVATE ${LLVM_LIBS})
target_sources(paw
    PUBLIC  ${PAW_HEADER_FILES}
    PRIVATE ${PAW_SOURCE_FILES})
target_include_directories(paw
    PUBLIC ${PAW_SOURCE_DIR}/src)
# TODO: use PAW_ROOT_DIR to compute location of stdlib files, need to copy into install dir
target_compile_definitions(paw
    PUBLIC PAW_PRELUDE_PATH="${PAW_PRELUDE_PATH}"
           PAW_STDLIB_PATH="${PAW_STDLIB_PATH}"
           PAW_CLANG_PATH="${PAW_CLANG_PATH}"
           PAW_ROOT_DIR="${PAW_ROOT_DIR}"
           PAW_STRESS=${PAW_STRESS})
target_link_libraries(paw
    PRIVATE paw_context paw_codegen)

if(PAW_BuildTests)
    include(CTest)
    enable_testing()
    add_subdirectory(test)
endif()

if(PAW_BuildFuzzers)
    add_subdirectory(fuzz)
endif()
