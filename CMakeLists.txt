
cmake_minimum_required(VERSION 3.14)
set(CMAKE_TRY_COMPILE_TARGET_TYPE STATIC_LIBRARY)
project(paw
    LANGUAGES C
    VERSION 0.0.1)
 
set(CMAKE_C_STANDARD 11) 
set(CMAKE_C_STANDARD_REQUIRED On) 
 
set(MAIN_PROJECT Off)
if (CMAKE_CURRENT_SOURCE_DIR STREQUAL CMAKE_SOURCE_DIR)
    set(MAIN_PROJECT On)
endif()

set(PAW_LLVM_PATH "" CACHE PATH "Path to LLVM library")
set(PAW_GC_DIR "" CACHE PATH "Path to BDWGC directory")
set(PAW_CLANG_PATH "" CACHE PATH "Path to clang binary")
set(PAW_BINARY_DIR "" CACHE PATH "Location of compilation artifacts")

# TODO: installation support
# TODO:   PAW_ROOT_DIR=("paw", "paw_stdc.o", <stdlib sources>)
set(PAW_ROOT_DIR ${CMAKE_ROOT_DIR})

if(PAW_LLVM_PATH)
    find_package(LLVM REQUIRED CONFIG PATHS ${PAW_LLVM_PATH})
else()
    include(FetchContent)
    FetchContent_Declare(llvm
        GIT_REPOSITORY https://github.com/llvm/llvm-project
        GIT_TAG llvmorg-21.1.2)
    FetchContent_MakeAvailable(llvm)
endif()

add_definitions(${LLVM_DEFINITIONS})
llvm_map_components_to_libnames(LLVM_LIBS 
    core native passes support irreader)


option(PAW_BuildFuzzers "Build the fuzz targets" Off)
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

set(PAW_SOURCE_DIR ${PROJECT_SOURCE_DIR}/src)

set(PAW_HEADER_FILES
    ${PAW_SOURCE_DIR}/api.h
    ${PAW_SOURCE_DIR}/ast.h
    ${PAW_SOURCE_DIR}/auxlib.h
    ${PAW_SOURCE_DIR}/call.h
    ${PAW_SOURCE_DIR}/code.h
    ${PAW_SOURCE_DIR}/compile.h
    ${PAW_SOURCE_DIR}/debug.h
    ${PAW_SOURCE_DIR}/env.h
    ${PAW_SOURCE_DIR}/error.h
    ${PAW_SOURCE_DIR}/hir.h
    ${PAW_SOURCE_DIR}/ir_type.h
    ${PAW_SOURCE_DIR}/lex.h
    ${PAW_SOURCE_DIR}/lib.h
    ${PAW_SOURCE_DIR}/map.h
    ${PAW_SOURCE_DIR}/match.h
    ${PAW_SOURCE_DIR}/mem.h
    ${PAW_SOURCE_DIR}/mir.h
    ${PAW_SOURCE_DIR}/os.h
    ${PAW_SOURCE_DIR}/parse.h
    ${PAW_SOURCE_DIR}/pool.h
    ${PAW_SOURCE_DIR}/prefix.h
    ${PAW_SOURCE_DIR}/resolve.h
    ${PAW_SOURCE_DIR}/source.h
    ${PAW_SOURCE_DIR}/ssa.h
    ${PAW_SOURCE_DIR}/stats.h
    ${PAW_SOURCE_DIR}/str.h
    ${PAW_SOURCE_DIR}/trait.h
    ${PAW_SOURCE_DIR}/type_folder.h
    ${PAW_SOURCE_DIR}/unify.h
    ${PAW_SOURCE_DIR}/util.h
    ${PAW_SOURCE_DIR}/value.h)

set(PAW_SOURCE_FILES
    ${PAW_SOURCE_DIR}/api.c
    ${PAW_SOURCE_DIR}/ast.c
    ${PAW_SOURCE_DIR}/auxlib.c
    ${PAW_SOURCE_DIR}/call.c
    ${PAW_SOURCE_DIR}/code.c
    ${PAW_SOURCE_DIR}/codegen.c
    ${PAW_SOURCE_DIR}/collect_items.c
    ${PAW_SOURCE_DIR}/compile.c
    ${PAW_SOURCE_DIR}/debug.c
    ${PAW_SOURCE_DIR}/decision.c
    ${PAW_SOURCE_DIR}/dominance.c
    ${PAW_SOURCE_DIR}/env.c
    ${PAW_SOURCE_DIR}/error.c
    ${PAW_SOURCE_DIR}/exhaustiveness.c
    ${PAW_SOURCE_DIR}/hir.c
    ${PAW_SOURCE_DIR}/import.c
    ${PAW_SOURCE_DIR}/instantiate.c
    ${PAW_SOURCE_DIR}/ir_type.c
    ${PAW_SOURCE_DIR}/kfold.c
    ${PAW_SOURCE_DIR}/kprop.c
    ${PAW_SOURCE_DIR}/lex.c
    ${PAW_SOURCE_DIR}/list.c
    ${PAW_SOURCE_DIR}/lib.c
    ${PAW_SOURCE_DIR}/liveness.c
    ${PAW_SOURCE_DIR}/lower_ast.c
    ${PAW_SOURCE_DIR}/lower_hir.c
    ${PAW_SOURCE_DIR}/lower_type.c
    ${PAW_SOURCE_DIR}/map.c
    ${PAW_SOURCE_DIR}/mem.c
    ${PAW_SOURCE_DIR}/mir.c
    ${PAW_SOURCE_DIR}/monomorphize.c
    ${PAW_SOURCE_DIR}/os.c
    ${PAW_SOURCE_DIR}/parse.c
    ${PAW_SOURCE_DIR}/pool.c
    ${PAW_SOURCE_DIR}/resolve.c
    ${PAW_SOURCE_DIR}/source.c
    ${PAW_SOURCE_DIR}/ssa.c
    ${PAW_SOURCE_DIR}/stats.c
    ${PAW_SOURCE_DIR}/str.c
    ${PAW_SOURCE_DIR}/trait.c
    ${PAW_SOURCE_DIR}/type_folder.c
    ${PAW_SOURCE_DIR}/typeck.c
    ${PAW_SOURCE_DIR}/unify.c
    ${PAW_SOURCE_DIR}/util.c
    ${PAW_SOURCE_DIR}/value.c)

# TODO: Paw really needs better organization. At least split off a "compiler" directory to hold the numerous compiler source files
add_library(paw STATIC)
target_include_directories(paw 
    PRIVATE ${LLVM_INCLUDE_DIRS})
target_link_libraries(paw 
    PRIVATE ${LLVM_LIBS})
target_sources(paw
    PUBLIC  ${PAW_HEADER_FILES}
    PRIVATE ${PAW_SOURCE_FILES})
target_include_directories(paw
    PUBLIC ${PAW_SOURCE_DIR})
# TODO: use PAW_ROOT_DIR to compute location of stdlib files, need to copy into install dir
target_compile_definitions(paw
    PUBLIC PAW_PRELUDE_PATH="${PAW_PRELUDE_PATH}"
           PAW_STDLIB_PATH="${PAW_STDLIB_PATH}"
           PAW_CLANG_PATH="${PAW_CLANG_PATH}"
           PAW_ROOT_DIR="${PAW_ROOT_DIR}"
           PAW_GC_DIR="${PAW_GC_DIR}"
           PAW_STRESS=${PAW_STRESS})
target_link_libraries(paw
    PRIVATE paw_context paw_codegen)

if(PAW_BuildTests)
    # Speed up stack overflow test
    target_compile_definitions(paw
        PUBLIC PAW_STACK_MAX=10000) 
    include(CTest)
    enable_testing()
    add_subdirectory(test)
endif()

if(PAW_BuildFuzzers)
    add_subdirectory(fuzz)
endif()
