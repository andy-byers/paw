
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

option(PAW_BuildFuzzers "Build the fuzz targets" Off)
option(PAW_BuildTests "Build the tests" ${MAIN_PROJECT})

set(PAW_STRESS 0 CACHE STRING 
    "Enable expensive checks (up to level 2)")
 
add_library(paw_context INTERFACE)
if(${CMAKE_C_COMPILER_ID} STREQUAL MSVC)
    target_compile_options(paw_context
            INTERFACE /W2)
else()
    target_compile_options(paw_context
        INTERFACE -Wall -Wextra -Wpedantic
                  -Wsign-conversion)
    target_link_libraries(paw_context
            INTERFACE m)
endif()

include(FetchContent)
FetchContent_Declare(gc
    GIT_REPOSITORY https://github.com/ivmai/bdwgc.git
    GIT_TAG v8.2.6)
FetchContent_MakeAvailable(gc)

set(PAW_SOURCE_DIR ${PROJECT_SOURCE_DIR}/src)

add_library(paw STATIC)
target_sources(paw
    PUBLIC  ${PAW_SOURCE_DIR}/api.h
            ${PAW_SOURCE_DIR}/ast.h
            ${PAW_SOURCE_DIR}/auxlib.h
            ${PAW_SOURCE_DIR}/array.h
            ${PAW_SOURCE_DIR}/call.h
            ${PAW_SOURCE_DIR}/code.h
            ${PAW_SOURCE_DIR}/debug.h
            ${PAW_SOURCE_DIR}/env.h
            ${PAW_SOURCE_DIR}/gc_aux.h
            ${PAW_SOURCE_DIR}/lex.h
            ${PAW_SOURCE_DIR}/lib.h
            ${PAW_SOURCE_DIR}/map.h
            ${PAW_SOURCE_DIR}/mem.h
            ${PAW_SOURCE_DIR}/meta.h
            ${PAW_SOURCE_DIR}/opcode.h
            ${PAW_SOURCE_DIR}/os.h
            ${PAW_SOURCE_DIR}/parse.h
            ${PAW_SOURCE_DIR}/prefix.h
            ${PAW_SOURCE_DIR}/rt.h
            ${PAW_SOURCE_DIR}/str.h
            ${PAW_SOURCE_DIR}/type.h
            ${PAW_SOURCE_DIR}/unify.h
            ${PAW_SOURCE_DIR}/util.h
            ${PAW_SOURCE_DIR}/value.h
    PRIVATE ${PAW_SOURCE_DIR}/api.c
            ${PAW_SOURCE_DIR}/ast.c
            ${PAW_SOURCE_DIR}/auxlib.c
            ${PAW_SOURCE_DIR}/array.c
            ${PAW_SOURCE_DIR}/call.c
            ${PAW_SOURCE_DIR}/check.c
            ${PAW_SOURCE_DIR}/code.c
            ${PAW_SOURCE_DIR}/codegen.c
            ${PAW_SOURCE_DIR}/debug.c
            ${PAW_SOURCE_DIR}/env.c
            ${PAW_SOURCE_DIR}/gc_aux.c
            #${PAW_SOURCE_DIR}/iolib.c
            ${PAW_SOURCE_DIR}/lex.c
            ${PAW_SOURCE_DIR}/lib.c
            #${PAW_SOURCE_DIR}/mathlib.c
            ${PAW_SOURCE_DIR}/map.c
            ${PAW_SOURCE_DIR}/mem.c
            ${PAW_SOURCE_DIR}/meta.c
            ${PAW_SOURCE_DIR}/opcode.c
            ${PAW_SOURCE_DIR}/os.c
            ${PAW_SOURCE_DIR}/parse.c
            ${PAW_SOURCE_DIR}/rt.c
            ${PAW_SOURCE_DIR}/str.c
            ${PAW_SOURCE_DIR}/type.c
            ${PAW_SOURCE_DIR}/unify.c
            ${PAW_SOURCE_DIR}/util.c
            ${PAW_SOURCE_DIR}/value.c)
target_link_libraries(paw
    PRIVATE paw_context 
    PUBLIC gc)
target_include_directories(paw
    PUBLIC ${PAW_SOURCE_DIR})
target_compile_definitions(paw
    PUBLIC PAW_STRESS=${PAW_STRESS})

add_executable(pawc
    ${PAW_SOURCE_DIR}/paw.c)
target_link_libraries(pawc
    PRIVATE paw_context paw)
set_property(TARGET pawc 
    PROPERTY OUTPUT_NAME paw)

if(PAW_BuildTests)
    # Speed up stack overflow test
    target_compile_definitions(paw
        PRIVATE PAW_STACK_MAX=10000) 
    include(CTest)
    enable_testing()
    add_subdirectory(test)
endif()

if(PAW_BuildFuzzers)
    add_subdirectory(fuzz)
endif()
