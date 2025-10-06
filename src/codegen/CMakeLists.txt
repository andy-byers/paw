
cmake_minimum_required(VERSION 3.14)
set(CMAKE_TRY_COMPILE_TARGET_TYPE STATIC_LIBRARY)
project(paw_codegen
    LANGUAGES C CXX
    VERSION 0.0.1)
 
set(CMAKE_C_STANDARD 11) 
set(CMAKE_CXX_STANDARD 20) 
 
set(MAIN_PROJECT Off)
if (CMAKE_CURRENT_SOURCE_DIR STREQUAL CMAKE_SOURCE_DIR)
    set(MAIN_PROJECT On)
endif()

add_library(paw_codegen STATIC)
if(${CMAKE_C_COMPILER_ID} STREQUAL MSVC)
    target_compile_options(paw_codegen
        PRIVATE /W2)
else()
    target_compile_options(paw_codegen
        PRIVATE -Wall -Wextra -Wpedantic -Wsign-conversion)
    target_link_libraries(paw_codegen
        PRIVATE m)
endif()

target_sources(paw_codegen
    PUBLIC  ${CMAKE_CURRENT_SOURCE_DIR}/codegen.h
    PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/codegen.cc)
target_include_directories(paw_codegen
    PUBLIC  ${CMAKE_CURRENT_SOURCE_DIR} 
    PRIVATE ${LLVM_INCLUDE_DIRS})
target_link_libraries(paw_codegen
    PUBLIC paw ${LLVM_LIBS})

add_executable(paw_driver
    ${CMAKE_CURRENT_SOURCE_DIR}/paw.c)
target_link_libraries(paw_driver
    PRIVATE paw)

include(CTest)
enable_testing()
