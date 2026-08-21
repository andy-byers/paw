
cmake_minimum_required(VERSION 3.14)
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

find_package(LLVM REQUIRED CONFIG)
llvm_map_components_to_libnames(LLVM_LIBS native passes support)
separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
add_definitions(${LLVM_DEFINITIONS_LIST})

target_sources(paw_codegen
    PUBLIC  ${CMAKE_CURRENT_SOURCE_DIR}/abi.h
            ${CMAKE_CURRENT_SOURCE_DIR}/codegen.h
            ${CMAKE_CURRENT_SOURCE_DIR}/context.h
            ${CMAKE_CURRENT_SOURCE_DIR}/linker.h
            ${CMAKE_CURRENT_SOURCE_DIR}/state.h
            ${CMAKE_CURRENT_SOURCE_DIR}/type.h
            ${CMAKE_CURRENT_SOURCE_DIR}/value.h
    PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/abi.cc
            ${CMAKE_CURRENT_SOURCE_DIR}/codegen.cc
            ${CMAKE_CURRENT_SOURCE_DIR}/context.cc
            ${CMAKE_CURRENT_SOURCE_DIR}/linker.cc
            ${CMAKE_CURRENT_SOURCE_DIR}/state.cc
            ${CMAKE_CURRENT_SOURCE_DIR}/type.cc
            ${CMAKE_CURRENT_SOURCE_DIR}/value.cc)
target_include_directories(paw_codegen
    PUBLIC  ${CMAKE_CURRENT_SOURCE_DIR}
    SYSTEM PRIVATE ${LLVM_INCLUDE_DIRS})
target_link_libraries(paw_codegen
    PRIVATE paw ${LLVM_LIBS})

add_executable(pawc
    ${CMAKE_CURRENT_SOURCE_DIR}/paw.c)
target_link_libraries(pawc
    PRIVATE paw paw_codegen)

include(CTest)
enable_testing()


#
# Build the standard library
#

set(STD_SRC "${CMAKE_SOURCE_DIR}/std")
set(STD_DEST "${CMAKE_BINARY_DIR}/std")

add_library(paw_stdc STATIC)
if(${CMAKE_C_COMPILER_ID} STREQUAL MSVC)
    target_compile_options(paw_stdc
        PRIVATE /W2)
else()
    target_compile_options(paw_stdc
        PRIVATE -Wall -Wextra -Wpedantic -Wsign-conversion 
                -Wno-zero-length-array # used for Paw type "()"
    ) 
    target_link_libraries(paw_stdc
        PRIVATE m)
endif()

target_sources(paw_stdc
    PUBLIC  ${STD_SRC}/paw.h
            ${STD_SRC}/io.h
            ${STD_SRC}/pmath.h
    PRIVATE ${STD_SRC}/paw.c
            ${STD_SRC}/io.c
            ${STD_SRC}/pmath.c)
target_include_directories(paw_stdc
    PUBLIC ${STD_SRC})

#file(GLOB STD_SRC_FILES "${STD_SRC}/*.paw")
#add_custom_command(
#    OUTPUT ${STD_DEST}/.copy_done
#    COMMAND ${CMAKE_COMMAND} -E make_directory ${STD_DEST}
#    COMMAND ${CMAKE_COMMAND} -E copy ${STD_SRC_FILES} ${STD_DEST}
#    DEPENDS ${STD_SRC_FILES}
#    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
#    COMMENT "Copying \"std/*.paw\" files to build directory"
#)
#
#set(DRIVER_ARGS "${STD_DEST}/prelude.paw")
#add_custom_command(
#    OUTPUT ${STD_DEST}/.build_done
#    COMMAND ${CMAKE_BINARY_DIR}/src/codegen/paw_driver ${DRIVER_ARGS}
#    DEPENDS ${STD_DEST}/.copy_done
#    WORKING_DIRECTORY ${STD_DEST}
#    COMMENT "Building Paw portion of stdlib"
#)
#
#add_custom_target(paw_std ALL
#    DEPENDS "${STD_DEST}/.build_done")
