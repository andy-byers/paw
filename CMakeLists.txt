
cmake_minimum_required(VERSION 3.14)
project(paw
    LANGUAGES C CXX
    VERSION 0.0.1)
 
set(CMAKE_C_STANDARD 11) 
set(CMAKE_C_STANDARD_REQUIRED ON) 
set(CMAKE_CXX_STANDARD 20) 
set(CMAKE_CXX_STANDARD_REQUIRED ON) 
 
set(MAIN_PROJECT OFF)
if (CMAKE_CURRENT_SOURCE_DIR STREQUAL CMAKE_SOURCE_DIR)
    set(MAIN_PROJECT ON)
endif()

set(PAW_CLANG_PATH "" CACHE PATH "Path to clang binary")
set(PAW_BINARY_DIR "" CACHE PATH "Location of compilation artifacts")

# TODO: installation support
set(PAW_ROOT_DIR ${CMAKE_BINARY_DIR}/std)

option(PAW_BuildFuzzers "Build the fuzz targets" OFF)
option(PAW_BuildTests "Build the tests" ${MAIN_PROJECT})

set(PAW_PRELUDE_PATH "${CMAKE_SOURCE_DIR}/std/prelude.paw" CACHE STRING "Path to prelude module")
set(PAW_STDLIB_PATH "${CMAKE_SOURCE_DIR}/std/" CACHE STRING "Path to standard library source files")
set(PAW_STRESS 0 CACHE STRING "Enable expensive checks (supports up to level 2)")
 
set(PAW_SOURCE_DIR ${PROJECT_SOURCE_DIR})

add_subdirectory(src)
add_subdirectory(std)

if(PAW_BuildTests)
    include(CTest)
    enable_testing()
    add_subdirectory(test)
endif()

if(PAW_BuildFuzzers)
    add_subdirectory(fuzz)
endif()

