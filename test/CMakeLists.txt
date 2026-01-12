
cmake_minimum_required(VERSION 3.14)
set(CMAKE_TRY_COMPILE_TARGET_TYPE STATIC_LIBRARY)
project(paw_test
    LANGUAGES C CXX
    VERSION 0.0.1)
 
set(CMAKE_C_STANDARD 11) 
set(CMAKE_CXX_STANDARD 20) 

set(TEST_DIR "${CMAKE_SOURCE_DIR}/test")

set(TEST_SRC "${CMAKE_SOURCE_DIR}/test/test.sh")
set(TEST_DEST "${CMAKE_BINARY_DIR}/test.sh")

add_custom_command(
    OUTPUT "${TEST_DEST}"
    COMMAND ${CMAKE_COMMAND} -E copy "${TEST_SRC}" "${TEST_DEST}"
    DEPENDS "${TEST_SRC}"
    COMMENT "Copying test.sh to build directory")

add_custom_target(generate_test ALL
    DEPENDS "${TEST_DEST}")

set(TEST_ARGS "${CMAKE_BINARY_DIR}" "${TEST_DIR}" "${PAW_GC_DIR}")

add_custom_target(paw_test_scripts
    COMMAND "sh" ${TEST_DEST} ${TEST_ARGS}
    DEPENDS "${TEST_DEST}"
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
    COMMENT "Running test.sh"
)


add_executable(test_error test_error.c)
target_link_libraries(test_error PRIVATE paw)
