
cmake_minimum_required(VERSION 3.14)
project(paw_test
    LANGUAGES C CXX
    VERSION 0.0.1)
 
set(CMAKE_C_STANDARD 11) 
set(CMAKE_CXX_STANDARD 20) 

set(TEST_RUNNER_IN "${CMAKE_CURRENT_SOURCE_DIR}/test.sh")
set(TEST_RUNNER "${CMAKE_CURRENT_BINARY_DIR}/test.sh")

add_custom_command(
    OUTPUT "${TEST_RUNNER}"
    COMMAND ${CMAKE_COMMAND} -E copy "${TEST_RUNNER_IN}" "${TEST_RUNNER}"
    DEPENDS "${TEST_RUNNER_IN}"
    COMMENT "Copying test.sh to build directory")

add_custom_target(generate_test ALL
    DEPENDS "${TEST_RUNNER}")

add_executable(test_error test_error.c)
target_link_libraries(test_error 
    PRIVATE paw_config
            paw_ir
            paw_compiler)

add_test(NAME test_scripts
    COMMAND "${TEST_RUNNER}" 
        "${CMAKE_BINARY_DIR}/src/pawc" # DRIVER_PATH
        "${CMAKE_CURRENT_BINARY_DIR}" # OUTPUT_DIR
        "${CMAKE_BINARY_DIR}/std" # STDLIB_DIR
        "${CMAKE_CURRENT_SOURCE_DIR}") # TEST_DIR

add_test(NAME test_error
    COMMAND test_error)

add_subdirectory(abi)
