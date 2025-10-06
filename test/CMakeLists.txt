
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

add_custom_target(paw_test
    COMMAND "sh" ${TEST_DEST} ${TEST_ARGS}
    DEPENDS "${TEST_DEST}"
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
    COMMENT "Running test.sh"
)

