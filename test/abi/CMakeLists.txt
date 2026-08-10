
set(ABI_IN "${PROJECT_SOURCE_DIR}/abi/abi.paw")
set(ABI_OUT "${CMAKE_CURRENT_BINARY_DIR}/abi.o")
        
add_custom_command(
    OUTPUT "${ABI_OUT}"
    COMMAND pawc "${ABI_IN}" -c -d lir
    DEPENDS "${ABI_IN}"
    COMMENT "Building abi.paw...")

add_custom_target(generate_abi ALL
    DEPENDS "${ABI_OUT}")

add_executable(test_abi test.c abi.c abi.h "${ABI_OUT}")
target_link_libraries(test_abi PRIVATE paw_stdc)

add_test(NAME test_abi
    COMMAND test_abi)

