
set(TEST_DIR "${PROJECT_SOURCE_DIR}/test")
function(build_test NAME) 
    set(TESTNAME "paw_${NAME}")
    add_executable(${TESTNAME} 
        ${NAME}.c test.c test.h)
    target_link_libraries(${TESTNAME}
        PRIVATE paw paw_context)
    target_compile_definitions(${TESTNAME}
        PRIVATE TEST_PREFIX="${TEST_DIR}/")
    add_test(NAME ${TESTNAME}
        COMMAND ${TESTNAME})
endfunction()

function(test_script NAME ARGS)
    set(EXEC "${CMAKE_BINARY_DIR}/paw")
    set(SCRIPT "${TEST_DIR}/scripts/${NAME}.paw")
    add_test(NAME "test_${NAME}"
        COMMAND ${EXEC} ${SCRIPT} ${ARGS})
endfunction()

build_test(test_impl)
#build_test(test_api)
build_test(test_rt)
#build_test(test_oom)
#build_test(test_so)
build_test(test_error)
#build_test(test_limits)

#test_script(bubble "500")
#test_script(binary_trees "5")
#test_script(mandelbrot "-2.1,0.9,80,-1.3,1.3,24")
