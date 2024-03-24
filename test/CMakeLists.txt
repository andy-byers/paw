
function(build_test NAME) 
    set(TESTNAME "paw_${NAME}")
    add_executable(${TESTNAME} 
        ${NAME}.c test.c test.h)
    target_link_libraries(${TESTNAME}
        PRIVATE paw paw_context)
    target_compile_definitions(${TESTNAME}
        PRIVATE TEST_PREFIX="${PROJECT_SOURCE_DIR}/test/")
    add_test(NAME ${TESTNAME}
        COMMAND ${TESTNAME})
endfunction()

build_test(test_impl)
build_test(test_api)
build_test(test_rt)
build_test(test_oom)
build_test(test_so)
build_test(test_error)
