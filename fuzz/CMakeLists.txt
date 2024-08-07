
option(PAW_FuzzerUseMain "Compile with standalone main to reproduce runs" Off)

add_executable(paw_fuzz fuzz.c)
target_link_libraries(paw_fuzz PRIVATE paw)

add_executable(paw_fuzz_expr fuzz_expr.c)
target_link_libraries(paw_fuzz_expr PRIVATE paw)

if(PAW_FuzzerUseMain)
    target_sources(paw_fuzz PRIVATE standalone.c)
endif()
