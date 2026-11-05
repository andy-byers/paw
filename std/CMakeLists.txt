
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
    PUBLIC  paw.h
            io.h
            pmath.h
    PRIVATE paw.c
            io.c
            os.c
            pmath.c)

#file(GLOB STD_SRC_FILES "*.paw")
#add_custom_command(
#    OUTPUT ${STD_DEST}/.copy_done
#    COMMAND ${CMAKE_COMMAND} -E make_directory ${STD_DEST}
#    COMMAND ${CMAKE_COMMAND} -E copy ${STD_SRC_FILES} ${STD_DEST}
#    DEPENDS ${STD_SRC_FILES}
#    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
#    COMMENT "Copying \"std/*.paw\" files to build directory"
#)
#
#add_custom_command(
#    OUTPUT ${STD_DEST}/.build_done
#    COMMAND ${CMAKE_BINARY_DIR}/src/pawc ${STD_DEST}/prelude.paw
#    DEPENDS ${STD_DEST}/.copy_done
#    WORKING_DIRECTORY ${STD_DEST}
#    COMMENT "Building Paw portion of stdlib"
#)
#
#add_custom_target(paw_std ALL
#    DEPENDS "${STD_DEST}/.build_done")
