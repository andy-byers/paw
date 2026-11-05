
find_package(LLVM REQUIRED CONFIG)
llvm_map_components_to_libnames(LLVM_LIBS native passes support)
separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
add_definitions(${LLVM_DEFINITIONS_LIST})

add_library(paw_codegen STATIC)
target_sources(paw_codegen
    PUBLIC  abi.h
            codegen.h
            context.h
            linker.h
            state.h
            type.h
            value.h
    PRIVATE abi.cc
            codegen.cc
            context.cc
            linker.cc
            state.cc
            type.cc
            value.cc)
target_include_directories(paw_codegen
    PUBLIC  ${PROJECT_SOURCE_DIR}/src
    SYSTEM PRIVATE ${LLVM_INCLUDE_DIRS})
target_link_libraries(paw_codegen
    PRIVATE paw_compiler 
            paw_common
            paw_config
            paw_ir
            paw_mir
            ${LLVM_LIBS})

