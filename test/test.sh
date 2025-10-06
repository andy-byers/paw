#!/bin/sh

BUILD_PATH=$1
TEST_PATH=$2

TESTS=(
    "code"
    "basic"
    "cfg"
    "kprop"
    "primitive"
    "operator"
    "op_assign"
    "block"
    "cast"
    "loop"
    "function"
    "closure"
    "float"
    "integer"
    "string"
    "struct"
    "tuple"
    "enum"
    "test_list"
    "test_map"
    "stack"
    "never"
    "destructure"
    "method"
    "match_guard"
    "chain"
    "trait"
    "underscore"
    "iterator"
    "poly_function"
    "poly_struct"
    "poly_enum"
    "poly_method"
    "poly_trait"
    "unit_struct"
    "unit_variant"
    "infer_assoc_items"
    "deferred_init"
    "switch_branch"
    "layout"
    "import_variant"
    "match"
    "match_enum"
    "match_poly_enum"
    "match_or"
    "constants"
    "import"
    "alias"
    "misc"
    "modification_sequence"
    "argument_sequence"
    "modify_capture"
    "builtin_trait"
    "capture_upvalue"
    "close_loop_variable"
    "nan_infinity"
    "toplevel_constant"
    "global_const"
    "bubble"
    "nqueen"
    "matmul"
    "nqueen"
    "binary_trees"
    "elementary_cellular_automata"
    "ebnf"
)

# TODO: hardcoded paths won't work on other platforms...
CLANG_PATH="/usr/bin/clang"
LIBGC_DIR="/opt/homebrew/opt/bdw-gc"
ROOT_DIR=$BUILD_PATH/src/codegen
DRIVER_PATH=$ROOT_DIR/paw_driver
FAILURES=()

cmake -DPAW_STRESS=2 \
    -DPAW_CLANG_PATH=$CLANG_PATH \
    -DPAW_GC_DIR=$LIBGC_DIR \
    -DPAW_ROOT_DIR=$(pwd)/src/codegen \
    ..
cmake --build . --target paw_driver paw_stdc

for TESTNAME in "${TESTS[@]}"; do
    SCRIPT_PATH="$TEST_PATH/scripts/$TESTNAME.paw"
    OBJECT_PATH="$BUILD_PATH/test_$TESTNAME.o"
    EXEC_PATH="$BUILD_PATH/test_$TESTNAME"
    echo "Building $SCRIPT_PATH"

    # TODO: "-t" option always compiles and links the tests, but it would be nice to be able to compile but not link the tests
    # TODO: consider having "paw" accept an "action" argument, like "build", "test", etc., and split off "pawc" which just compiles but not links
    "$DRIVER_PATH" -c -t "$SCRIPT_PATH"
    if [ $? -eq 0 ]; then
        echo "[PASS]"
    else
        FAILURES+=($TESTNAME)
        echo "[FAIL]"
        continue
    fi

    echo "Running $EXEC_PATH"
    $EXEC_PATH
    if [ $? -eq 0 ]; then
        echo "[PASS]"
    else
        FAILURES+=($TESTNAME)
        echo "[FAIL]"
        continue
    fi
done

NUM_FAILURES=${#FAILURES[@]}
if [ $NUM_FAILURES -ne 0 ]; then
    echo "Failed $NUM_FAILURES test(s)"
    for FAILURE in "${FAILURES[@]}"; do
        echo "  $FAILURE"
    done
    exit 1
fi
