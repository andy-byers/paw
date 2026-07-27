#!/bin/sh

BUILD_PATH=$1
TEST_PATH=$2

TESTS=(
    "basic"
    "cfg"
    "kprop"
    "short_circuiting"
    "dispatch_based_on_pointerness"
    "nested_assoc_type"
    "nested_assoc_type_2"
    "recursive_assoc_item_constraints"
    "recursive_assoc_item_constraints_2"
    "recursive_assoc_item_constraints_3"
    "deferred_trait_selection"
    "infer_via_obligation"
    "normalize_projections"
    "recursive_supertraits"
    "impl_special_case"
    "impl_special_case_2"
    "assoc_type_in_bound"
    "assoc_type_in_bound_2"
    "into_implies_from"
    "conversions_are_reflexive"
    "try_operator_conversion"
    "recursive_normalization_with_nested_bounds"
    "trait_method_obligations"
    "projection_as_generic_arg"
    "const_generics"
    "supertraits"
    "poly_supertraits"
    "poly_supertraits2"
    "supertrait_assoc_type_bound"
    "supertrait_methods"
    "disambiguate_using_projection"
    "primitive"
    "operator"
    "op_assign"
    "block"
    "pointer"
    "drop"
#    "code"
    "cast"
    "loop"
    "function"
    "closure"
    "env_layout"
    "float"
    "integer"
    "test_string"
    "struct"
    "tuple"
    "enum"
    "getters"
    "fstring"
    "test_list"
    "test_map"
    "test_slice"
    "stack"
    "never"
    "destructure"
    "method"
    "chain"
    "trait"
    "unit_struct"
    "unit_variant"
    "underscore"
    "iterator"
    "test_string_builder"
    "poly_function"
    "poly_struct"
    "poly_enum"
    "poly_method"
    "poly_trait"
    "poly_trait2"
    "assoc_types"
    "assoc_type_bounds"
    "assoc_type_bounds2"
    "infer_assoc_items"
    "deferred_init"
    "switch_branch"
    "layout"
    "import_variant"
    "match"
    "match_enum"
    "match_poly_enum"
    "match_guard"
    "match_or"
    "match_indirect"
    "constants"
    "import"
    "alias"
    "misc"
    "modification_sequence"
    "argument_sequence"
    "builtin_trait"
#TODO    "recursive_impl"
#TODO    "ambiguous_method_call"
#TODO    "capture_upvalue"
    "close_loop_variable"
    "nan_infinity"
    "toplevel_constant"
    "global_const"
    "mono_impl"
    "poly_impl"
    "poly_impl2"
    "tuple_impl"
    "builtin_impl"
    "builtin_impl2"
    "inherent_impl"
    "trait_impl"
    "trait_impl2"
    "blanket_impl"
    "impl_selection"
    "sort"
    "example"
    "example_2"
    "bubble"
    "nqueen"
    "matmul"
#    "binary_trees"
    "elementary_cellular_automata"
#    "ebnf"
)

# TODO: hardcoded paths won't work on other platforms...
CLANG_PATH="/usr/bin/clang"
ROOT_DIR=$BUILD_PATH/src/codegen
DRIVER_PATH=$ROOT_DIR/pawc
FAILURES=()

for TESTNAME in "${TESTS[@]}"; do
    SCRIPT_PATH="$TEST_PATH/scripts/$TESTNAME.paw"
    OBJECT_PATH="$BUILD_PATH/test_$TESTNAME.o"
    EXEC_PATH="$BUILD_PATH/test_$TESTNAME"
    echo "Building $SCRIPT_PATH"

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
        echo " $FAILURE"
    done
    exit 1
fi
