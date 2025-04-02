#ifndef OP_ARITY
#define OP_ARITY

#include <string>
const int op_arities[] = {
    1, // DEBUG_EXECUTE_ARBITRARY
    1, // label
    3, // func_info
    0, // int_code_end
    2, // call
    3, // call_last
    2, // call_only
    2, // call_ext
    3, // call_ext_last
    2, // bif0
    4, // bif1
    5, // bif2
    2, // allocate
    3, // allocate_heap
    2, // -allocate_zero
    3, // -allocate_heap_zero
    2, // test_heap
    1, // -init
    1, // deallocate
    0, // return
    0, // send
    0, // remove_message
    0, // timeout
    2, // loop_rec
    1, // loop_rec_end
    1, // wait
    2, // wait_timeout
    4, // -m_plus
    4, // -m_minus
    4, // -m_times
    4, // -m_div
    4, // -int_div
    4, // -int_rem
    4, // -int_band
    4, // -int_bor
    4, // -int_bxor
    4, // -int_bsl
    4, // -int_bsr
    3, // -int_bnot
    3, // is_lt
    3, // is_ge
    3, // is_eq
    3, // is_ne
    3, // is_eq_exact
    3, // is_ne_exact
    2, // is_integer
    2, // is_float
    2, // is_number
    2, // is_atom
    2, // is_pid
    2, // is_reference
    2, // is_port
    2, // is_nil
    2, // is_binary
    2, // -is_constant
    2, // is_list
    2, // is_nonempty_list
    2, // is_tuple
    3, // test_arity
    3, // select_val
    3, // select_tuple_arity
    1, // jump
    2, // catch
    1, // catch_end
    2, // move
    3, // get_list
    3, // get_tuple_element
    3, // set_tuple_element
    3, // -put_string
    3, // put_list
    2, // -put_tuple
    1, // -put
    1, // badmatch
    0, // if_end
    1, // case_end
    1, // call_fun
    3, // -make_fun
    2, // is_function
    2, // call_ext_only
    2, // -bs_start_match
    5, // -bs_get_integer
    5, // -bs_get_float
    5, // -bs_get_binary
    4, // -bs_skip_bits
    2, // -bs_test_tail
    1, // -bs_save
    1, // -bs_restore
    2, // -bs_init
    2, // -bs_final
    5, // -bs_put_integer
    5, // -bs_put_binary
    5, // -bs_put_float
    2, // -bs_put_string
    1, // -bs_need_buf
    0, // -fclearerror
    1, // -fcheckerror
    2, // fmove
    2, // fconv
    4, // fadd
    4, // fsub
    4, // fmul
    4, // fdiv
    3, // fnegate
    1, // -make_fun2
    2, // try
    1, // try_end
    1, // try_case
    1, // try_case_end
    2, // raise
    6, // -bs_init2
    3, // -bs_bits_to_bytes
    5, // -bs_add
    1, // apply
    2, // apply_last
    2, // is_boolean
    3, // is_function2
    5, // -bs_start_match2
    7, // bs_get_integer2
    7, // bs_get_float2
    7, // bs_get_binary2
    5, // bs_skip_bits2
    3, // bs_test_tail2
    2, // -bs_save2
    2, // -bs_restore2
    5, // gc_bif1
    6, // gc_bif2
    2, // -bs_final2
    2, // -bs_bits_to_bytes2
    2, // -put_literal
    2, // is_bitstr
    1, // -bs_context_to_binary
    3, // bs_test_unit
    4, // bs_match_string
    0, // bs_init_writable
    8, // -bs_append
    6, // -bs_private_append
    2, // trim
    6, // -bs_init_bits
    5, // bs_get_utf8
    4, // bs_skip_utf8
    5, // bs_get_utf16
    4, // bs_skip_utf16
    5, // bs_get_utf32
    4, // bs_skip_utf32
    3, // -bs_utf8_size
    3, // -bs_put_utf8
    3, // -bs_utf16_size
    3, // -bs_put_utf16
    3, // -bs_put_utf32
    0, // on_load
    1, // -recv_mark
    1, // -recv_set
    7, // gc_bif3
    1, // line
    5, // put_map_assoc
    5, // put_map_exact
    2, // is_map
    3, // has_map_fields
    3, // get_map_elements
    4, // is_tagged_tuple
    0, // build_stacktrace
    0, // raw_raise
    2, // get_hd
    2, // get_tl
    2, // put_tuple2
    3, // bs_get_tail
    4, // bs_start_match3
    3, // bs_get_position
    2, // bs_set_position
    2, // swap
    4, // bs_start_match4
    3, // make_fun3
    1, // init_yregs
    2, // recv_marker_bind
    1, // recv_marker_clear
    1, // recv_marker_reserve
    1, // recv_marker_use
    6, // bs_create_bin
    3, // call_fun2
    0, // nif_start
    1, // badrecord
    5, // update_record
    3, // bs_match
    2, // executable_line
};


const std::string op_names[] = {
    "DEBUG execute_aribtrary", // not a real beam instr
    "label",
    "func_info",
    "int_code_end",
    "call",
    "call_last",
    "call_only",
    "call_ext",
    "call_ext_last",
    "bif0",
    "bif1",
    "bif2",
    "allocate",
    "allocate_heap",
    "-allocate_zero",
    "-allocate_heap_zero",
    "test_heap",
    "-init",
    "deallocate",
    "return",
    "send",
    "remove_message",
    "timeout",
    "loop_rec",
    "loop_rec_end",
    "wait",
    "wait_timeout",
    "-m_plus",
    "-m_minus",
    "-m_times",
    "-m_div",
    "-int_div",
    "-int_rem",
    "-int_band",
    "-int_bor",
    "-int_bxor",
    "-int_bsl",
    "-int_bsr",
    "-int_bnot",
    "is_lt",
    "is_ge",
    "is_eq",
    "is_ne",
    "is_eq_exact",
    "is_ne_exact",
    "is_integer",
    "is_float",
    "is_number",
    "is_atom",
    "is_pid",
    "is_reference",
    "is_port",
    "is_nil",
    "is_binary",
    "-is_constant",
    "is_list",
    "is_nonempty_list",
    "is_tuple",
    "test_arity",
    "select_val",
    "select_tuple_arity",
    "jump",
    "catch",
    "catch_end",
    "move",
    "get_list",
    "get_tuple_element",
    "set_tuple_element",
    "-put_string",
    "put_list",
    "-put_tuple",
    "-put",
    "badmatch",
    "if_end",
    "case_end",
    "call_fun",
    "-make_fun",
    "is_function",
    "call_ext_only",
    "-bs_start_match",
    "-bs_get_integer",
    "-bs_get_float",
    "-bs_get_binary",
    "-bs_skip_bits",
    "-bs_test_tail",
    "-bs_save",
    "-bs_restore",
    "-bs_init",
    "-bs_final",
    "-bs_put_integer",
    "-bs_put_binary",
    "-bs_put_float",
    "-bs_put_string",
    "-bs_need_buf",
    "-fclearerror",
    "-fcheckerror",
    "fmove",
    "fconv",
    "fadd",
    "fsub",
    "fmul",
    "fdiv",
    "fnegate",
    "-make_fun2",
    "try",
    "try_end",
    "try_case",
    "try_case_end",
    "raise",
    "-bs_init2",
    "-bs_bits_to_bytes",
    "-bs_add",
    "apply",
    "apply_last",
    "is_boolean",
    "is_function2",
    "-bs_start_match2",
    "bs_get_integer2",
    "bs_get_float2",
    "bs_get_binary2",
    "bs_skip_bits2",
    "bs_test_tail2",
    "-bs_save2",
    "-bs_restore2",
    "gc_bif1",
    "gc_bif2",
    "-bs_final2",
    "-bs_bits_to_bytes2",
    "-put_literal",
    "is_bitstr",
    "-bs_context_to_binary",
    "bs_test_unit",
    "bs_match_string",
    "bs_init_writable",
    "-bs_append",
    "-bs_private_append",
    "trim",
    "-bs_init_bits",
    "bs_get_utf8",
    "bs_skip_utf8",
    "bs_get_utf16",
    "bs_skip_utf16",
    "bs_get_utf32",
    "bs_skip_utf32",
    "-bs_utf8_size",
    "-bs_put_utf8",
    "-bs_utf16_size",
    "-bs_put_utf16",
    "-bs_put_utf32",
    "on_load",
    "-recv_mark",
    "-recv_set",
    "gc_bif3",
    "line",
    "put_map_assoc",
    "put_map_exact",
    "is_map",
    "has_map_fields",
    "get_map_elements",
    "is_tagged_tuple",
    "build_stacktrace",
    "raw_raise",
    "get_hd",
    "get_tl",
    "put_tuple2",
    "bs_get_tail",
    "bs_start_match3",
    "bs_get_position",
    "bs_set_position",
    "swap",
    "bs_start_match4",
    "make_fun3",
    "init_yregs",
    "recv_marker_bind",
    "recv_marker_clear",
    "recv_marker_reserve",
    "recv_marker_use",
    "bs_create_bin",
    "call_fun2",
    "nif_start",
    "badrecord",
    "update_record",
    "bs_match",
    "executable_line"
};

enum OpCode {
    DEBUG_EXECUTE_ARBITRARY = 0, // not a real beam instr
    LABEL_OP = 1,
    FUNC_INFO_OP = 2,
    INT_CODE_END_OP = 3,
    CALL_OP = 4,
    CALL_LAST_OP = 5,
    CALL_ONLY_OP = 6,
    CALL_EXT_OP = 7,
    CALL_EXT_LAST_OP = 8,
    BIF0_OP = 9,
    BIF1_OP = 10,
    BIF2_OP = 11,
    ALLOCATE_OP = 12,
    ALLOCATE_HEAP_OP = 13,
    ALLOCATE_ZERO_OP = 14,
    ALLOCATE_HEAP_ZERO_OP = 15,
    TEST_HEAP_OP = 16,
    INIT_OP = 17,
    DEALLOCATE_OP = 18,
    RETURN_OP = 19,
    SEND_OP = 20,
    REMOVE_MESSAGE_OP = 21,
    TIMEOUT_OP = 22,
    LOOP_REC_OP = 23,
    LOOP_REC_END_OP = 24,
    WAIT_OP = 25,
    WAIT_TIMEOUT_OP = 26,
    M_PLUS_OP = 27,
    M_MINUS_OP = 28,
    M_TIMES_OP = 29,
    M_DIV_OP = 30,
    INT_DIV_OP = 31,
    INT_REM_OP = 32,
    INT_BAND_OP = 33,
    INT_BOR_OP = 34,
    INT_BXOR_OP = 35,
    INT_BSL_OP = 36,
    INT_BSR_OP = 37,
    INT_BNOT_OP = 38,
    IS_LT_OP = 39,
    IS_GE_OP = 40,
    IS_EQ_OP = 41,
    IS_NE_OP = 42,
    IS_EQ_EXACT_OP = 43,
    IS_NE_EXACT_OP = 44,
    IS_INTEGER_OP = 45,
    IS_FLOAT_OP = 46,
    IS_NUMBER_OP = 47,
    IS_ATOM_OP = 48,
    IS_PID_OP = 49,
    IS_REFERENCE_OP = 50,
    IS_PORT_OP = 51,
    IS_NIL_OP = 52,
    IS_BINARY_OP = 53,
    IS_CONSTANT_OP = 54,
    IS_LIST_OP = 55,
    IS_NONEMPTY_LIST_OP = 56,
    IS_TUPLE_OP = 57,
    TEST_ARITY_OP = 58,
    SELECT_VAL_OP = 59,
    SELECT_TUPLE_ARITY_OP = 60,
    JUMP_OP = 61,
    CATCH_OP = 62,
    CATCH_END_OP = 63,
    MOVE_OP = 64,
    GET_LIST_OP = 65,
    GET_TUPLE_ELEMENT_OP = 66,
    SET_TUPLE_ELEMENT_OP = 67,
    PUT_STRING_OP = 68,
    PUT_LIST_OP = 69,
    PUT_TUPLE_OP = 70,
    PUT_OP = 71,
    BADMATCH_OP = 72,
    IF_END_OP = 73,
    CASE_END_OP = 74,
    CALL_FUN_OP = 75,
    MAKE_FUN_OP = 76,
    IS_FUNCTION_OP = 77,
    CALL_EXT_ONLY_OP = 78,
    BS_START_MATCH_OP = 79,
    BS_GET_INTEGER_OP = 80,
    BS_GET_FLOAT_OP = 81,
    BS_GET_BINARY_OP = 82,
    BS_SKIP_BITS_OP = 83,
    BS_TEST_TAIL_OP = 84,
    BS_SAVE_OP = 85,
    BS_RESTORE_OP = 86,
    BS_INIT_OP = 87,
    BS_FINAL_OP = 88,
    BS_PUT_INTEGER_OP = 89,
    BS_PUT_BINARY_OP = 90,
    BS_PUT_FLOAT_OP = 91,
    BS_PUT_STRING_OP = 92,
    BS_NEED_BUF_OP = 93,
    FCLEARERROR_OP = 94,
    FCHECKERROR_OP = 95,
    FMOVE_OP = 96,
    FCONV_OP = 97,
    FADD_OP = 98,
    FSUB_OP = 99,
    FMUL_OP = 100,
    FDIV_OP = 101,
    FNEGATE_OP = 102,
    MAKE_FUN2_OP = 103,
    TRY_OP = 104,
    TRY_END_OP = 105,
    TRY_CASE_OP = 106,
    TRY_CASE_END_OP = 107,
    RAISE_OP = 108,
    BS_INIT2_OP = 109,
    BS_BITS_TO_BYTES_OP = 110,
    BS_ADD_OP = 111,
    APPLY_OP = 112,
    APPLY_LAST_OP = 113,
    IS_BOOLEAN_OP = 114,
    IS_FUNCTION2_OP = 115,
    BS_START_MATCH2_OP = 116,
    BS_GET_INTEGER2_OP = 117,
    BS_GET_FLOAT2_OP = 118,
    BS_GET_BINARY2_OP = 119,
    BS_SKIP_BITS2_OP = 120,
    BS_TEST_TAIL2_OP = 121,
    BS_SAVE2_OP = 122,
    BS_RESTORE2_OP = 123,
    GC_BIF1_OP = 124,
    GC_BIF2_OP = 125,
    BS_FINAL2_OP = 126,
    BS_BITS_TO_BYTES2_OP = 127,
    PUT_LITERAL_OP = 128,
    IS_BITSTR_OP = 129,
    BS_CONTEXT_TO_BINARY_OP = 130,
    BS_TEST_UNIT_OP = 131,
    BS_MATCH_STRING_OP = 132,
    BS_INIT_WRITABLE_OP = 133,
    BS_APPEND_OP = 134,
    BS_PRIVATE_APPEND_OP = 135,
    TRIM_OP = 136,
    BS_INIT_BITS_OP = 137,
    BS_GET_UTF8_OP = 138,
    BS_SKIP_UTF8_OP = 139,
    BS_GET_UTF16_OP = 140,
    BS_SKIP_UTF16_OP = 141,
    BS_GET_UTF32_OP = 142,
    BS_SKIP_UTF32_OP = 143,
    BS_UTF8_SIZE_OP = 144,
    BS_PUT_UTF8_OP = 145,
    BS_UTF16_SIZE_OP = 146,
    BS_PUT_UTF16_OP = 147,
    BS_PUT_UTF32_OP = 148,
    ON_LOAD_OP = 149,
    RECV_MARK_OP = 150,
    RECV_SET_OP = 151,
    GC_BIF3_OP = 152,
    LINE_OP = 153,
    PUT_MAP_ASSOC_OP = 154,
    PUT_MAP_EXACT_OP = 155,
    IS_MAP_OP = 156,
    HAS_MAP_FIELDS_OP = 157,
    GET_MAP_ELEMENTS_OP = 158,
    IS_TAGGED_TUPLE_OP = 159,
    BUILD_STACKTRACE_OP = 160,
    RAW_RAISE_OP = 161,
    GET_HD_OP = 162,
    GET_TL_OP = 163,
    PUT_TUPLE2_OP = 164,
    BS_GET_TAIL_OP = 165,
    BS_START_MATCH3_OP = 166,
    BS_GET_POSITION_OP = 167,
    BS_SET_POSITION_OP = 168,
    SWAP_OP = 169,
    BS_START_MATCH4_OP = 170,
    MAKE_FUN3_OP = 171,
    INIT_YREGS_OP = 172,
    RECV_MARKER_BIND_OP = 173,
    RECV_MARKER_CLEAR_OP = 174,
    RECV_MARKER_RESERVE_OP = 175,
    RECV_MARKER_USE_OP = 176,
    BS_CREATE_BIN_OP = 177,
    CALL_FUN2_OP = 178,
    NIF_START_OP = 179,
    BADRECORD_OP = 180,
    UPDATE_RECORD_OP = 181,
    BS_MATCH_OP = 182,
    EXECUTABLE_LINE_OP = 183
};

#endif
