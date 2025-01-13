#ifndef OP_ARITY
#define OP_ARITY

#include <string>
const int op_arities[] = {
    -1, // dummy for indexing
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
    "dummy for indexing",
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

#endif
