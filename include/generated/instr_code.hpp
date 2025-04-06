// THIS FILE WAS GENERATED BY instr_code_gen.py
#ifndef INSTR_CODE_H
#define INSTR_CODE_H

#include <cstdint>
#include <vector>

enum AsmSnippet {
  GET_LIST_SNIP, // assembly/get_list_m.S
  SEND_SNIP, // assembly/send_m.S
  LOAD_3_ARGS_SNIP, // assembly/load_3_args_m.S
  CALL_EXT_BIF_SNIP, // assembly/call_ext_bif_m.S
  CALL_SNIP, // assembly/call_m.S
  DEALLOCATE_SNIP, // assembly/deallocate_m.S
  CALL_FINISH_SNIP, // assembly/call_finish_m.S
  RETURN_SNIP, // assembly/return_m.S
  DEBUG_EXECUTE_ARIBITRARY_SNIP, // assembly/debug_execute_aribitrary_m.S
  TEARDOWN_SNIP, // assembly/teardown_m.S
  MAKE_FUN3_SNIP, // assembly/make_fun3_m.S
  STORE_DOUBLEWORD_TEST_SNIP, // assembly/store_doubleword_test_m.S
  COMPILE_STUB_SNIP, // assembly/compile_stub_m.S
  GOTO_LABEL_SNIP, // assembly/goto_label_m.S
  SETUP_SNIP, // assembly/setup_m.S
  ALLOCATE_SNIP, // assembly/allocate_m.S
  CALL_SETUP_SNIP, // assembly/call_setup_m.S
  ENTER_FUNC_SNIP, // assembly/enter_func_m.S
  TEMP_SNIP, // assembly/temp_m.S
  CALL_EXT_SNIP, // assembly/call_ext_m.S
  LOAD_1_ARG_SNIP, // assembly/load_1_arg_m.S
};

std::vector<uint8_t> get_riscv(AsmSnippet snippet);

#endif
