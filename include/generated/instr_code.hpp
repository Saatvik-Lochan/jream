// THIS FILE WAS GENERATED BY instr_code_gen.py
#ifndef INSTR_CODE_H
#define INSTR_CODE_H

#include <cstdint>
#include <vector>

enum AsmSnippet {
  DEALLOCATE_SNIP, // assembly/build/deallocate.S
  ENTER_FUNC_SNIP, // assembly/build/enter_func.S
  GOTO_LABEL_SNIP, // assembly/build/goto_label.S
  CALL_EXT_BIF_SNIP, // assembly/build/call_ext_bif.S
  LOOP_REC_1_SNIP, // assembly/build/loop_rec_1.S
  TEMP_SNIP, // assembly/build/temp.S
  MAKE_FUN3_SNIP, // assembly/build/make_fun3.S
  RETURN_SNIP, // assembly/build/return.S
  LOOP_REC_3_SNIP, // assembly/build/loop_rec_3.S
  ALLOCATE_SNIP, // assembly/build/allocate.S
  SEND_SNIP, // assembly/build/send.S
  LOOP_REC0_SNIP, // assembly/build/loop_rec0.S
  CALL_SNIP, // assembly/build/call.S
  CALL_FINISH_SNIP, // assembly/build/call_finish.S
  LOAD_1_ARG_SNIP, // assembly/build/load_1_arg.S
  LOAD_3_ARGS_SNIP, // assembly/build/load_3_args.S
  STORE_DOUBLEWORD_TEST_SNIP, // assembly/build/store_doubleword_test.S
  TEARDOWN_SNIP, // assembly/build/teardown.S
  CALL_SETUP_SNIP, // assembly/build/call_setup.S
  LOOP_REC_2_SNIP, // assembly/build/loop_rec_2.S
  SETUP_SNIP, // assembly/build/setup.S
  GET_LIST_SNIP, // assembly/build/get_list.S
  CALL_EXT_SNIP, // assembly/build/call_ext.S
  DEBUG_EXECUTE_ARIBITRARY_SNIP, // assembly/build/debug_execute_aribitrary.S
  COMPILE_STUB_SNIP, // assembly/build/compile_stub.S
};

std::vector<uint8_t> get_riscv(AsmSnippet snippet);

#endif
