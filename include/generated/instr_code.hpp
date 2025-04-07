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
  INIT_YREGS_SNIP, // assembly/build/init_yregs.S
  LOOP_REC_1_SNIP, // assembly/build/loop_rec_1.S
  TEMP_SNIP, // assembly/build/temp.S
  IS_NIL_SNIP, // assembly/build/is_nil.S
  MAKE_FUN3_SNIP, // assembly/build/make_fun3.S
  IS_TUPLE_1_SNIP, // assembly/build/is_tuple_1.S
  RETURN_SNIP, // assembly/build/return.S
  IS_NONEMPTY_LIST_SNIP, // assembly/build/is_nonempty_list.S
  WAIT_SNIP, // assembly/build/wait.S
  TEST_ARITY_SNIP, // assembly/build/test_arity.S
  ALLOCATE_SNIP, // assembly/build/allocate.S
  SEND_SNIP, // assembly/build/send.S
  CALL_SNIP, // assembly/build/call.S
  CALL_FINISH_SNIP, // assembly/build/call_finish.S
  GET_TUPLE_ELEMENT_SNIP, // assembly/build/get_tuple_element.S
  LOAD_1_ARG_SNIP, // assembly/build/load_1_arg.S
  CALL_LAST_SNIP, // assembly/build/call_last.S
  LOAD_3_ARGS_SNIP, // assembly/build/load_3_args.S
  STORE_DOUBLEWORD_TEST_SNIP, // assembly/build/store_doubleword_test.S
  TEARDOWN_SNIP, // assembly/build/teardown.S
  REMOVE_SNIP, // assembly/build/remove.S
  IS_TUPLE_2_SNIP, // assembly/build/is_tuple_2.S
  CALL_SETUP_SNIP, // assembly/build/call_setup.S
  LOOP_REC_2_SNIP, // assembly/build/loop_rec_2.S
  SETUP_SNIP, // assembly/build/setup.S
  GET_LIST_SNIP, // assembly/build/get_list.S
  PUT_LIST_SNIP, // assembly/build/put_list.S
  CALL_EXT_SNIP, // assembly/build/call_ext.S
  DEBUG_EXECUTE_ARIBITRARY_SNIP, // assembly/build/debug_execute_aribitrary.S
  COMPILE_STUB_SNIP, // assembly/build/compile_stub.S
};

std::vector<uint8_t> get_riscv(AsmSnippet snippet);

#endif
