#ifndef INSTR_CODE_H
#define INSTR_CODE_H

#include <cstdint>
#include <vector>

enum AsmSnippet {
  GET_LIST_SNIP, // assembly/get_list_m.S
  LOAD_3_ARGS_SNIP, // assembly/load_3_args_m.S
  CALL_SNIP, // assembly/call_m.S
  CALL_FUN_SNIP, // assembly/call_fun_m.S
  DEALLOCATE_SNIP, // assembly/deallocate_m.S
  TEARDOWN_SNIP, // assembly/teardown_m.S
  SETUP_SNIP, // assembly/setup_m.S
  ALLOCATE_SNIP, // assembly/allocate_m.S
  TEMP_SNIP, // assembly/temp_m.S
};

std::vector<uint8_t> get_riscv(AsmSnippet snippet);

#endif
