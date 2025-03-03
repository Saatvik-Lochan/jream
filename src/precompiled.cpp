#include "precompiled.hpp"
#include "execution.hpp"
#include "generated/instr_code.hpp"

enter_asm_func_p get_startup_code() {
  auto code = get_riscv(SETUP_SNIP);
  return reinterpret_cast<enter_asm_func_p>(move_code_to_memory(code));
}

uint8_t *get_teardown_code() {
  auto code = get_riscv(TEARDOWN_SNIP);
  return move_code_to_memory(code);
}

uint8_t *get_compile_stub_code() {
  auto code = get_riscv(COMPILE_STUB_SNIP);
  return move_code_to_memory(code);
}

const enter_asm_func_p PreCompiled::setup_and_enter_asm = get_startup_code();
const uint8_t *PreCompiled::teardown_code = get_teardown_code();
const uint8_t *PreCompiled::compile_stub = get_compile_stub_code();
