#include "precompiled.hpp"
#include "translation.hpp"
#include "generated/instr_code.hpp"

goto_asm_label_p get_goto_label_code() {
  auto code = get_riscv(SETUP_SNIP);

  return reinterpret_cast<goto_asm_label_p>(move_code_to_memory(code));
}

uint8_t *get_teardown_code() {
  auto code = get_riscv(TEARDOWN_SNIP);
  return move_code_to_memory(code);
}

uint8_t *get_compile_stub_code() {
  auto code = get_riscv(COMPILE_STUB_SNIP);
  return move_code_to_memory(code);
}

const goto_asm_label_p PreCompiled::setup_and_goto_label = get_goto_label_code();
const uint8_t *PreCompiled::teardown_code = get_teardown_code();
const uint8_t *PreCompiled::compile_stub = get_compile_stub_code();
