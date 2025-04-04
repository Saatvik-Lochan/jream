#include "asm_callable.hpp"
#include "execution.hpp"
#include "external_term.hpp"
#include <iostream>

void print_int(uint64_t a) { std::cout << a << std::endl; }

const uint8_t *get_or_compile_label(CodeChunk *code_chunk, uint64_t label) {
  uint64_t func_index = code_chunk->label_func_table[label];

  auto& compiled_code = code_chunk->compiled_functions[label];

  if (compiled_code == nullptr) {
    compiled_code = compile_erlang_func(*code_chunk, func_index);
  }

  auto offset = code_chunk->label_offsets[label];
  auto label_loc = compiled_code + offset;

  return label_loc;
}

void send_message(ErlTerm message, ErlTerm destination_pid) {

  ProcessControlBlock *process =
      reinterpret_cast<ProcessControlBlock *>(destination_pid & (-0x10));

  // TODO fix this if garbage collection
  // assume there is space!
  auto heap_top = process->get_shared<HTOP>();
  auto copied_handle = deepcopy(message, heap_top); // updates heap_top
  process->set_shared<HTOP>(heap_top);
  
  emulator_main.scheduler.signal(process);
}
