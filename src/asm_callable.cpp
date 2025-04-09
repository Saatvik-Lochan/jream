#include "asm_callable.hpp"
#include "execution.hpp"
#include "external_term.hpp"
#include "pcb.hpp"
#include <compare>
#include <iostream>

void print_int(uint64_t a) { std::cout << a << std::endl; }

const uint8_t *get_or_compile_label(CodeChunk *code_chunk, uint64_t label) {
  uint64_t func_index = code_chunk->label_func_table[label];

  auto &compiled_code = code_chunk->compiled_functions[func_index];

  if (compiled_code == nullptr) {
    compiled_code = compile_erlang_func(*code_chunk, func_index);
  }

  auto offset = code_chunk->label_offsets[label];
  auto label_loc = compiled_code + offset;

  return label_loc;
}

// need an external function to get the offsets for code_chunk right
void update_code_chunk_registers(CodeChunk *code_chunk) {

  asm volatile("mv s2, %0\n"
               "mv s6, %1\n"
               "mv s10, %2\n"
               :
               : "r"(code_chunk->compacted_arg_p_array),
                 "r"(code_chunk->label_jump_locations),
                 "r"(code_chunk->external_jump_locations)
               :);

  // should not clobber s6, s10 since we are updating it for the
  // following parts of the program.

  // by sheer luck this works
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wuninitialized"

  register ProcessControlBlock *pcb asm("s1");
  pcb->set_shared<CODE_CHUNK_P>(code_chunk);

#pragma GCC diagnostic pop
}

void send_message(ErlTerm *xregs) {

  uint64_t destination_pid = xregs[0];
  uint64_t message = xregs[1];

  ProcessControlBlock *process = from_pid(destination_pid);

  // TODO fix this if garbage collection
  // assume there is space!
  auto heap_top = process->get_shared<HTOP>();
  auto copied_handle = deepcopy(message, heap_top); // updates heap_top
  process->set_shared<HTOP>(heap_top);

  auto msg = new Message(copied_handle);
  process->queue_message(msg);

  emulator_main.scheduler.signal(process);
}

uint64_t compare(uint64_t term1, uint64_t term2) {
  auto result = ErlTerm(term1) <=> ErlTerm(term2);

  if (result == std::strong_ordering::less)
    return -1;
  if (result == std::strong_ordering::greater)
    return 1;

  return 0;
}

void free_msg(Message *msg) { delete msg; }
