#include "asm_callable.hpp"
#include "asm_utility.hpp"
#include "execution.hpp"
#include "external_term.hpp"
#include "op_arity.hpp"
#include "pcb.hpp"
#include "profiler.hpp"
#include "translation.hpp"
#include <compare>
#include <glog/logging.h>
#include <iostream>

void print_int(uint64_t a) { std::cout << a << std::endl; }

const uint8_t *get_or_compile_label(CodeChunk *code_chunk, uint64_t label) {
  PROFILE();
  uint64_t func_index = code_chunk->label_func_table[label];

  auto &compiled_code = code_chunk->compiled_functions[func_index];

#ifdef ENABLE_JIT_LOG
  LOG(INFO) << "\tcompiling label: " << label;
#endif

  if (compiled_code == nullptr) {
#ifdef ENABLE_JIT_LOG
    LOG(INFO) << "\tcompiling function: " << func_index;
#endif
    compiled_code = compile_erlang_func(*code_chunk, func_index);
  }

  auto offset = code_chunk->label_offsets[label];
  auto label_loc = compiled_code + offset;

  // assert label_loc is four byte aligned
  assert((reinterpret_cast<uintptr_t>(label_loc) & 0b11) == 0);

  return label_loc;
}

// need an external function to get the offsets for code_chunk right
void update_code_chunk_registers(CodeChunk *code_chunk) {

  asm volatile("mv s6, %0\n"
               "mv s10, %1\n"
               :
               : "r"(code_chunk->label_jump_locations),
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
  PROFILE();

  uint64_t destination_pid = xregs[0];
  uint64_t message = xregs[1];

  ProcessControlBlock *process = from_pid(destination_pid);

  auto size = get_heap_size(message);
  auto heap_frag = process->allocate_heap_frag(size);
  auto copied_handle = deepcopy(message, heap_frag);

  auto msg = new Message(copied_handle);
  process->queue_message(msg);

  emulator_main.scheduler.signal(process);
}

uint64_t compare(uint64_t term1, uint64_t term2) {
  PROFILE();
  auto result = ErlTerm(term1) <=> ErlTerm(term2);

  if (result == std::strong_ordering::less)
    return -1;
  if (result == std::strong_ordering::greater)
    return 1;

  return 0;
}

void free_msg(Message *msg) { delete msg; }

void print_op_name(uint64_t op_code) {
  LOG(INFO) << op_names[op_code] << ": " << op_code;
}

void execute_minor_gc(size_t new_term_size, size_t xregs) {
  auto pcb = get_pcb();
  pcb->do_gc(new_term_size, xregs);
}

void log_label(uint64_t label) { LOG(INFO) << "|-> AT LABEL: " << label; }

void log_xregs(size_t num_xregs) {
  auto xregs = get_pcb()->get_shared<XREG_ARRAY>();

  LOG(INFO) << "x registers at this moment:";
  for (size_t i = 0; i < num_xregs; i++) {
    LOG(INFO) << "  " << i << ". " << to_string(xregs[i]);
  }
}

void log_function(size_t module_index, size_t function_index, size_t arity) {
  auto module_name =
      emulator_main.get_atom_string_current(make_atom(module_index));
  auto function_name =
      emulator_main.get_atom_string_current(make_atom(function_index));

  LOG(INFO) << "Entering function - " << module_name << ":" << function_name
            << "/" << arity;
}
