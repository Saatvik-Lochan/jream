#ifndef EXECUTION_H
#define EXECUTION_H

#include "pcb.hpp"
#include <cassert>
#include <cstdint>
#include <type_traits>
#include <unordered_set>

// must match with meta_assembly_compile
enum ErlReturnCode { FINISH = 0, YIELD = 1, ERROR = 2 };

struct Scheduler {
  std::unordered_set<ProcessControlBlock *> runnable;
  std::unordered_set<ProcessControlBlock *> waiting;

  ProcessControlBlock *pick_next();
  bool signal(ProcessControlBlock *process);
};

struct Emulator {
  Scheduler scheduler;
};

inline Emulator emulator_main;

static_assert(std::is_standard_layout_v<Message>,
              "Message is not standard layout. Required.");

uint8_t *move_code_to_memory(const std::vector<uint8_t> &code);
void run_code_section(CodeChunk &code_chunk, const CodeSection code_sec,
                      ProcessControlBlock *pcb);
uint8_t *compile_erlang_func(CodeChunk &code_chunk, uint64_t func_index);

ProcessControlBlock *create_process(CodeChunk &code_chunk, uint64_t func_index);
ProcessControlBlock *create_process(CodeChunk &code_chunk);
ErlReturnCode resume_process(ProcessControlBlock *pcb);

void create_emulator(std::vector<BeamSrc *> files);

#endif
