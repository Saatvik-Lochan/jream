#ifndef EXECUTION_H
#define EXECUTION_H

#include "beam_defs.hpp"
#include "pcb.hpp"
#include <cassert>
#include <cstdint>
#include <type_traits>
#include <unordered_map>
#include <unordered_set>

// must match with meta_assembly_compile
enum ErlReturnCode { ERROR = -1, FINISH = 0, YIELD = 1, WAIT = 2 };

struct Scheduler {
  std::unordered_set<ProcessControlBlock *> runnable;
  std::unordered_set<ProcessControlBlock *> waiting;

  ProcessControlBlock *executing_process = nullptr;

  ProcessControlBlock *pick_next();
  bool signal(ProcessControlBlock *process);
};

struct Emulator {
  Scheduler scheduler;
  std::unordered_map<std::string, BeamSrc *> beam_sources;

  void register_beam_sources(std::vector<BeamSrc *>);
  void run(GlobalFunctionId id);
  EntryPoint get_entry_point(GlobalFunctionId);
};

inline Emulator emulator_main;

static_assert(std::is_standard_layout_v<Message>,
              "Message is not standard layout. Required.");

uint8_t *move_code_to_memory(const std::vector<uint8_t> &code);
void run_code_section(CodeChunk &code_chunk, const CodeSection code_sec,
                      ProcessControlBlock *pcb);
uint8_t *compile_erlang_func(CodeChunk &code_chunk, uint64_t func_index);

ProcessControlBlock *create_process(EntryPoint entry_point);

ErlReturnCode resume_process(ProcessControlBlock *pcb);

#endif
