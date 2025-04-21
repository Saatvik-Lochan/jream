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
enum ErlReturnCode {
  ERROR = -1,
  FINISH = 0,
  YIELD = 1,
  WAIT = 2,
  BADMATCH = 3,
  HEAP_SPACE = 4
};

struct Scheduler {
  std::unordered_set<ProcessControlBlock *> runnable;
  std::unordered_set<ProcessControlBlock *> waiting;

  ProcessControlBlock *pick_next();
  bool signal(ProcessControlBlock *process);

  ProcessControlBlock *get_current_process() { return executing_process; }

private:
  ProcessControlBlock *executing_process = nullptr;
};

struct Emulator {
  Scheduler scheduler;
  std::unordered_map<std::string, BeamSrc *> beam_sources;
  std::vector<ProcessControlBlock *> dead_processes;
  ProcessControlBlock *create_process(EntryPoint entry_point,
                                      size_t heap_size = 1024);

  void register_beam_sources(std::vector<BeamSrc *>);
  EntryPoint get_entry_point(GlobalFunctionId);
  ErlTerm get_atom_current(std::string atom_name);
  std::string get_atom_string_current(ErlTerm e);

  ErlTerm read_and_execute(std::string func_string);
  ErlTerm run(ProcessControlBlock *);
};

inline Emulator emulator_main;

static_assert(std::is_standard_layout_v<Message>,
              "Message is not standard layout. Required.");

ErlReturnCode resume_process(ProcessControlBlock *pcb);

uint64_t get_current_space();

#endif
