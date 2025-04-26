#ifndef EXECUTION_H
#define EXECUTION_H

#include "beam_defs.hpp"
#include "pcb.hpp"
#include "profiler.hpp"
#include <algorithm>
#include <cassert>
#include <condition_variable>
#include <cstdint>
#include <deque>
#include <mutex>
#include <sched.h>
#include <sstream>
#include <type_traits>
#include <unordered_map>
#include <unordered_set>
#include "libs/blockingconcurrentqueue.h"

#ifdef ENABLE_SCHEDULER_LOG
#define SLOG(...) LOG(INFO) << __VA_ARGS__
#else
#define SLOG(...) (void)0
#endif

// must match with meta_assembly_compile
enum ErlReturnCode {
  ERROR = -1,
  FINISH = 0,
  YIELD = 1,
  WAIT = 2,
  BADMATCH = 3,
  HEAP_SPACE = 4
};

struct ThreadSafeQueue {
private:
  struct QueueVal {
    ProcessControlBlock *pcb;
    bool is_quit = false;
  };
  moodycamel::BlockingConcurrentQueue<QueueVal> queue;

public:
  void push(ProcessControlBlock *p) {
    PROFILE();
    queue.enqueue({p});
  }

  bool pop(ProcessControlBlock *&out) {
    PROFILE();
    QueueVal val;
    queue.wait_dequeue(val);

    if (val.is_quit == true) {
      return false;
    }

    out = val.pcb;

    return true;
  }

  void push_stop(size_t num) {
    auto stop_tokens = new QueueVal[num];
    QueueVal stop = {.pcb = nullptr, .is_quit = true};
    std::fill_n(stop_tokens, num, stop);
    queue.enqueue_bulk(stop_tokens, num);
  }

  bool contains(ProcessControlBlock *pcb) {
    return false;
  }
};

struct WaitingPool {
private:
  std::unordered_set<ProcessControlBlock *> pool;
  std::mutex mtx;

public:
  // Insert a process into the pool
  bool insert_if_empty(ProcessControlBlock *pcb) {
    PROFILE();
    std::lock_guard<std::mutex> lock(mtx);

    if (!pcb->msg_q_empty()) {
      return false;
    }

    SLOG(std::format("Inserting {:x} into the waiting pool",
                     reinterpret_cast<uintptr_t>(pcb)));
    pool.insert(pcb);
    return true;
  }

  // Remove a process by ID; return it if found
  bool remove(ProcessControlBlock *pcb) {
    PROFILE();
    std::lock_guard<std::mutex> lock(mtx);
    return static_cast<bool>(pool.erase(pcb));
  }

  bool contains(ProcessControlBlock *pcb) {
    std::lock_guard<std::mutex> lock(mtx);
    return pool.contains(pcb);
  }

  std::string print_contents() {
    std::lock_guard<std::mutex> lock(mtx);
    std::ostringstream out;

    out << "{ ";

    for (auto i : pool) {
      out << i << " ";
    }

    out << "}";

    return out.str();
  }
};

struct Scheduler {
  ThreadSafeQueue runnable;
  WaitingPool waiting;

  bool signal(ProcessControlBlock *process);
  ProcessControlBlock *get_current_process() { return executing_process; }

  static thread_local ProcessControlBlock *executing_process;
};

struct Emulator {
  Scheduler scheduler;
  std::unordered_map<std::string, BeamSrc *> beam_sources;
  std::vector<ProcessControlBlock *> dead_processes;

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
