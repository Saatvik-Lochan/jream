#ifndef EXECUTION_H
#define EXECUTION_H

#include "beam_defs.hpp"
#include "pcb.hpp"
#include <cassert>
#include <condition_variable>
#include <cstdint>
#include <deque>
#include <queue>
#include <sched.h>
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

struct ThreadSafeQueue {
private:
  std::queue<ProcessControlBlock *> queue;
  std::mutex mtx;
  std::condition_variable cv;
  bool shutdown = false;

public:
  void push(ProcessControlBlock *p) {
    {
      std::lock_guard<std::mutex> lock(mtx);
      queue.push(std::move(p));
    }
    cv.notify_one();
  }

  bool pop(ProcessControlBlock *&out) {
    std::unique_lock<std::mutex> lock(mtx);
    cv.wait(lock, [&] { return shutdown || !queue.empty(); });

    if (shutdown && queue.empty())
      return false;

    out = std::move(queue.front());
    queue.pop();
    return true;
  }

  void stop() {
    {
      std::lock_guard<std::mutex> lock(mtx);
      shutdown = true;
    }
    cv.notify_all();
  }
};

struct WaitingPool {
private:
  std::unordered_set<ProcessControlBlock *> pool;
  std::mutex mtx;

public:
  // Insert a process into the pool
  void insert(ProcessControlBlock *pcb) {
    std::lock_guard<std::mutex> lock(mtx);
    pool.insert(pcb);
  }

  // Remove a process by ID; return it if found
  bool remove(ProcessControlBlock *pcb) {
    std::lock_guard<std::mutex> lock(mtx);
    return static_cast<bool>(pool.erase(pcb));
  }
};

struct Scheduler {
  ThreadSafeQueue runnable;
  WaitingPool waiting;

  bool signal(ProcessControlBlock *process);
  ProcessControlBlock *get_current_process() { return executing_process; }

private:
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
