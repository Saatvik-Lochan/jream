#include <cstdlib>
#include <glog/logging.h>

#include <cassert>
#include <cstdint>
#include <stdexcept>
#include <string>
#include <strings.h>
#include <sys/mman.h>
#include <unordered_map>

#include "asm_callable.hpp"
#include "asm_utility.hpp"
#include "beam_defs.hpp"
#include "execution.hpp"
#include "external_term.hpp"
#include "pcb.hpp"
#include "precompiled.hpp"
#include "profiler.hpp"

ErlReturnCode setup_and_go_label(ProcessControlBlock *pcb, uint64_t label_num) {
  PROFILE();

  auto code_chunk = pcb->get_shared<CODE_CHUNK_P>();

  return PreCompiled::setup_and_goto_label(
      code_chunk, pcb, all_funs, PreCompiled::teardown_code, label_num);
}

uint64_t get_current_space() {
  auto pcb = emulator_main.scheduler.get_current_process();
  return pcb->get_shared<STOP>() - pcb->get_shared<HTOP>();
}

ErlReturnCode resume_process(ProcessControlBlock *pcb) {
  DLOG(INFO) << "\tresuming at label: " << pcb->get_shared<RESUME_LABEL>();
  return setup_and_go_label(pcb, pcb->get_shared<RESUME_LABEL>());
}

ProcessControlBlock *Emulator::create_process(EntryPoint entry_point,
                                              size_t heap_size) {
  PROFILE();
  ProcessControlBlock *pcb;

  if (dead_processes.empty()) {
    pcb = new ProcessControlBlock;
  } else {
    pcb = std::move(dead_processes.back());
    dead_processes.pop_back();
  }

  pcb->set_shared<CODE_CHUNK_P>(entry_point.code_chunk);
  pcb->set_shared<RESUME_LABEL>(entry_point.label);
  pcb->set_shared<REDUCTIONS>(100);
  pcb->set_shared<CODE_POINTER>(PreCompiled::teardown_code);

  // allocate space
  // TODO make xreg amount dynamic
  pcb->set_shared<XREG_ARRAY>(new ErlTerm[5]);

  auto heap = new ErlTerm[heap_size];
  pcb->set_shared<HTOP>(heap);
  pcb->set_shared<STOP>(heap + heap_size);
  pcb->heap = {heap, heap_size};
  pcb->highwater = heap;

  // message passing
  pcb->set_shared<MBOX_HEAD>(nullptr);
  auto head = pcb->get_address<MBOX_HEAD>();
  pcb->set_shared<MBOX_TAIL>(head);
  pcb->set_shared<MBOX_SAVE>(head);

  return pcb;
}

ProcessControlBlock *Scheduler::pick_next() {
  PROFILE();
  auto chosen_it = runnable.begin();

  if (chosen_it == runnable.end()) {
    return nullptr;
  }

  auto value = runnable.extract(chosen_it);
  auto pcb = value.value();

  pcb->set_shared<REDUCTIONS>(1000);

  executing_process = pcb;
  return pcb;
}

bool Scheduler::signal(ProcessControlBlock *process) {
  auto it = waiting.find(process);

  if (it == waiting.end()) {
    return false;
  }

  auto node = waiting.extract(it);
  runnable.insert(std::move(node));

  return true;
}

void Emulator::register_beam_sources(std::vector<BeamSrc *> sources) {
  for (auto source_p : sources) {
    beam_sources[source_p->module] = source_p;
  }
}

EntryPoint Emulator::get_entry_point(GlobalFunctionId function_id) {

  auto id_it = beam_sources.find(function_id.module);

  if (id_it == beam_sources.end()) {
    throw std::logic_error(std::format(
        "Could not find module '{}' in beam sources", function_id.module));
  }

  auto beam_src = id_it->second;
  auto export_id = beam_src->get_external_id(function_id);

  return EntryPoint{.code_chunk = &beam_src->code_chunk,
                    .label = export_id.label};
}

std::string queue_string(const std::unordered_set<ProcessControlBlock *> q) {
  std::string out = "{";

  if (q.empty()) {
    return "{}";
  }

  for (auto ele : q) {
    out += std::format("{:p}, ", static_cast<void *>(ele));
  }

  out.erase(out.size() - 2);

  out += "}";
  return out;
}

ErlTerm Emulator::get_atom_current(std::string atom_name) {
  auto pcb = scheduler.get_current_process();
  auto index =
      pcb->get_shared<CODE_CHUNK_P>()->atom_chunk->atom_index[atom_name];

  return make_atom(index);
}

std::string Emulator::get_atom_string_current(ErlTerm e) {
  assert(e.getTagType() == ATOM_T);

  auto index = e.term >> 6;

  auto pcb = scheduler.get_current_process();
  auto value = pcb->get_shared<CODE_CHUNK_P>()->atom_chunk->atoms[index];

  return value;
}

ErlTerm Emulator::run(GlobalFunctionId initial_func) {
  PROFILE();

  assert(initial_func.arity == 0);

  auto initial_entry_point = get_entry_point(initial_func);
  auto pcb = create_process(initial_entry_point);

  auto &scheduler = emulator_main.scheduler;
  scheduler.runnable.insert(pcb);

#ifdef EXEC_LOG
  auto count = 1;
#endif

  while (auto to_run = scheduler.pick_next()) {
    DLOG(INFO) << "Now executing: " << to_run;

    auto result = resume_process(to_run);

    switch (result) {
    case ERROR: {
      throw std::logic_error("Internal process finished with an error");
    }
    case FINISH: {
      DLOG(INFO) << "A process finished: " << to_run;

      // TODO check for issues with the message passing to a dead process
      emulator_main.dead_processes.push_back(to_run);
      break;
    }
    case YIELD: {
      DLOG(INFO) << "A process yielded: " << to_run;
      scheduler.runnable.insert(to_run);
      break;
    }
    case WAIT: {
      DLOG(INFO) << "A process is waiting: " << to_run;
      scheduler.waiting.insert(to_run);
      break;
    }
    case BADMATCH: {
      throw std::runtime_error("A process has failed due to a badmatch");
    }
    case HEAP_SPACE:
      throw std::runtime_error(
          "A process has failed due to a lack of heap space");
    }

#ifdef EXEC_LOG
    LOG(INFO) << "After " << count++ << ":";
    LOG(INFO) << "\twaiting: " << queue_string(scheduler.waiting);
    LOG(INFO) << "\trunnable: " << queue_string(scheduler.runnable);
#endif
  }

  return pcb->get_shared<XREG_ARRAY>()[0];
}
