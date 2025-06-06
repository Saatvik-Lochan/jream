#include <cstdlib>
#include <glog/logging.h>

#include <cassert>
#include <cstdint>
#include <stdexcept>
#include <string>
#include <strings.h>
#include <sys/mman.h>

#include "asm_callable.hpp"
#include "asm_utility.hpp"
#include "beam_defs.hpp"
#include "beamparser.hpp"
#include "execution.hpp"
#include "bif.hpp"
#include "external_term.hpp"
#include "parsing.hpp"
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
#if defined(ENABLE_INSTR_LOG) || defined(ENABLE_JIT_LOG)
  LOG(INFO) << "\tresuming at label: " << pcb->get_shared<RESUME_LABEL>();
#endif
  return setup_and_go_label(pcb, pcb->get_shared<RESUME_LABEL>());
}

ProcessControlBlock *Scheduler::pick_next() {
  PROFILE();

  if (runnable.empty()) {
    executing_process = nullptr;
    return nullptr;
  }

  auto pcb = runnable.front();
  runnable.pop_front();

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
  runnable.push_back(std::move(node.value()));

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

template <std::ranges::range R> std::string get_queue_string(R q) {
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
  auto atom_index = pcb->get_shared<CODE_CHUNK_P>()->atom_chunk->atom_index;
  auto it = atom_index.find(atom_name);
      
  if (it == atom_index.end()) {
    return make_atom(0); // this atom will be ignored since it is never used
  }

  return make_atom(it->second);
}

std::string Emulator::get_atom_string_current(ErlTerm e) {
  assert(e.getTagType() == ATOM_T);

  auto index = e.term >> 6;

  auto pcb = scheduler.get_current_process();
  auto value = pcb->get_shared<CODE_CHUNK_P>()->atom_chunk->atoms[index];

  return value;
}

ErlTerm Emulator::run(ProcessControlBlock *pcb) {
  PROFILE();

  auto &scheduler = emulator_main.scheduler;
  scheduler.runnable.push_back(pcb);

#ifdef ENABLE_SCHEDULER_LOG
#define SLOG(...) LOG(INFO) << __VA_ARGS__
#else
#define SLOG(...) (void)0
#endif

#ifdef ENABLE_SCHEDULER_LOG
  auto count = 1;
#endif

  while (auto to_run = scheduler.pick_next()) {
    SLOG("Now executing: " << to_run);

    auto result = resume_process(to_run);

    switch (result) {
    case ERROR: {
      throw std::logic_error("Internal process finished with an error");
    }
    case FINISH: {
      if (to_run != pcb) {
        delete to_run;
      } else {
        io_write(to_run->get_shared<XREG_ARRAY>()[0]);
      }
      SLOG("A process finished: " << to_run);
      break;
    }
    case YIELD: {
      SLOG("A process yielded: " << to_run);
      scheduler.runnable.push_back(to_run);
      break;
    }
    case WAIT: {
      SLOG("A process is waiting: " << to_run);
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

    SLOG("After " << count++ << ":");
    SLOG("\twaiting: " << get_queue_string(scheduler.waiting));
    SLOG("\trunnable: " << get_queue_string(scheduler.runnable));
  }

  return pcb->get_shared<XREG_ARRAY>()[0];
}

ErlTerm Emulator::read_and_execute(std::string func_string) {
  PROFILE();

  auto [func_id, arguments] =
      parse_func_call<ArgumentAllocator>(func_string, ArgumentAllocator{});
  auto file_name = func_id.module + ".beam";

  auto beamfile = read_chunks(file_name);

#ifdef ENABLE_PARSE_LOG
  beamfile.log();
#endif

  emulator_main.register_beam_sources({&beamfile});

  auto initial_entry_point = get_entry_point(func_id);
  ProcessControlBlock pcb(initial_entry_point);
  auto xregs = pcb.get_shared<XREG_ARRAY>();

  std::ranges::copy(arguments, xregs);

  // can't print atoms past this point!
  auto value = emulator_main.run(&pcb);

  return value;
}
