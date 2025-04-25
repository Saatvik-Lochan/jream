#include "pcb.hpp"
#include "external_term.hpp"
#include "garbage_collection.hpp"
#include "precompiled.hpp"
#include "profiler.hpp"
#include <algorithm>
#include <glog/logging.h>
#include <iterator>
#include <memory>
#include <mutex>

ErlTerm make_pid(ProcessControlBlock *pcb) {
  return (reinterpret_cast<uint64_t>(pcb) & PID_TAGGING_MASK) + 0b0011;
}

ProcessControlBlock *from_pid(ErlTerm term) {
  assert(term.getTagType() == PID_T);

  return reinterpret_cast<ProcessControlBlock *>(term & PID_TAGGING_MASK);
}

void ProcessControlBlock::queue_message(Message *msg) {
  PROFILE();
  std::lock_guard<std::mutex> guard(message_queue);
  auto mbox_tail = get_shared<MBOX_TAIL>();
  *mbox_tail = msg;
  set_shared<MBOX_TAIL>(msg->get_next_address());
}

bool ProcessControlBlock::msg_q_empty() {
  std::lock_guard<std::mutex> guard(message_queue);
  auto tail = get_shared<MBOX_TAIL>();
  auto saved = get_shared<MBOX_SAVE>();

  return tail == saved;
}

std::span<ErlTerm> get_next_to_space(ProcessControlBlock *pcb,
                                     size_t alloc_amount) {
  PROFILE();
  // in words
  auto highwater_num = std::distance(pcb->heap.data(), pcb->highwater);
  auto required_amount = pcb->heap.size() + alloc_amount - highwater_num;

  for (auto v : pcb->heap_fragments) {
    required_amount += v.size();
  }

  if (pcb->prev_to_space.size() >= required_amount) {
    return pcb->prev_to_space;
  }

  // otherwise we allocate more
  auto wanted_amount = required_amount * 2;
  auto new_to_space = new ErlTerm[wanted_amount];

  // free previous
  auto prev_data = pcb->prev_to_space.data();
  if (prev_data) {
    MLOG("Free: prev_to_space - " << pcb->prev_to_space.size());
    delete[] prev_data;
  }

  return {new_to_space, wanted_amount};
}

std::vector<std::span<ErlTerm>>
get_root_set(ProcessControlBlock *pcb, size_t xregs, std::span<ErlTerm> stack) {
  std::vector<std::span<ErlTerm>> out;

  // stack
  out.push_back(stack);

  // registers
  out.push_back(std::span<ErlTerm>{pcb->get_shared<XREG_ARRAY>(), xregs});

  // mboxes
  auto message = pcb->get_shared<MBOX_HEAD>();

  while (message != nullptr) {
    out.emplace_back(std::span<ErlTerm>(message->get_payload_address(), 1));
    message = message->get_next();
  }

  // process dict elements
  for (auto& [_, value] : pcb->process_dict) {
    out.emplace_back(std::span<ErlTerm>(std::addressof(value), 1));
  }

  return out;
}

ErlTerm *ProcessControlBlock::do_gc(size_t size, size_t xregs) {
  PROFILE();

  auto htop = get_shared<HTOP>();
  std::span<ErlTerm> to_space = get_next_to_space(this, size);

#ifdef ENABLE_MEMORY_LOG
  LOG(INFO) << "GC: old_size: " << heap.size()
            << " | new_size: " << to_space.size();
#endif

  // copy stack
  auto stack_span = get_stack();
  std::span<ErlTerm> to_space_stack(to_space.end() - stack_span.size(),
                                    to_space.end());
  std::ranges::copy(stack_span, to_space_stack.data());

  // do gc
  std::span<ErlTerm> new_heap_space(to_space.data(),
                                    to_space.size() - to_space_stack.size());
  auto root_set = get_root_set(this, xregs, to_space_stack);
  auto result = minor_gc(root_set, new_heap_space,
                         {.heap_start = heap.data(),
                          .heap_top = htop,
                          .highwater = highwater,
                          .frags = heap_fragments},
                         old_heap);

  // store in case we need in the future
  prev_to_space = heap;

  // dealloc heap frags
  for (auto frags : heap_fragments) {
    delete[] frags.data();
  }

  heap_fragments.clear();

  // set new values
  heap = to_space;
  set_shared<HTOP>(result.heap_top + size); // new top after alloc
  set_shared<STOP>(to_space_stack.data());
  highwater = result.highwater;

  return result.heap_top;
}

ProcessControlBlock::ProcessControlBlock(EntryPoint entry_point,
                                         size_t heap_size) {
  PROFILE();
  set_shared<CODE_CHUNK_P>(entry_point.code_chunk);
  set_shared<RESUME_LABEL>(entry_point.label);
  set_shared<CODE_POINTER>(PreCompiled::teardown_code);

  // allocate space
  // TODO make xreg amount dynamic
  set_shared<XREG_ARRAY>(new ErlTerm[5]);

  auto heap = new ErlTerm[heap_size];
  set_shared<HTOP>(heap);
  set_shared<STOP>(heap + heap_size);
  this->heap = {heap, heap_size};
  highwater = heap;

  // message passing
  set_shared<MBOX_HEAD>(nullptr);
  auto head = get_address<MBOX_HEAD>();
  set_shared<MBOX_TAIL>(head);
  set_shared<MBOX_SAVE>(head);
}
