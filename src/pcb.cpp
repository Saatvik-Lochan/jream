#include "pcb.hpp"
#include "external_term.hpp"
#include "garbage_collection.hpp"
#include <algorithm>
#include <iterator>

ErlTerm make_pid(ProcessControlBlock *pcb) {
  return (reinterpret_cast<uint64_t>(pcb) & PID_TAGGING_MASK) + 0b0011;
}

ProcessControlBlock *from_pid(ErlTerm term) {
  assert(term.getTagType() == PID_T);

  return reinterpret_cast<ProcessControlBlock *>(term & PID_TAGGING_MASK);
}

void ProcessControlBlock::queue_message(Message *msg) {
  auto mbox_tail = get_shared<MBOX_TAIL>();
  *mbox_tail = msg;
  set_shared<MBOX_TAIL>(msg->get_next_address());
}

ErlTerm *ProcessControlBlock::allocate_heap(size_t size) {
  const auto heap = get_shared<HTOP>();
  const auto new_top = heap + size;

  assert(new_top <= get_shared<STOP>());

  set_shared<HTOP>(new_top);

  return heap;
}

ErlTerm *ProcessControlBlock::allocate_tuple(size_t size) {
  assert((size << 6) >> 6 == size);

  auto heap_slots = allocate_heap(size + 1);
  heap_slots[0] = size << 6;

  return heap_slots;
}

std::span<ErlTerm> ProcessControlBlock::get_next_to_space(size_t alloc_amount) {
  // in words
  auto required_amount = heap.size() + alloc_amount;

  if (prev_to_space.size() >= required_amount) {
    return prev_to_space;
  }

  // otherwise we allocate more
  auto wanted_amount = required_amount / 6 * 10;
  auto new_to_space = new ErlTerm[wanted_amount];

  // free previous
  delete[] prev_to_space.data();
  return {new_to_space, wanted_amount};
}

std::vector<std::span<ErlTerm>>
ProcessControlBlock::get_root_set(size_t xregs) {
  std::vector<std::span<ErlTerm>> out;

  // stack
  out.push_back(get_stack());

  // registers
  out.push_back(std::span<ErlTerm>{get_shared<XREG_ARRAY>(), xregs});

  // mboxes
  auto message = get_shared<MBOX_HEAD>();

  while (message != nullptr) {
    out.push_back(std::span<ErlTerm>{message->get_payload_address(), 1});
    message = message->get_next();
  }

  return out;
}

ErlTerm *ProcessControlBlock::allocate_and_gc(size_t size, size_t xregs) {
  auto htop = get_shared<HTOP>();
  auto new_top = htop + size;

  ErlTerm *stop = get_shared<STOP>();

  if (new_top >= stop) {
    std::span<ErlTerm> to_space = get_next_to_space(size);

    // copy stack
    std::ranges::copy_backward(get_stack(), to_space.end());

    // do gc
    auto root_set = get_root_set(xregs);
    auto result = minor_gc(root_set, to_space.data(),
                           {
                               .heap_start = heap.data(),
                               .heap_top = htop,
                               .highwater = highwater,
                           },
                           old_heap);

    // store in case we need in the future
    prev_to_space = heap;

    // set new values
    heap = to_space;
    set_shared<HTOP>(result.heap_top + size); // new top after alloc
    highwater = result.highwater;

    htop = result.heap_top;
  }

  return htop;
}
