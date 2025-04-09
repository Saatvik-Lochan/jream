#include "pcb.hpp"
#include "external_term.hpp"

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
