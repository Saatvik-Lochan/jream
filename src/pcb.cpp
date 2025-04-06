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

