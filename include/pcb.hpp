#ifndef PCB_H
#define PCB_H

#include "external_term.hpp"
#include "messages.hpp"
#include <cstdint>

#include "beam_defs.hpp"
#include "generated/shared_variables.hpp"

#define ENUM_TYPE(ENUM, TYPE)                                                  \
  template <> struct getFieldType<ENUM> {                                      \
    using type = TYPE;                                                         \
  };

template <PCBSharedFields> struct getFieldType {
  using type = void;
  using pointer_type = type *;
};

ENUM_TYPE(HTOP, ErlTerm *)
ENUM_TYPE(STOP, ErlTerm *)
ENUM_TYPE(XREG_ARRAY, ErlTerm *)
ENUM_TYPE(CODE_CHUNK_P, CodeChunk *)
ENUM_TYPE(CODE_POINTER, const uint8_t *)
ENUM_TYPE(REDUCTIONS, uint64_t)
ENUM_TYPE(RESUME_LABEL, uint64_t)
ENUM_TYPE(MBOX_HEAD, Message *)
ENUM_TYPE(MBOX_TAIL, Message * volatile*)
ENUM_TYPE(MBOX_SAVE, Message * volatile*)

// we align by 16 bytes so we use 4 tag in pointers
struct __attribute__((aligned(16))) ProcessControlBlock {
  volatile uint64_t shared[SHARED_FIELDS];

  template <PCBSharedFields Field>
  inline typename getFieldType<Field>::type get_shared() {
    return reinterpret_cast<getFieldType<Field>::type>(this->shared[Field]);
  }

  template <PCBSharedFields Field>
  inline void set_shared(typename getFieldType<Field>::type new_val) {
    this->shared[Field] = reinterpret_cast<uint64_t>(new_val);
  }

  template <PCBSharedFields Field>
  inline typename getFieldType<Field>::type volatile *get_address() {
    return reinterpret_cast<getFieldType<Field>::type volatile *>(shared +
                                                                  Field);
  }

  void queue_message(Message *msg);
  ErlTerm *allocate_heap(size_t size);
  ErlTerm *allocate_tuple(size_t size);
};

constexpr uint64_t PID_TAGGING_MASK = ~0UL << 4;

ErlTerm make_pid(ProcessControlBlock *pcb);
ProcessControlBlock *from_pid(ErlTerm term);

#endif
