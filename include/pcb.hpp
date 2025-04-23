#ifndef PCB_H
#define PCB_H

#include "external_term.hpp"
#include "garbage_collection.hpp"
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
ENUM_TYPE(MBOX_TAIL, Message *volatile *)
ENUM_TYPE(MBOX_SAVE, Message *volatile *)

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

  // message passing
  void queue_message(Message *msg);

  // allocate on the pcb heap funcs
  // The default xreg value will make all xregisters dangling!
  ErlTerm *allocate_tuple(size_t size, size_t xregs = 0);
  ErlTerm *allocate_and_gc(size_t size, size_t xregs);
  std::span<ErlTerm> get_stack() {
    return std::span<ErlTerm>{get_shared<STOP>(), heap.data() + heap.size()};
  }
  ErlTerm *allocate_heap_frag(size_t size) {
    auto ptr = new ErlTerm[size];
    heap_fragments.push_back({ptr, size});
    return ptr;
  }

  // gc
  ErlTerm *do_gc(size_t size, size_t xregs);

  std::span<ErlTerm> heap;
  std::vector<std::span<ErlTerm>> heap_fragments;
  ErlTerm *highwater;

  GeneralPurposeHeap old_heap;
  std::span<ErlTerm> prev_to_space;

  // ctor/dtor
  ProcessControlBlock(EntryPoint entry_point, size_t heap_size = 1024);
  ~ProcessControlBlock() {
    old_heap.free_all();
    delete[] heap.data();
    delete[] get_shared<XREG_ARRAY>();
  }
};

constexpr uint64_t PID_TAGGING_MASK = ~0UL << 4;

ErlTerm make_pid(ProcessControlBlock *pcb);
ProcessControlBlock *from_pid(ErlTerm term);

#endif
