#include "external_term.h"
#include <cstdint>
#ifndef PCB_H
#define PCB_H

#include "generated/shared_variables.h"

struct ProcessControlBlock {
  volatile uint64_t shared[SHARED_FIELDS];
};

template <PCBSharedFields> struct getFieldType {
  using type = void;
};
template <> struct getFieldType<HTOP> {
  using type = ErlTerm *;
};
template <> struct getFieldType<STOP> {
  using type = ErlTerm *;
};
template <> struct getFieldType<ARENA> {
  using type = ErlTerm *;
};
template <> struct getFieldType<ARENA_SIZE> {
  using type = uint64_t;
};

template <PCBSharedFields Field>
inline typename getFieldType<Field>::type
get_shared(const ProcessControlBlock &pcb) {
  return reinterpret_cast<getFieldType<Field>::type>(pcb.shared[Field]);
}

#endif
