#include "external_term.h"
#include <cstdint>
#ifndef PCB_H
#define PCB_H

#include "generated/shared_variables.h"

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

struct ProcessControlBlock {
  volatile uint64_t shared[SHARED_FIELDS];

  template <PCBSharedFields Field>
  inline typename getFieldType<Field>::type get_shared() {
    return reinterpret_cast<getFieldType<Field>::type>(this->shared[Field]);
  }

  template <PCBSharedFields Field>
  inline void set_shared(typename getFieldType<Field>::type new_val) {
    this->shared[Field] = reinterpret_cast<uint64_t>(new_val);
  }
};

#endif
