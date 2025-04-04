#ifndef PCB_H
#define PCB_H

#include "external_term.hpp"
#include <cstdint>

#include "beam_defs.hpp"
#include "generated/shared_variables.hpp"

#define ENUM_TYPE(ENUM, TYPE)                                                  \
  template <> struct getFieldType<ENUM> {                                      \
    using type = TYPE;                                                         \
  };

template <PCBSharedFields> struct getFieldType {
  using type = void;
};

ENUM_TYPE(HTOP, ErlTerm *);
ENUM_TYPE(STOP, ErlTerm *);
ENUM_TYPE(XREG_ARRAY, ErlTerm *);
ENUM_TYPE(CODE_CHUNK_P, CodeChunk *);
ENUM_TYPE(CODE_POINTER, uint8_t *);
ENUM_TYPE(REDUCTIONS, uint64_t);
ENUM_TYPE(RESUME_LABEL, uint64_t);

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
};

#endif
