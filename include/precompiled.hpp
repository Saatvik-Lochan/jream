#ifndef PRECOMPILED_H
#define PRECOMPILED_H

#include "pcb.hpp"

typedef void (*enter_asm_func_p)(ProcessControlBlock *pcb, uint64_t **arg_array,
                                 std::uintptr_t func_array[],
                                 const uint8_t *volatile *func_pointers,
                                 uint64_t func_index,
                                 const uint8_t *teardown_code);

namespace PreCompiled {
extern const enter_asm_func_p setup_and_enter_asm;
extern const uint8_t *teardown_code;
extern const uint8_t *compile_stub;
} // namespace PreCompiled

#endif
