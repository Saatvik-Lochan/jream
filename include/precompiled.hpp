#ifndef PRECOMPILED_H
#define PRECOMPILED_H

#include "execution.hpp"
#include "pcb.hpp"

typedef ErlReturnCode (*goto_asm_label_p)(
    ProcessControlBlock *pcb, uint64_t **arg_array, std::uintptr_t func_array[],
    const uint8_t *volatile *label_loc_pointers, uint64_t label_num,
    const uint8_t *teardown_code, volatile ext_func *external_jump_locations);

namespace PreCompiled {
extern const goto_asm_label_p setup_and_goto_label;
extern const uint8_t *teardown_code;
extern const uint8_t *compile_stub;
} // namespace PreCompiled

#endif
