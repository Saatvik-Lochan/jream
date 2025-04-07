// Do not add additional comments to this file
// It is parsed by meta_assembly_compiler.py

#ifndef ASM_CALLABLE_H
#define ASM_CALLABLE_H

#include "beam_defs.hpp"
#include "messages.hpp"
#include <cstdint>

void print_int(uint64_t a);
const uint8_t *get_or_compile_label(CodeChunk *code_chunk, uint64_t func_index);
void update_code_chunk_registers(CodeChunk *code_chunk);
void free_msg(Message *);

#define CAST(Func) reinterpret_cast<std::uintptr_t>(&Func)

// This array can not be longer than 2048 / 8 = 256 elements long
// so we can access the elemnts in one instruction from riscv.
//
// Every function must have a comment with it's meta meta assembly name
// or the indexing will be wrong
inline std::uintptr_t all_funs[] = {
    CAST(print_int),                   // m_asm: PRINT_INT
    CAST(get_or_compile_label),        // m_asm: COMPILE_LABEL
    CAST(update_code_chunk_registers), // m_asm: SET_NEW_CODE_CHUNK
    CAST(free_msg), // m_asm: FREE_MSG
};

#endif
