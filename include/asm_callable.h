// Do not add additional comments to this file
// It is parsed by meta_assembly_compiler.py

#ifndef ASM_CALLABLE_H
#define ASM_CALLABLE_H

#include <cstdint>
void print_int(uint64_t a);

// This array can not be longer than 2048 / 8 = 256 elements long
// so we can access the elemnts in one instruction from riscv.
//
// Every function must have a comment with it's meta meta assembly name
// or the indexing will be wrong
inline std::uintptr_t all_funs[] = {
    reinterpret_cast<std::uintptr_t>(&print_int), // m_asm: PRINT_INT
};

#endif
