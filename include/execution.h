#ifndef EXECUTION_H
#define EXECUTION_H

#include <cstdint>
#include "beam_defs.h"

struct RISCV_Instruction {
  uint8_t raw[4];

  RISCV_Instruction(uint32_t instr) {
    memcpy(this->raw, &instr, 4);
  }
};

RISCV_Instruction create_load_doubleword(uint8_t rd, uint8_t rs, int16_t imm);

void run_code_section(CodeChunk &code_chunk, const CodeSection code_sec,
                      ProcessControlBlock *pcb);

#endif
