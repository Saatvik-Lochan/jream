#ifndef EXECUTION_H
#define EXECUTION_H

#include <cstdint>

struct RISCV_Instruction {
  uint8_t raw[4];
};

RISCV_Instruction create_load_doubleword(uint8_t rd, uint8_t rs, int16_t imm);

#endif
