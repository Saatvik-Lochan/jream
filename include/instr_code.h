#ifndef INSTR_CODE_H
#define INSTR_CODE_H

#include <cstdint>
#include <vector>
#include "op_arity.h"

std::vector<uint8_t> get_riscv(OpCode op);

#endif
