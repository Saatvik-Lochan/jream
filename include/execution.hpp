#ifndef EXECUTION_H
#define EXECUTION_H

#include "pcb.hpp"
#include <cassert>
#include <cstdint>
#include <type_traits>
#include <unordered_set>

struct RISCV_Instruction {
  uint8_t raw[4];

  RISCV_Instruction(uint32_t instr) { memcpy(this->raw, &instr, 4); }
  RISCV_Instruction() { memset(raw, 0, 4); }

  // all the 'set' functions assume the bits they are setting are all 0

  void set_opcode(uint8_t op_code) { set_bits(0, 7, op_code); }

  void set_rd(uint8_t rd) { set_bits(7, 12, rd); }

  void set_funct3(uint8_t funct3) { set_bits(12, 15, funct3); }

  void set_rs1(uint8_t rs1) { set_bits(15, 20, rs1); }

  void set_rs2(uint8_t rs2) { set_bits(20, 25, rs2); }

  void set_bits(uint8_t start, uint8_t stop, uint16_t value) {
    assert(value <= 1 << (stop - start));

    uint32_t *p = reinterpret_cast<uint32_t *>(raw);
    *p |= value << start;
  }
};

// must match with meta_assembly_compile
enum ErlReturnCode { FINISH = 0, YIELD = 1, ERROR = 2 };

struct Scheduler {
  std::unordered_set<ProcessControlBlock *> runnable;
  std::unordered_set<ProcessControlBlock *> waiting;

  ProcessControlBlock *pick_next();
  bool signal(ProcessControlBlock *process);
};

struct Emulator {
  Scheduler scheduler;
};

inline Emulator emulator_main;

struct Message {
  uint64_t values[2];

  inline explicit Message(ErlTerm e) { values[0] = e; };

  inline ErlTerm get_payload() { return ErlTerm(values[0]); }

  // we assume that the pointer to the Message struct
  // is the same as the pointer to the first field (values)
  // see static assert below
  inline void set_next(Message *next) {
    values[1] = reinterpret_cast<uint64_t>(next);
  }
};

static_assert(std::is_standard_layout_v<Message>,
              "Message is not standard layout. Required.");

RISCV_Instruction create_load_doubleword(uint8_t rd, uint8_t rs, int16_t imm);
RISCV_Instruction create_store_doubleword(uint8_t rd, uint8_t rs, int16_t imm);

uint8_t *move_code_to_memory(const std::vector<uint8_t> &code);
void run_code_section(CodeChunk &code_chunk, const CodeSection code_sec,
                      ProcessControlBlock *pcb);
uint8_t *compile_erlang_func(const CodeChunk &code_chunk, uint64_t func_index);

ProcessControlBlock *create_process(CodeChunk &code_chunk, uint64_t func_index);
ProcessControlBlock *create_process(CodeChunk &code_chunk);
ErlReturnCode resume_process(ProcessControlBlock *pcb);

void create_emulator(std::vector<BeamFile *> files);

#endif
