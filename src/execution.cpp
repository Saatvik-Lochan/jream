#include <glog/logging.h>

#include <cstdint>
#include <cassert>

#include "instr_code.h"
#include "beam_defs.h"
#include "op_arity.h"
#include "pcb.h"
#include "exceptions.h"

// garbage collection is tricky
uint8_t *translate_function(Instruction *func_start,
                            ProcessControlBlock *pcb_p) {
  std::vector<uint8_t> compiled;
  std::unordered_map<uint64_t, size_t> label_pointers;

  for (auto instr_p = func_start; instr_p++;) {

    switch (instr_p->opCode) {
    case LABEL_OP: {
      auto label_arg = instr_p->arguments[0];
      assert(label_arg.tag == LITERAL_TAG);
      label_pointers[label_arg.arg_raw.arg_num] = compiled.size();
      break;
    }

    case LINE_OP: { // ignore, debug info
      break;
    }

    case FUNC_INFO_OP: { // could check function identifier instead
      if (instr_p > func_start + 1) {
        goto end;
      }

      break;
    }
    default: {
      // create and add load here
      auto result = get_riscv(instr_p->opCode);
    }
    }
  }
// TODO should probably remove this somehow
end:

  // add starting move of a0, a1 to s1, s2 and saving of s1, s2 on stack
  // add compiled code
  // add return of s1, s2

  throw NotImplementedException("once code is compiled doesn't return it yet");
  return nullptr;
}

struct RISCV_Instruction {
  uint8_t raw[4];
};

RISCV_Instruction create_load_doubleword(uint8_t rd, uint8_t rs, int16_t imm) {
  // since that's the size of immediate we can directly inject
  assert(-2047 < imm && imm < 2048);
  assert(0 <= rd && rd < 32);
  assert(0 <= rs && rs < 32);

  uint32_t instr = 0;
  constexpr auto load_instr_bits = 0b0000011; //ld
  constexpr auto funct3_bits = 0b011; // ld
  constexpr auto load_and_funct = load_instr_bits & (funct3_bits << 12);

  const auto dest_reg_bits = rd & 0b11111;
  const auto source_reg_bits = rs & 0b11111;

  instr |= load_and_funct;
  instr |= dest_reg_bits << 7;
  instr |= source_reg_bits << 15;
  instr |= imm << 20;

  RISCV_Instruction out;
  memcpy(out.raw, &instr, 4);

  return out;
}

uint64_t *get_compact_and_cache_instr_args(Instruction &instr) {
  if (instr.compacted_args != nullptr) {
    return instr.compacted_args;
  }

  auto num_args = instr.arguments.size();

  instr.compacted_args = new uint64_t[num_args];

  // do compaction
  for (size_t i = 0; i < num_args; i++) {
    const auto &argument = instr.arguments[i];

    // not implemented yet
    assert(argument.tag != EXT_LIST_TAG && argument.tag != EXT_ALLOC_LIST_TAG);

    instr.compacted_args[i] = argument.arg_raw.arg_num;
  }

  return instr.compacted_args;
}

void spawn_process(const CodeChunk &code_chunk, FunctionIdentifier f_id) {
  // initialise memory
  //  i.e. stack + heap (and old heap)
  const size_t ARENA_SIZE = 1024;
  [[maybe_unused]] ErlTerm *arena = new ErlTerm[ARENA_SIZE];
  [[maybe_unused]] ErlTerm *stop = arena + (ARENA_SIZE - 1);
  [[maybe_unused]] ErlTerm *htop = arena;

  Instruction *start_instruction_p = code_chunk.function_table.at(f_id);

  // TODO fix the constructor for this
  ProcessControlBlock pcb;

  [[maybe_unused]]
  uint8_t *code = translate_function(start_instruction_p, &pcb);
  // cache code?
  // load code
  // execute code

  // have to deal with reducing reductions on a call as well, then
  // do more scheduling shit
}
