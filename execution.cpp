#include "exceptions.h"
#include <bitset>
#include <cstdint>
#define GLOG_USE_GLOG_EXPORT
#include <glog/logging.h>

#include <cassert>

#include "beam_defs.h"
#include "op_arity.h"
#include "pcb.h"

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
    case LINE_OP: {
      // ignore, debug info
      break;
    }
    case FUNC_INFO_OP: {
      // could check function identifier instead
      if (instr_p > func_start + 1) {
        // exit loop here
        throw NotImplementedException(
            "don't know what to do when I reach the end!");
      } // else ignore
      break;
    }
    default: {
      // do the average cases here (most non-'meta' cases)
    }
    }
  }

  throw NotImplementedException("once code is compiled doesn't return it yet");
  return nullptr;
}

std::vector<uint8_t> compile_instruction(const Instruction &instr) {
  switch (instr.opCode) {
  default: {
    LOG(FATAL) << "opCode " << instr.opCode << " is not implemented yet.";
  }
  }
}

struct RISCV_Instruction {
  uint8_t raw[4];
};

RISCV_Instruction create_load_doubleword(uint8_t rd, uint8_t rs, uint16_t imm) {
  // since that's the size of immediate we can directly inject
  assert(imm < 2048);

  throw NotImplementedException("must do");
}

uint64_t *get_compact_and_cache_instr_args(Instruction &instr) {
  if (instr.compacted_args != nullptr) {
    return instr.compacted_args;
  }

  auto num_args = instr.arguments.size();

  instr.compacted_args = new uint64_t[num_args];

  // do compaction
  for (int i = 0; i < num_args; i++) {
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
  ErlTerm *arena = new ErlTerm[ARENA_SIZE];
  ErlTerm *stop = arena + (ARENA_SIZE - 1);
  ErlTerm *htop = arena;

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
