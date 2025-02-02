#include "op_arity.h"
#include <cassert>
#define GLOG_USE_GLOG_EXPORT
#include <glog/logging.h>

#include "beam_defs.h"

void spawn_process(const CodeChunk &code_chunk, FunctionIdentifier f_id) {
  // initialise memory
  //  i.e. stack + heap (and old heap)

  // execute code
  //  call function, compile and cache if necessary
}

// garbage collection is tricky
uint8_t *translate_function(Instruction *func_start) {
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
    default: {
      LOG(FATAL) << "opCode " << instr_p->opCode
                 << " has not been implemented yet";
    }
    }
  }

  return nullptr;
}
