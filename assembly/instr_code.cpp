#define GLOG_USE_GLOG_EXPORT
#include <glog/logging.h>

// generate file here!
#include "instr_code.h"

std::vector<uint8_t> get_riscv(OpCode op) {
  switch (op) {
  case ALLOCATE_OP: {
    return {0x83, 1, 2, 3};
  }
  default:
    LOG(FATAL) << "opCode " << op << " has not been implemented yet";
  }
}
