
#include <glog/logging.h>

#include "instr_code.h"

std::vector<uint8_t> get_riscv(OpCode op) {
  switch (op) {
  case CALL_OP:
    return { 0x13, 0x01, 0x81, 0xff, 0x23, 0x30, 0x11, 0x00, 0x83, 0xb2, 0x89, 0x00, 0x83, 0xb2, 0x84, 0x00, 0xe7, 0x00, 0x0a, 0x00, 0x83, 0x30, 0x01, 0x00, 0x13, 0x01, 0x81, 0x00, 0x67, 0x80, 0x00, 0x00 };
  case CALL_FUN_OP:
    return { 0x13, 0x01, 0x81, 0xff, 0x23, 0x30, 0x11, 0x00, 0x83, 0x32, 0x01, 0x00, 0x83, 0xb2, 0x89, 0x00, 0x83, 0xb2, 0x84, 0x01, 0xe7, 0x00, 0x0a, 0x00, 0x83, 0x30, 0x01, 0x00, 0x13, 0x01, 0x81, 0x00, 0x67, 0x80, 0x00, 0x00 };
  case ALLOCATE_OP:
    return { 0x13, 0x05, 0x50, 0x00, 0xe7, 0x00, 0x0a, 0x00 };

  default:
    LOG(FATAL) << "opCode " << op << " has not been implemented yet";
  }
}
