
#include <glog/logging.h>

#include "instr_code.h"

std::vector<uint8_t> get_riscv(OpCode op) {
  switch (op) {
  case CALL_OP:
    return { 0x13, 0x01, 0x81, 0xff, 0x23, 0x30, 0x11, 0x00, 0x83, 0xb2, 0x89, 0x00, 0x83, 0xb2, 0x84, 0x01, 0x83, 0x32, 0x0a, 0x00, 0xe7, 0x80, 0x02, 0x00, 0x83, 0x30, 0x01, 0x00, 0x13, 0x01, 0x81, 0x00, 0x67, 0x80, 0x00, 0x00 };
  case CALL_FUN_OP:
    return { 0x13, 0x01, 0x81, 0xff, 0x23, 0x30, 0x11, 0x00, 0x83, 0x32, 0x01, 0x00, 0x83, 0xb2, 0x89, 0x00, 0x83, 0xb2, 0x04, 0x01, 0x83, 0x32, 0x0a, 0x00, 0xe7, 0x80, 0x02, 0x00, 0x83, 0x30, 0x01, 0x00, 0x13, 0x01, 0x81, 0x00, 0x67, 0x80, 0x00, 0x00 };
  case ALLOCATE_OP:
    return { 0x83, 0xb2, 0x09, 0x00, 0x03, 0xb3, 0x84, 0x00, 0x93, 0x92, 0x32, 0x00, 0x33, 0x03, 0x53, 0x00, 0x23, 0xb4, 0x64, 0x00 };

  default:
    LOG(FATAL) << "opCode " << op << " has not been implemented yet";
  }
}
