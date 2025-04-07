// THIS FILE WAS GENERATED BY instr_code_gen.py
#include "generated/instr_code.hpp"
#include <glog/logging.h>

std::vector<uint8_t> get_riscv(AsmSnippet snippet) {
  switch (snippet) {

  case DEALLOCATE_SNIP:
    return { 0x03, 0xb3, 0x84, 0x00, 0x83, 0xb2, 0x09, 0x00, 0x83, 0x33, 0x03, 0x00, 0x23, 0xb4, 0x74, 0x02, 0x13, 0x03, 0x83, 0x00, 0x93, 0x92, 0x32, 0x00, 0x33, 0x03, 0x53, 0x00, 0x23, 0xb4, 0x64, 0x00 };

  case ENTER_FUNC_SNIP:
    return { 0x93, 0x12, 0x37, 0x00, 0xb3, 0x0c, 0x5b, 0x00, 0x83, 0xb2, 0x0c, 0x00, 0x67, 0x80, 0x02, 0x00 };

  case GOTO_LABEL_SNIP:
    return { 0x67, 0x00, 0x07, 0x00 };

  case CALL_EXT_BIF_SNIP:
    return { 0x83, 0xb2, 0x89, 0x00, 0x13, 0x93, 0x32, 0x00, 0xb3, 0x03, 0xa3, 0x01, 0x13, 0x85, 0x0a, 0x00, 0x83, 0xb5, 0x84, 0x01, 0x83, 0xb2, 0x03, 0x00, 0xe7, 0x80, 0x02, 0x00 };

  case LOOP_REC_1_SNIP:
    return { 0x83, 0xb2, 0x84, 0x04, 0x03, 0xb3, 0x04, 0x04 };

  case TEMP_SNIP:
    return { 0x83, 0x32, 0x41, 0x00 };

  case MAKE_FUN3_SNIP:
    return { 0x83, 0xb3, 0x09, 0x00, 0x03, 0xb3, 0x04, 0x00, 0x93, 0x8e, 0xf2, 0xff, 0x13, 0x9e, 0x6e, 0x00, 0x13, 0x0e, 0x4e, 0x01, 0x23, 0x30, 0xc3, 0x01, 0x23, 0x34, 0x73, 0x00, 0x93, 0x92, 0x32, 0x00, 0x33, 0x0f, 0x53, 0x00, 0x23, 0xb0, 0xe4, 0x01 };

  case RETURN_SNIP:
    return { 0x83, 0xb2, 0x84, 0x02, 0x67, 0x80, 0x02, 0x00 };

  case LOOP_REC_3_SNIP:
    return { 0x93, 0x02, 0x40, 0x00 };

  case ALLOCATE_SNIP:
    return { 0x83, 0xb2, 0x09, 0x00, 0x03, 0xb3, 0x84, 0x00, 0x93, 0x92, 0x32, 0x00, 0x33, 0x03, 0x53, 0x40, 0x13, 0x03, 0x83, 0xff, 0x83, 0xb3, 0x84, 0x02, 0x23, 0x30, 0x73, 0x00, 0x23, 0xb4, 0x64, 0x00 };

  case SEND_SNIP:
    return {  };

  case LOOP_REC0_SNIP:
    return { 0x83, 0xb3, 0x84, 0x03 };

  case CALL_SNIP:
    return { 0x83, 0xb5, 0x89, 0x00, 0x83, 0xb2, 0x04, 0x02, 0x63, 0x98, 0x02, 0x00, 0x23, 0xb8, 0xb4, 0x02, 0x13, 0x0c, 0x10, 0x00, 0x67, 0x80, 0x0b, 0x00, 0x93, 0x82, 0xf2, 0xff, 0x23, 0xb0, 0x54, 0x02, 0x13, 0x96, 0x35, 0x00, 0x33, 0x06, 0x66, 0x01, 0x03, 0x33, 0x06, 0x00, 0x97, 0x02, 0x00, 0x00, 0x93, 0x82, 0x02, 0x01, 0x23, 0xb4, 0x54, 0x02, 0x67, 0x00, 0x03, 0x00 };

  case CALL_FINISH_SNIP:
    return { 0x13, 0x16, 0x37, 0x00, 0x33, 0x06, 0x66, 0x01, 0x03, 0x33, 0x06, 0x00, 0x97, 0x02, 0x00, 0x00, 0x93, 0x82, 0x02, 0x01, 0x23, 0xb4, 0x54, 0x02, 0x67, 0x00, 0x03, 0x00 };

  case LOAD_1_ARG_SNIP:
    return { 0x83, 0xb2, 0x09, 0x00 };

  case LOAD_3_ARGS_SNIP:
    return { 0x83, 0xb2, 0x09, 0x00, 0x03, 0xb3, 0x09, 0x01, 0x03, 0xbe, 0x89, 0x01 };

  case STORE_DOUBLEWORD_TEST_SNIP:
    return { 0x23, 0xb4, 0x6a, 0x00 };

  case TEARDOWN_SNIP:
    return { 0x13, 0x05, 0x0c, 0x00, 0x83, 0x30, 0x01, 0x00, 0x83, 0x34, 0x81, 0x00, 0x03, 0x39, 0x01, 0x01, 0x83, 0x39, 0x81, 0x01, 0x03, 0x3a, 0x01, 0x02, 0x83, 0x3a, 0x81, 0x02, 0x03, 0x3b, 0x01, 0x03, 0x83, 0x3b, 0x81, 0x03, 0x03, 0x3c, 0x01, 0x04, 0x83, 0x3c, 0x81, 0x04, 0x83, 0x3c, 0x01, 0x05, 0x13, 0x01, 0x01, 0x06, 0x67, 0x80, 0x00, 0x00 };

  case CALL_SETUP_SNIP:
    return { 0x83, 0xb2, 0x04, 0x02, 0x63, 0x98, 0x02, 0x00, 0x23, 0xb8, 0xe4, 0x02, 0x13, 0x0c, 0x10, 0x00, 0x67, 0x80, 0x0b, 0x00, 0x93, 0x82, 0xf2, 0xff, 0x23, 0xb0, 0x54, 0x02 };

  case LOOP_REC_2_SNIP:
    return { 0x03, 0xb3, 0x02, 0x00, 0x83, 0x33, 0x03, 0x00 };

  case SETUP_SNIP:
    return { 0x13, 0x01, 0x01, 0xfa, 0x23, 0x38, 0xa1, 0x05, 0x23, 0x34, 0x91, 0x05, 0x23, 0x30, 0x81, 0x05, 0x23, 0x3c, 0x71, 0x03, 0x23, 0x38, 0x61, 0x03, 0x23, 0x34, 0x51, 0x03, 0x23, 0x30, 0x41, 0x03, 0x23, 0x3c, 0x31, 0x01, 0x23, 0x38, 0x21, 0x01, 0x23, 0x34, 0x91, 0x00, 0x23, 0x30, 0x11, 0x00, 0x93, 0x84, 0x05, 0x00, 0x13, 0x0a, 0x06, 0x00, 0x83, 0xba, 0x04, 0x01, 0x93, 0x8b, 0x06, 0x00, 0x13, 0x0c, 0x00, 0x00, 0x93, 0x0c, 0x07, 0x00, 0x83, 0x32, 0x0a, 0x01, 0xe7, 0x80, 0x02, 0x00, 0x23, 0xb4, 0x74, 0x03, 0x93, 0x85, 0x0c, 0x00, 0x93, 0x92, 0x3c, 0x00, 0xb3, 0x0c, 0x5b, 0x00, 0x83, 0xb2, 0x0c, 0x00, 0x67, 0x80, 0x02, 0x00 };

  case GET_LIST_SNIP:
    return { 0x93, 0xf2, 0xc2, 0xff, 0x03, 0xb3, 0x02, 0x00, 0x83, 0xb3, 0x82, 0x00 };

  case CALL_EXT_SNIP:
    return { 0x83, 0xb2, 0x89, 0x00, 0x13, 0x9e, 0x32, 0x00, 0xb3, 0x82, 0xa2, 0x01, 0x83, 0x32, 0x0a, 0x01, 0xe7, 0x80, 0x02, 0x00, 0x83, 0xb2, 0x04, 0x02, 0x63, 0x98, 0x02, 0x00, 0x23, 0xb8, 0xe4, 0x02, 0x13, 0x0c, 0x10, 0x00, 0x67, 0x80, 0x0b, 0x00, 0x93, 0x82, 0xf2, 0xff, 0x23, 0xb0, 0x54, 0x02, 0x13, 0x16, 0x37, 0x00, 0x33, 0x06, 0x66, 0x01, 0x03, 0x33, 0x06, 0x00, 0x97, 0x02, 0x00, 0x00, 0x93, 0x82, 0x82, 0x01, 0x23, 0xb4, 0x54, 0x02, 0x67, 0x00, 0x03, 0x00, 0x13, 0x00, 0x00, 0x00, 0x13, 0x00, 0x00, 0x00, 0x97, 0x02, 0x00, 0x00, 0x93, 0x82, 0x82, 0xff, 0x03, 0xb5, 0x02, 0x00, 0x83, 0x32, 0x0a, 0x01, 0xe7, 0x80, 0x02, 0x00 };

  case DEBUG_EXECUTE_ARIBITRARY_SNIP:
    return { 0x83, 0xb2, 0x09, 0x00, 0x03, 0xb5, 0x89, 0x00, 0xe7, 0x80, 0x02, 0x00 };

  case COMPILE_STUB_SNIP:
    return { 0x03, 0xb5, 0x84, 0x01, 0x83, 0x32, 0x8a, 0x00, 0xe7, 0x80, 0x02, 0x00, 0x23, 0xb0, 0xac, 0x00, 0x67, 0x00, 0x05, 0x00 };

  default:
    LOG(FATAL) << "AsmSnippet " << snippet << " does not exist.";
  }
}
