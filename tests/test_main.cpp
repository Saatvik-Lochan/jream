#include "../include/beam_defs.h"
#include "../include/execution.h"
#include <gtest/gtest.h>

TEST(Assembly, CreateLoadDoubleWord) {
  uint8_t rd = 31;
  uint8_t rs = 10;
  int16_t imm = 100;

  // when
  auto result = create_load_doubleword(rd, rs, imm);

  // then

  uint8_t should[4] = {0x83, 0x3f, 0x45, 0x06};

  for (int i = 0; i < 4; i++) {
    ASSERT_EQ(result.raw[i], should[i]);
  }
}

CodeChunk create_code_chunk(const std::vector<Instruction> instructions) {
  return CodeChunk(std::move(instructions), 0, 0, {}, {});
}

Argument get_lit(uint64_t arg) {
  return Argument{LITERAL_TAG, {.arg_num = arg}};
}

TEST(RISC, Allocate) {
  // given
  auto instructions = {Instruction{ALLOCATE_OP, {get_lit(3)}}};
  auto code_chunk = create_code_chunk(std::move(instructions));

  ErlTerm e[5];

  ProcessControlBlock pcb;
  pcb.set_shared<STOP>(e);

  // when
  run_code_section(code_chunk, CodeSection{0, 1}, &pcb);

  // then
  auto val = pcb.get_shared<STOP>();
  ASSERT_EQ(val, e + 3);
}
