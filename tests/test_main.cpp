#include "../include/beam_defs.h"
#include "../include/execution.h"
#include "../include/external_term.h"
#include "../include/generated/instr_code.h"
#include "../include/setup_logging.h"
#include <gtest/gtest.h>

TEST(ErlTerm, ErlListFromVec) {
  auto list = erl_list_from_vec({20, 30}, get_nil_term());

  auto pointer = reinterpret_cast<uint64_t *>(list.term & TAGGING_MASK);
  ASSERT_EQ(*pointer, 20);
}

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

CodeChunk create_code_chunk(const std::vector<Instruction> &instructions) {
  return CodeChunk(instructions, 0, 0, {}, {});
}

Argument get_lit(uint64_t arg) {
  return Argument{LITERAL_TAG, {.arg_num = arg}};
}

TEST(Assembly, StoreDoubleWord) {
  uint8_t rs1 = 21; // the one which holds the pointer
  uint8_t rs2 = 6;
  int16_t imm = 8;

  // when
  auto result = create_store_doubleword(rs1, rs2, imm);

  // then
  auto should = get_riscv(STORE_DOUBLEWORD_TEST_SNIP);

  for (int i = 0; i < 4; i++) {
    ASSERT_EQ(result.raw[i], should[i]);
  }
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
  ASSERT_EQ(val, e + 3) << "'e' was " << e;
}

TEST(RISC, Deallocate) {
  // given
  auto instructions = {Instruction{DEALLOCATE_OP, {get_lit(3)}}};
  auto code_chunk = create_code_chunk(std::move(instructions));

  ErlTerm e[5];

  ProcessControlBlock pcb;
  pcb.set_shared<STOP>(e);

  // when
  run_code_section(code_chunk, CodeSection{0, 1}, &pcb);

  // then
  auto val = pcb.get_shared<STOP>();
  ASSERT_EQ(val, e - 3) << "'e' was " << e;
}

TEST(RISCV, GetList) {
  ProcessControlBlock pcb;

  ErlTerm xreg[1001];
  pcb.set_shared<XREG_ARRAY>(xreg);

  auto list = erl_list_from_vec({20, 30}, get_nil_term());
  xreg[0] = list;

  std::vector<Instruction> instructions = {
      Instruction{GET_LIST_OP,
                  {
                      Argument{X_REGISTER_TAG, {.arg_num = 0}},
                      Argument{X_REGISTER_TAG, {.arg_num = 1}},
                      Argument{X_REGISTER_TAG, {.arg_num = 2}},
                  }}};

  auto code_chunk = create_code_chunk(instructions);

  // when
  run_code_section(code_chunk, CodeSection{0, 1}, &pcb);

  // then
  ASSERT_EQ(xreg[1], 20);
  // TODO check that the list is correct & other registers unchanged
}

int main(int argc, char **argv) {
  setup_logging(argv[0]);
  testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
