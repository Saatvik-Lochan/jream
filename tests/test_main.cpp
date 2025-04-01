#include "../include/beam_defs.hpp"
#include "../include/execution.hpp"
#include "../include/external_term.hpp"
#include "../include/generated/instr_code.hpp"
#include "../include/setup_logging.hpp"
#include <gtest/gtest.h>
#include <vector>

CodeChunk create_code_chunk(std::vector<Instruction> instructions) {
  return CodeChunk(instructions, 0, 0);
}

inline Argument get_lit(uint64_t arg) {
  return Argument{LITERAL_TAG, {.arg_num = arg}};
}

inline Argument get_tag(Tag tag, uint64_t num) {
  return Argument{tag, {.arg_num = num}};
}

void wrap_in_function(std::vector<Instruction> &instructions) {
  auto start = {
      Instruction{FUNC_INFO_OP, // I don't actually parse the arguments
                  {get_tag(ATOM_TAG, 0), get_tag(ATOM_TAG, 0), get_lit(0)}},
      Instruction{LABEL_OP, {get_lit(1)}}};

  auto end = Instruction{RETURN_OP, {}};

  instructions.insert(instructions.begin(), start.begin(), start.end());
  instructions.push_back(end);
}

void try_crashing() { std::vector<int> temp = {1, 2, 3, 4}; }

TEST(JIT, DISABLED_SetupAndTeardown) {
  std::vector<Instruction> instructions;
  wrap_in_function(instructions);

  auto code_chunk = CodeChunk(std::move(instructions), 1, 1);

  auto pcb = create_process(code_chunk);

  execute_erlang_func(pcb, code_chunk, 0);

  std::vector<Argument> argument = {get_lit(0), get_lit(0), get_lit(0)};
}

TEST(JIT, Empty) {
  std::vector<Instruction> instructions;
  wrap_in_function(instructions);

  auto code_chunk = CodeChunk(std::move(instructions), 1, 0);

  std::cout << code_chunk.function_count << std::endl;

  ErlTerm e[5];

  auto pcb = create_process(code_chunk);

  for (int i = 0; i < SHARED_FIELDS; i++) {
    pcb->shared[i] = i;
  }

  pcb->set_shared<CODE_CHUNK_P>(&code_chunk);

  // when
  execute_erlang_func(pcb, code_chunk, 0);

  // then
  auto val = pcb->get_shared<STOP>();
  ASSERT_EQ(val, e + 3) << "'e' was " << e;
}

TEST(JIT, Allocate) {
  // given
  std::vector<Instruction> instructions = {
      Instruction{ALLOCATE_OP, {get_lit(3)}}};

  wrap_in_function(instructions);
  auto code_chunk = CodeChunk(std::move(instructions), 1, 0);

  ErlTerm e[5];

  auto pcb = create_process(code_chunk);
  pcb->set_shared<STOP>(e);

  // when
  execute_erlang_func(pcb, code_chunk, 0);

  // then
  auto val = pcb->get_shared<STOP>();
  ASSERT_EQ(val, e + 3) << "'e' was " << e;
}
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
  std::vector<Instruction> instructions = {
      Instruction{ALLOCATE_OP, {get_lit(3)}}};
  wrap_in_function(instructions);
  auto code_chunk = CodeChunk(std::move(instructions), 1, 0);

  ErlTerm e[5];

  auto pcb = create_process(code_chunk);
  pcb->set_shared<STOP>(e);

  // when
  execute_erlang_func(pcb, code_chunk, 0);

  // then
  auto val = pcb->get_shared<STOP>();
  ASSERT_EQ(val, e + 3) << "'e' was " << e;
}

TEST(RISC, Deallocate) {
  // given
  std::vector<Instruction> instructions = {
      Instruction{DEALLOCATE_OP, {get_lit(3)}}};

  wrap_in_function(instructions);
  auto code_chunk = CodeChunk(std::move(instructions), 1, 0);

  ErlTerm e[5];

  auto pcb = create_process(code_chunk);
  pcb->set_shared<STOP>(e);

  // when
  execute_erlang_func(pcb, code_chunk, 0);

  // then
  auto val = pcb->get_shared<STOP>();
  ASSERT_EQ(val, e - 3) << "'e' was " << e;
}

TEST(RISCV, GetList) {
  std::vector<Instruction> instructions = {
      Instruction{GET_LIST_OP,
                  {
                      Argument{X_REGISTER_TAG, {.arg_num = 0}},
                      Argument{X_REGISTER_TAG, {.arg_num = 1}},
                      Argument{X_REGISTER_TAG, {.arg_num = 2}},
                  }}};

  wrap_in_function(instructions);
  auto code_chunk = CodeChunk(std::move(instructions), 1, 0);

  auto pcb = create_process(code_chunk);

  ErlTerm xreg[1001];
  pcb->set_shared<XREG_ARRAY>(xreg);

  auto list = erl_list_from_vec({20, 30}, get_nil_term());
  xreg[0] = list;

  // when
  execute_erlang_func(pcb, code_chunk, 0);

  // then
  ASSERT_EQ(xreg[1], 20);
  // TODO check that the list is correct & other registers unchanged
}

int main(int argc, char **argv) {
  setup_logging(argv[0]);
  testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
