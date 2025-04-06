#include "../include/beam_defs.hpp"
#include "../include/execution.hpp"
#include "../include/external_term.hpp"
#include "../include/generated/instr_code.hpp"
#include "../include/setup_logging.hpp"
#include <cassert>
#include <gtest/gtest.h>
#include <iterator>

TEST(ErlTerm, ErlListFromVecAndBack) {
  std::vector<ErlTerm> initial_vec = {0, 1, 2, 3, 4, 5};
  auto list = erl_list_from_vec(initial_vec, get_nil_term());
  auto transformed_vec = vec_from_erl_list(list);

  ASSERT_NE(&initial_vec, &transformed_vec);
  ASSERT_EQ(initial_vec, transformed_vec);
}

TEST(ErlTerm, DeepcopyList) {
  // given
  std::vector<ErlTerm> vec = {1, 2, 3, 4, 5};
  auto list = erl_list_from_vec(vec, get_nil_term());

  // for 5 nodes
  ErlTerm new_list_area[10];
  ErlTerm *start = new_list_area;

  // when
  auto copy = deepcopy(list, start);

  // then
  ErlList initial_list(list);
  ErlList copy_list(copy);

  auto it_i = initial_list.begin();
  auto it_c = copy_list.begin();

  // assert elements equal, but in different locations
  for (; it_i != initial_list.end() && it_c != copy_list.end();
       ++it_i, ++it_c) {
    ASSERT_EQ(*it_i, *it_c);
    ASSERT_NE(it_i.get_current_node(), it_c.get_current_node());
  }

  // assert same size
  ASSERT_EQ(it_i, initial_list.end());
  ASSERT_EQ(it_c, copy_list.end());
  ASSERT_EQ(start, new_list_area + 10);
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

CodeChunk create_code_chunk(std::vector<Instruction> instructions) {
  return CodeChunk(instructions, 0, 0);
}

Argument get_lit(uint64_t arg) {
  return Argument{LITERAL_TAG, {.arg_num = arg}};
}

Argument get_tag(Tag tag, uint64_t num) {
  return Argument{tag, {.arg_num = num}};
}

void wrap_in_function(std::vector<Instruction> &instructions,
                      uint64_t label = 0) {
  auto start = {
      Instruction{FUNC_INFO_OP, // I don't actually parse the arguments
                  {get_tag(ATOM_TAG, 0), get_tag(ATOM_TAG, 0), get_lit(0)}},
      Instruction{LABEL_OP, {get_lit(label)}}};

  auto end = Instruction{RETURN_OP, {}};

  instructions.insert(instructions.begin(), start.begin(), start.end());
  instructions.push_back(end);
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

TEST(JIT, SetupAndTeardown) {
  std::vector<Instruction> instructions;
  wrap_in_function(instructions);
  CodeChunk code_chunk(std::move(instructions), 1, 1);

  auto pcb = create_process(code_chunk, 0);

  resume_process(pcb);

  SUCCEED();
}

TEST(RISCV, Allocate) {
  // given
  std::vector<Instruction> instructions = {
      Instruction{ALLOCATE_OP, {get_lit(3)}}};
  wrap_in_function(instructions);
  CodeChunk code_chunk(std::move(instructions), 1, 1);

  ErlTerm e[5];
  uint8_t code[20];

  auto pcb = create_process(code_chunk, 0);
  pcb->set_shared<STOP>(e + 4);

  auto code_ptr = code + 5; // arbitrary
  pcb->set_shared<CODE_POINTER>(code_ptr);

  // when
  resume_process(pcb);

  // then
  auto val = pcb->get_shared<STOP>();

  // 3 + 1 from code pointer
  ASSERT_EQ(val, e);
  // no test for code pointer storing here, see call tests
}

// we can't have a deallocate only test because it loads
// the location in CODE_POINTER and tries to jump there...
TEST(RISCV, AllocateAndDeallocate) {
  // given
  std::vector<Instruction> instructions = {
      Instruction{ALLOCATE_OP, {get_lit(3)}},
      Instruction{DEALLOCATE_OP, {get_lit(3)}}};

  wrap_in_function(instructions);
  CodeChunk code_chunk(std::move(instructions), 1, 1);

  ErlTerm e[5];

  auto pcb = create_process(code_chunk, 0);
  pcb->set_shared<STOP>(e);

  // when
  resume_process(pcb);

  // then
  auto val = pcb->get_shared<STOP>();
  // remains the same because allocate and deallocate
  ASSERT_EQ(val, e) << "'e' was " << e;
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
  CodeChunk code_chunk(std::move(instructions), 1, 1);

  auto pcb = create_process(code_chunk, 0);

  ErlTerm xreg[1001];
  pcb->set_shared<XREG_ARRAY>(xreg);

  auto list = erl_list_from_vec({20, 30}, get_nil_term());
  xreg[0] = list;

  // when
  resume_process(pcb);

  // then
  ASSERT_EQ(xreg[1], 20);
  // TODO check that the list is correct & other registers unchanged
}

void set_flag(bool *address) { *address = true; }

CodeChunk getCallCodeChunk(bool *flag) {
  std::vector<Instruction> func_1 = {
      Instruction{ALLOCATE_OP, {get_lit(1)}},
      Instruction{CALL_OP,
                  {
                      Argument{LITERAL_TAG, {.arg_num = 1}}, // arity
                      Argument{LABEL_TAG, {.arg_num = 2}}    // label
                  }},
      Instruction{DEALLOCATE_OP, {get_lit(1)}},
  };

  std::vector<Instruction> func_2 = {
      Instruction{
          DEBUG_EXECUTE_ARBITRARY,
          {Argument{LITERAL_TAG,
                    {.arg_num = reinterpret_cast<uint64_t>(set_flag)}},
           Argument{LITERAL_TAG,
                    {.arg_num = reinterpret_cast<uint64_t>(flag)}}},
      },
  };

  // assume labels in incrementing order from 1
  // otherwise WILL NOT WORK! Label table allocation
  // is messed up
  wrap_in_function(func_1, 1);
  wrap_in_function(func_2, 2);

  auto both = std::move(func_1);
  both.insert(both.end(), std::make_move_iterator(func_2.begin()),
              std::make_move_iterator(func_2.end()));

  return CodeChunk(std::move(both), 2, 2);
}

TEST(RISCV, CallStandard) {
  // given
  bool called = false;
  auto code_chunk = getCallCodeChunk(&called);
  auto pcb = create_process(code_chunk, 0);

  ErlTerm e[5];
  pcb->set_shared<STOP>(e + 4);
  pcb->set_shared<REDUCTIONS>(5);

  // when
  auto result = resume_process(pcb);

  // then
  ASSERT_EQ(called, true);
  ASSERT_EQ(result, FINISH);

  auto reductions = pcb->get_shared<REDUCTIONS>();
  ASSERT_EQ(reductions, 4);
}

TEST(RISCV, CallNoReductions) {
  // given
  bool called = false;
  auto code_chunk = getCallCodeChunk(&called);
  auto pcb = create_process(code_chunk, 0);

  ErlTerm e[5];
  pcb->set_shared<STOP>(e + 4);

  // set no reductions
  pcb->set_shared<REDUCTIONS>(0);

  // when
  auto result = resume_process(pcb);

  // then
  ASSERT_EQ(called, false);
  ASSERT_EQ(result, YIELD);

  auto resume_label = pcb->get_shared<RESUME_LABEL>();
  ASSERT_EQ(resume_label, 2); // resume at label 2
}

BeamSrc get_beam_file(CodeChunk c, AtomChunk a, ImportTableChunk i) {
  std::vector<ErlTerm> terms;
  LiteralChunk l(terms);

  std::vector<AnonymousFunctionId> anons;
  FunctionTableChunk f(anons);

  return BeamSrc(std::move(a), std::move(c), std::move(l), std::move(i),
                 std::move(f));
}

TEST(RISCV, CallExtBif) {
  std::vector<Instruction> instructions = {
      Instruction{ALLOCATE_OP, {get_lit(0)}},
      Instruction{CALL_EXT_OP,
                  {
                      Argument{LITERAL_TAG, {.arg_num = 1}}, // arity
                      Argument{LITERAL_TAG, {.arg_num = 0}}  // import index
                  }},
      Instruction{DEALLOCATE_OP, {get_lit(0)}},
  };
  wrap_in_function(instructions);
  CodeChunk code_chunk(std::move(instructions), 1, 1);

  AtomChunk a(std::vector<std::string>({"dummy", "erlang", "test"}));

  std::vector<GlobalFunctionIdentifier> imports = {
      GlobalFunctionIdentifier{.module = 1, .function_name = 2, .arity = 0}};

  ImportTableChunk i(imports);
  auto file = get_beam_file(std::move(code_chunk), std::move(a), std::move(i));

  std::vector<BeamSrc *> files = {&file};

  auto pcb = create_process(file.code_chunk, 0);

  ErlTerm e[5];
  pcb->set_shared<STOP>(e + 4);
  pcb->get_shared<XREG_ARRAY>()[0] = 0;

  // when
  resume_process(pcb);

  // then
  auto x_reg = pcb->get_shared<XREG_ARRAY>();
  ASSERT_EQ(x_reg[0], 100); // this is what the test bif does
}

int main(int argc, char **argv) {
  setup_logging(argv[0]);
  testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
