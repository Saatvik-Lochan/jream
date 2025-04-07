#include "../include/beam_defs.hpp"
#include "../include/execution.hpp"
#include "../include/external_term.hpp"
#include "../include/generated/instr_code.hpp"
#include "../include/riscv_gen.hpp"
#include "../include/setup_logging.hpp"
#include <cassert>
#include <gtest/gtest.h>
#include <iterator>
#include <optional>

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

TEST(Assembly, CreateBranchEquals) {
  uint8_t rs1 = 5;
  uint8_t rs2 = 6;
  int16_t imm = 0x30;

  // when
  auto result = create_branch_equal(rs1, rs2, imm);

  // then
  uint8_t should[] = {0x63, 0x88, 0x62, 0x02};

  for (int i = 0; i < 4; i++) {
    ASSERT_EQ(result.raw[i], should[i]) << result.display_hex();
  }
}

TEST(Assembly, SetBTypeImmediate) {
  uint8_t rs1 = 5;
  uint8_t rs2 = 6;

  // when
  auto result = create_branch_equal(rs1, rs2, 0x30);
  set_imm_B_type_instruction(result, 0x48);

  // then
  auto should = create_branch_equal(rs1, rs2, 0x48);

  for (int i = 0; i < 4; i++) {
    ASSERT_EQ(result.raw[i], should.raw[i]) << result.display_hex();
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

TEST(RISCV, MakeFun) {
  // given
  std::vector<Argument> save_list = {
      Argument{X_REGISTER_TAG, {.arg_num = 1}},
      Argument{X_REGISTER_TAG, {.arg_num = 2}},
  };

  uint64_t used_index = 62;

  std::vector<Instruction> instructions = {Instruction{
      MAKE_FUN3_OP,
      {
          Argument{LITERAL_TAG, {.arg_num = used_index}},    // old_index
          Argument{X_REGISTER_TAG, {.arg_num = 0}},          // x0 (dest)
          Argument{EXT_LIST_TAG, {.arg_vec_p = &save_list}}, // terms
      }}};

  wrap_in_function(instructions);
  CodeChunk code_chunk(std::move(instructions), 1, 1);

  auto pcb = create_process(code_chunk, 0);
  ErlTerm heap[5];
  ErlTerm xregs[5];

  auto x1_start_val = 20;
  auto x2_start_val = 30;

  xregs[1] = x1_start_val;
  xregs[2] = x2_start_val;

  pcb->set_shared<HTOP>(heap);
  pcb->set_shared<XREG_ARRAY>(xregs);

  // when
  resume_process(pcb);

  const auto &header = heap[0];
  const auto &index = heap[1];
  const auto &x1_val = heap[2];
  const auto &x2_val = heap[3];

  ASSERT_EQ(header & 0b111111, 0b010100); // header tag
  ASSERT_EQ(header >> 6, 3);              // the header size is correct
  ASSERT_EQ(index, used_index);
  ASSERT_EQ(x1_val, x1_start_val);
  ASSERT_EQ(x2_val, x2_start_val);
  ASSERT_EQ(xregs[0].as_ptr(), &header);
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

BeamSrc get_beam_file(CodeChunk c, AtomChunk a, ImportTableChunk i,
                      std::vector<AnonymousFunctionId> anons) {
  std::vector<ErlTerm> terms;
  LiteralChunk l(terms);

  FunctionTableChunk f(anons);

  return BeamSrc(std::move(a), std::move(c), std::move(l), std::move(i),
                 std::move(f));
}

BeamSrc
get_call_ext_file(std::string module_name, std::string function_name,
                  uint32_t arity,
                  std::optional<std::vector<AnonymousFunctionId>> anons) {

  // don't need alloc/dealloc if it is a bif
  std::vector<Instruction> instructions = {
      Instruction{CALL_EXT_OP,
                  {
                      Argument{LITERAL_TAG, {.arg_num = arity}},
                      Argument{LITERAL_TAG, {.arg_num = 0}} // import index
                  }},
  };

  wrap_in_function(instructions);
  CodeChunk code_chunk(std::move(instructions), 1, 1);

  AtomChunk a(std::vector<std::string>({"dummy", module_name, function_name}));

  std::vector<GlobalFunctionIdentifier> imports = {GlobalFunctionIdentifier{
      .module = 1, .function_name = 2, .arity = arity}};

  ImportTableChunk i(imports);

  if (!anons) {
    anons = std::vector<AnonymousFunctionId>();
  }

  auto file =
      get_beam_file(std::move(code_chunk), std::move(a), std::move(i), *anons);

  return file;
}

TEST(RISCV, CallExtBif) {
  auto file = get_call_ext_file("erlang", "test", 0, std::nullopt);
  auto pcb = create_process(file.code_chunk, 0);

  pcb->get_shared<XREG_ARRAY>()[0] = 0;

  // when
  resume_process(pcb);

  // then
  auto x_reg = pcb->get_shared<XREG_ARRAY>();
  ASSERT_EQ(x_reg[0], 100); // this is what the test bif does
}

TEST(RISCV, Spawn) {
  // given
  uint32_t label = 20;

  std::vector<AnonymousFunctionId> lambdas = {AnonymousFunctionId{
      .arity = 3,
      .label = label,
      .num_free = 3,
  }};

  auto file = get_call_ext_file("erlang", "spawn", 1, lambdas);
  auto pcb = create_process(file.code_chunk, 0);

  auto header = 0b100010100;
  auto index = 0;
  auto arg_1 = 10;
  auto arg_2 = 20;
  auto arg_3 = 30;

  ErlTerm fun[] = {
      header, index, arg_1, arg_2, arg_3,
  };

  pcb->get_shared<XREG_ARRAY>()[0] = make_boxed(fun);

  // when
  resume_process(pcb);

  // then
  auto result = pcb->get_shared<XREG_ARRAY>()[0];
  ProcessControlBlock *spawned_process = from_pid(result);

  auto new_xregs = spawned_process->get_shared<XREG_ARRAY>();

  ASSERT_TRUE(emulator_main.scheduler.runnable.contains(spawned_process));
  ASSERT_EQ(new_xregs[0], 10);
  ASSERT_EQ(new_xregs[1], 20);
  ASSERT_EQ(new_xregs[2], 30);
  ASSERT_EQ(spawned_process->get_shared<RESUME_LABEL>(), label);
}

Instruction set_flag_instr(bool *flag) {
  return Instruction{
      DEBUG_EXECUTE_ARBITRARY,
      {Argument{LITERAL_TAG, {.arg_num = reinterpret_cast<uint64_t>(set_flag)}},
       Argument{LITERAL_TAG, {.arg_num = reinterpret_cast<uint64_t>(flag)}}},
  };
}

std::vector<Instruction> get_loop_rec_instructions(bool *a, bool *b) {
  return {
      Instruction{LOOP_REC_OP,
                  {Argument{LABEL_TAG, {.arg_num = 2}},
                   Argument{X_REGISTER_TAG, {.arg_num = 0}}}},
      Instruction{LABEL_OP, {Argument{LITERAL_TAG, {.arg_num = 1}}}},
      set_flag_instr(a),
      Instruction{RETURN_OP, {}},
      Instruction{LABEL_OP, {Argument{LITERAL_TAG, {.arg_num = 2}}}},
      set_flag_instr(b),
  };
}

TEST(RISCV, LoopRecEmptyMbox) {
  bool flag_a = false;
  bool flag_b = false;

  auto instructions = get_loop_rec_instructions(&flag_a, &flag_b);
  wrap_in_function(instructions, 0);
  CodeChunk code_chunk(std::move(instructions), 1, 1);

  auto pcb = create_process(code_chunk, 0);

  // when
  resume_process(pcb);

  // then
  ASSERT_FALSE(flag_a);
  ASSERT_TRUE(flag_b);
}

TEST(RISCV, LoopRec) {
  bool flag_a = false;
  bool flag_b = false;

  auto instructions = get_loop_rec_instructions(&flag_a, &flag_b);
  wrap_in_function(instructions, 0);
  CodeChunk code_chunk(std::move(instructions), 1, 1);

  auto pcb = create_process(code_chunk, 0);

  Message new_msg(10);
  pcb->queue_message(&new_msg);

  // when
  resume_process(pcb);

  // then
  ASSERT_TRUE(flag_a);
  ASSERT_FALSE(flag_b);

  auto x0 = pcb->get_shared<XREG_ARRAY>()[0];
  ASSERT_EQ(x0, 10);
}

TEST(RISCV, RemoveLastMessage) {
  std::vector<Instruction> instructions = {Instruction{REMOVE_MESSAGE_OP, {}}};

  wrap_in_function(instructions, 0);
  CodeChunk code_chunk(std::move(instructions), 1, 1);

  auto pcb = create_process(code_chunk, 0);

  auto new_msg = new Message(10);
  pcb->queue_message(new_msg);

  // when
  resume_process(pcb);

  // then
  auto head_addr = pcb->get_address<MBOX_HEAD>();
  auto tail = pcb->get_shared<MBOX_TAIL>();
  auto save = pcb->get_shared<MBOX_SAVE>();

  ASSERT_EQ(save, head_addr);
  ASSERT_EQ(tail, head_addr);
}

TEST(RISCV, Remove) {
  std::vector<Instruction> instructions = {Instruction{REMOVE_MESSAGE_OP, {}}};

  wrap_in_function(instructions, 0);
  CodeChunk code_chunk(std::move(instructions), 1, 1);

  auto pcb = create_process(code_chunk, 0);

  auto msg_one = new Message(10);
  auto msg_two = new Message(20);

  pcb->queue_message(msg_one);
  pcb->queue_message(msg_two);

  // when
  resume_process(pcb);

  // then
  auto head_addr = pcb->get_address<MBOX_HEAD>();
  auto head = pcb->get_shared<MBOX_HEAD>();
  auto tail = pcb->get_shared<MBOX_TAIL>();
  auto save = pcb->get_shared<MBOX_SAVE>();

  ASSERT_EQ(save, head_addr);
  ASSERT_EQ(tail, msg_two->get_next_address());
  ASSERT_EQ(head, msg_two);
}

TEST(RISCV, Wait) {
  uint64_t wait_label = 22;

  std::vector<Instruction> instructions = {
      Instruction{WAIT_OP, {Argument{LABEL_TAG, {.arg_num = wait_label}}}}};

  wrap_in_function(instructions);
  CodeChunk code_chunk(std::move(instructions), 1, 1);

  auto pcb = create_process(code_chunk, 0);

  // when
  auto result = resume_process(pcb);

  // then
  ASSERT_EQ(result, WAIT);
  ASSERT_EQ(pcb->get_shared<RESUME_LABEL>(), wait_label);
}

TEST(RISCV, Send) {
  std::vector<Instruction> instructions = {Instruction{SEND_OP, {}}};
  wrap_in_function(instructions);
  CodeChunk code_chunk(std::move(instructions), 1, 1);

  auto pcb = create_process(code_chunk, 0);
  auto xreg = pcb->get_shared<XREG_ARRAY>();

  auto other_pcb = create_process(code_chunk, 0);
  emulator_main.scheduler.waiting.insert(other_pcb);

  std::vector<ErlTerm> list = {0, 1, 2, 3, 4};
  auto erl_list = erl_list_from_vec(list, get_nil_term());
  xreg[0] = make_pid(other_pcb);
  xreg[1] = erl_list;

  ErlTerm heap[10];
  other_pcb->set_shared<HTOP>(heap);

  // when
  resume_process(pcb);

  // assert other htop
  auto other_htop = other_pcb->get_shared<HTOP>();
  ASSERT_EQ(other_htop, heap + 10);

  auto other_mbox_head = other_pcb->get_shared<MBOX_HEAD>();
  auto msg_payload = other_mbox_head->get_payload();

  auto msg_vec = vec_from_erl_list(msg_payload);

  ASSERT_EQ(msg_vec, list);
  ASSERT_NE(msg_payload, erl_list);

  const auto &scheduler = emulator_main.scheduler;
  ASSERT_TRUE(scheduler.runnable.contains(other_pcb));
  ASSERT_FALSE(scheduler.waiting.contains(other_pcb));
}

std::vector<Instruction> get_test_instrs(Instruction instr, bool *a, bool *b) {
  std::vector<Instruction> out = {
      instr,
      set_flag_instr(a),
      Instruction{RETURN_OP, {}},
      Instruction{LABEL_OP, {Argument{LITERAL_TAG, {.arg_num = 1}}}},
      set_flag_instr(b),
  };

  wrap_in_function(out, 0);

  return out;
}

ProcessControlBlock *setup_is_tuple(bool *a, bool *b) {
  auto instructions =
      get_test_instrs(Instruction{IS_TUPLE_OP,
                                  {Argument{LABEL_TAG, {.arg_num = 1}},
                                   Argument{X_REGISTER_TAG, {.arg_num = 0}}}},
                      a, b);

  auto code_chunk = new CodeChunk(std::move(instructions), 1, 2);

  return create_process(*code_chunk, 0);
}

TEST(RISCV, IsTupleOp) {
  bool a, b;

  auto pcb = setup_is_tuple(&a, &b);
  auto xregs = pcb->get_shared<XREG_ARRAY>();

  ErlTerm e[3] = {0b11000000, 1, 2};
  xregs[0] = make_boxed(e);

  // when
  resume_process(pcb);

  // then
  ASSERT_TRUE(a);
  ASSERT_FALSE(b);
}

TEST(RISCV, IsTupleOpNotBoxed) {
  bool a, b;

  auto pcb = setup_is_tuple(&a, &b);
  auto xregs = pcb->get_shared<XREG_ARRAY>();

  ErlTerm e[3] = {0b11000000, 1, 2};
  xregs[0] = 0b1011111; // 5 as a small integer

  // when
  resume_process(pcb);

  // then
  ASSERT_FALSE(a);
  ASSERT_TRUE(b);
}

TEST(RISCV, IsTupleOpWrongBoxed) {
  bool a, b;

  auto pcb = setup_is_tuple(&a, &b);
  auto xregs = pcb->get_shared<XREG_ARRAY>();

  ErlTerm e[1] = {0b11010100}; // is a function now
  xregs[0] = make_boxed(e);

  // when
  resume_process(pcb);

  // then
  ASSERT_FALSE(a);
  ASSERT_TRUE(b);
}

int main(int argc, char **argv) {
  setup_logging(argv[0]);
  testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
