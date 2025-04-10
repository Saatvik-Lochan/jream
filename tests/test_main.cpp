#include "../include/beam_defs.hpp"
#include "../include/bif.hpp"
#include "../include/execution.hpp"
#include "../include/external_term.hpp"
#include "../include/generated/instr_code.hpp"
#include "../include/riscv_gen.hpp"
#include "../include/setup_logging.hpp"
#include <cassert>
#include <cstdint>
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

TEST(ErlTerm, ErlListFromVecAndBackEmpty) {
  std::vector<ErlTerm> initial_vec = {};
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

TEST(ErlTerm, DeepcopyTuple) {
  // given
  ErlTerm heap[] = { 3 << 6, 1, 2, 3 };
  auto tuple = make_boxed(heap);

  ErlTerm new_heap[4];
  ErlTerm *start = new_heap;

  // when
  auto copy = deepcopy(tuple, start);

  // assert elements equal, but in different locations
  ASSERT_NE(tuple, copy);
  for (int i = 0; i < 4; i++) {
    ASSERT_EQ(heap[i], new_heap[i]);
  }
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

ProcessControlBlock *get_process(CodeChunk *code_chunk) {
  return create_process({.code_chunk = code_chunk, .label = 1});
}

ProcessControlBlock *get_process(CodeChunk &code_chunk) {
  return create_process({.code_chunk = &code_chunk, .label = 1});
}

Argument get_lit(uint64_t arg) {
  return Argument{LITERAL_TAG, {.arg_num = arg}};
}

Argument get_tag(Tag tag, uint64_t num) {
  return Argument{tag, {.arg_num = num}};
}

void wrap_in_function(std::vector<Instruction> &instructions,
                      uint64_t label = 1, bool include_return = true) {
  auto start = {
      Instruction{LABEL_OP, {get_lit(label)}},
      Instruction{LINE_OP, {get_lit(0)}},
      Instruction{FUNC_INFO_OP, // I don't actually parse the arguments
                  {get_tag(ATOM_TAG, 0), get_tag(ATOM_TAG, 0), get_lit(0)}},
  };

  auto end = Instruction{RETURN_OP, {}};

  instructions.insert(instructions.begin(), start.begin(), start.end());

  if (include_return) {
    instructions.push_back(end);
  }
}

struct BeamFileConstructor {
  CodeChunk c;
  std::optional<AtomChunk> a = std::nullopt;
  std::optional<ImportTableChunk> i = std::nullopt;
  std::optional<ExportTableChunk> e = std::nullopt;
  std::optional<LiteralChunk> l = std::nullopt;
  std::optional<FunctionTableChunk> f = std::nullopt;
};

BeamSrc get_beam_file(BeamFileConstructor b) {

  AtomChunk a_({});
  ImportTableChunk i_({});
  ExportTableChunk e_({});
  LiteralChunk l_({});
  FunctionTableChunk f_({});

  return BeamSrc(b.a ? *b.a : a_, b.c, b.l ? *b.l : l_, b.i ? *b.i : i_,
                 b.e ? *b.e : e_, b.f ? *b.f : f_);
}

void set_flag(bool *address) { *address = true; }

Instruction set_flag_instr(bool *flag) {
  return Instruction{
      DEBUG_EXECUTE_ARBITRARY,
      {Argument{LITERAL_TAG, {.arg_num = reinterpret_cast<uint64_t>(set_flag)}},
       Argument{LITERAL_TAG, {.arg_num = reinterpret_cast<uint64_t>(flag)}}},
  };
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

  auto pcb = get_process(code_chunk);

  resume_process(pcb);

  SUCCEED();
}

TEST(RISCV, Move) {
  // only testing with X register for now
  std::vector<Instruction> instructions = {
      Instruction{MOVE_OP,
                  {
                      get_tag(X_REGISTER_TAG, 0), // source
                      get_tag(X_REGISTER_TAG, 1)  // destination
                  }}};

  wrap_in_function(instructions);
  CodeChunk code_chunk(std::move(instructions), 1, 1);

  auto pcb = get_process(code_chunk);
  auto xregs = pcb->get_shared<XREG_ARRAY>();

  xregs[0] = 25;
  xregs[1] = 30;

  // when
  resume_process(pcb);

  // then
  ASSERT_EQ(xregs[0], 25);
  ASSERT_EQ(xregs[1], 25);
}

TEST(RISCV, MoveYRegs) {
  // only testing with X register for now
  std::vector<Instruction> instructions = {
      Instruction{MOVE_OP,
                  {
                      get_tag(Y_REGISTER_TAG, 0), // source
                      get_tag(Y_REGISTER_TAG, 1)  // destination
                  }}};

  wrap_in_function(instructions);
  CodeChunk code_chunk(std::move(instructions), 1, 1);

  auto pcb = get_process(code_chunk);
  ErlTerm stack[] = {0, 12, 34};
  pcb->set_shared<STOP>(stack);

  // when
  resume_process(pcb);

  // then
  ASSERT_EQ(stack[1], 12); // y0
  ASSERT_EQ(stack[2], 12); // y1
}

TEST(RISCV, MoveLiteral) {
  std::vector<Instruction> instructions = {
      Instruction{MOVE_OP,
                  {
                      get_tag(EXT_LITERAL_TAG, 0), // source
                      get_tag(X_REGISTER_TAG, 1)   // destination
                  }}};

  wrap_in_function(instructions);
  CodeChunk code_chunk(std::move(instructions), 1, 1);
  AtomChunk atom_chunk({"dummy", "module"});

  LiteralChunk literals({57});

  auto file = get_beam_file(
      BeamFileConstructor{.c = code_chunk, .a = atom_chunk, .l = literals});
  auto pcb = get_process(file.code_chunk);

  // when
  resume_process(pcb);

  // then
  auto xregs = pcb->get_shared<XREG_ARRAY>();
  ASSERT_EQ(xregs[1], 57);
}

TEST(RISCV, Swap) {
  // only testing with X register for now
  std::vector<Instruction> instructions = {
      Instruction{SWAP_OP,
                  {
                      get_tag(X_REGISTER_TAG, 0), // source
                      get_tag(X_REGISTER_TAG, 1)  // destination
                  }}};

  wrap_in_function(instructions);
  CodeChunk code_chunk(std::move(instructions), 1, 1);

  auto pcb = get_process(code_chunk);
  auto xregs = pcb->get_shared<XREG_ARRAY>();

  xregs[0] = 25;
  xregs[1] = 30;

  // when
  resume_process(pcb);

  // then
  ASSERT_EQ(xregs[0], 30);
  ASSERT_EQ(xregs[1], 25);
}

TEST(RISCV, Allocate) {
  // given
  std::vector<Instruction> instructions = {
      Instruction{ALLOCATE_OP, {get_lit(3)}}};
  wrap_in_function(instructions);
  CodeChunk code_chunk(std::move(instructions), 1, 1);

  ErlTerm e[5];

  auto pcb = get_process(code_chunk);
  pcb->set_shared<STOP>(e + 4);

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

  auto pcb = get_process(code_chunk);
  pcb->set_shared<STOP>(e);

  // when
  resume_process(pcb);

  // then
  auto val = pcb->get_shared<STOP>();
  // remains the same because allocate and deallocate
  ASSERT_EQ(val, e) << "'e' was " << e;
}

TEST(RISCV, TestGetTupleElement) {
  std::vector<Instruction> instructions = {
      Instruction{GET_TUPLE_ELEMENT_OP,
                  {
                      Argument{X_REGISTER_TAG, {.arg_num = 0}}, // source
                      Argument{LITERAL_TAG, {.arg_num = 1}},    // tuple index
                      Argument{X_REGISTER_TAG, {.arg_num = 1}}, // destination
                  }}};

  wrap_in_function(instructions);
  CodeChunk code_chunk(std::move(instructions), 1, 1);

  auto pcb = get_process(code_chunk);
  auto xregs = pcb->get_shared<XREG_ARRAY>();

  // tuple of arity 3, elements (0, 1, 2)
  ErlTerm heap[] = {3 << 6, 0, 10, 20};
  xregs[0] = make_boxed(heap);

  // when
  resume_process(pcb);

  // then
  ASSERT_EQ(xregs[1], 10);
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

  auto pcb = get_process(code_chunk);

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

TEST(RISCV, PutList) {
  std::vector<Instruction> instructions = {
      Instruction{PUT_LIST_OP,
                  {
                      Argument{X_REGISTER_TAG, {.arg_num = 0}}, // head
                      Argument{X_REGISTER_TAG, {.arg_num = 1}}, // tail
                      Argument{X_REGISTER_TAG, {.arg_num = 2}}, // destination
                  }}};

  wrap_in_function(instructions);
  CodeChunk code_chunk(std::move(instructions), 1, 1);

  auto pcb = get_process(code_chunk);
  auto xregs = pcb->get_shared<XREG_ARRAY>();

  ErlTerm heap[2];
  pcb->set_shared<HTOP>(heap);

  xregs[0] = 0;
  xregs[1] = erl_list_from_vec({1, 2, 3}, get_nil_term());

  // when
  resume_process(pcb);

  // then
  std::vector<ErlTerm> out_list = vec_from_erl_list(xregs[2]);
  std::vector<ErlTerm> final_list = {0, 1, 2, 3};
  ASSERT_EQ(out_list, final_list);
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

  auto pcb = get_process(code_chunk);
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

  const auto result = xregs[0].as_ptr();

  const auto &header = result[0];
  const auto &index = result[1];
  const auto &x1_val = result[2];
  const auto &x2_val = result[3];

  ASSERT_EQ(header & 0b111111, 0b010100); // header tag
  ASSERT_EQ(header >> 6, 3);              // the header size is correct
  ASSERT_EQ(index, used_index);
  ASSERT_EQ(x1_val, x1_start_val);
  ASSERT_EQ(x2_val, x2_start_val);

  ASSERT_EQ(xregs[0].getErlMajorType(), FUN_ET);
}

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
  auto pcb = get_process(code_chunk);

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
  auto pcb = get_process(code_chunk);

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

BeamSrc get_file_with_import(std::string module_name, std::string function_name,
                             uint32_t arity,
                             std::optional<FunctionTableChunk> f,
                             std::vector<Instruction> instructions) {

  CodeChunk code_chunk(std::move(instructions), 1, 1);

  AtomChunk a(std::vector<std::string>({"dummy", module_name, function_name}));

  std::vector<ExternalFunctionId> imports = {
      ExternalFunctionId{.module = 1, .function_name = 2, .arity = arity}};

  ImportTableChunk i(imports);

  auto file = get_beam_file(BeamFileConstructor{.c = std::move(code_chunk),
                                                .a = std::move(a),
                                                .i = std::move(i),
                                                .f = std::move(f)});
  return file;
}

TEST(RISCV, CallExtBif) {
  uint64_t arity = 0;

  // don't need alloc/dealloc if it is a bif
  std::vector<Instruction> instructions = {
      Instruction{CALL_EXT_OP,
                  {
                      Argument{LITERAL_TAG, {.arg_num = arity}},
                      Argument{LITERAL_TAG, {.arg_num = 0}} // import index
                  }},
  };

  wrap_in_function(instructions);
  auto file = get_file_with_import("erlang", "test", arity, std::nullopt,
                                   std::move(instructions));

  auto pcb = get_process(file.code_chunk);

  pcb->get_shared<XREG_ARRAY>()[0] = 0;

  // when
  resume_process(pcb);

  // then
  auto x_reg = pcb->get_shared<XREG_ARRAY>();
  ASSERT_EQ(x_reg[0], 100); // this is what the test bif does
}

TEST(RISCV, CallExtBif2Args) {
  uint64_t arity = 2;

  // don't need alloc/dealloc if it is a bif
  std::vector<Instruction> instructions = {
      Instruction{CALL_EXT_OP,
                  {
                      Argument{LITERAL_TAG, {.arg_num = arity}},
                      Argument{LITERAL_TAG, {.arg_num = 0}} // import index
                  }},
  };

  wrap_in_function(instructions);
  auto file = get_file_with_import("erlang", "test", arity, std::nullopt,
                                   std::move(instructions));

  auto pcb = get_process(file.code_chunk);
  auto xregs = pcb->get_shared<XREG_ARRAY>();

  xregs[0] = 12;
  xregs[1] = 134;

  // when
  resume_process(pcb);

  // then
  ASSERT_EQ(xregs[0], 12 + 134);
}

TEST(RISCV, CallExtOnlyBif) {
  uint64_t arity = 0;

  // don't need alloc/dealloc if it is a bif
  std::vector<Instruction> instructions = {
      Instruction{CALL_EXT_ONLY_OP,
                  {
                      Argument{LITERAL_TAG, {.arg_num = arity}},
                      Argument{LITERAL_TAG, {.arg_num = 0}} // import index
                  }},
  };

  wrap_in_function(instructions, 1, false); // don't use return!
  auto file = get_file_with_import("erlang", "test", arity, std::nullopt,
                                   std::move(instructions));

  auto pcb = get_process(file.code_chunk);
  auto xregs = pcb->get_shared<XREG_ARRAY>();
  xregs[0] = 0;

  // when
  auto result = resume_process(pcb);

  // then
  ASSERT_EQ(xregs[0], 100); // this is what the test bif does

  // the thing about ONLY is that it should finish
  ASSERT_EQ(result, FINISH);
}

TEST(RISCV, Bif0) {
  std::vector<Instruction> instructions = {
      Instruction{BIF0_OP,
                  {Argument{LITERAL_TAG, {.arg_num = 0}}, // import index
                   Argument{X_REGISTER_TAG, {.arg_num = 2}}}},
  };

  wrap_in_function(instructions);
  auto file = get_file_with_import("erlang", "test", 0, std::nullopt,
                                   std::move(instructions));

  auto pcb = get_process(file.code_chunk);
  auto xregs = pcb->get_shared<XREG_ARRAY>();
  xregs[2] = 0;

  // when
  resume_process(pcb);

  // then
  ASSERT_EQ(xregs[2], 100);
}

TEST(RISCV, GCBif1) {
  bool a, b;

  // don't need alloc/dealloc if it is a bif
  std::vector<Instruction> instructions = {
      Instruction{GC_BIF1_OP,
                  {
                      get_tag(LABEL_TAG, 2),   // fail label
                      get_tag(LITERAL_TAG, 0), // Live X regs (not used till gc)
                      get_tag(LITERAL_TAG, 0), // bif_num (import index)
                      get_tag(X_REGISTER_TAG, 3), // arg_1
                      get_tag(X_REGISTER_TAG, 2), // destination
                  }},
      set_flag_instr(&a),
      Instruction{RETURN_OP},
      Instruction{LABEL_OP, {get_lit(2)}},
      set_flag_instr(&b),
  };

  wrap_in_function(instructions);
  auto file = get_file_with_import("erlang", "test", 1, std::nullopt,
                                   std::move(instructions));

  auto pcb = get_process(file.code_chunk);
  auto xregs = pcb->get_shared<XREG_ARRAY>();
  xregs[3] = 35;

  // when
  resume_process(pcb);

  // then
  ASSERT_EQ(xregs[2], 35 * 10);
  ASSERT_TRUE(a);
  ASSERT_FALSE(b);
}

TEST(RISCV, GCBif2) {
  bool a, b;

  // don't need alloc/dealloc if it is a bif
  std::vector<Instruction> instructions = {
      Instruction{GC_BIF2_OP,
                  {
                      get_tag(LABEL_TAG, 2),   // fail label
                      get_tag(LITERAL_TAG, 0), // Live X regs (not used till gc)
                      get_tag(LITERAL_TAG, 0), // bif_num (import index)
                      get_tag(X_REGISTER_TAG, 1), // arg_1
                      get_tag(X_REGISTER_TAG, 3), // arg_2
                      get_tag(X_REGISTER_TAG, 2), // destination
                  }},
      set_flag_instr(&a),
      Instruction{RETURN_OP},
      Instruction{LABEL_OP, {get_lit(2)}},
      set_flag_instr(&b),
  };

  wrap_in_function(instructions);
  auto file = get_file_with_import("erlang", "test", 2, std::nullopt,
                                   std::move(instructions));

  auto pcb = get_process(file.code_chunk);
  auto xregs = pcb->get_shared<XREG_ARRAY>();
  xregs[1] = 25;
  xregs[3] = 37;

  // when
  resume_process(pcb);

  // then
  ASSERT_EQ(xregs[2], 25 + 37);
  ASSERT_TRUE(a);
  ASSERT_FALSE(b);
}

TEST(RISCV, GCBif2Fail) {
  bool a, b;

  // don't need alloc/dealloc if it is a bif
  std::vector<Instruction> instructions = {
      Instruction{GC_BIF2_OP,
                  {
                      get_tag(LABEL_TAG, 2),   // fail label
                      get_tag(LITERAL_TAG, 0), // Live X regs (not used till gc)
                      get_tag(LITERAL_TAG, 0), // bif_num (import index)
                      get_tag(X_REGISTER_TAG, 1), // arg_1
                      get_tag(X_REGISTER_TAG, 3), // arg_2
                      get_tag(X_REGISTER_TAG, 2), // destination
                  }},
      set_flag_instr(&a),
      Instruction{RETURN_OP},
      Instruction{LABEL_OP, {get_lit(2)}},
      set_flag_instr(&b),
  };

  wrap_in_function(instructions);
  auto file = get_file_with_import("erlang", "test_fail", 2, std::nullopt,
                                   std::move(instructions));

  auto pcb = get_process(file.code_chunk);

  // when
  resume_process(pcb);

  // then
  ASSERT_FALSE(a);
  ASSERT_TRUE(b);
}

TEST(RISCV, Spawn) {
  // given
  uint32_t label = 20;
  uint64_t arity = 1;

  std::vector<AnonymousFunctionId> lambdas = {AnonymousFunctionId{
      .arity = 3,
      .label = label,
      .num_free = 3,
  }};

  std::vector<Instruction> instructions = {
      Instruction{CALL_EXT_OP,
                  {
                      Argument{LITERAL_TAG, {.arg_num = arity}},
                      Argument{LITERAL_TAG, {.arg_num = 0}} // import index
                  }},
  };
  wrap_in_function(instructions);

  auto file = get_file_with_import("erlang", "spawn", arity, lambdas,
                                   std::move(instructions));
  auto pcb = get_process(file.code_chunk);

  auto header = 0b100010100;
  auto index = 0;
  auto arg_1 = 10;
  auto arg_2 = 20;
  auto arg_3 = 30;

  ErlTerm fun[] = {
      header, index, arg_1, arg_2, arg_3,
  };

  pcb->get_shared<XREG_ARRAY>()[0] = make_boxed(fun);

  emulator_main.scheduler.runnable.insert(pcb);
  emulator_main.scheduler.pick_next();

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

std::vector<Instruction> get_loop_rec_instructions(bool *a, bool *b) {
  return {
      Instruction{LOOP_REC_OP,
                  {Argument{LABEL_TAG, {.arg_num = 2}},
                   Argument{X_REGISTER_TAG, {.arg_num = 0}}}},
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
  wrap_in_function(instructions);
  CodeChunk code_chunk(std::move(instructions), 1, 1);

  auto pcb = get_process(code_chunk);

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
  wrap_in_function(instructions);
  CodeChunk code_chunk(std::move(instructions), 1, 1);

  auto pcb = get_process(code_chunk);

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

  wrap_in_function(instructions);
  CodeChunk code_chunk(std::move(instructions), 1, 1);

  auto pcb = get_process(code_chunk);

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

  wrap_in_function(instructions);
  CodeChunk code_chunk(std::move(instructions), 1, 1);

  auto pcb = get_process(code_chunk);

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

  auto pcb = get_process(code_chunk);

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

  auto pcb = get_process(code_chunk);
  auto xreg = pcb->get_shared<XREG_ARRAY>();

  auto other_pcb = get_process(code_chunk);
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
      Instruction{LABEL_OP, {Argument{LITERAL_TAG, {.arg_num = 2}}}},
      set_flag_instr(b),
  };

  wrap_in_function(out);

  return out;
}

ProcessControlBlock *setup_is_tuple(bool *a, bool *b) {
  auto instructions =
      get_test_instrs(Instruction{IS_TUPLE_OP,
                                  {Argument{LABEL_TAG, {.arg_num = 2}},
                                   Argument{X_REGISTER_TAG, {.arg_num = 0}}}},
                      a, b);

  auto code_chunk = new CodeChunk(std::move(instructions), 1, 2);

  return get_process(code_chunk);
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

TEST(RISCV, InitYRegs) {

  std::vector<Argument> arguments = {
      Argument{Y_REGISTER_TAG, {.arg_num = 0}},
      Argument{Y_REGISTER_TAG, {.arg_num = 1}},
  };

  std::vector<Instruction> instructions = {
      Instruction{ALLOCATE_OP, {get_lit(3)}},
      Instruction{INIT_YREGS_OP,
                  {Argument{EXT_LIST_TAG, {.arg_vec_p = &arguments}}}},
      Instruction{WAIT_OP, {get_tag(LABEL_TAG, 0)}} // label doesn't matter
  };

  wrap_in_function(instructions);

  CodeChunk code_chunk(std::move(instructions), 1, 1);
  auto pcb = get_process(code_chunk);

  ErlTerm stack[4];
  pcb->set_shared<STOP>(stack + 4);

  // when
  resume_process(pcb);

  // then
  ASSERT_NE(stack[0], get_nil_term()); // code pointer
  ASSERT_EQ(stack[1], get_nil_term()); // y0
  ASSERT_EQ(stack[2], get_nil_term()); // y1
  ASSERT_NE(stack[3], get_nil_term()); // y2 (not initialised)
}

CodeChunk getCallLastCodeChunk(bool *flag) {
  std::vector<Instruction> func_1 = {
      Instruction{ALLOCATE_OP, {get_lit(3)}},
      Instruction{CALL_LAST_OP,
                  {
                      Argument{LITERAL_TAG, {.arg_num = 1}}, // arity
                      Argument{LABEL_TAG, {.arg_num = 2}},   // label
                      Argument{LITERAL_TAG, {.arg_num = 3}}  // num deallocate
                  }},
  };

  std::vector<Instruction> func_2 = {set_flag_instr(flag)};

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

TEST(RISCV, CallLast) {
  bool flag = false;
  auto code_chunk = getCallLastCodeChunk(&flag);
  auto pcb = get_process(code_chunk);

  ErlTerm stack[4];
  pcb->set_shared<STOP>(stack + 4);
  pcb->set_shared<REDUCTIONS>(1000);

  // when
  resume_process(pcb);

  // then
  ASSERT_TRUE(flag);
  ASSERT_EQ(pcb->get_shared<STOP>(), stack + 4);
}

TEST(RISCV, TestArityTrue) {
  bool a = false;
  bool b = false;

  auto instructions =
      get_test_instrs(Instruction{TEST_ARITY_OP,
                                  {get_tag(LABEL_TAG, 2),
                                   get_tag(X_REGISTER_TAG, 0), get_lit(3)}},
                      &a, &b);

  CodeChunk code_chunk(std::move(instructions), 1, 2);
  auto pcb = get_process(code_chunk);

  auto xregs = pcb->get_shared<XREG_ARRAY>();
  ErlTerm heap[] = {3 << 6, 1, 2, 3}; // arity 3
  xregs[0] = make_boxed(heap);

  // when
  resume_process(pcb);

  // then
  ASSERT_TRUE(a);
  ASSERT_FALSE(b);
}

TEST(RISCV, TestArityFalse) {
  bool a = false;
  bool b = false;

  auto instructions =
      get_test_instrs(Instruction{TEST_ARITY_OP,
                                  {get_tag(LABEL_TAG, 2),
                                   get_tag(X_REGISTER_TAG, 0), get_lit(3)}},
                      &a, &b);

  CodeChunk code_chunk(std::move(instructions), 1, 2);
  auto pcb = get_process(code_chunk);

  auto xregs = pcb->get_shared<XREG_ARRAY>();
  ErlTerm heap[] = {4 << 6, 1, 2, 3, 4}; // arity 4
  xregs[0] = make_boxed(heap);

  // when
  resume_process(pcb);

  // then
  ASSERT_FALSE(a);
  ASSERT_TRUE(b);
}

TEST(RISCV, TestIsNonEmptyListTrue) {
  bool a = false;
  bool b = false;

  auto instructions =
      get_test_instrs(Instruction{IS_NONEMPTY_LIST_OP,
                                  {get_tag(LABEL_TAG, 2),
                                   get_tag(X_REGISTER_TAG, 0), get_lit(3)}},
                      &a, &b);

  CodeChunk code_chunk(std::move(instructions), 1, 2);
  auto pcb = get_process(code_chunk);

  auto xregs = pcb->get_shared<XREG_ARRAY>();
  xregs[0] = erl_list_from_vec({1, 2, 3}, get_nil_term());

  // when
  resume_process(pcb);

  // then
  ASSERT_TRUE(a);
  ASSERT_FALSE(b);
}

TEST(RISCV, TestIsNonEmptyListFalse) {
  bool a = false;
  bool b = false;

  auto instructions =
      get_test_instrs(Instruction{IS_NONEMPTY_LIST_OP,
                                  {get_tag(LABEL_TAG, 2),
                                   get_tag(X_REGISTER_TAG, 0), get_lit(3)}},
                      &a, &b);

  CodeChunk code_chunk(std::move(instructions), 1, 2);
  auto pcb = get_process(code_chunk);

  auto xregs = pcb->get_shared<XREG_ARRAY>();
  xregs[0] = get_nil_term();

  // when
  resume_process(pcb);

  // then
  ASSERT_FALSE(a);
  ASSERT_TRUE(b);
}

TEST(RISCV, TestIsNilTrue) {
  bool a = false;
  bool b = false;

  auto instructions =
      get_test_instrs(Instruction{IS_NIL_OP,
                                  {get_tag(LABEL_TAG, 2),
                                   get_tag(X_REGISTER_TAG, 0), get_lit(3)}},
                      &a, &b);

  CodeChunk code_chunk(std::move(instructions), 1, 2);
  auto pcb = get_process(code_chunk);

  auto xregs = pcb->get_shared<XREG_ARRAY>();
  xregs[0] = get_nil_term();

  // when
  resume_process(pcb);

  // then
  ASSERT_TRUE(a);
  ASSERT_FALSE(b);
}

TEST(RISCV, TestIsNilFalse) {
  bool a = false;
  bool b = false;

  auto instructions =
      get_test_instrs(Instruction{IS_NIL_OP,
                                  {get_tag(LABEL_TAG, 2),
                                   get_tag(X_REGISTER_TAG, 0), get_lit(3)}},
                      &a, &b);

  CodeChunk code_chunk(std::move(instructions), 1, 2);
  auto pcb = get_process(code_chunk);

  auto xregs = pcb->get_shared<XREG_ARRAY>();
  xregs[0] = erl_list_from_vec({1, 2, 3}, get_nil_term());

  // when
  resume_process(pcb);

  // then
  ASSERT_FALSE(a);
  ASSERT_TRUE(b);
}

void do_compare_test(OpCode opcode, ErlTerm arg1, ErlTerm arg2,
                     bool should_jump) {
  bool a = false;
  bool b = false;

  auto instructions =
      get_test_instrs(Instruction{opcode,
                                  {
                                      get_tag(LABEL_TAG, 2),
                                      get_tag(X_REGISTER_TAG, 1),
                                      get_tag(X_REGISTER_TAG, 2),
                                  }},
                      &a, &b);

  CodeChunk code_chunk(std::move(instructions), 1, 2);
  auto pcb = get_process(code_chunk);
  auto xregs = pcb->get_shared<XREG_ARRAY>();
  xregs[1] = arg1;
  xregs[2] = arg2;

  // when
  resume_process(pcb);

  // then
  if (should_jump) {
    ASSERT_FALSE(a);
    ASSERT_TRUE(b);
  } else {
    ASSERT_TRUE(a);
    ASSERT_FALSE(b);
  }
}

TEST(RISCV, CompDiffTypesGT) {
  auto arg1 = erl_list_from_vec({make_small_int(1)}, get_nil_term());
  auto arg2 = make_small_int(4000);

  auto should_ge_jump = false;
  do_compare_test(IS_GE_OP, arg1, arg2, should_ge_jump);
  do_compare_test(IS_LT_OP, arg1, arg2, !should_ge_jump);
}

TEST(RISCV, CompDiffTypesLT) {
  auto arg1 = make_small_int(4000);
  auto arg2 = erl_list_from_vec({make_small_int(1)}, get_nil_term());

  auto should_ge_jump = true;
  do_compare_test(IS_GE_OP, arg1, arg2, should_ge_jump);
  do_compare_test(IS_LT_OP, arg1, arg2, !should_ge_jump);
}

TEST(RISCV, CompListGTLen) {
  auto arg1 =
      erl_list_from_vec({make_small_int(1), make_small_int(2)}, get_nil_term());
  auto arg2 = erl_list_from_vec(
      {make_small_int(1), make_small_int(2), make_small_int(3)},
      get_nil_term());

  auto should_ge_jump = false;
  do_compare_test(IS_GE_OP, arg1, arg2, should_ge_jump);
  do_compare_test(IS_LT_OP, arg1, arg2, !should_ge_jump);
}

TEST(RISCV, CompListGTValue) {
  auto arg1 = erl_list_from_vec({make_small_int(2)}, get_nil_term());
  auto arg2 =
      erl_list_from_vec({make_small_int(1), make_small_int(2)}, get_nil_term());

  auto should_ge_jump = false;
  do_compare_test(IS_GE_OP, arg1, arg2, should_ge_jump);
  do_compare_test(IS_LT_OP, arg1, arg2, !should_ge_jump);
}

TEST(RISCV, CompTupleGTLen) {
  ErlTerm heap1[] = {2 << 6, make_small_int(1), make_small_int(2)};
  ErlTerm heap2[] = {1 << 6, make_small_int(1)};

  auto arg1 = make_boxed(heap1);
  auto arg2 = make_boxed(heap2);

  auto should_ge_jump = false;
  do_compare_test(IS_GE_OP, arg1, arg2, should_ge_jump);
  do_compare_test(IS_LT_OP, arg1, arg2, !should_ge_jump);
}

TEST(RISCV, CompTupleGTValue) {
  ErlTerm heap1[] = {1 << 6, make_small_int(2)};
  ErlTerm heap2[] = {2 << 6, make_small_int(1), make_small_int(2)};

  auto arg1 = make_boxed(heap1);
  auto arg2 = make_boxed(heap2);

  auto should_ge_jump = false;
  do_compare_test(IS_GE_OP, arg1, arg2, should_ge_jump);
  do_compare_test(IS_LT_OP, arg1, arg2, !should_ge_jump);
}

TEST(RISCV, CompIntEq) {
  auto arg1 = make_small_int(4);
  auto arg2 = make_small_int(4);

  auto should_ge_jump = false;
  do_compare_test(IS_GE_OP, arg1, arg2, should_ge_jump);
  do_compare_test(IS_LT_OP, arg1, arg2, !should_ge_jump);
}

TEST(RISCV, CompIntLT) {
  auto arg1 = make_small_int(2);
  auto arg2 = make_small_int(4);

  auto should_ge_jump = true;
  do_compare_test(IS_GE_OP, arg1, arg2, should_ge_jump);
  do_compare_test(IS_LT_OP, arg1, arg2, !should_ge_jump);
}

TEST(RISCV, Badmatch) {
  std::vector<Instruction> instructions = {
      Instruction{BADMATCH_OP, {get_tag(Y_REGISTER_TAG, 0)}}};
  wrap_in_function(instructions);

  CodeChunk code_chunk(std::move(instructions), 1, 1);
  auto pcb = get_process(code_chunk);

  // then
  auto result = resume_process(pcb);

  // then
  ASSERT_EQ(result, ERROR);
}

TEST(BuiltInFunction, ListsSplit) {
  auto split_loc = make_small_int(3);
  auto list = erl_list_from_vec({1, 2, 3, 4, 5}, get_nil_term());

  ProcessControlBlock pcb;
  ErlTerm heap[3];
  pcb.set_shared<HTOP>(heap);
  pcb.set_shared<STOP>(heap + 3);

  emulator_main.scheduler.runnable.insert(&pcb);
  emulator_main.scheduler.pick_next();

  // when
  list_split(split_loc, list);

  // then
  auto htop = pcb.get_shared<HTOP>();
  ASSERT_EQ(htop, heap + 3);
  ASSERT_EQ(heap[0], 2 << 6);

  auto first_list = vec_from_erl_list(heap[1]);
  std::vector<ErlTerm> expected_first = {1, 2, 3};
  ASSERT_EQ(first_list, expected_first);

  auto second_list = vec_from_erl_list(heap[2]);
  std::vector<ErlTerm> expected_second = {4, 5};
  ASSERT_EQ(second_list, expected_second);
}

TEST(BuiltInFunction, ListsSplitEdgeCase) {
  auto split_loc = make_small_int(4);
  auto list = erl_list_from_vec({1, 2, 3, 4}, get_nil_term());

  ProcessControlBlock pcb;
  ErlTerm heap[3];
  pcb.set_shared<HTOP>(heap);
  pcb.set_shared<STOP>(heap + 3);

  emulator_main.scheduler.runnable.insert(&pcb);
  emulator_main.scheduler.pick_next();

  // when
  auto result = list_split(split_loc, list);

  // then
  ErlTerm res_term(result.a0);

  ASSERT_EQ(res_term.getErlMajorType(), TUPLE_ET);

  auto tuple_loc = res_term.as_ptr();

  auto htop = pcb.get_shared<HTOP>();
  ASSERT_EQ(htop, tuple_loc + 3);
  ASSERT_EQ(tuple_loc[0], 2 << 6);

  auto first_list = vec_from_erl_list(tuple_loc[1]);
  std::vector<ErlTerm> expected_first = {1, 2, 3, 4};
  ASSERT_EQ(first_list, expected_first);

  auto second_list = vec_from_erl_list(tuple_loc[2]);
  std::vector<ErlTerm> expected_second = {};
  ASSERT_EQ(second_list, expected_second);
}

TEST(BuiltInFunction, ErlDiv) {
  auto a = make_small_int(25);
  auto b = make_small_int(4);

  // when
  auto result = erl_div(a, b);

  // then
  ASSERT_EQ(result.a0 >> 4, 6);
  ASSERT_EQ(ErlTerm(result.a0).getTagType(), SMALL_INT_T);
  ASSERT_EQ(result.a1, 0);
}

int main(int argc, char **argv) {
  setup_logging(argv[0]);
  testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
