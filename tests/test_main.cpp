#include "../include/allocator.hpp"
#include "../include/beam_defs.hpp"
#include "../include/bif.hpp"
#include "../include/execution.hpp"
#include "../include/external_term.hpp"
#include "../include/generated/instr_code.hpp"
#include "../include/parsing.hpp"
#include "../include/riscv_gen.hpp"
#include "../include/setup_logging.hpp"
#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstdint>
#include <gtest/gtest.h>
#include <iterator>
#include <numeric>
#include <optional>
#include <ranges>
#include <unistd.h>

struct BeamFileConstructor {
  CodeChunk c;
  std::optional<AtomChunk> a = std::nullopt;
  std::optional<ImportTableChunk> i = std::nullopt;
  std::optional<ExportTableChunk> e = std::nullopt;
  std::optional<LiteralChunk> l = std::nullopt;
  std::optional<FunctionTableChunk> f = std::nullopt;
};

std::unique_ptr<BeamSrc> get_beam_file(BeamFileConstructor b) {

  AtomChunk a_({});
  ImportTableChunk i_({});
  ExportTableChunk e_({});
  LiteralChunk l_({});
  FunctionTableChunk f_({});

  return std::make_unique<BeamSrc>(b.a ? *b.a : a_, b.c, b.l ? *b.l : l_,
                                   b.i ? *b.i : i_, b.e ? *b.e : e_,
                                   b.f ? *b.f : f_);
}

void set_current_pcb(ProcessControlBlock &pcb) {
  emulator_main.scheduler.runnable.clear();
  emulator_main.scheduler.runnable.push_back(&pcb);
  emulator_main.scheduler.pick_next();
}

std::unique_ptr<ProcessControlBlock> get_process(CodeChunk *code_chunk,
                                                 size_t heap_size = 50) {

  auto pcb = std::make_unique<ProcessControlBlock>(
      EntryPoint{.code_chunk = code_chunk, .label = 1}, heap_size);
  set_current_pcb(*pcb);
  return pcb;
}

std::unique_ptr<ProcessControlBlock> get_process(CodeChunk &code_chunk,
                                                 size_t heap_size = 50) {
  return get_process(&code_chunk, heap_size);
}

Argument get_lit(uint64_t arg) {
  return Argument{LITERAL_TAG, {.arg_num = arg}};
}

Argument get_tag(Tag tag, uint64_t num) {
  return Argument{tag, {.arg_num = num}};
}

ErlTerm mi(uint64_t a) { return make_small_int(a); }

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

CodeChunk get_minimal_code_chunk() {
  return CodeChunk({{RETURN_OP, {}}}, 1, 1);
}

void assert_tuple(ErlTerm term, std::initializer_list<ErlTerm> elements) {
  ASSERT_EQ(term.getErlMajorType(), TUPLE_ET);

  auto ptr = term.as_ptr();
  const auto header = *ptr;

  const auto size = header >> 6;
  ASSERT_EQ(size, elements.size());

  for (auto e : elements) {
    ASSERT_EQ(e, *++ptr);
  }
}

TEST(Parsing, GetAtomCurrent) {
  CodeChunk code_chunk({Instruction{RETURN_OP}}, 1, 1);
  AtomChunk atoms({"dummy", "module", "ok", "error"});

  auto file = get_beam_file({.c = code_chunk, .a = atoms});

  emulator_main.register_beam_sources({file.get()});

  get_process(file->code_chunk);

  // when
  auto result = emulator_main.get_atom_current("ok");

  // then
  // 10 for the index, 001011 for the atom tag
  ASSERT_EQ(result, 0b10001011) << std::format("{:b}", result.term);
}

TEST(ErlTerm, ErlListFromVecAndBack) {
  std::vector<ErlTerm> initial_vec = {0, 1, 2, 3, 4, 5};
  auto list = erl_list_from_range(initial_vec, get_nil_term());
  auto transformed_vec = vec_from_erl_list(list);

  ASSERT_NE(&initial_vec, &transformed_vec);
  ASSERT_EQ(initial_vec, transformed_vec);
}

TEST(ErlTerm, ErlListFromVecAndBackEmpty) {
  std::vector<ErlTerm> initial_vec = {};
  auto list = erl_list_from_range(initial_vec, get_nil_term());
  auto transformed_vec = vec_from_erl_list(list);

  ASSERT_NE(&initial_vec, &transformed_vec);
  ASSERT_EQ(initial_vec, transformed_vec);
}

TEST(ErlTerm, GetHeapSizeImmediate) {
  // given
  auto immediate = make_small_int(3);

  // when
  auto size = get_heap_size(immediate);

  // then
  ASSERT_EQ(size, 0);
}

TEST(ErlTerm, GetHeapSizeNested) {
  // given
  std::vector<ErlTerm> vec = {1, 2, 3, 4, 5};
  auto list = erl_list_from_range(vec | std::views::all |
                                      std::views::transform(make_small_int),
                                  get_nil_term());

  ErlTerm heap[] = {3 << 6, list, make_small_int(2), list};
  auto tuple = make_boxed(heap);

  // when
  auto value = get_heap_size(tuple);

  // then
  // 2 * 5 (list) + 4 (tuple)
  ASSERT_EQ(value, 14);
}

void assert_deepcopy_list(ErlTerm list_a, ErlTerm list_b) {
  ErlList initial_list(list_a);
  ErlList copy_list(list_b);

  auto it_i = initial_list.begin();
  auto it_c = copy_list.begin();

  auto a = vec_from_erl_list(list_a);
  auto b = vec_from_erl_list(list_b);

  ASSERT_EQ(a, b);

  // assert elements equal, but in different locations
  for (; it_i != initial_list.end() && it_c != copy_list.end();
       ++it_i, ++it_c) {
    ASSERT_EQ(*it_i, *it_c);
    ASSERT_NE(it_i.get_current_node(), it_c.get_current_node());
  }

  // assert same size
  ASSERT_EQ(it_i, initial_list.end());
  ASSERT_EQ(it_c, copy_list.end());
}

TEST(ErlTerm, DeepcopyList) {
  // given
  std::vector<ErlTerm> vec = {1, 2, 3, 4, 5};
  for (auto &val : vec) {
    val = make_small_int(val);
  }

  auto list = erl_list_from_range(vec, get_nil_term());

  // for 5 nodes
  ErlTerm new_list_area[10];

  // when
  auto copy = deepcopy(list, new_list_area);

  // then
  assert_deepcopy_list(copy, list);
}

TEST(ErlTerm, DeepcopyTuple) {
  // given
  ErlTerm heap[] = {3 << 6, make_small_int(1), make_small_int(2),
                    make_small_int(3)};

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

// TODO fix this test
TEST(ErlTerm, DeepCopyNestedShared) {
  // given
  std::vector<ErlTerm> vec = {1};
  auto list = erl_list_from_range(vec | std::views::all |
                                      std::views::transform(make_small_int),
                                  get_nil_term());

  ErlTerm heap[] = {3 << 6, list, make_small_int(2), list};
  auto tuple = make_boxed(heap);

  ErlTerm new_heap[30];

  // when
  auto copy = deepcopy(tuple, new_heap);

  // assert elements equal, but in different locations
  ASSERT_NE(tuple, copy);

  auto tuple_ptr = tuple.as_ptr();
  auto copy_ptr = copy.as_ptr();

  ASSERT_EQ(copy_ptr[0], tuple_ptr[0]);
  ASSERT_EQ(copy_ptr[2], tuple_ptr[2]);

  // i.e. both point to the same element
  ASSERT_EQ(copy_ptr[1], copy_ptr[3]);

  // points to it's own element
  assert_deepcopy_list(copy_ptr[1], tuple_ptr[1]);
}

ErlTerm try_parse(std::string term, bool parse_multiple = false) {

  auto code_chunk = get_minimal_code_chunk();
  auto pcb = get_process(code_chunk);

  // when
  if (!parse_multiple) {
    return parse_term(term);
  } else {
    return parse_multiple_terms(term);
  }
}

TEST(ErlTerm, ParseList) {
  std::string test = "[1, 2, 3, 4, 5].";

  // when
  auto result = try_parse(test);

  // then
  auto vec = vec_from_erl_list(result);
  std::vector<ErlTerm> expected_vec = {1, 2, 3, 4, 5};

  for (auto &val : expected_vec) {
    val = make_small_int(val);
  }

  ASSERT_EQ(vec, expected_vec);
}

TEST(ErlTerm, ParseListBadFormatting) {
  std::string test = "[  1 , 2  , 3 , 4  , 5 ].";

  // when
  auto result = try_parse(test);

  // then
  auto vec = vec_from_erl_list(result);
  std::vector<ErlTerm> expected_vec = {1, 2, 3, 4, 5};

  for (auto &val : expected_vec) {
    val = make_small_int(val);
  }

  ASSERT_EQ(vec, expected_vec);
}

TEST(ErlTerm, ParseMix) {
  std::string test = "{1, [2, 3], 4}.";

  // when
  auto result = try_parse(test);

  // then
  ASSERT_EQ(result.getErlMajorType(), TUPLE_ET);

  auto ptr = result.as_ptr();

  ASSERT_EQ(ptr[1], make_small_int(1));

  auto list = ptr[2];
  ASSERT_EQ(list.getErlMajorType(), LIST_ET);

  std::vector<ErlTerm> expected_vec = {2, 3};
  for (auto &v : expected_vec) {
    v = make_small_int(v);
  }

  auto received_vec = vec_from_erl_list(list);

  ASSERT_EQ(received_vec, expected_vec);

  ASSERT_EQ(ptr[3], make_small_int(4));
}

TEST(ErlTerm, ParseMultiple) {
  std::string test = "1, 2, 3, 4.";

  // when
  auto result = try_parse(test, true);

  // then
  auto received_vec = vec_from_erl_list(result);

  std::vector<ErlTerm> expected_vec = {1, 2, 3, 4};
  for (auto &v : expected_vec) {
    v = make_small_int(v);
  }

  ASSERT_EQ(received_vec, expected_vec);
}

TEST(ErlTerm, ParseUnderscoreInt) {
  std::string test = "10_000";

  // when
  auto result = try_parse(test);

  // then
  ASSERT_EQ(result, mi(10000));
}

TEST(ErlTerm, ParseFunctionString) {
  std::string mfa = "gray:main(5000, {2001})";

  auto [func_id, terms] = parse_func_call(mfa, ArgumentAllocator{});

  GlobalFunctionId expected{
      .module = "gray", .function_name = "main", .arity = 2};

  ASSERT_EQ(func_id, expected);
  ASSERT_EQ(terms[0], mi(5000));
  assert_tuple(terms[1], {mi(2001)});
}

TEST(ErlTerm, ParseNoArgsFunctionString) {
  std::string mfa = "gray:main()";

  auto [func_id, terms] = parse_func_call(mfa, ArgumentAllocator{});

  GlobalFunctionId expected{
      .module = "gray", .function_name = "main", .arity = 0};

  ASSERT_EQ(func_id, expected);
}

TEST(ErlTerm, ToStringList) {
  std::vector<ErlTerm> to_print = {1, 2, 3, 4, 5};

  for (auto &val : to_print) {
    val = make_small_int(val);
  }

  auto list = erl_list_from_range(to_print, get_nil_term());

  // when
  auto result = to_string(list);

  // then
  ASSERT_EQ(result, "[1, 2, 3, 4, 5]");
}

TEST(ErlTerm, ToStringNilPair) {
  ErlTerm tuple[3] = {2 << 6, get_nil_term(), get_nil_term()};
  auto handle = make_boxed(tuple);

  // when
  auto result = to_string(handle);

  // then
  ASSERT_EQ(result, "{[], []}");
}

TEST(ErlTerm, ToStringTupleListAtom) {
  std::vector<ErlTerm> to_print = {1, 2, 3};

  for (auto &val : to_print) {
    val = make_small_int(val);
  }

  auto list = erl_list_from_range(to_print, get_nil_term());

  AtomChunk a({"dummy", "ok"});
  CodeChunk code_chunk({{RETURN_OP}}, 1, 1);
  code_chunk.atom_chunk = &a;

  get_process(code_chunk);

  ErlTerm heap[] = {3 << 6, list, emulator_main.get_atom_current("ok"),
                    make_small_int(7)};

  // when
  auto result = to_string(make_boxed(heap));

  // then
  ASSERT_EQ(result, "{[1, 2, 3], ok, 7}");
}

TEST(Parsing, FromBinary) {
  uint8_t data[] = {131, 108, 0, 0, 0, 1, 97, 55, 106};
  auto result = ErlTerm::from_binary(data, true);
  ErlTerm parsed = result.first;

  auto parsed_vec = vec_from_erl_list(parsed);
  std::vector<ErlTerm> expected = {make_small_int(55)};

  ASSERT_EQ(parsed_vec, expected);
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

  resume_process(pcb.get());

  SUCCEED();
}

TEST(JIT, ExportTableSameNameDiffArity) {
  auto code_chunk = get_minimal_code_chunk();
  AtomChunk atoms({"dummy", "module", "function"});

  auto inital_arity0_id = ExportFunctionId{2, 0, 0}; // function/0
  auto inital_arity2_id = ExportFunctionId{2, 2, 0}; // function/2

  ExportTableChunk e({inital_arity0_id, inital_arity2_id});
  auto file = get_beam_file({.c = code_chunk, .a = atoms, .e = e});

  auto found_arity0_id = file->get_external_id(GlobalFunctionId{
      .module = "module", .function_name = "function", .arity = 0});

  auto found_arity2_id = file->get_external_id(GlobalFunctionId{
      .module = "module", .function_name = "function", .arity = 2});

  ASSERT_EQ(inital_arity0_id, found_arity0_id);
  ASSERT_EQ(inital_arity2_id, found_arity2_id);
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

  xregs[0] = mi(25);
  xregs[1] = mi(30);

  // when
  resume_process(pcb.get());

  // then
  ASSERT_EQ(xregs[0], mi(25));
  ASSERT_EQ(xregs[1], mi(25));
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
  resume_process(pcb.get());

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
  auto pcb = get_process(file->code_chunk);

  // when
  resume_process(pcb.get());

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

  xregs[0] = mi(25);
  xregs[1] = mi(30);

  // when
  resume_process(pcb.get());

  // then
  ASSERT_EQ(xregs[0], mi(30));
  ASSERT_EQ(xregs[1], mi(25));
}

TEST(RISCV, Allocate) {
  // given
  std::vector<Instruction> instructions = {
      Instruction{ALLOCATE_OP, {get_lit(3), get_lit(0)}}};
  wrap_in_function(instructions);
  CodeChunk code_chunk(std::move(instructions), 1, 1);

  auto pcb = get_process(code_chunk);
  auto initial = pcb->get_shared<STOP>();

  // when
  resume_process(pcb.get());

  // then
  auto after = pcb->get_shared<STOP>();
  ASSERT_EQ(after, initial - 4);
  // no test for code pointer storing here, see call tests
}

// we can't have a deallocate only test because it loads
// the location in CODE_POINTER and tries to jump there...
TEST(RISCV, AllocateAndDeallocate) {
  // given
  std::vector<Instruction> instructions = {
      Instruction{ALLOCATE_OP, {get_lit(3), get_lit(0)}},
      Instruction{DEALLOCATE_OP, {get_lit(3)}}};

  wrap_in_function(instructions);
  CodeChunk code_chunk(std::move(instructions), 1, 1);

  ErlTerm e[5];

  auto pcb = get_process(code_chunk);
  pcb->set_shared<STOP>(e);

  // when
  resume_process(pcb.get());

  // then
  auto val = pcb->get_shared<STOP>();
  // remains the same because allocate and deallocate
  ASSERT_EQ(val, e) << "'e' was " << e;
}

TEST(RISCV, Trim) {
  // given
  auto trim_amount = 2;
  auto remaining = 1;
  std::vector<Instruction> instructions = {
      Instruction{TRIM_OP, {get_lit(trim_amount), get_lit(remaining)}}};

  wrap_in_function(instructions);
  CodeChunk code_chunk(std::move(instructions), 1, 1);

  auto code_ptr = 321412;
  ErlTerm stack[5] = {code_ptr, 0, 1, 2};

  auto pcb = get_process(code_chunk);
  pcb->set_shared<STOP>(stack);

  // when
  resume_process(pcb.get());

  // then
  auto new_stop = pcb->get_shared<STOP>();
  ASSERT_EQ(new_stop, stack + trim_amount);

  ASSERT_EQ(new_stop[0], code_ptr);
  ASSERT_EQ(new_stop[1], 2);
}

ErlReturnCode test_heap(Argument arg, size_t heap_size) {
  // given
  std::vector<Instruction> instructions = {
      Instruction{TEST_HEAP_OP, {arg, get_lit(0)}},
  };

  wrap_in_function(instructions);
  CodeChunk code_chunk(std::move(instructions), 1, 1);

  auto pcb = get_process(code_chunk, heap_size);
  set_current_pcb(*pcb);

  // when
  return resume_process(pcb.get());
}

TEST(RISCV, TestHeapNoGCLiteralYes) {
  // given
  auto result = test_heap(get_lit(4), 4);

  // then
  ASSERT_EQ(result, FINISH);
}

TEST(RISCV, TestHeapNoGCLiteralNo) {
  // given
  auto result = test_heap(get_lit(5), 4);

  // then
  ASSERT_EQ(result, FINISH);
}

TEST(RISCV, TestHeapNoGCAllocListYes) {
  // given
  AllocList a = {.words = 2, .floats = 0, .funs = 1};
  auto result = test_heap(Argument(EXT_ALLOC_LIST_TAG, {.alloc_list = &a}), 4);

  // then
  ASSERT_EQ(result, FINISH);
}

TEST(RISCV, TestHeapNoGCAllocListNo) {
  // given
  AllocList a = {.words = 2, .floats = 0, .funs = 1};
  auto result = test_heap(Argument(EXT_ALLOC_LIST_TAG, {.alloc_list = &a}), 3);

  // then
  ASSERT_EQ(result, FINISH);
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
  ErlTerm heap[] = {3 << 6, 0, mi(10), 20};
  xregs[0] = make_boxed(heap);

  // when
  resume_process(pcb.get());

  // then
  ASSERT_EQ(xregs[1], mi(10));
}

TEST(RISCV, PutTuple2) {
  std::vector<Argument> element_list = {
      Argument{X_REGISTER_TAG, {.arg_num = 1}},
      Argument{X_REGISTER_TAG, {.arg_num = 2}},
  };

  std::vector<Instruction> instructions = {Instruction{
      PUT_TUPLE2_OP,
      {
          Argument{X_REGISTER_TAG, {.arg_num = 0}},             // x0 (dest)
          Argument{EXT_LIST_TAG, {.arg_vec_p = &element_list}}, // terms
      }}};

  wrap_in_function(instructions);
  CodeChunk code_chunk(std::move(instructions), 1, 1);

  auto pcb = get_process(code_chunk);

  auto xregs = pcb->get_shared<XREG_ARRAY>();
  xregs[1] = mi(10);
  xregs[2] = mi(20);

  // when
  resume_process(pcb.get());

  // then
  assert_tuple(xregs[0], {mi(10), mi(20)});
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

  auto xregs = pcb->get_shared<XREG_ARRAY>();

  auto list = erl_list_from_range({mi(20), mi(30)}, get_nil_term());
  xregs[0] = list;

  // when
  resume_process(pcb.get());

  // then
  ASSERT_EQ(xregs[1], mi(20));
  auto other_list = vec_from_erl_list(xregs[2]);
  std::vector<ErlTerm> expected_list = {mi(30)};
  ASSERT_EQ(other_list, expected_list);
}

TEST(RISCV, GetTail) {
  std::vector<Instruction> instructions = {
      Instruction{GET_TL_OP,
                  {
                      Argument{X_REGISTER_TAG, {.arg_num = 0}},
                      Argument{X_REGISTER_TAG, {.arg_num = 1}},
                  }}};

  wrap_in_function(instructions);
  CodeChunk code_chunk(std::move(instructions), 1, 1);

  auto pcb = get_process(code_chunk);

  auto xreg = pcb->get_shared<XREG_ARRAY>();

  auto list = erl_list_from_range({mi(20), mi(30)}, get_nil_term());
  xreg[0] = list;

  // when
  resume_process(pcb.get());

  // then
  auto tail = vec_from_erl_list(xreg[1]);
  std::vector<ErlTerm> expected_tail = {mi(30)};
  ASSERT_EQ(tail, expected_tail);
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
  xregs[1] = erl_list_from_range({1, 2, 3}, get_nil_term());

  // when
  resume_process(pcb.get());

  // then
  std::vector<ErlTerm> out_list = vec_from_erl_list(xregs[2]);
  std::vector<ErlTerm> final_list = {0, 1, 2, 3};
  ASSERT_EQ(out_list, final_list);
  ASSERT_EQ(pcb->get_shared<HTOP>(), heap + 2);
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

  auto x1_start_val = 20;
  auto x2_start_val = 30;

  auto xregs = pcb->get_shared<XREG_ARRAY>();
  xregs[1] = x1_start_val;
  xregs[2] = x2_start_val;

  // when
  resume_process(pcb.get());

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
      Instruction{ALLOCATE_OP, {get_lit(1), get_lit(0)}},
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
  pcb->set_shared<REDUCTIONS>(5);

  // when
  auto result = resume_process(pcb.get());

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

  // set no reductions
  pcb->set_shared<REDUCTIONS>(0);

  // when
  auto result = resume_process(pcb.get());

  // then
  ASSERT_EQ(called, false);
  ASSERT_EQ(result, YIELD);

  auto resume_label = pcb->get_shared<RESUME_LABEL>();
  ASSERT_EQ(resume_label, 2); // resume at label 2
}

std::unique_ptr<BeamSrc>
get_file_with_import(std::string module_name, std::string function_name,
                     uint32_t arity, std::optional<FunctionTableChunk> f,
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

  auto pcb = get_process(file->code_chunk);

  // when
  resume_process(pcb.get());

  // then
  auto x_reg = pcb->get_shared<XREG_ARRAY>();
  ASSERT_EQ(x_reg[0], mi(100)); // this is what the test bif does
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

  auto pcb = get_process(file->code_chunk);
  auto xregs = pcb->get_shared<XREG_ARRAY>();

  xregs[0] = 12;
  xregs[1] = 134;

  // when
  resume_process(pcb.get());

  // then
  ASSERT_EQ(xregs[0], make_small_int(12 + 134));
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

  auto pcb = get_process(file->code_chunk);

  // when
  auto result = resume_process(pcb.get());

  // then
  auto xregs = pcb->get_shared<XREG_ARRAY>();
  ASSERT_EQ(xregs[0], make_small_int(100)); // this is what the test bif does

  // the thing about ONLY is that it should finish
  ASSERT_EQ(result, FINISH);
}

TEST(RISCV, CallExtLastBif) {
  uint64_t arity = 2;

  // don't need alloc/dealloc if it is a bif
  std::vector<Instruction> instructions = {
      Instruction{ALLOCATE_OP, {get_lit(2), get_lit(0)}},
      Instruction{CALL_EXT_LAST_OP,
                  {
                      Argument{LITERAL_TAG, {.arg_num = arity}},
                      Argument{LITERAL_TAG, {.arg_num = 0}}, // import index
                      Argument{LITERAL_TAG, {.arg_num = 2}}  // dealloc
                  }},
  };

  wrap_in_function(instructions, 1, false); // don't use return
  auto file = get_file_with_import("erlang", "test", arity, std::nullopt,
                                   std::move(instructions));

  auto pcb = get_process(file->code_chunk);
  auto xregs = pcb->get_shared<XREG_ARRAY>();

  xregs[0] = 12;
  xregs[1] = 134;

  auto initial_stack_loc = pcb->get_shared<STOP>();

  // when
  auto result = resume_process(pcb.get());

  // then
  ASSERT_EQ(xregs[0], mi(12 + 134));
  ASSERT_EQ(result, FINISH);

  auto new_stop = pcb->get_shared<STOP>();
  ASSERT_EQ(new_stop, initial_stack_loc);
}

TEST(RISCV, Bif0) {
  std::vector<Instruction> instructions = {
      Instruction{BIF0_OP,
                  {Argument{LITERAL_TAG, {.arg_num = 0}}, // import index
                   Argument{X_REGISTER_TAG, {.arg_num = 0}}}},
  };

  wrap_in_function(instructions);
  auto file = get_file_with_import("erlang", "test", 0, std::nullopt,
                                   std::move(instructions));

  auto pcb = get_process(file->code_chunk);
  auto xregs = pcb->get_shared<XREG_ARRAY>();
  xregs[0] = 0;

  // when
  resume_process(pcb.get());

  // then
  ASSERT_EQ(xregs[0], mi(100));
}

TEST(RISCV, GCBif1) {
  bool a, b;

  // don't need alloc/dealloc if it is a bif
  std::vector<Instruction> instructions = {
      Instruction{GC_BIF1_OP,
                  {
                      get_tag(LABEL_TAG, 2),      // fail label
                      get_tag(LITERAL_TAG, 0),    // Live X regs
                      get_tag(LITERAL_TAG, 0),    // bif_num (import index)
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

  auto pcb = get_process(file->code_chunk);
  auto xregs = pcb->get_shared<XREG_ARRAY>();
  xregs[3] = 35;

  // when
  resume_process(pcb.get());

  // then
  ASSERT_EQ(xregs[2], mi(35 * 10));
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

  auto pcb = get_process(file->code_chunk);
  auto xregs = pcb->get_shared<XREG_ARRAY>();
  xregs[1] = 25;
  xregs[3] = 37;

  // when
  resume_process(pcb.get());

  // then
  ASSERT_EQ(xregs[2], mi(25 + 37));
  ASSERT_TRUE(a);
  ASSERT_FALSE(b);
}

TEST(RISCV, GCBif2Fail) {
  bool a = false;
  bool b = false;

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

  auto pcb = get_process(file->code_chunk);

  // when
  resume_process(pcb.get());

  // then
  ASSERT_FALSE(a);
  ASSERT_TRUE(b);
}

TEST(RISCV, FastBif) {
  bool a = false;
  bool b = false;

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
  auto file = get_file_with_import("erlang", "+", 2, std::nullopt,
                                   std::move(instructions));

  auto pcb = get_process(file->code_chunk);
  auto xregs = pcb->get_shared<XREG_ARRAY>();
  xregs[1] = make_small_int(222);
  xregs[3] = make_small_int(1000);

  // when
  resume_process(pcb.get());

  // then
  ASSERT_TRUE(a);
  ASSERT_FALSE(b);
  ASSERT_EQ(xregs[2], make_small_int(1000 + 222));
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
  auto pcb = get_process(file->code_chunk);

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
  resume_process(pcb.get());

  // then
  auto result = pcb->get_shared<XREG_ARRAY>()[0];
  ProcessControlBlock *spawned_process = from_pid(result);

  auto new_xregs = spawned_process->get_shared<XREG_ARRAY>();

  auto &runnable = emulator_main.scheduler.runnable;
  ASSERT_NE(std::ranges::find(runnable, spawned_process), runnable.end());
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
  resume_process(pcb.get());

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

  Message new_msg(mi(10));
  pcb->queue_message(&new_msg);

  // when
  resume_process(pcb.get());

  // then
  ASSERT_TRUE(flag_a);
  ASSERT_FALSE(flag_b);

  auto x0 = pcb->get_shared<XREG_ARRAY>()[0];
  ASSERT_EQ(x0, mi(10));
}

TEST(RISCV, RemoveLastMessage) {
  std::vector<Instruction> instructions = {Instruction{REMOVE_MESSAGE_OP, {}}};

  wrap_in_function(instructions);
  CodeChunk code_chunk(std::move(instructions), 1, 1);

  auto pcb = get_process(code_chunk);

  auto new_msg = new Message(10);
  pcb->queue_message(new_msg);

  // when
  resume_process(pcb.get());

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
  resume_process(pcb.get());

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
  auto result = resume_process(pcb.get());

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
  emulator_main.scheduler.waiting.insert(other_pcb.get());

  std::vector<ErlTerm> list = {0, 1, 2, 3, 4};

  for (auto &val : list) {
    val = make_small_int(val);
  }

  auto erl_list = erl_list_from_range(list, get_nil_term());
  xreg[0] = make_pid(other_pcb.get());
  xreg[1] = erl_list;

  // when
  resume_process(pcb.get());

  auto other_mbox_head = other_pcb->get_shared<MBOX_HEAD>();
  auto msg_payload = other_mbox_head->get_payload();

  auto msg_vec = vec_from_erl_list(msg_payload);

  ASSERT_EQ(msg_vec, list);
  ASSERT_NE(msg_payload, erl_list);

  const auto &scheduler = emulator_main.scheduler;
  const auto &runnable = scheduler.runnable;
  ASSERT_NE(std::ranges::find(runnable, other_pcb.get()), runnable.end());
  ASSERT_FALSE(scheduler.waiting.contains(other_pcb.get()));
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

std::unique_ptr<CodeChunk> setup_is_tuple(bool *a, bool *b) {
  auto instructions =
      get_test_instrs(Instruction{IS_TUPLE_OP,
                                  {Argument{LABEL_TAG, {.arg_num = 2}},
                                   Argument{X_REGISTER_TAG, {.arg_num = 0}}}},
                      a, b);

  auto code_chunk = std::make_unique<CodeChunk>(std::move(instructions), 1, 2);

  return code_chunk;
}

TEST(RISCV, IsTupleOp) {
  bool a, b;

  auto code_chunk = setup_is_tuple(&a, &b);
  auto pcb = get_process(code_chunk.get());
  auto xregs = pcb->get_shared<XREG_ARRAY>();

  ErlTerm e[3] = {0b11000000, mi(1), mi(2)};
  xregs[0] = make_boxed(e);

  // when
  resume_process(pcb.get());

  // then
  ASSERT_TRUE(a);
  ASSERT_FALSE(b);
}

TEST(RISCV, IsTupleOpNotBoxed) {
  bool a, b;

  auto code_chunk = setup_is_tuple(&a, &b);
  auto pcb = get_process(code_chunk.get());
  auto xregs = pcb->get_shared<XREG_ARRAY>();

  xregs[0] = 0b1011111; // 5 as a small integer

  // when
  resume_process(pcb.get());

  // then
  ASSERT_FALSE(a);
  ASSERT_TRUE(b);
}

TEST(RISCV, IsTupleOpWrongBoxed) {
  bool a, b;

  auto code_chunk = setup_is_tuple(&a, &b);
  auto pcb = get_process(code_chunk.get());

  auto xregs = pcb->get_shared<XREG_ARRAY>();

  ErlTerm e[1] = {0b11010100}; // is a function now
  xregs[0] = make_boxed(e);

  // when
  resume_process(pcb.get());

  // then
  ASSERT_FALSE(a);
  ASSERT_TRUE(b);
}

std::unique_ptr<BeamSrc> setup_is_tagged_tuple(bool *a, bool *b) {
  auto instructions = get_test_instrs(
      Instruction{
          IS_TAGGED_TUPLE_OP,
          {Argument{LABEL_TAG, {.arg_num = 2}},
           Argument{X_REGISTER_TAG, {.arg_num = 0}},
           Argument{LITERAL_TAG, {.arg_num = 2}}, // arity
           Argument{ATOM_TAG, {.arg_num = 0}}},
      },
      a, b);

  CodeChunk code_chunk(std::move(instructions), 1, 2);
  AtomChunk atom_chunk({"dummy", "atom1"});
  auto file =
      get_beam_file({.c = std::move(code_chunk), .a = std::move(atom_chunk)});

  return file;
}

TEST(RISCV, IsTaggedTuple) {
  bool a, b;
  auto file = setup_is_tagged_tuple(&a, &b);
  auto pcb = get_process(file->code_chunk);

  auto xregs = pcb->get_shared<XREG_ARRAY>();
  ErlTerm heap[] = {2 << 6, make_atom(0), mi(2)};
  xregs[0] = make_boxed(heap);

  // when
  resume_process(pcb.get());

  // then
  ASSERT_TRUE(a);
  ASSERT_FALSE(b);
}

TEST(RISCV, IsTaggedTupleWrongArity) {
  bool a, b;
  auto file = setup_is_tagged_tuple(&a, &b);
  auto pcb = get_process(file->code_chunk);

  auto xregs = pcb->get_shared<XREG_ARRAY>();
  ErlTerm heap[] = {3 << 6, make_atom(0), mi(2), mi(4)};
  xregs[0] = make_boxed(heap);

  // when
  resume_process(pcb.get());

  // then
  ASSERT_FALSE(a);
  ASSERT_TRUE(b);
}

TEST(RISCV, IsTaggedTupleWrongAtom) {
  bool a, b;
  auto file = setup_is_tagged_tuple(&a, &b);
  auto pcb = get_process(file->code_chunk);

  auto xregs = pcb->get_shared<XREG_ARRAY>();
  ErlTerm heap[] = {2 << 6, make_atom(1), mi(2)};
  xregs[0] = make_boxed(heap);

  // when
  resume_process(pcb.get());

  // then
  ASSERT_FALSE(a);
  ASSERT_TRUE(b);
}

TEST(RISCV, IsTaggedTupleNotBoxed) {
  bool a, b;
  auto file = setup_is_tagged_tuple(&a, &b);
  auto pcb = get_process(file->code_chunk);

  auto xregs = pcb->get_shared<XREG_ARRAY>();
  ErlTerm heap[] = {2 << 6, make_atom(3), mi(2)};
  xregs[0] = ErlTerm(reinterpret_cast<uint64_t>(heap));

  // when
  resume_process(pcb.get());

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
      Instruction{ALLOCATE_OP, {get_lit(3), get_lit(0)}},
      Instruction{INIT_YREGS_OP,
                  {Argument{EXT_LIST_TAG, {.arg_vec_p = &arguments}}}},
      Instruction{WAIT_OP, {get_tag(LABEL_TAG, 0)}} // label doesn't matter
  };

  wrap_in_function(instructions);

  CodeChunk code_chunk(std::move(instructions), 1, 1);
  auto pcb = get_process(code_chunk, 1000);

  // when
  resume_process(pcb.get());

  auto stack = pcb->get_shared<STOP>();
  // then
  ASSERT_NE(stack[0], get_nil_term()); // code pointer
  ASSERT_EQ(stack[1], get_nil_term()); // y0
  ASSERT_EQ(stack[2], get_nil_term()); // y1
  ASSERT_NE(stack[3], get_nil_term()); // y2 (not initialised)
}

CodeChunk getCallLastCodeChunk(bool *flag) {
  std::vector<Instruction> func_1 = {
      Instruction{ALLOCATE_OP, {get_lit(3), get_lit(0)}},
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

  pcb->set_shared<REDUCTIONS>(1000);
  auto initial_stack = pcb->get_shared<STOP>();

  // when
  resume_process(pcb.get());

  // then
  ASSERT_TRUE(flag);
  ASSERT_EQ(pcb->get_shared<STOP>(), initial_stack);
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
  ErlTerm heap[] = {3 << 6, mi(1), mi(2), mi(3)}; // arity 3
  xregs[0] = make_boxed(heap);

  // when
  resume_process(pcb.get());

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
  ErlTerm heap[] = {4 << 6, mi(1), mi(2), mi(3), mi(4)}; // arity 4
  xregs[0] = make_boxed(heap);

  // when
  resume_process(pcb.get());

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
  xregs[0] = erl_list_from_range({mi(1), mi(2), mi(3)}, get_nil_term());

  // when
  resume_process(pcb.get());

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
  resume_process(pcb.get());

  // then
  ASSERT_FALSE(a);
  ASSERT_TRUE(b);
}

TEST(RISCV, TestIsNonEmptyListOtherFalse) {
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
  xregs[0] = make_small_int(5);

  // when
  resume_process(pcb.get());

  // then
  ASSERT_FALSE(a);
  ASSERT_TRUE(b);
}

TEST(RISCV, TestIsNilTrue) {
  bool a = false;
  bool b = false;

  auto instructions = get_test_instrs(
      Instruction{IS_NIL_OP,
                  {get_tag(LABEL_TAG, 2), get_tag(X_REGISTER_TAG, 0)}},
      &a, &b);

  CodeChunk code_chunk(std::move(instructions), 1, 2);
  auto pcb = get_process(code_chunk);

  auto xregs = pcb->get_shared<XREG_ARRAY>();
  xregs[0] = get_nil_term();

  // when
  resume_process(pcb.get());

  // then
  ASSERT_TRUE(a);
  ASSERT_FALSE(b);
}

TEST(RISCV, TestIsNilFalse) {
  bool a = false;
  bool b = false;

  auto instructions = get_test_instrs(
      Instruction{IS_NIL_OP,
                  {get_tag(LABEL_TAG, 2), get_tag(X_REGISTER_TAG, 0)}},
      &a, &b);

  CodeChunk code_chunk(std::move(instructions), 1, 2);
  auto pcb = get_process(code_chunk);

  auto xregs = pcb->get_shared<XREG_ARRAY>();
  xregs[0] = erl_list_from_range({mi(1), mi(2), mi(3)}, get_nil_term());

  // when
  resume_process(pcb.get());

  // then
  ASSERT_FALSE(a);
  ASSERT_TRUE(b);
}

TEST(RISCV, TestIsAtomTrue) {
  bool a = false;
  bool b = false;

  auto instructions = get_test_instrs(
      Instruction{IS_ATOM_OP,
                  {get_tag(LABEL_TAG, 2), get_tag(X_REGISTER_TAG, 0)}},
      &a, &b);

  CodeChunk code_chunk(std::move(instructions), 1, 2);
  AtomChunk atoms({"one", "two"});
  auto file = get_beam_file({.c = code_chunk, .a = atoms});
  auto pcb = get_process(file->code_chunk);

  auto xregs = pcb->get_shared<XREG_ARRAY>();
  xregs[0] = make_atom(1);

  // when
  resume_process(pcb.get());

  // then
  ASSERT_TRUE(a);
  ASSERT_FALSE(b);
}

TEST(RISCV, TestIsAtomFalse) {
  bool a = false;
  bool b = false;

  auto instructions =
      get_test_instrs(Instruction{IS_ATOM_OP,
                                  {get_tag(LABEL_TAG, 2),
                                   get_tag(X_REGISTER_TAG, 0), get_lit(3)}},
                      &a, &b);

  CodeChunk code_chunk(std::move(instructions), 1, 2);
  AtomChunk atoms({"one", "two"});
  auto file = get_beam_file({.c = code_chunk, .a = atoms});
  auto pcb = get_process(file->code_chunk);

  auto xregs = pcb->get_shared<XREG_ARRAY>();
  xregs[0] = make_small_int(100);

  // when
  resume_process(pcb.get());

  // then
  ASSERT_FALSE(a);
  ASSERT_TRUE(b);
}

TEST(RISCV, TestIsNilOtherFalse) {
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
  xregs[0] = make_small_int(5);

  // when
  resume_process(pcb.get());

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
  resume_process(pcb.get());

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
  auto arg1 = erl_list_from_range({make_small_int(1)}, get_nil_term());
  auto arg2 = make_small_int(4000);

  auto should_ge_jump = false;
  do_compare_test(IS_GE_OP, arg1, arg2, should_ge_jump);
  do_compare_test(IS_LT_OP, arg1, arg2, !should_ge_jump);
}

TEST(RISCV, CompDiffTypesLT) {
  auto arg1 = make_small_int(4000);
  auto arg2 = erl_list_from_range({make_small_int(1)}, get_nil_term());

  auto should_ge_jump = true;
  do_compare_test(IS_GE_OP, arg1, arg2, should_ge_jump);
  do_compare_test(IS_LT_OP, arg1, arg2, !should_ge_jump);
}

TEST(RISCV, CompListGTLen) {
  auto arg1 = erl_list_from_range({make_small_int(1), make_small_int(2)},
                                  get_nil_term());
  auto arg2 = erl_list_from_range(
      {make_small_int(1), make_small_int(2), make_small_int(3)},
      get_nil_term());

  auto should_ge_jump = false;
  do_compare_test(IS_GE_OP, arg1, arg2, should_ge_jump);
  do_compare_test(IS_LT_OP, arg1, arg2, !should_ge_jump);
}

TEST(RISCV, CompListGTValue) {
  auto arg1 = erl_list_from_range({make_small_int(2)}, get_nil_term());
  auto arg2 = erl_list_from_range({make_small_int(1), make_small_int(2)},
                                  get_nil_term());

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

TEST(RISCV, CompLTGTIntEqual) {
  auto arg1 = make_small_int(1000);
  auto arg2 = make_small_int(1000);

  auto should_ge_jump = false;
  do_compare_test(IS_GE_OP, arg1, arg2, should_ge_jump);
  do_compare_test(IS_LT_OP, arg1, arg2, !should_ge_jump);
}

TEST(RISCV, CompGTClose) {
  auto arg1 = make_small_int(5000);
  auto arg2 = make_small_int(5001);

  auto should_ge_jump = true;
  do_compare_test(IS_GE_OP, arg1, arg2, should_ge_jump);
  do_compare_test(IS_LT_OP, arg1, arg2, !should_ge_jump);
}

TEST(RISCV, CompEqExactTrue) {
  auto arg1 = make_small_int(0);
  auto arg2 = make_small_int(0);

  auto should_jump = false;
  do_compare_test(IS_EQ_EXACT_OP, arg1, arg2, should_jump);
}

TEST(RISCV, Badmatch) {
  std::vector<Instruction> instructions = {
      Instruction{BADMATCH_OP, {get_tag(Y_REGISTER_TAG, 0)}}};
  wrap_in_function(instructions);

  CodeChunk code_chunk(std::move(instructions), 1, 1);
  auto pcb = get_process(code_chunk);

  // then
  auto result = resume_process(pcb.get());

  // then
  ASSERT_EQ(result, BADMATCH);
}

TEST(BuiltInFunction, ListSeq) {
  auto code_chunk = get_minimal_code_chunk();
  auto pcb = get_process(code_chunk);
  ErlTerm get_seq = lists_seq(mi(1), mi(5)).a0;

  ASSERT_EQ(to_string(get_seq), "[1, 2, 3, 4, 5]");
}

TEST(BuiltInFunction, ListZip) {
  auto code_chunk = get_minimal_code_chunk();
  auto pcb = get_process(code_chunk);

  ErlTerm get_seq = lists_seq(mi(1), mi(5)).a0;
  std::vector<ErlTerm> as_vec = vec_from_erl_list(get_seq);

  ErlTerm other_list =
      erl_list_from_range({mi(7), mi(3), mi(10), mi(9), mi(2)}, get_nil_term());

  ErlTerm zipped = lists_zip(get_seq, other_list).a0;

  ASSERT_EQ(to_string(zipped), "[{1, 7}, {2, 3}, {3, 10}, {4, 9}, {5, 2}]");
}

TEST(BuiltInFunction, Concat) {
  auto code_chunk = get_minimal_code_chunk();
  auto pcb = get_process(code_chunk);

  ErlTerm get_seq = lists_seq(mi(1), mi(5)).a0;

  ErlTerm other_list =
      erl_list_from_range({mi(7), mi(3), mi(10), mi(9), mi(2)}, get_nil_term());

  ErlTerm result = concat(get_seq, other_list).a0;

  ASSERT_EQ(to_string(result), "[1, 2, 3, 4, 5, 7, 3, 10, 9, 2]");
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

TEST(BuiltInFunction, ErlSub) {
  auto a = make_small_int(25);
  auto b = make_small_int(4);

  // when
  auto result = erl_sub(a, b);

  // then
  ASSERT_EQ(result.a0 >> 4, 21);
  ASSERT_EQ(ErlTerm(result.a0).getTagType(), SMALL_INT_T);
  ASSERT_EQ(result.a1, 0);
}

TEST(BuiltInFunction, ErlAdd) {
  auto a = make_small_int(25);
  auto b = make_small_int(4);

  // when
  auto result = erl_add(a, b);

  // then
  ASSERT_EQ(result.a0 >> 4, 29);
  ASSERT_EQ(ErlTerm(result.a0).getTagType(), SMALL_INT_T);
  ASSERT_EQ(result.a1, 0);
}

TEST(BuiltInFunction, ErlBXor) {
  auto a = make_small_int(25);
  auto b = make_small_int(4);

  // when
  auto result = erl_bxor(a, b);

  // then
  ASSERT_EQ(result.a0 >> 4, 25 ^ 4);
  ASSERT_EQ(ErlTerm(result.a0).getTagType(), SMALL_INT_T);
  ASSERT_EQ(result.a1, 0);
}

TEST(BuiltInFunction, ErlBsr) {
  auto a = make_small_int(25);
  auto b = make_small_int(2);

  // when
  auto result = erl_bsr(a, b);

  // then
  ASSERT_EQ(result.a0 >> 4, 25 >> 2);
  ASSERT_EQ(ErlTerm(result.a0).getTagType(), SMALL_INT_T);
  ASSERT_EQ(result.a1, 0);
}

ErlTerm test_fast_bif2(std::string name, ErlTerm a, ErlTerm b) {
  std::vector<Instruction> instructions = {
      Instruction{BIF2_OP,
                  {
                      Argument{LABEL_TAG, {.arg_num = 0}},      // label
                      Argument{LITERAL_TAG, {.arg_num = 0}},    // import index
                      Argument{X_REGISTER_TAG, {.arg_num = 0}}, // arg 1
                      Argument{X_REGISTER_TAG, {.arg_num = 1}}, // arg 2
                      Argument{X_REGISTER_TAG, {.arg_num = 2}}, // dest
                  }},
  };

  wrap_in_function(instructions);
  auto file = get_file_with_import("erlang", name, 2, std::nullopt,
                                   std::move(instructions));
  auto pcb = get_process(file->code_chunk);

  auto xregs = pcb->get_shared<XREG_ARRAY>();
  xregs[0] = a;
  xregs[1] = b;

  resume_process(pcb.get());

  return xregs[2];
}

TEST(FastBif, ErlBxor) {
  auto result =
      test_fast_bif2("bxor", make_small_int(2314), make_small_int(4231));

  // then
  ASSERT_EQ(result, make_small_int(2314 ^ 4231));
}

TEST(FastBif, ErlBxorWith0) {
  auto result = test_fast_bif2("bxor", make_small_int(23), make_small_int(0));

  // then
  ASSERT_EQ(result, make_small_int(23 ^ 0));
}

TEST(FastBif, ErlBsr) {
  auto result = test_fast_bif2("bsr", make_small_int(1234), make_small_int(4));

  // then
  ASSERT_EQ(result, make_small_int(1234 >> 4));
}

TEST(FastBif, ErlAdd) {
  auto result = test_fast_bif2("+", make_small_int(1234), make_small_int(5555));

  // then
  ASSERT_EQ(result, make_small_int(1234 + 5555));
}

TEST(FastBif, ErlSub) {
  auto result = test_fast_bif2("-", make_small_int(1234), make_small_int(234));

  // then
  ASSERT_EQ(result, make_small_int(1234 - 234));
}

TEST(GC, StablePoolAllocator) {
  using alloc_type = std::array<ErlTerm, 2>;
  StablePoolAllocator<alloc_type> allocator(10);

  std::vector<alloc_type *> alloced;

  const auto INITIAL_ALLOC = 100;
  const auto DEALLOC = 100;
  const auto FINAL_ALLOC = 200;

  auto alloc_and_push = [&](size_t i) {
    auto result = allocator.alloc();
    result->at(0) = i;
    result->at(1) = i * 3;

    alloced.push_back(result);
  };

  auto assert_correct = [&](size_t i) {
    auto array = *alloced.at(i);
    ASSERT_EQ(array[0], i);
    ASSERT_EQ(array[1], i * 3);
  };

  for (size_t i = 0; i < INITIAL_ALLOC; i++) {
    alloc_and_push(i);
  }

  // assert preserved
  for (int i = 0; i < INITIAL_ALLOC; i++) {
    assert_correct(i);
  }

  auto num_free = allocator.get_free_num();

  for (int i = 0; i < DEALLOC; i++) {
    allocator.free(alloced[i]);
  }

  auto num_free_after_dealloc = allocator.get_free_num();

  ASSERT_EQ(num_free + DEALLOC, num_free_after_dealloc);

  for (int i = INITIAL_ALLOC; i < FINAL_ALLOC; i++) {
    alloc_and_push(i);
  }

  for (int i = DEALLOC; i < FINAL_ALLOC; i++) {
    assert_correct(i);
  }
}

TEST(GC, AllocateEnough) {
  auto code_chunk = get_minimal_code_chunk();
  auto pcb = get_process(code_chunk);

  set_current_pcb(*pcb);

  // force gc
  const auto ALLOC_SIZE = pcb->heap.size();
  ErlTerm *val = pcb->do_gc(ALLOC_SIZE, 0);

  ASSERT_LE(val + ALLOC_SIZE, pcb->heap.data() + pcb->heap.size());
}

TEST(GC, CopyFirstPass) {
  auto code_chunk = get_minimal_code_chunk();
  auto pcb = get_process(code_chunk, 10);

  set_current_pcb(*pcb);
  auto xregs = pcb->get_shared<XREG_ARRAY>();

  auto current_stack = pcb->get_shared<STOP>();

  const auto STACK_SIZE = 2;
  assert(current_stack - STACK_SIZE >= pcb->heap.data());

  std::iota(current_stack - STACK_SIZE, current_stack, 0);
  pcb->set_shared<STOP>(current_stack - STACK_SIZE);

  auto check_stack = [&]() {
    auto count = 0;
    for (auto val : pcb->get_stack()) {
      ASSERT_EQ(val.term, count);
      count++;
    }

    ASSERT_EQ(pcb->get_stack().size(), STACK_SIZE);
  };

  check_stack();

  auto tuple = pcb->allocate_heap_frag(11);
  tuple[0] = 10 << 6;

  std::iota(tuple + 1, tuple + 11, 1);
  auto initial = make_boxed(tuple);
  xregs[0] = initial;

  // force gc
  const auto ALLOC_SIZE = pcb->heap.size();
  pcb->do_gc(ALLOC_SIZE, 1);

  ASSERT_NE(xregs[0], initial); // expect pointer to have changed
  assert_tuple(xregs[0], {1, 2, 3, 4, 5, 6, 7, 8, 9, 10});

  check_stack();
}

TEST(GC, CopyShared) {
  auto code_chunk = get_minimal_code_chunk();
  auto pcb = get_process(code_chunk);

  set_current_pcb(*pcb);
  auto xregs = pcb->get_shared<XREG_ARRAY>();

  auto tuple = pcb->allocate_heap_frag(11);
  tuple[0] = 10 << 6;

  std::iota(tuple + 1, tuple + 11, 1);
  auto initial = make_boxed(tuple);
  xregs[0] = initial;
  xregs[1] = initial;

  // force gc
  const auto ALLOC_SIZE = pcb->heap.size();
  pcb->do_gc(ALLOC_SIZE, 2);

  ASSERT_NE(xregs[0], initial);
  assert_tuple(xregs[0], {1, 2, 3, 4, 5, 6, 7, 8, 9, 10});

  ASSERT_EQ(xregs[0], xregs[1]);
}

TEST(GC, GenerationPromotion) {
  auto code_chunk = get_minimal_code_chunk();
  auto pcb = get_process(code_chunk);

  set_current_pcb(*pcb);
  auto xregs = pcb->get_shared<XREG_ARRAY>();

  auto tuple = pcb->get_shared<HTOP>();
  std::iota(tuple + 1, tuple + 11, 1);
  pcb->set_shared<HTOP>(tuple + 11);

  auto initial = make_boxed(tuple);
  xregs[0] = initial;

  // force gc
  auto ALLOC_SIZE = pcb->heap.size();
  pcb->do_gc(ALLOC_SIZE, 1);

  ASSERT_FALSE(pcb->old_heap.contains(xregs[0].as_ptr()));

  // force gc a second time
  ALLOC_SIZE = pcb->heap.size();
  pcb->do_gc(ALLOC_SIZE, 1);

  // Now x0 should have been moved to old heap
  ASSERT_TRUE(pcb->old_heap.contains(xregs[0].as_ptr()));
}

TEST(GC, HeapFragment) {
  auto code_chunk = get_minimal_code_chunk();
  auto pcb = get_process(code_chunk);

  set_current_pcb(*pcb);
  auto frag = pcb->allocate_heap_frag(1000);
  auto list = erl_list_from_range({make_small_int(1), make_small_int(2)},
                                  get_nil_term());

  auto xregs = pcb->get_shared<XREG_ARRAY>();
  xregs[0] = deepcopy(list, frag);
  auto initial_handle = xregs[0];

  // when
  pcb->do_gc(0, 1);

  // then
  // heap frags should be empty
  ASSERT_TRUE(pcb->heap_fragments.empty());

  // data should be on the new heap
  auto new_handle = xregs[0];
  ASSERT_NE(new_handle, initial_handle);

  auto gced_ptr = new_handle.as_ptr();
  ASSERT_LE(pcb->heap.data(), gced_ptr);
  ASSERT_LT(gced_ptr, pcb->heap.data() + pcb->heap.size());
}

int main(int argc, char **argv) {
  setup_logging(argv[0]);
  testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
