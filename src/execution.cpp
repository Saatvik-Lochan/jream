#include <algorithm>
#include <glog/logging.h>

#include <cassert>
#include <cstdint>
#include <stdexcept>
#include <sys/mman.h>
#include <unordered_map>

#include "asm_callable.h"
#include "asm_utility.h"
#include "beam_defs.h"
#include "execution.h"
#include "generated/instr_code.h"
#include "op_arity.h"
#include "pcb.h"

uint64_t *get_compact_and_cache_instr_args(const CodeChunk &code_chunk,
                                           size_t index) {

  auto &c_args_ptr = code_chunk.compacted_arg_p_array[index];
  const auto &args = code_chunk.instructions[index].arguments;

  if (c_args_ptr != nullptr) {
    return c_args_ptr;
  }

  auto num_args = args.size();

  c_args_ptr = new uint64_t[num_args];

  // do compaction
  for (size_t i = 0; i < num_args; i++) {
    const auto &argument = args[i];

    // not implemented yet
    assert(argument.tag != EXT_LIST_TAG && argument.tag != EXT_ALLOC_LIST_TAG);

    c_args_ptr[i] = argument.arg_raw.arg_num;
  }

  return c_args_ptr;
}

inline std::vector<uint8_t> get_riscv_from_snippet(OpCode op) {
  AsmSnippet snip;

  switch (op) {
  case ALLOCATE_OP:
    snip = ALLOCATE_SNIP;
    break;

  case DEALLOCATE_OP:
    snip = DEALLOCATE_SNIP;
    break;

  default:
    LOG(FATAL) << "No Snippet for Op " << op_names[op];
  }

  return get_riscv(snip);
}

RISCV_Instruction create_I_type_instruction(uint8_t opcode, uint8_t rd,
                                            uint8_t funct3, uint8_t rs1,
                                            int16_t imm) {
  assert(-2047 < imm && imm < 2048);
  assert(0 <= rd && rd < 32);
  assert(0 <= rs1 && rs1 < 32);

  RISCV_Instruction out;
  out.set_opcode(opcode);
  out.set_funct3(funct3);
  out.set_rs1(rs1);
  out.set_rd(rd);

  out.set_bits(20, 32, imm & 0xfff);

  return out;
}

RISCV_Instruction create_S_type_instruction(uint8_t opcode, uint8_t funct3,
                                            uint8_t rs1, uint8_t rs2,
                                            int16_t imm) {
  assert(-2047 < imm && imm < 2048);
  assert(0 <= rs1 && rs1 < 32);
  assert(0 <= rs2 && rs2 < 32);

  RISCV_Instruction out;
  out.set_opcode(opcode);
  out.set_funct3(funct3);
  out.set_rs1(rs1);
  out.set_rs2(rs2);

  out.set_bits(7, 12, imm & 0b11111);
  out.set_bits(25, 32, (imm >> 5) & 0b1111111);

  return out;
}

inline RISCV_Instruction create_load_doubleword(uint8_t rd, uint8_t rs,
                                                int16_t imm) {
  constexpr auto load_instr_bits = 0b0000011; // load
  constexpr auto funct3_bits = 0b011;         // width

  return create_I_type_instruction(load_instr_bits, rd, funct3_bits, rs, imm);
}

inline RISCV_Instruction create_store_doubleword(uint8_t rs1, uint8_t rs2,
                                                 int16_t imm) {
  constexpr auto store_instr_bits = 0b0100011; // store
  constexpr auto funct3_bits = 0b011;          // width

  return create_S_type_instruction(store_instr_bits, funct3_bits, rs1, rs2, imm);
}

inline RISCV_Instruction create_load_x_reg(uint8_t riscv_dest_reg,
                                           int16_t x_reg_num,
                                           uint8_t x_array_register) {

  // ld riscv_dest_reg, x_reg_num(s5)
  return create_load_doubleword(riscv_dest_reg, x_array_register, x_reg_num * 8);
}

inline std::vector<RISCV_Instruction>
create_load_y_reg(uint8_t riscv_dest_reg, uint16_t y_reg_num,
                  uint8_t pcb_p_register) {

  return std::vector<RISCV_Instruction>{
      // ld riscv_dest_reg, STOP_index(pcb_p_register)
      create_load_doubleword(riscv_dest_reg, pcb_p_register, STOP * 8),

      // ld riscv_dest_reg, y_reg_num(riscv_dest_reg)
      create_store_doubleword(riscv_dest_reg, riscv_dest_reg, y_reg_num * 8)};
}

inline RISCV_Instruction create_store_x_reg(uint8_t riscv_dest_reg,
                                            uint16_t x_reg_num,
                                            uint8_t x_array_register) {
  // sd riscv_dest_reg, x_reg_num(s5)
  return create_store_doubleword(x_array_register, riscv_dest_reg, x_reg_num * 8);
}

inline std::vector<RISCV_Instruction>
create_store_y_reg(uint8_t riscv_dest_reg, uint16_t y_reg_num,
                   uint8_t pcb_p_register) {

  return std::vector<RISCV_Instruction>{
      // ld riscv_dest_reg, STOP_index(pcb_p_register)
      create_load_doubleword(riscv_dest_reg, pcb_p_register, STOP * 8),

      // sd riscv_dest_reg, y_reg_num(riscv_dest_reg)
      create_store_doubleword(riscv_dest_reg, riscv_dest_reg, y_reg_num * 8)};
}

inline std::vector<RISCV_Instruction>
create_load_appropriate(Argument arg, uint8_t dest_reg) {
  switch (arg.tag) {
  case X_REGISTER_TAG: {
    // assume s5 is the x array register
    return std::vector<RISCV_Instruction>{
        create_load_x_reg(dest_reg, arg.arg_raw.arg_num, 21)};
  }
  case Y_REGISTER_TAG: {
    // assumes s1 points to the pcb
    return create_load_y_reg(dest_reg, arg.arg_raw.arg_num, 9);
  }
  case LITERAL_TAG: {
    // TODO I am unsure if the 'LITERAL' tag is what I think it is
    // return an empty vector as we don't need to make any changes
    return std::vector<RISCV_Instruction>();
  }
  default:
    throw std::logic_error(
        std::format("Cannot load the tag {}", TagToString(arg.tag)));
  }
}

inline std::vector<RISCV_Instruction>
create_store_appropriate(Argument arg, uint8_t dest_reg) {
  switch (arg.tag) {
  case X_REGISTER_TAG: {
    // assume s5 is the x array register
    return std::vector<RISCV_Instruction>{
        create_store_x_reg(dest_reg, arg.arg_raw.arg_num, 21)};
  }
  case Y_REGISTER_TAG: {
    // assumes s1 points to the pcb
    return create_store_y_reg(dest_reg, arg.arg_raw.arg_num, 9);
  }
  default:
    throw std::logic_error(
        std::format("Cannot store at this tag", TagToString(arg.tag)));
  }
}

// garbage collection is tricky
std::vector<uint8_t> translate_function(const CodeChunk &code_chunk,
                                        CodeSection code_sec) {
  std::vector<uint8_t> compiled;
  std::unordered_map<uint64_t, size_t> label_pointers;

  // convenience lambdas
  auto add_code = [&compiled](const std::vector<uint8_t> &code) {
    compiled.insert(compiled.end(), code.begin(), code.end());
  };

  auto add_riscv_instr = [&compiled](RISCV_Instruction instr) {
    compiled.insert(compiled.end(), instr.raw, instr.raw + 4);
  };

  auto add_riscv_instrs =
      [&add_riscv_instr](const std::vector<RISCV_Instruction> &code) {
        std::for_each(code.begin(), code.end(), add_riscv_instr);
      };

  // main loop
  for (size_t instr_index = code_sec.start; instr_index < code_sec.end;
       instr_index++) {

    auto add_setup_code = [&add_riscv_instr, &instr_index]() {
      // load the pointer at index'th value in the argument array (pointer to
      // this in s2=x18) to the s3=x19 register
      add_riscv_instr(create_load_doubleword(19, 18, instr_index * 8));
    };

    get_compact_and_cache_instr_args(code_chunk, instr_index);

    const auto &instr = code_chunk.instructions[instr_index];

    switch (instr.op_code) {
    case LABEL_OP: {
      auto label_arg = instr.arguments[0];
      assert(label_arg.tag == LITERAL_TAG);
      label_pointers[label_arg.arg_raw.arg_num] = compiled.size();
      break;
    }

    case LINE_OP: { // ignore, debug info
      break;
    }

    case FUNC_INFO_OP: { // could assert on function identifier
      break;
    }

    case GET_LIST_OP: {
      add_setup_code();
      add_code(get_riscv(LOAD_1_ARG_SNIP));

      // load the register pointed at in t1
      add_riscv_instrs(create_load_appropriate(instr.arguments[0], 5));

      add_code(get_riscv(GET_LIST_SNIP));

      // store from t1 and t2
      add_riscv_instrs(create_store_appropriate(instr.arguments[1], 6));
      add_riscv_instrs(create_store_appropriate(instr.arguments[2], 7));
      break;
    }

    default: {
      add_setup_code();

      // get translated code
      auto result = get_riscv_from_snippet(instr.op_code);
      compiled.insert(compiled.end(), result.begin(), result.end());
    }
    }
  }

  auto setup_code = get_riscv(SETUP_SNIP);
  auto teardown_code = get_riscv(TEARDOWN_SNIP);

  compiled.insert(compiled.begin(), setup_code.begin(), setup_code.end());
  compiled.insert(compiled.end(), teardown_code.begin(), teardown_code.end());

  return compiled;
}

void spawn_process(const CodeChunk &code_chunk, FunctionIdentifier f_id) {
  // initialise memory
  //  i.e. stack + heap (and old heap)
  // cache code?
  // load code
  // execute code

  // have to deal with reducing reductions on a call as well, then
  // do more scheduling shit
}

compiled_func_p move_code_to_memory(const std::vector<uint8_t> &code) {
  // allocate page aligned memory
  void *const allocated_mem = mmap(0, code.size(), PROT_READ | PROT_WRITE,
                                   MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);

  if (allocated_mem == MAP_FAILED) {
    std::string msg =
        std::format("Could not allocate memory with mmap. Errno: {}", errno);
    throw std::runtime_error(msg);
  }

  std::copy(code.begin(), code.end(),
            reinterpret_cast<uint8_t *>(allocated_mem));

  // make memory executable
  const auto result =
      mprotect(allocated_mem, code.size(), PROT_READ | PROT_EXEC);

  if (result == -1) {
    std::string msg =
        std::format("Could not make memory executable. Errno: {}", errno);
    throw std::runtime_error(msg);
  }

  return reinterpret_cast<compiled_func_p>(allocated_mem);
}

void run_code_section(CodeChunk &code_chunk, const CodeSection code_sec,
                      ProcessControlBlock *pcb) {
  auto &cached = code_chunk.cached_code_sections;

  if (!cached.contains(code_sec)) {
    auto code = translate_function(code_chunk, code_sec);
    LOG(INFO) << "Code being loaded into memory: " << code;

    auto func = move_code_to_memory(code);
    cached[code_sec] = func;
  }

  compiled_func_p func = cached[code_sec];
  func(pcb, code_chunk.compacted_arg_p_array, all_funs);
}
