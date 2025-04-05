#include <algorithm>
#include <glog/logging.h>

#include <cassert>
#include <cstdint>
#include <optional>
#include <stdexcept>
#include <sys/mman.h>
#include <unordered_map>

#include "asm_callable.hpp"
#include "asm_utility.hpp"
#include "beam_defs.hpp"
#include "bif.hpp"
#include "execution.hpp"
#include "generated/instr_code.hpp"
#include "op_arity.hpp"
#include "pcb.hpp"
#include "precompiled.hpp"

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

inline RISCV_Instruction create_add_immediate(uint8_t rd, uint8_t rs,
                                              uint16_t imm) {
  constexpr auto op_code_bits = 0b0010011; // load
  constexpr auto funct3_bits = 0x0;
  //
  return create_I_type_instruction(op_code_bits, rd, funct3_bits, rs, imm);
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

  return create_S_type_instruction(store_instr_bits, funct3_bits, rs1, rs2,
                                   imm);
}

inline RISCV_Instruction create_load_x_reg(uint8_t riscv_dest_reg,
                                           int16_t x_reg_num,
                                           uint8_t x_array_register) {

  // ld riscv_dest_reg, x_reg_num(s5)
  return create_load_doubleword(riscv_dest_reg, x_array_register,
                                x_reg_num * 8);
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
  return create_store_doubleword(x_array_register, riscv_dest_reg,
                                 x_reg_num * 8);
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

std::optional<ext_func> bif_from_id(GlobalFunctionIdentifier id,
                                    const AtomChunk &atom_chunk) {

  auto &atoms = atom_chunk.atoms;

  auto id_string = std::format("{}:{}/{}", atoms[id.module],
                               atoms[id.function_name], id.arity);
  auto result = name_bif_map.find(id_string);

  if (result == name_bif_map.end()) {
    return std::nullopt;
  }

  return std::make_optional(result->second);
}

// garbage collection is tricky
inline std::vector<uint8_t> translate_code_section(const CodeChunk &code_chunk,
                                                   CodeSection code_sec) {
  std::vector<uint8_t> compiled;
  std::unordered_map<uint64_t, size_t> label_offsets;

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

    auto add_setup_args_code = [&add_riscv_instr, &instr_index]() {
      // load the pointer at index'th value in the argument array (pointer to
      // this in s2=x18) to the s3=x19 register
      add_riscv_instr(create_load_doubleword(19, 18, instr_index * 8));
    };

    get_compact_and_cache_instr_args(code_chunk, instr_index);

    const auto &instr = code_chunk.instructions[instr_index];

    switch (instr.op_code) {
    case DEBUG_EXECUTE_ARBITRARY: {
      [[maybe_unused]]
      auto flag_pos = instr.arguments[0];
      assert(flag_pos.tag == LITERAL_TAG);

      add_setup_args_code();
      add_code(get_riscv(DEBUG_EXECUTE_ARIBITRARY_SNIP));
      break;
    }
    case LABEL_OP: {
      auto label_arg = instr.arguments[0];
      assert(label_arg.tag == LITERAL_TAG);
      label_offsets[label_arg.arg_raw.arg_num] = compiled.size();
      break;
    }

    case LINE_OP: { // ignore, debug info
      break;
    }

    case FUNC_INFO_OP: { // could assert on function identifier
      break;
    }

    case CALL_OP: {
      add_setup_args_code();

      [[maybe_unused]]
      auto arity = instr.arguments[0];
      assert(arity.tag == LITERAL_TAG);

      [[maybe_unused]]
      auto label = instr.arguments[1];
      assert(label.tag == LABEL_TAG);

      // check reductions and maybe yield
      add_code(get_riscv(CALL_SNIP));
      break;
    }

    case CALL_EXT_OP: {
      add_setup_args_code();

      [[maybe_unused]]
      auto arity = instr.arguments[0];
      assert(arity.tag == LITERAL_TAG);

      auto destination = instr.arguments[1];
      assert(destination.tag == LITERAL_TAG);

      auto index = destination.arg_raw.arg_num;
      auto func_id = code_chunk.import_table_chunk->imports[index];

      auto result = bif_from_id(func_id, *code_chunk.atom_chunk);

      if (result) {
        code_chunk.external_jump_locations[index].func = *result;
        get_riscv(CALL_EXT_BIF_SNIP);

      } else {
        // external call which is either not implemented or user defined
        // if external then what?
        throw std::logic_error("Bif not yet defined");
      }
      break;
    }

    case RETURN_OP: {
      // load code pointer and jump
      add_code(get_riscv(RETURN_SNIP));
      break;
    }

    case GET_LIST_OP: {
      add_setup_args_code();
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
      add_setup_args_code();

      // get translated code
      auto result = get_riscv_from_snippet(instr.op_code);
      add_code(result);
    }
    }
  }

  return compiled;
}

std::vector<uint8_t> translate_function(const CodeChunk &code_chunk,
                                        uint64_t func_index) {
  assert(func_index < code_chunk.function_count);

  auto end_instr_index = code_chunk.instructions.size();
  if (func_index + 1 < code_chunk.function_count) {
    end_instr_index =
        code_chunk.label_table[code_chunk.func_label_table[func_index + 1]];
  }

  auto section = CodeSection{
      code_chunk.label_table[code_chunk.func_label_table[func_index]],
      end_instr_index};

  return translate_code_section(code_chunk, section);
}

uint8_t *move_code_to_memory(const std::vector<uint8_t> &code) {
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

  return reinterpret_cast<uint8_t *>(allocated_mem);
}

uint8_t *compile_erlang_func(const CodeChunk &code_chunk, uint64_t func_index) {
  auto code = translate_function(code_chunk, func_index);
  auto code_ptr = move_code_to_memory(code);
  return code_ptr;
}

ErlReturnCode setup_and_go_label(ProcessControlBlock *pcb, uint64_t label_num) {

  auto code_chunk = pcb->get_shared<CODE_CHUNK_P>();

  return PreCompiled::setup_and_goto_label(
      code_chunk, pcb, all_funs, PreCompiled::teardown_code, label_num);
}

ErlReturnCode resume_process(ProcessControlBlock *pcb) {
  return setup_and_go_label(pcb, pcb->get_shared<RESUME_LABEL>());
}

ProcessControlBlock *create_process(CodeChunk &code_chunk) {
  ProcessControlBlock *pcb = new ProcessControlBlock;

  // TODO initialise all shared code
  pcb->set_shared<CODE_CHUNK_P>(&code_chunk);
  pcb->set_shared<XREG_ARRAY>(new ErlTerm[1001]);

  return pcb;
}

ProcessControlBlock *create_process(CodeChunk &code_chunk,
                                    uint64_t func_index) {
  auto pcb = create_process(code_chunk);

  auto label_num = code_chunk.func_label_table[func_index];
  pcb->set_shared<RESUME_LABEL>(label_num);

  return pcb;
}

ProcessControlBlock *Scheduler::pick_next() {
  auto chosen_it = runnable.begin();

  if (chosen_it == runnable.end()) {
    return nullptr;
  }

  auto value = runnable.extract(chosen_it);
  return value.value();
}

bool Scheduler::signal(ProcessControlBlock *process) {
  auto it = waiting.find(process);

  if (it == waiting.end()) {
    return false;
  }

  auto node = waiting.extract(it);
  runnable.insert(std::move(node));

  return true;
}

// TODO this can actually just go in the compilation step, I
// anyway need to find the bif_from_id there, so I might as well fill in the
// relevant thing...
void init_ext_jump(BeamFile *file) {
  const auto &imports = file->import_table_chunk.imports;

  // allocate imports
  auto import_num = imports.size();
  auto ext_jumps = new ExtJump[import_num];

  for (size_t i = 0; i < import_num; i++) {
    const auto func_id = imports[i];

    auto result = bif_from_id(func_id, file->atom_chunk);

    if (result) {
      ext_jumps[i].func = *result;
    }
  }

  file->code_chunk.external_jump_locations = ext_jumps;
}

void create_emulator(std::vector<BeamFile *> files) {
  Emulator out;

  // set imports/exports
  for (auto file_p : files) {
    file_p->code_chunk.import_table_chunk = &file_p->import_table_chunk;
    file_p->code_chunk.function_table_chunk = &file_p->function_table_chunk;
    file_p->code_chunk.atom_chunk = &file_p->atom_chunk;
    init_ext_jump(file_p);
  }
}

void run_emulator(Emulator emulator, CodeChunk &code_chunk,
                  uint64_t func_index) {

  auto &scheduler = emulator.scheduler;

  auto process = create_process(code_chunk, func_index);
  scheduler.waiting.insert(process);

  while (auto to_run = scheduler.pick_next()) {
    resume_process(to_run);
  }
}
