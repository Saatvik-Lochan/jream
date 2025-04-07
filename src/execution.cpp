#include <algorithm>
#include <cstdlib>
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
#include "riscv_gen.hpp"

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
    assert(argument.tag != EXT_ALLOC_LIST_TAG);

    if (argument.tag == EXT_LIST_TAG) {
      c_args_ptr[i] = reinterpret_cast<uint64_t>(argument.arg_raw.arg_vec_p);
    } else {
      c_args_ptr[i] = argument.arg_raw.arg_num;
    }
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
inline std::vector<uint8_t> translate_code_section(CodeChunk &code_chunk,
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

  // reserve a spot which will need to be filled
  struct LinkRequest {
    ssize_t branch_instr_location;
    uint64_t label;
  };

  std::vector<LinkRequest> link_requests;

  // must be called before adding the branch instr
  auto reserve_branch_label = [&link_requests, &compiled](uint64_t label) {
    ssize_t curr_offset = compiled.size();
    link_requests.push_back(
        LinkRequest{.branch_instr_location = curr_offset, .label = label});
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
        code_chunk.set_external_jump_loc(index, *result);
        add_code(get_riscv(CALL_EXT_BIF_SNIP));
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

    case REMOVE_MESSAGE_OP: {
      add_code(get_riscv(REMOVE_SNIP));
      break;
    }

    case WAIT_OP: {
      add_setup_args_code();
      add_code(get_riscv(WAIT_SNIP));
      break;
    }

    case SEND_OP: {
      add_code(get_riscv(SEND_SNIP));
      break;
    }

    case LOOP_REC_OP: {
      auto label = instr.arguments[0];
      assert(label.tag == LABEL_TAG);

      add_code(get_riscv(LOOP_REC_1_SNIP));
      reserve_branch_label(label.arg_raw.arg_num);

      // beq t0, t1, LABEL
      add_riscv_instr(create_branch_equal(5, 6, 0));

      add_code(get_riscv(LOOP_REC_2_SNIP));

      auto destination = instr.arguments[1];

      // save t2
      add_riscv_instrs(create_store_appropriate(destination, 7));
      break;
    }

    case GET_LIST_OP: {
      add_setup_args_code();
      add_code(get_riscv(LOAD_1_ARG_SNIP));

      // load the register pointed at in t0
      add_riscv_instrs(create_load_appropriate(instr.arguments[0], 5));

      add_code(get_riscv(GET_LIST_SNIP));

      // store from t1 and t2
      add_riscv_instrs(create_store_appropriate(instr.arguments[1], 6));
      add_riscv_instrs(create_store_appropriate(instr.arguments[2], 7));
      break;
    }

    case MAKE_FUN3_OP: {
      add_setup_args_code();

      auto index = instr.arguments[0];
      assert(index.tag == LITERAL_TAG);

      auto freeze_list = instr.arguments[2];
      assert(freeze_list.tag == EXT_LIST_TAG);

      auto freeze_list_val = freeze_list.arg_raw.arg_vec_p;

      auto num_free = freeze_list_val->size();

      // now we alloc heap space and store index
      // store num spots to allocate in t0
      add_riscv_instr(create_add_immediate(5, 0, num_free + 2));
      add_code(get_riscv(MAKE_FUN3_SNIP));

      // now pointing to pos right after the index loc
      int position = 16;

      for (const auto &reg_to_save : *freeze_list_val) {
        // load reg to t0
        add_riscv_instrs(create_load_appropriate(reg_to_save, 5));

        // t1 has the heap pointer
        // sd t0, pos(t1)
        add_riscv_instr(create_store_doubleword(6, 5, position));

        position += 8;
      }

      // the value in t1 (i.e. the heap pointer before this)
      add_riscv_instrs(create_store_appropriate(instr.arguments[1], 6));

      break;
    }

    case IS_TUPLE_OP: {
      auto label = instr.arguments[0];
      assert(label.tag == LABEL_TAG);

      auto label_val = label.arg_raw.arg_num;

      auto source = instr.arguments[1];

      // load into t0
      add_riscv_instrs(create_load_appropriate(source, 5));
      add_code(get_riscv(IS_TUPLE_1_SNIP));

      reserve_branch_label(label_val);
      add_riscv_instr(create_branch_not_equal(6, 7, 0));

      add_code(get_riscv(IS_TUPLE_2_SNIP));

      reserve_branch_label(label_val);
      add_riscv_instr(create_branch_not_equal(6, 7, 0));

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

  // fix link requests
  for (auto request : link_requests) {
    auto index = request.branch_instr_location;
    auto data_loc = &compiled[index];
    auto as_riscv_instr = reinterpret_cast<RISCV_Instruction *>(data_loc);

    // check if label is inside function scope
    assert(label_offsets.contains(request.label));

    ssize_t label_val = label_offsets[request.label];
    int16_t offset = label_val - index;

    set_imm_B_type_instruction(*as_riscv_instr, offset);
  }

  code_chunk.label_offsets = label_offsets;

  return compiled;
}

std::vector<uint8_t> translate_function(CodeChunk &code_chunk,
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

uint8_t *compile_erlang_func(CodeChunk &code_chunk, uint64_t func_index) {
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

  auto head = pcb->get_address<MBOX_HEAD>();
  pcb->set_shared<MBOX_TAIL>(head);
  pcb->set_shared<MBOX_SAVE>(head);

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
void init_ext_jump(BeamSrc *file) {
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

void create_emulator(std::vector<BeamSrc *> files) {
  Emulator out;

  // set imports/exports
  for (auto file_p : files) {
    file_p->code_chunk.import_table_chunk = &file_p->import_table_chunk;
    file_p->code_chunk.function_table_chunk = &file_p->function_table_chunk;
    file_p->code_chunk.atom_chunk = &file_p->atom_chunk;
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
