#include "bif.hpp"
#include "beam_defs.hpp"
#include "execution.hpp"
#include "external_term.hpp"
#include "pcb.hpp"
#include <cassert>
#include <cstdint>
#include <fstream>
#include <glog/logging.h>
#include <iostream>

ProcessControlBlock *get_pcb() {
  return emulator_main.scheduler.get_current_process();
}

/* The functions here define the BIFs
 *
 * They must take trivial types as arguments. Pointers, uint64_t, for example.
 *
 * If they are going to return they return an BIFReturn
 */

BIFReturn ret100() { return 100; }

BIFReturn mul10(uint64_t a) { return a * 10; }

BIFReturn add(uint64_t a, uint64_t b) { return a + b; }

BIFReturn test_fail(uint64_t a, uint64_t b) { return fail(); }

BIFReturn spawn_1(uint64_t fun_raw) {

  auto fun = ErlTerm(fun_raw);
  auto code_chunk_p = get_pcb()->get_shared<CODE_CHUNK_P>();

  assert(fun.getErlMajorType() == FUN_ET);

  auto header_ptr = fun.as_ptr();
  auto header = *header_ptr;

  auto index = header_ptr[1];
  auto &functions = code_chunk_p->function_table_chunk->functions;

  assert(index.term < functions.size());
  auto func_id = functions[index];

  // +1 for the index ptr
  assert(header >> 6 == func_id.num_free + 1);

  // create process with entry point
  auto pcb =
      emulator_main.create_process(EntryPoint{code_chunk_p, func_id.label});
  DLOG(INFO) << "\tspawning at label: " << func_id.label;

  auto new_x_reg = pcb->get_shared<XREG_ARRAY>();
  auto func_arity = func_id.arity - func_id.num_free;
  auto num_free = func_id.num_free;

  assert(func_arity == 0);

  // copy frozen variales
  std::copy(header_ptr + 2, header_ptr + 2 + num_free, new_x_reg + func_arity);

  // move to ready queue
  emulator_main.scheduler.runnable.insert(pcb);

  // prepare return value
  return make_pid(pcb);
}

BIFReturn length(uint64_t list_raw) {
  ErlList list(list_raw);

  uint64_t count = 0;

  for (auto _ : list) {
    count++;
  }

  return make_small_int(count);
}

BIFReturn self() {
  auto pcb = get_pcb();
  return make_pid(pcb);
}

template <typename T>
ErlTerm do_arith(uint64_t a, uint64_t b, T&& f) {
  assert(ErlTerm(a).getTagType() == SMALL_INT_T);
  assert(ErlTerm(b).getTagType() == SMALL_INT_T);

  return make_small_int(f((a >> 4), (b >> 4)));
}

BIFReturn erl_div(uint64_t a, uint64_t b) {
  return do_arith(a, b, [](auto a, auto b){ return a / b; });
}

BIFReturn erl_sub(uint64_t a, uint64_t b) {
  return do_arith(a, b, [](auto a, auto b){ return a - b; });
}

// WARNING: This can not be used in erlang because it is inplace!
// and we are mutating immutable lists!
BIFReturn list_split(uint64_t first_size_raw, uint64_t list_raw,
                     uint64_t xregs) {
  assert(ErlTerm(first_size_raw).getTagType() == SMALL_INT_T);
  assert(ErlTerm(list_raw).getErlMajorType() == LIST_ET);

  ErlTerm first_size = first_size_raw >> 4;

  auto pcb = get_pcb();

  auto tuple = pcb->allocate_tuple(2, xregs);
  tuple[1] = list_raw;

  // just point to the tuple
  auto curr = tuple + 1;
  size_t count = 0;

  while (count++ < first_size.term) {
    if (curr->getTagType() != LIST_T) {
      return fail();
    }

    curr = curr->as_ptr() + 1;
  }

  tuple[2] = *curr;
  *curr = get_nil_term();

  return make_boxed(tuple);
}

BIFReturn file_consult(uint64_t file_name_raw, uint64_t xregs) {
  assert(ErlTerm(file_name_raw).getErlMajorType() == LIST_ET);
  ErlList file_list(file_name_raw);

  std::string file_name;

  for (auto c : file_list) {
    file_name.push_back(c.term >> 4);
  }

  std::ifstream file(file_name,
                     std::ios::in | std::ios::binary | std::ios::ate);

  auto pcb = get_pcb();
  auto tuple = pcb->allocate_tuple(2, xregs);

  if (!file) {
    tuple[1] = emulator_main.get_atom_current("error");
    tuple[2] = get_nil_term();

    return make_boxed(tuple);
  }

  std::streamsize size = file.tellg();
  file.seekg(0, std::ios::beg);

  // prealloc and then read
  std::string buffer(size, '\0');
  file.read(&buffer[0], size);

  auto result = parse_multiple_terms(buffer);

  tuple[1] = emulator_main.get_atom_current("ok");
  tuple[2] = result;

  return make_boxed(tuple);
}

// TODO make general
BIFReturn io_write(uint64_t term) {
  std::cout << to_string(ErlTerm(term)) << "\n";
  return 0;
}
