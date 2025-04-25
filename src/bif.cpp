#include "bif.hpp"
#include "asm_utility.hpp"
#include "beam_defs.hpp"
#include "execution.hpp"
#include "external_term.hpp"
#include "pcb.hpp"
#include "profiler.hpp"
#include <cassert>
#include <cstdint>
#include <fstream>
#include <glog/logging.h>
#include <iostream>
#include <stdexcept>

/* The functions here define the BIFs
 *
 * They must take trivial types as arguments. Pointers, uint64_t, for example.
 *
 * If they are going to return they return an BIFReturn
 *
 * BIF's should always allocate using heap frags, otherwise they will remain
 * pointing to old data! Even if we have the live registers, it doesn't make
 * a difference
 *
 */

BIFReturn ret100() { return make_small_int(100); }

BIFReturn test_mul10(uint64_t a) { return make_small_int(a * 10); }

BIFReturn test_add(uint64_t a, uint64_t b) { return make_small_int(a + b); }

BIFReturn test_fail(uint64_t a, uint64_t b) { return fail(); }

BIFReturn spawn_1(uint64_t fun_raw) {
  PROFILE();

  auto fun = ErlTerm(fun_raw);
  auto code_chunk_p = get_pcb()->get_shared<CODE_CHUNK_P>();

  assert(fun.getErlMajorType() == FUN_ET);

  auto header_ptr = fun.as_ptr();
  [[maybe_unused]]
  auto header = *header_ptr;

  auto index = header_ptr[1];
  auto &functions = code_chunk_p->function_table_chunk->functions;

  assert(index.term < functions.size());
  auto func_id = functions[index];

  // +1 for the index ptr
  assert(header >> 6 == func_id.num_free + 1);

  // create process with entry point
  auto pcb = new ProcessControlBlock(EntryPoint{code_chunk_p, func_id.label});

#ifdef ENABLE_BIF_LOG
  LOG(INFO) << "\tspawning at label: " << func_id.label;
#endif

  auto new_x_reg = pcb->get_shared<XREG_ARRAY>();
  auto func_arity = func_id.arity - func_id.num_free;
  auto num_free = func_id.num_free;

  assert(func_arity == 0);

  // copy frozen variales
  std::copy(header_ptr + 2, header_ptr + 2 + num_free, new_x_reg + func_arity);

  // move to ready queue
  emulator_main.scheduler.runnable.push(pcb);

  // prepare return value
  return make_pid(pcb);
}

BIFReturn erl_length(uint64_t list_raw) {
  ErlList list(list_raw);
  return make_small_int(list.length());
}

BIFReturn erl_error(uint64_t a) {
  throw std::logic_error(
      std::format("Erlang threw an exception: {}", to_string(ErlTerm(a))));
}

BIFReturn self() {
  auto pcb = get_pcb();
  return make_pid(pcb);
}

template <typename T> ErlTerm do_arith(uint64_t a, uint64_t b, T &&f) {
  assert(ErlTerm(a).getTagType() == SMALL_INT_T);
  assert(ErlTerm(b).getTagType() == SMALL_INT_T);

  return make_small_int(f((a >> 4), (b >> 4)));
}

BIFReturn erl_div(int64_t a, int64_t b) {
  return do_arith(a, b, [](auto a, auto b) { return a / b; });
}

BIFReturn erl_sub(int64_t a, int64_t b) {
  PROFILE();
  // the 0b1111 for each a and b cancel out
  // we just reapply it after
  return (a - b) | 0b1111;
}

BIFReturn erl_add(int64_t a, int64_t b) {
  PROFILE();
  // Since we add an extra 0b1111 twice, we subtract it so it's equal to only
  // once
  return (a + b - 0b1111);
}

BIFReturn erl_bsr(int64_t a, int64_t amount) {
  PROFILE();
  // we don't have to unshift it first and then reshift
  return (a >> (amount >> 4)) | 0b1111;
}

BIFReturn erl_bxor(int64_t a, int64_t b) {
  PROFILE();
  // the 0b1111 at the end doesn't change anything
  return (a ^ b) | 0b1111;
}

BIFReturn concat(uint64_t list1_raw, uint64_t list2_raw) {
  ErlList list1(list1_raw);
  ErlList list2(list2_raw);

  auto pcb = get_pcb();
  auto new_list_len = list1.length() + list2.length();
  auto loc = pcb->allocate_heap_frag(new_list_len * 2);

  ErlListBuilder out;

  for (auto e : list1) {
    out.add_term(e, loc);
    loc += 2;
  }

  for (auto e : list2) {
    out.add_term(e, loc);
    loc += 2;
  }

  out.set_end(get_nil_term());
  return out.get_list();
}

BIFReturn lists_seq(uint64_t start_raw, uint64_t stop_raw) {
  assert(ErlTerm(start_raw).getTagType() == SMALL_INT_T);
  assert(ErlTerm(stop_raw).getTagType() == SMALL_INT_T);

  auto start = start_raw >> 4;
  auto stop = (stop_raw >> 4) + 1;

  auto pcb = get_pcb();
  auto loc = pcb->allocate_heap_frag((stop - start) * 2);

  ErlListBuilder out;

  for (size_t i = start; i < stop; i++) {
    out.add_term(make_small_int(i), loc);
    loc += 2;
  }

  out.set_end(get_nil_term());
  return out.get_list();
}

BIFReturn lists_zip(uint64_t list1_raw, uint64_t list2_raw) {
  ErlList list1(list1_raw);
  ErlList list2(list2_raw);

  auto it1 = list1.begin();
  auto it2 = list2.begin();

  // calculate size and allocate first
  auto len_list_1 = list1.length();
  auto len_list_2 = list2.length();

  if (len_list_1 != len_list_2) {
    return fail();
  }

  // we will be allocating a list of length len_list_1
  // then for each element a tuple of size 2 (heap space 3)
  auto pcb = get_pcb();
  auto loc = pcb->allocate_heap_frag(len_list_1 * 5);

  ErlListBuilder out;

  for (; it1 != list1.end() && it2 != list2.end(); ++it1, ++it2) {
    auto tuple = loc;
    tuple[0] = 2 << 6;
    tuple[1] = *it1;
    tuple[2] = *it2;
    loc += 3;

    out.add_term(make_boxed(tuple), loc);
    loc += 2;
  }

  out.set_end(get_nil_term());
  return out.get_list();
}

BIFReturn file_consult(uint64_t file_name_raw, uint64_t xregs) {
  PROFILE();
  assert(ErlTerm(file_name_raw).getErlMajorType() == LIST_ET);
  ErlList file_list(file_name_raw);

  std::string file_name;

  for (auto c : file_list) {
    file_name.push_back(c.term >> 4);
  }

  std::ifstream file(file_name,
                     std::ios::in | std::ios::binary | std::ios::ate);

  auto pcb = get_pcb();
  auto tuple = pcb->allocate_heap_frag(3);
  tuple[0] = 2 << 6;

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
  PROFILE();
  std::cout << to_string(ErlTerm(term)) << "\n";
  return emulator_main.get_atom_current("ok");
}

BIFReturn put(uint64_t key, uint64_t value) {
  auto pcb = get_pcb();
  auto &dict = pcb->process_dict;

  ErlTerm out;

  auto it = dict.find(key);
  if (it != dict.end()) {
    out = it->second;
  } else {
    out = emulator_main.get_atom_current("undefined");
  }

  pcb->process_dict[key] = value;

  return out;
}

BIFReturn get(uint64_t key) {
  auto pcb = get_pcb();
  auto &dict = pcb->process_dict;

  auto it = dict.find(key);
  if (it != dict.end()) {
    return it->second;
  } else {
    return emulator_main.get_atom_current("undefined");
  }
}

// debug ops
uintptr_t curr_cp() {
  auto code_pointer = get_pcb()->get_stack().data()[0];
  return code_pointer.term;
}

size_t stack_space() {
  auto code_pointer = get_pcb()->get_stack().data()[0];
  return code_pointer.term;
}

size_t num_highwater() {
  auto pcb = get_pcb();
  return pcb->highwater - pcb->heap.data();
}
