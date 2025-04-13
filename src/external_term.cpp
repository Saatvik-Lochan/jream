#include <glog/logging.h>

#include "exceptions.hpp"
#include "execution.hpp"
#include "external_term.hpp"
#include "int_from_bytes.hpp"
#include "pcb.hpp"
#include <cassert>
#include <cstdint>
#include <format>
#include <stdexcept>
#include <string>
#include <type_traits>
#include <vector>

std::string ErlTerm::raw_display() { return std::format("{:b}", term); }

std::string ErlTerm::display() {
  switch (getTagType()) {
  case HEADER_T:
    return "Header";
  case LIST_T:
    return "List";
  case BOXED_T:
    return "Boxed";
  case PID_T:
    return "PID";
  case PORT_T:
    return "Port";
  case SMALL_INT_T:
    return "Small Integer " + std::to_string(term >> 4);
  case ATOM_T:
    return "Atom " + std::to_string(term >> 6);
  case CATCH_T:
    return "Catch";
  case NIL_T:
    return "Nil";
  default:
    throw std::logic_error("No such tag type");
  }
}

ErlTerm from_integer(const std::vector<uint8_t> &big_integer) {
  throw NotImplementedException("Really big integers not supported now");
}

// TODO test this!
template <typename T> ErlTerm ErlTerm::from_integer(T integer) {
  static_assert(std::is_integral_v<T>, "T must be an integral value");
  // TODO this static assert fails on characters on the RISC-V VM
  /*static_assert(std::is_signed_v<T>, "T must be a signed value");*/

  const ssize_t converted = integer;

  // only C++20 onwards since << was undefined on negative signed integrals
  if (converted << 4 >> 4 != integer) {
    throw NotImplementedException("big integers not supported yet");
  }

  size_t term = (converted << 4) + 0b1111;
  return ErlTerm(std::move(term));
}

std::pair<ErlTerm, uint8_t *> ErlTerm::from_binary(uint8_t *data,
                                                   bool is_initial) {
  if (is_initial) {
    assert(*data == 131);
    data++;
  }
  uint8_t type_byte = data[0];

  switch (type_byte) {
  case 97: // small_integer_ext
    return {ErlTerm((data[1] << 4) & 0b1111), data + 2};
  case 98: { // integer_ext
    auto integer = big_endian_from_bytes<int32_t>(data + 1);
    return {from_integer(integer), data + 5};
  }
  case 104: { // small_tuple_ext
    uint8_t arity = data[1];

    ErlTerm *tuple_p = new ErlTerm[arity + 1];
    tuple_p[0] = arity << (4 + 2);

    data += 2;

    for (int i = 0; i < arity; i++) {
      auto result = from_binary(data, false);

      tuple_p[i + 1] = result.first;
      data = result.second;
    }

    ErlTerm out((reinterpret_cast<size_t>(tuple_p) & 0b00) + 0b10);
    return {out, data};
  }
  case 106: { // nil_ext
    return {get_nil_term(), data + 1};
  }
  case 107: { // string_ext
    auto string_len = big_endian_from_bytes<uint16_t>(++data);
    data += 2;

    std::vector<ErlTerm> terms;

    for (int i = 0; i < string_len; i++) {
      char character = data[i];
      ErlTerm char_term = from_integer(character);

      terms.push_back(char_term);
    }

    ErlTerm list = erl_list_from_vec(terms, get_nil_term());
    return {list, data + string_len};
  }
  case 108: { // list_ext
    auto list_len = big_endian_from_bytes<uint32_t>(data + 1);

    data += 5; // place after list length
    std::vector<ErlTerm> terms;

    for (uint32_t i = 0; i < list_len; i++) { // +1 for tail of the list
      auto result = from_binary(data, false);
      data = result.second;

      terms.push_back(result.first);
    }

    auto end = from_binary(data, false).first;
    ErlTerm list = erl_list_from_vec(terms, end);

    return {list, data};
  }
  default: {
    const std::string err_msg =
        "Not implemented binary_to_term conversion for type byte " +
        std::to_string(type_byte);

    throw NotImplementedException(err_msg.c_str());
  }
  }
}

ErlTerm erl_list_from_vec(std::vector<ErlTerm> terms, ErlTerm end) {
  ErlListBuilder builder;

  for (auto term : terms) {
    builder.add_term(term, new ErlTerm[2]);
  }

  builder.set_end(end);
  return builder.get_list();
}

std::vector<ErlTerm> vec_from_erl_list(ErlTerm e, bool include_end) {
  ErlList list(e);
  std::vector<ErlTerm> out;

  auto it = list.begin();
  for (; it != list.end(); ++it) {
    out.push_back(*it);
  }

  if (include_end) {
    out.push_back(it.get_end());
  }

  return out;
}

// Assume that all required space has already been allocated contiguously.
// Copies anything necessary to to_loc and returns the the word representing
// the whole type
ErlTerm deepcopy(ErlTerm e, ErlTerm *&to_loc, ErlTerm *max_alloc) {
  const uint8_t bits = e.term & 0b11;

  switch (bits) {
  case 0b11: { // only one word to copy
    return e;
  }
  case 0b10: {
    ErlTerm *tuple_start = e.as_ptr();
    ErlTerm header = *tuple_start;
    assert((header & 0b11) == 0b00);

    uint64_t length = header >> 6;
    uint64_t whole_tuple_size = length + 1;

    auto box_end = tuple_start + whole_tuple_size;

    assert(to_loc + whole_tuple_size <= max_alloc);

    // here we assume that to_loc has enough room
    std::copy(tuple_start, box_end, to_loc);

    return make_boxed(to_loc);
  }
  case 0b01: {
    ErlList e_list(e);
    ErlListBuilder builder;

    auto it = e_list.begin();
    for (; it != e_list.end(); ++it) {
      assert(to_loc + 2 <= max_alloc);

      builder.add_term(*it, to_loc);
      to_loc += 2;
    }

    builder.set_end(it.get_end());

    return builder.get_list();
  }
  default: {
    throw std::logic_error(
        "Deepcopy not possibly for return address/header words (tag: 0b00)");
  }
  }
}

ErlTerm make_boxed(ErlTerm *ptr) {
  return (reinterpret_cast<uint64_t>(ptr) & TAGGING_MASK) + 0b10;
}

ErlTerm make_small_int(uint64_t num) {
  assert(((num << 4) >> 4) == num);

  return ErlTerm((num << 4) + 0b1111);
}

ErlTerm make_atom(uint64_t index) {
  assert(((index << 6) >> 6) == index);

  return ErlTerm((index << 6) + 0b1011);
}

bool is_num(char c) { return 48 <= c && c < 58; }

ErlTerm parse_term(const std::string &term, size_t &from,
                   ProcessControlBlock *pcb);

ErlTerm parse_int(const std::string &term, size_t &from,
                  ProcessControlBlock *pcb) {

  auto num = 0;

  char curr;

  for (; is_num(curr = term[from]); from++) {
    num *= 10;
    num += curr - 48;
  }

  return make_small_int(num);
}

std::vector<ErlTerm> collect_till(const std::string &term, char end, size_t &from,
                             ProcessControlBlock *pcb) {
  std::vector<ErlTerm> terms;

  // TODO make it work for empty array...
  terms.push_back(parse_term(term, from, pcb));

  while (term[from] != end) {
    switch (term[from]) {
    case ',': {
      from++;
      terms.push_back(parse_term(term, from, pcb));
      break;
    }
    case ' ': {
      from++;
      break;
    }
    default: {
      throw std::logic_error(
          std::format("Invalid parsing. Did not expect '{}'.", term[from]));
    }
    }
  }

  from++;
  return terms;
}

ErlTerm terms_to_list(const std::vector<ErlTerm> &terms,
                      ProcessControlBlock *pcb) {
  ErlListBuilder builder;

  auto curr = pcb->allocate_heap(terms.size() * 2);

  for (auto term : terms) {
    builder.add_term(term, curr);
    curr += 2;
  }

  builder.set_end(get_nil_term());
  return builder.get_list();
}

void check_start(std::string term_name, char c, char needed) {
  if (c != needed) {
    throw std::logic_error(std::format("parse {} but incorrect opening {}",
                           term_name, c));
  }
}

ErlTerm parse_erl_list(const std::string &term, size_t &from,
                       ProcessControlBlock *pcb) {

  check_start("list", term[from], '[');
  from++;

  auto terms = collect_till(term, ']', from, pcb);

  return terms_to_list(terms, pcb);
}

ErlTerm parse_tuple(const std::string &term, size_t &from,
                    ProcessControlBlock *pcb) {

  check_start("tuple", term[from], '{');
  from++;

  auto terms = collect_till(term, '}', from, pcb);

  auto alloced = pcb->allocate_tuple(terms.size());
  for (size_t i = 0; i < terms.size(); i++) {
    alloced[i + 1] = terms[i];
  }

  return make_boxed(alloced);
}

ErlTerm parse_term(const std::string &term, size_t &from,
                   ProcessControlBlock *pcb) {

  while (true) {
    switch (term[from]) {
    case '{':
      return parse_tuple(term, from, pcb);
    case '[':
      return parse_erl_list(term, from, pcb);
    case ' ': {
      from++;
      break;
    }
    default: {
      if (is_num(term[from])) {
        return parse_int(term, from, pcb);
      } else {
        throw std::logic_error("Unexpected type when parsing");
      }
    }
    }
  }
}

ErlTerm parse_terms_into_list(const std::string &term,
                              ProcessControlBlock *pcb) {
  size_t from = 0;
  auto terms = collect_till(term, '.', from, pcb);

  return terms_to_list(terms, pcb);
}

ErlTerm parse_term(const std::string &term) {
  auto pcb = emulator_main.scheduler.get_current_process();
  size_t count = 0;
  return parse_term(term, count, pcb);
}

ErlTerm parse_multiple_terms(const std::string &terms) {
  auto pcb = emulator_main.scheduler.get_current_process();
  return parse_terms_into_list(terms, pcb);
}

std::string to_string(ErlTerm erl_term);

std::string to_string(std::vector<ErlTerm> terms, std::string start,
                      std::string end) {

  if (terms.size() == 0) {
    return start + end;
  }

  std::string out_string = start;

  for (size_t i = 0; i < terms.size() - 1; i++) {
    out_string += to_string(terms[i]) + ", ";
  }

  out_string += to_string(terms[terms.size() - 1]);

  return out_string + end;
}

std::string to_string(ErlTerm erl_term) {
  switch (erl_term.getTagType()) {
  case LIST_T: {
    std::vector<ErlTerm> out = vec_from_erl_list(erl_term);
    return to_string(out, "[", "]");
  }
  case BOXED_T: {
    auto header_ptr = erl_term.as_ptr();
    auto header = *header_ptr;

    auto header_tag = (header >> 2) & 0b1111;

    switch (header_tag) {
    case 0b0000: {
      auto length = header >> 6;

      std::vector<ErlTerm> out;

      for (auto it = header_ptr + 1; it < header_ptr + length + 1; it++) {
        out.push_back(*it);
      }

      return to_string(out, "{", "}");
    }
    default:
      throw std::logic_error(std::format(
          "Cannot yet print the boxed type with tag {:04b}", header_tag));
    }
  }
  case SMALL_INT_T: {
    auto value = erl_term.term >> 4;
    return std::format("{}", value);
  }
  case ATOM_T: {
    return emulator_main.get_atom_string_current(erl_term);
  }
  case NIL_T:
  case PID_T:
  case PORT_T:
  case HEADER_T:
  case CATCH_T:
  default: {
    throw std::logic_error(
        std::format("Cannot yet print term with tag '{}'",
                    static_cast<int>(erl_term.getTagType())));
  }
  }
}
