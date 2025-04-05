#include <glog/logging.h>

#include "exceptions.hpp"
#include "external_term.hpp"
#include "int_from_bytes.hpp"
#include <cassert>
#include <cstdint>
#include <format>
#include <iostream>
#include <stdexcept>
#include <string>
#include <type_traits>
#include <vector>

TagType ErlTerm::getTagType() {
  const uint8_t tag = term & 0b11;

  switch (tag) {
  case 0b00:
    return HEADER_T;
  case 0b01:
    return LIST_T;
  case 0b10:
    return BOXED_T;
  case 0b11: {
    const uint8_t immediate_tag = (tag >> 2) & 0b11;

    switch (immediate_tag) {
    case 0b00:
      return PID_T;
    case 0b01:
      return PORT_T;
    case 0b11:
      return SMALL_INT_T;
    case 0b10: {
      const uint8_t immediate2_tag = (tag >> 4) & 0b11;

      switch (immediate2_tag) {
      case 0b00:
        return ATOM_T;
      case 0b01:
        return CATCH_T;
      case 0b10:
        throw std::domain_error("unknown term");
      case 0b11:
        return NIL_T;
      }
    }
    }
  }
  default:
    throw std::logic_error("invalid switch case");
  }
}

BoxedType ErlTerm::getBoxedType() {
  assert(getTagType() == BOXED_T);

  ErlTerm header = *as_ptr();
  auto tag = (header >> 2) & 0b1111;

  switch (tag) {
  case 0b0000:
    return ARITYVAL_T;
  case 0b0101:
    return FUN_T;
  default:
    throw std::domain_error("Boxed type not yet supported");
  }
}

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

ErlTerm get_nil_term() { return 0b111011; }

ErlTerm erl_list_from_vec(std::vector<ErlTerm> terms, ErlTerm end) {
  ErlListBuilder builder;

  for (auto term : terms) {
    builder.add_term(term, new ErlTerm[2]);
  }

  builder.set_end(end);
  return builder.get_head();
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
ErlTerm deepcopy(ErlTerm e, ErlTerm *&to_loc) {
  const uint8_t bits = e.term & 0b11;

  switch (bits) {
  case 0b11: { // only one word to copy
    return e;
  }
  case 0b10: {
    ErlTerm *header_ptr = e.as_ptr();
    ErlTerm header = *header_ptr;
    assert((header & 0b11) == 0b00);

    uint64_t length = header >> 6;

    auto box_start = header_ptr + 1;
    auto box_end = box_start + length;

    // here we assume that to_loc has enough room
    std::copy(box_start, box_end, to_loc);

    return e;
  }
  case 0b01: {
    ErlList e_list(e);
    ErlListBuilder builder;

    auto it = e_list.begin();
    for (; it != e_list.end(); ++it) {
      builder.add_term(*it, to_loc);
      to_loc += 2;
    }

    builder.set_end(it.get_end());

    return builder.get_head();
  }
  default: {
    throw std::logic_error(
        "Deepcopy not possibly for return address/header words (tag: 0b00)");
  }
  }
}
