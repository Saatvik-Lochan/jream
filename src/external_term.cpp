#include <glog/logging.h>

#include "exceptions.hpp"
#include "external_term.hpp"
#include "int_from_bytes.hpp"
#include <cassert>
#include <cstdint>
#include <format>
#include <optional>
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

class ErlListIterator {
private:
  ErlTerm *curr_node_ptr;
  ErlTerm end;

public:
  explicit ErlListIterator(ErlTerm p) : curr_node_ptr(p.as_ptr()) {};
  explicit ErlListIterator(ErlTerm *ptr) : curr_node_ptr(ptr) {};

  ErlTerm operator*() const { return *(curr_node_ptr); }
  ErlListIterator &operator++() {
    // ++ must not be called after it goes out of bounds
    end = *(curr_node_ptr + 1);
    bool next_node_is_cons = (end & 0b11) == 0b10;
    curr_node_ptr = next_node_is_cons ? end.as_ptr() : nullptr;

    return *this;
  }

  bool operator!=(const ErlListIterator &other) const {
    return other.curr_node_ptr != curr_node_ptr;
  }

  ErlTerm get_end() {
    assert(curr_node_ptr == nullptr);
    return end;
  }
};

class ErlList {
private:
  ErlTerm head;

public:
  explicit ErlList(ErlTerm e) : head(e) {};

  ErlListIterator begin() { return ErlListIterator(head); }
  ErlListIterator end() { return ErlListIterator(nullptr); }
};

ErlTerm get_nil_term() { return 0b111011; }

ErlTerm erl_list_from_vec(std::vector<ErlTerm> terms, ErlTerm end) {
  ErlTerm head;
  ErlTerm *curr = &head;

  for (auto term : terms) {
    ErlTerm *const new_node = new ErlTerm[2]();

    curr->term = (reinterpret_cast<uint64_t>(new_node) & TAGGING_MASK) | 0b01;
    new_node[0] = term;

    curr = new_node + 1;
  }

  curr->term = end;
  return head;
}

class ErlListBuilder {
private:
  ErlTerm head;
  ErlTerm *tail = &head;

public:
  // loc should have space for two ErlTerms
  void add_term(ErlTerm e, ErlTerm *loc) {
    tail->term = (reinterpret_cast<uint64_t>(loc) & TAGGING_MASK) | 0b01;
    loc[0] = e;

    tail = loc + 1;
  }

  void set_end(ErlTerm e) { tail->term = e; }

  ErlTerm get_head() { return head; }
};

// Assume that all required space has already been allocated contiguously.
// Copies anything necessary to to_loc and returns the the word representing
// the whole type
//
// TODO tests!
ErlTerm deepcopy(ErlTerm e, ErlTerm *to_loc) {
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

    size_t counter = 0;

    auto it = e_list.begin();
    for (; it != e_list.end(); ++it) {
      builder.add_term(*it, to_loc + counter);
      counter += 2; // space for two more terms
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
