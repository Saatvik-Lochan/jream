#include <concepts>
#include <glog/logging.h>

#include "exceptions.hpp"
#include "execution.hpp"
#include "external_term.hpp"
#include "int_from_bytes.hpp"
#include "pcb.hpp"
#include <cassert>
#include <cstdint>
#include <format>
#include <stack>
#include <stdexcept>
#include <string>
#include <type_traits>
#include <unordered_map>
#include <unordered_set>
#include <variant>
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

    ErlTerm list = erl_list_from_range(terms, get_nil_term());
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
    ErlTerm list = erl_list_from_range(terms, end);

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

template <typename T, typename U, typename V>
void traverse_term(T e, U &&do_each_block, V &&combine)
  requires requires(T t, U u, V v, ErlTerm next, bool b) {
    { v(u(t, b), next) } -> std::same_as<T>;
    { t.next } -> std::same_as<ErlTerm &>;
  }
{
  auto is_pointer = [](ErlTerm e) {
    auto tag = e.term & 0b11;
    return (tag == 0b10 || tag == 0b01);
  };

  if (!is_pointer(e.next)) {
    return;
  }

  std::stack<T> to_check;
  to_check.push(e); // root

  std::unordered_set<uint64_t> visited;

  while (!to_check.empty()) {
    T to_consider = to_check.top();
    to_check.pop();

    ErlTerm next = to_consider.next;

    auto in_visited = visited.contains(next);
    auto other = do_each_block(to_consider, in_visited);

    if (in_visited) {
      continue;
    }

    visited.insert(next);

    // assume it is a pointer
    const uint8_t bits = next & 0b11;

    switch (bits) {
    case 0b10: {
      ErlTerm *tuple_start = next.as_ptr();
      ErlTerm header = *tuple_start;
      assert((header & 0b11) == 0b00);

      uint64_t length = header >> 6;

      std::span<ErlTerm> tuple_span(tuple_start + 1, length);

      for (auto val : tuple_span) {
        if (is_pointer(val)) {
          to_check.push(combine(other, val));
        }
      }
      break;
    }
    case 0b01: {
      for (auto val : std::span<ErlTerm>{next.as_ptr(), 2}) {
        if (is_pointer(val)) {
          to_check.push(combine(other, val));
        }
      }
      break;
    }
    case 0b11: { // only one word to copy
      throw std::logic_error(
          "Logic error. Should never reach here whent traversing!");
    }
    default: {
      throw std::logic_error("Heap size not a valid operation for return "
                             "address/header words (tag: 0b00)");
    }
    }
  }
}

size_t get_heap_size(const ErlTerm e) {
  auto count = 0;

  struct out {
    ErlTerm next;
  };

  traverse_term<out>(
      {e},
      [&count](out val, bool in_visited) {
        if (!in_visited) {
          auto next = val.next;
          auto tag = next & 0b11;

          switch (tag) {
          case 0b01: {
            count += 2;
            break;
          }
          case 0b10: {
            auto size = *next.as_ptr() >> 6;
            count += (size + 1);
            break;
          }
          }
        }
        return std::monostate();
      },
      [](const std::monostate, ErlTerm next) { return out{.next = next}; });

  return count;
}

// Assume that all required space has already been allocated contiguously.
// Copies anything necessary to to_loc and returns the the word representing
// the whole type
ErlTerm deepcopy(ErlTerm e, ErlTerm *const to_loc) {
  std::unordered_map<uint64_t, ErlTerm> alr_copied;

  auto current = to_loc;

  struct with_copy_parent {
    ErlTerm next;
    ErlTerm *copy_parent;
  };

  ErlTerm out_handle = e;

  traverse_term<with_copy_parent>(
      {.next = e, .copy_parent = &out_handle},
      [&current, &alr_copied](with_copy_parent val, bool in_visited) {
        if (in_visited) {
          auto copy_parent = val.copy_parent;
          *copy_parent = alr_copied[val.next];
        } else {
          auto next = val.next;
          auto copy_parent = val.copy_parent;
          // first time encountering so copy
          auto tag = next & 0b11;
          auto heap_vals_ptr = next.as_ptr();

          switch (tag) {
          case 0b01: {
            // copy cons cell
            current[0] = heap_vals_ptr[0];
            current[1] = heap_vals_ptr[1];

            auto handle = make_cons(current);
            alr_copied[next] = handle;
            *copy_parent = handle;

            auto parent_val = current;
            current += 2;

            return parent_val;
          }
          case 0b10: {
            // copy tuple
            auto size = heap_vals_ptr[0] >> 6;
            auto new_end = heap_vals_ptr + size + 1;
            std::copy(heap_vals_ptr, new_end, current);

            auto handle = make_boxed(current);
            alr_copied[next] = handle;
            *copy_parent = handle;

            auto parent_val = current;
            current = new_end;

            return parent_val;
          }
          }
        }

        throw std::logic_error("Not a pointer type yet reached in traversal");
      },
      [](ErlTerm *copy_parent, ErlTerm next) {
        return with_copy_parent{.next = next, .copy_parent = copy_parent};
      });

  return out_handle;
}

ErlTerm make_boxed(ErlTerm *ptr) {
  return (reinterpret_cast<uint64_t>(ptr) & TAGGING_MASK) + 0b10;
}

ErlTerm make_cons(ErlTerm *ptr) {
  return (reinterpret_cast<uint64_t>(ptr) & TAGGING_MASK) + 0b01;
}

ErlTerm tag(ErlTerm *ptr, std::bitset<2> tag_bits) {
  return (reinterpret_cast<uint64_t>(ptr) & TAGGING_MASK) |
         tag_bits.to_ullong();
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

std::vector<ErlTerm> collect_till(const std::string &term, char end,
                                  size_t &from, ProcessControlBlock *pcb) {
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

  auto curr = pcb->allocate_and_gc(terms.size() * 2, 0);

  for (auto term : terms) {
    builder.add_term(term, curr);
    curr += 2;
  }

  builder.set_end(get_nil_term());
  return builder.get_list();
}

void check_start(std::string term_name, char c, char needed) {
  if (c != needed) {
    throw std::logic_error(
        std::format("parse {} but incorrect opening {}", term_name, c));
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
