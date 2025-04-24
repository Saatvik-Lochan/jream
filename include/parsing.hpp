#ifndef PARSING_H
#define PARSING_H

#include "external_term.hpp"
#include "profiler.hpp"
#include <concepts>
#include <cstdint>

struct ArgumentAllocator {
  ErlTerm *allocate_heap_frag(size_t a) { return new ErlTerm[a]; }
};

template <typename T>
concept ErlAllocator = requires(T a, uint64_t size) {
  { a.allocate_heap_frag(size) } -> std::same_as<ErlTerm *>;
};

template <ErlAllocator T>
std::vector<ErlTerm> collect_till(const std::string &term, char end,
                                  size_t &from, T *pcb) {
  PROFILE();
  std::vector<ErlTerm> terms;

  while (term[from] == ' ') {
    from++;
  }

  if (term[from] == end) {
    return terms;
  }

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

inline bool is_num(char c) { return 48 <= c && c < 58; }

template <ErlAllocator T>
ErlTerm parse_term(const std::string &term, size_t &from, T *pcb);

template <ErlAllocator T>
ErlTerm parse_int(const std::string &term, size_t &from, T *pcb) {

  auto num = 0;

  char curr;

  for (; is_num(curr = term[from]) || term[from] == '_'; from++) {
    if (term[from] == '_') {
      continue;
    }

    num *= 10;
    num += curr - 48;
  }

  return make_small_int(num);
}

template <ErlAllocator T>
ErlTerm terms_to_list(const std::vector<ErlTerm> &terms, T *pcb) {
  ErlListBuilder builder;

  auto curr = pcb->allocate_heap_frag(terms.size() * 2);

  for (auto term : terms) {
    builder.add_term(term, curr);
    curr += 2;
  }

  builder.set_end(get_nil_term());
  return builder.get_list();
}

inline void check_start(std::string term_name, char c, char needed) {
  if (c != needed) {
    throw std::logic_error(
        std::format("parse {} but incorrect opening {}", term_name, c));
  }
}

template <ErlAllocator T>
ErlTerm parse_erl_list(const std::string &term, size_t &from, T *pcb) {

  check_start("list", term[from], '[');
  from++;

  auto terms = collect_till(term, ']', from, pcb);

  return terms_to_list(terms, pcb);
}

template <ErlAllocator T>
ErlTerm parse_tuple(const std::string &term, size_t &from, T *pcb) {

  check_start("tuple", term[from], '{');
  from++;

  auto terms = collect_till(term, '}', from, pcb);

  auto alloced = pcb->allocate_heap_frag(terms.size() + 1);
  alloced[0] = terms.size() << 2;

  for (size_t i = 0; i < terms.size(); i++) {
    alloced[i + 1] = terms[i];
  }

  return make_boxed(alloced);
}

template <ErlAllocator T>
ErlTerm parse_term(const std::string &term, size_t &from, T *pcb) {

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

template <ErlAllocator T>
ErlTerm parse_terms_into_list(const std::string &term, T *pcb) {
  PROFILE();
  size_t from = 0;
  auto terms = collect_till(term, '.', from, pcb);

  return terms_to_list(terms, pcb);
}

template <ErlAllocator T>
std::pair<GlobalFunctionId, std::vector<ErlTerm>>
parse_func_call(const std::string &term, T allocator) {
  auto colon_pos = term.find(':');
  auto paren_pos = term.find('(');

  if (colon_pos == std::string::npos || paren_pos == std::string::npos) {
    throw std::invalid_argument("Invalid format for func call");
  }

  auto module = term.substr(0, colon_pos);
  auto function = term.substr(colon_pos + 1, paren_pos - colon_pos - 1);

  // parse arguments
  size_t from = paren_pos + 1;
  auto terms = collect_till<T>(term, ')', from, &allocator);

  return std::make_pair(
      GlobalFunctionId{.module = module,
                       .function_name = function,
                       .arity = static_cast<uint32_t>(terms.size())},
      terms);
}

#endif
