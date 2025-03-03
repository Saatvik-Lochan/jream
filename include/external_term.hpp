#ifndef EXTERNAL_TERM
#define EXTERNAL_TERM
#include <cstdint>
#include <string>
#include <vector>

constexpr uint64_t TAGGING_MASK = 0xfffffffffffffffc;

enum TagType {
  HEADER_T,
  LIST_T,
  BOXED_T,
  PID_T,
  PORT_T,
  SMALL_INT_T,
  ATOM_T,
  CATCH_T,
  NIL_T,
};

struct ErlTerm {
  uint64_t term;
  TagType getTagType();

  ErlTerm(uint64_t term) : term(term) {}
  ErlTerm() : term(0) {}

  operator uint64_t() const { return term; }

  static std::pair<ErlTerm, uint8_t *> from_binary(uint8_t *data,
                                                   bool is_initial = true);
  template <typename T> static ErlTerm from_integer(T integer);

  std::string display();
  std::string raw_display();
  ~ErlTerm() {} // TODO make a destructor
};

ErlTerm erl_list_from_vec(const std::vector<ErlTerm> &terms, ErlTerm end);
ErlTerm get_nil_term();

#endif
