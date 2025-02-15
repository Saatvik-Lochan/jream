#ifndef EXTERNAL_TERM
#define EXTERNAL_TERM
#include <cstddef>
#include <cstdint>
#include <string>
#include <vector>

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

class ErlTerm {
  size_t term;
  TagType getTagType();

public:
  ErlTerm(size_t term) : term(term) {}
  ErlTerm() : term(0) {}
  static std::pair<ErlTerm, uint8_t *> from_binary(uint8_t *data,
                                                   bool is_initial = true);
  template <typename T> static ErlTerm from_integer(T integer);

  std::string display();
  std::string raw_display();
  ~ErlTerm() {} // TODO make a destructor

  friend ErlTerm erl_list_from_vec(const std::vector<ErlTerm> &terms,
                                   ErlTerm end);
  friend ErlTerm get_nil_term();
};

#endif
