#ifndef EXTERNAL_TERM
#define EXTERNAL_TERM
#include <algorithm>
#include <cassert>
#include <cstdint>
#include <format>
#include <stdexcept>
#include <string>
#include <type_traits>
#include <vector>

constexpr uint64_t TAGGING_MASK = 0xfffffffffffffffc;

enum TagType {
  HEADER_T,
  BOXED_T,
  SMALL_INT_T,
  ATOM_T,
  PID_T,
  PORT_T,
  NIL_T,
  LIST_T,
  CATCH_T,
};

enum ErlMajorType {
  INT_ET = 0,
  ATOM_ET = 1,
  REF_ET = 2,
  FUN_ET = 3,
  PID_ET = 4,
  PORT_ET = 5,
  TUPLE_ET = 6,
  MAP_ET = 7,
  LIST_ET = 8,
  BINARY_ET = 9,
};

enum BoxedType { ARITYVAL_T, FUN_T };

struct ErlTerm {
  uint64_t term;
  inline TagType getTagType() const;
  inline ErlMajorType getErlMajorType() const;

  ErlTerm(uint64_t term) : term(term) {}
  ErlTerm() : term(0) {}

  operator uint64_t() const { return term; }

  static std::pair<ErlTerm, uint8_t *> from_binary(uint8_t *data,
                                                   bool is_initial = true);
  template <typename T> static ErlTerm from_integer(T integer);

  inline ErlTerm *as_ptr() const {
    return reinterpret_cast<ErlTerm *>(this->term & TAGGING_MASK);
  }

  inline ErlTerm *list_ptr_or_null() const {
    if (getTagType() != LIST_T) {
      return nullptr;
    }

    return as_ptr();
  }

  inline auto operator<=>(const ErlTerm &other) const;

  std::string display();
  std::string raw_display();
  ~ErlTerm() {} // TODO make a destructor
};

static_assert(std::is_standard_layout_v<ErlTerm>);
static_assert(sizeof(ErlTerm) == 8);

ErlTerm erl_list_from_vec(std::vector<ErlTerm> terms, ErlTerm end);
inline constexpr uint64_t get_nil_term() { return 0b111011; }
ErlTerm deepcopy(ErlTerm e, ErlTerm *&to_loc);
std::vector<ErlTerm> vec_from_erl_list(ErlTerm e, bool include_end = false);

ErlTerm make_boxed(ErlTerm *);
ErlTerm make_small_int(uint64_t num);

class ErlListIterator {
private:
  ErlTerm *curr_node_ptr;
  ErlTerm end;

public:
  explicit ErlListIterator(ErlTerm p) : curr_node_ptr(p.list_ptr_or_null()) {};
  explicit ErlListIterator(ErlTerm *ptr) : curr_node_ptr(ptr) {};

  ErlTerm operator*() const { return *(curr_node_ptr); }
  ErlListIterator &operator++() {
    // ++ must not be called after it goes out of bounds
    end = *(curr_node_ptr + 1);
    curr_node_ptr = end.list_ptr_or_null();

    return *this;
  }

  inline bool operator==(const ErlListIterator &other) const {
    return other.curr_node_ptr == curr_node_ptr;
  }

  inline bool operator!=(const ErlListIterator &other) const {
    return other.curr_node_ptr != curr_node_ptr;
  }

  inline ErlTerm *get_current_node() { return curr_node_ptr; }

  inline ErlTerm get_end() {
    assert(curr_node_ptr == nullptr);
    return end;
  }
};

class ErlList {
private:
  ErlTerm head;

public:
  explicit ErlList(ErlTerm e) : head(e) {
    assert(e.getErlMajorType() == LIST_ET);
  };

  inline ErlListIterator begin() { return ErlListIterator(head); }
  inline ErlListIterator end() { return ErlListIterator(nullptr); }
};

class ErlListBuilder {
private:
  ErlTerm head;
  ErlTerm *tail = &head;

public:
  // loc should have space for two ErlTerms
  inline void add_term(ErlTerm e, ErlTerm *loc) {
    tail->term = (reinterpret_cast<uint64_t>(loc) & TAGGING_MASK) | 0b01;
    loc[0] = e;

    tail = loc + 1;
  }

  inline void set_end(ErlTerm e) { tail->term = e; }

  inline ErlTerm get_head() { return head; }
};

TagType ErlTerm::getTagType() const {
  const uint8_t tag = term & 0b11;

  switch (tag) {
  case 0b00:
    return HEADER_T;
  case 0b01:
    return LIST_T;
  case 0b10:
    return BOXED_T;
  case 0b11: {
    const uint8_t immediate_tag = (term >> 2) & 0b11;

    switch (immediate_tag) {
    case 0b00:
      return PID_T;
    case 0b01:
      return PORT_T;
    case 0b11:
      return SMALL_INT_T;
    case 0b10: {
      const uint8_t immediate2_tag = (term >> 4) & 0b11;

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

inline ErlMajorType ErlTerm::getErlMajorType() const {
  auto tag_type = getTagType();

  switch (tag_type) {
  case BOXED_T: {
    auto header = *as_ptr();
    assert(header.getTagType() == HEADER_T);

    switch (header.term >> 2 & 0b1111) {
    case 0b0000:
      return TUPLE_ET;
    case 0b0100:
      return REF_ET;
    case 0b0101:
      return FUN_ET;
    default:
      throw std::logic_error(std::format(
          "Boxed type with header '{:04b}' not yet implemented", header.term));
    }
  }
  case SMALL_INT_T:
    return INT_ET;
  case ATOM_T:
    return ATOM_ET;
  case PID_T:
    return PID_ET;
  case PORT_T:
    return PORT_ET;
  case NIL_T:
  case LIST_T:
    return LIST_ET;
  case CATCH_T:
  case HEADER_T:
  default: {
    throw std::logic_error("Not a valid type");
  }
  }
}

inline auto ErlTerm::operator<=>(const ErlTerm &other) const {
  auto this_type = getErlMajorType();
  auto comp = this_type <=> other.getErlMajorType();

  if (comp != 0) {
    return comp;
  }

  // they are of the same type!
  switch (this_type) {
  case PORT_ET:
  case PID_ET:
  case INT_ET: {
    // assume small int
    return (term >> 4 <=> other >> 4);
  }
  case ATOM_ET: {
    // TODO make alphabetic (requires AtomChunk)
    return (term >> 6 <=> other >> 6);
  }
  case TUPLE_ET: {
    const auto this_ptr = as_ptr();
    const auto other_ptr = other.as_ptr();

    const auto this_arity = *this_ptr >> 6;
    const auto other_arity = *other_ptr >> 6;

    for (size_t i = 0; i < std::min(this_arity, other_arity); i++) {
      ErlTerm this_term = this_ptr[i + 1];
      ErlTerm other_term = other_ptr[i + 1];

      auto comp = this_term <=> other_term;

      if (comp != 0) {
        return comp;
      }
    }

    return this_arity <=> other_arity;
  }
  case LIST_ET: {
    auto this_list = ErlList(term);
    auto other_list = ErlList(term);

    auto this_iter = this_list.begin();
    auto other_iter = other_list.begin();

    while (this_iter != this_list.end() && other_iter != other_list.end()) {
      ErlTerm this_term = *this_iter;
      ErlTerm other_term = *other_iter;

      auto comp = this_term <=> other_term;

      if (comp != 0) {
        return comp;
      }

      ++this_iter;
      ++other_iter;
    }

    auto this_not_ended = this_iter != this_list.end();
    auto other_not_ended = other_iter != other_list.end();

    // this ended and the other has not: false <=> true gives the right value
    return this_not_ended <=> other_not_ended;
  }
  case MAP_ET:
  case FUN_ET:
  case BINARY_ET:
  case REF_ET: 
  default: {
    throw std::domain_error(
        std::format("Not implemented comparison for ErlMajorType {}",
                    static_cast<uint16_t>(this_type)));
  }
  }
}

#endif
