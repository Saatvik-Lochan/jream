#ifndef EXTERNAL_TERM
#define EXTERNAL_TERM
#include <cassert>
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

  inline ErlTerm *as_ptr() {
    return reinterpret_cast<ErlTerm *>(this->term & TAGGING_MASK);
  }

  std::string display();
  std::string raw_display();
  ~ErlTerm() {} // TODO make a destructor
};

ErlTerm erl_list_from_vec(std::vector<ErlTerm> terms, ErlTerm end);
ErlTerm get_nil_term();
ErlTerm deepcopy(ErlTerm e, ErlTerm *to_loc);
std::vector<ErlTerm> vec_from_erl_list(ErlTerm e, bool include_end = false);

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

  inline bool operator==(const ErlListIterator &other) const {
    return other.curr_node_ptr == curr_node_ptr;
  }

  inline bool operator!=(const ErlListIterator &other) const {
    return other.curr_node_ptr != curr_node_ptr;
  }

  inline ErlTerm *get_current_node() {
    return curr_node_ptr;
  }

  inline ErlTerm get_end() {
    assert(curr_node_ptr == nullptr);
    return end;
  }
};

class ErlList {
private:
  ErlTerm head;

public:
  explicit ErlList(ErlTerm e) : head(e) {};

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

#endif
