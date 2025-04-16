#ifndef MESSAGES
#define MESSAGES

#include "external_term.hpp"
#include <cstdint>

struct Message {
  uint64_t values[2];

  inline explicit Message(ErlTerm e) {
    values[0] = e;
    values[1] = reinterpret_cast<uint64_t>(nullptr);
  };

  inline ErlTerm get_payload() { return ErlTerm(values[0]); }
  inline Message *get_next() { return reinterpret_cast<Message *>(values[1]); }
  inline ErlTerm *get_payload_address() {
    return reinterpret_cast<ErlTerm *>(values);
  }
  inline Message **get_next_address() {
    return reinterpret_cast<Message **>(values + 1);
  }
};

// we assume that the pointer to the Message struct
// is the same as the pointer to the first field (values)
static_assert(std::is_standard_layout_v<Message>,
              "Message is not standard layout. Required.");

#endif
