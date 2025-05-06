#ifndef MESSAGES
#define MESSAGES

#include "external_term.hpp"
#include <cstddef>

struct Message {
  ErlTerm payload;
  Message *next;

  inline explicit Message(ErlTerm e): payload(e), next(nullptr) {}

  inline ErlTerm get_payload() { return payload; }
  inline Message *get_next() { return next; }
  inline ErlTerm *get_payload_address() {
    return &payload;
  }
  inline Message **get_next_address() {
    return &next;
  }
};

// we assume that the pointer to the Message struct
// is the same as the pointer to the first field (values)
static_assert(offsetof(Message, payload) == 0,
              "Message payload must be at offset 0");
static_assert(offsetof(Message, next) == 8,
              "Message next ptr must be at offset 8");
#endif
