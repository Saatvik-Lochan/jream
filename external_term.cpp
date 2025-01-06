#include "exceptions.h"
#include "int_from_bytes.h"
#include <cstddef>
#include <cstdint>
#include <stdexcept>
#include <string>
#include <type_traits>
#include <vector>

enum TagType {
  HEADER,
  LIST,
  BOXED,
  PID,
  PORT,
  SMALL_INT,
  ATOM,
  CATCH,
  NIL,
};

class ErlTerm {
  size_t term;
  ErlTerm(size_t term) : term(std::move(term)) {}
  ErlTerm(): term(0) {}

  TagType getTagType();

public:
  std::string display();
  std::pair<ErlTerm, uint8_t *> from_binary(uint8_t *data);
  template <typename T> ErlTerm from_integer(T integer);
  ~ErlTerm() {}
};

TagType ErlTerm::getTagType() {
  const uint8_t tag = term & 0b11;

  switch (tag) {
  case 0b00:
    return HEADER;
  case 0b01:
    return LIST;
  case 0b10:
    return BOXED;
  case 0b11: {
    const uint8_t immediate_tag = (tag >> 2) & 0b11;

    switch (immediate_tag) {
    case 0b00:
      return PID;
    case 0b01:
      return PORT;
    case 0b11:
      return SMALL_INT;
    case 0b10: {
      const uint8_t immediate2_tag = (tag >> 4) & 0b11;

      switch (immediate2_tag) {
      case 0b00:
        return ATOM;
      case 0b01:
        return CATCH;
      case 0b10:
        throw std::domain_error("unknown term");
      case 0b11:
        return NIL;
      }
    }
    }
  }
  default:
    throw std::logic_error("invalid switch case");
  }
}

std::string ErlTerm::display() {
  switch (getTagType()) {
  case HEADER:
    return "Header";
  case LIST:
    return "List";
  case BOXED:
    return "Boxed";
  case PID:
    return "PID";
  case PORT:
    return "Port";
  case SMALL_INT:
    return "Small Integer " + std::to_string(term >> 4);
  case ATOM:
    return "Atom " + std::to_string(term >> 6);
  case CATCH:
    return "Catch";
  case NIL:
    return "Nil";
  }
}

ErlTerm from_integer(std::vector<uint8_t> big_integer) {
  throw NotImplementedException("Really big integers not supported now");
}

// TODO test this!
template <typename T> ErlTerm ErlTerm::from_integer(T integer) {
  static_assert(std::is_integral_v<T>, "T must be an integral value");
  static_assert(std::is_signed_v<T>, "T must be a signed value");

  const ssize_t converted = integer;

  // only C++20 onwards since << was undefined on negative signed integrals
  if (converted << 4 >> 4 != integer) {
    throw NotImplementedException("big integers not supported yet");
  }

  size_t term = (converted << 4) + 0b1111;
  return ErlTerm(std::move(term));
}

std::pair<ErlTerm, uint8_t*> ErlTerm::from_binary(uint8_t *data) {
  uint8_t type_byte = data[0];

  switch (type_byte) {
  case 97: // small_integer_ext
    return {ErlTerm((data[1] << 4) & 0b1111), data + 2};
  case 98: { // integer_ext
    auto integer = big_endian_from_bytes<int32_t>(data + 1);
    return {from_integer(integer), data + 5};
  }
  case 109: { // list_ext
    auto list_len = big_endian_from_bytes<uint32_t>(data + 1);

    data += 5; // place after list length
    ErlTerm head;
    ErlTerm *curr = &head;

    for (uint32_t i = 0; i < list_len; i++) {

      ErlTerm *const temp = new ErlTerm[2]();

      *curr = reinterpret_cast<size_t>(temp) & 0b00 + 0b01;
      std::tie(temp[0], data) = from_binary(data);

      curr = temp + 1;
    }

    *curr = 0b111011;
  }
  default: {
    const std::string err_msg =
        "Not implemented binary_to_term conversion for type byte " +
        std::to_string(type_byte);

    throw NotImplementedException(err_msg.c_str());
  }
  }
}
