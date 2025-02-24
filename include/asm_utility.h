#include <cstdint>
#include <format>
#include <ostream>
#include <vector>

#ifndef ASM_UTILITY_H
#define ASM_UTILITY_H

inline std::ostream &operator<<(std::ostream &os,
                                const std::vector<uint8_t> &v) {
  for (auto e : v) {
    os << std::format("{:02x}, ", e);
  }

  return os;
}

#endif
