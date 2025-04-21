#include "execution.hpp"
#include "pcb.hpp"
#include <cstdint>
#include <format>
#include <ostream>
#include <vector>

#ifndef ASM_UTILITY_H
#define ASM_UTILITY_H

inline std::ostream &operator<<(std::ostream &os,
                                const std::vector<uint8_t> &v) {

  os << "printing vector:\n";
  for (auto e : v) {
    os << std::format("{:02x}, ", static_cast<unsigned int>(e));
  }

  return os;
}

inline ProcessControlBlock *get_pcb() {
  return emulator_main.scheduler.get_current_process();
}

#endif
