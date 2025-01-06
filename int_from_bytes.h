#ifndef INT_FROM_BYTES
#define INT_FROM_BYTES

#include <cstdint>
#include <type_traits>

template<typename T> T big_endian_from_bytes(uint8_t *buffer) {

  static_assert(std::is_integral<T>::value, "T must be an integral type");

  T out = 0;
  int N = sizeof(T);

  for (int i = 0; i < N; i++) {
    out |= static_cast<T>(buffer[N-1-i]) << i * 8;
  }

  return out;
}

#endif
