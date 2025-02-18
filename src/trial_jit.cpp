// This code has heavily borrowed from:
// https://eli.thegreenplace.net/2013/11/05/how-to-jit-an-introduction
#include <alloca.h>
#include <cstdint>
#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/syscall.h>

// memory allocated on the page boundary
void *alloc_writable_memory(size_t size) {
  void *ptr = 
    mmap(0, size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  if (ptr == (void *)-1) {
    perror("mmap");
    return NULL;
  }
  return (unsigned char *)ptr;
}

// sets a RX permission on the given memory, which must be page-aligned
int make_memory_executable(void *m, size_t size) {
  if (mprotect(m, size, PROT_READ | PROT_EXEC) == -1) {
    perror("mprotect");
    return -1;
  }
  return 0;
}

void emit_print(unsigned char *m) {

  // riscv-64 code for the write syscall
  unsigned char code[] = {
      0x13, 0x86, 0x05, 0x00, // mv a2, a1
      0x93, 0x05, 0x05, 0x00, // mv a1, a0
      0x13, 0x05, 0x10, 0x00, // li a0, 1
      0x93, 0x08, 0x00, 0x04, // li a7, 63
      0x73, 0x00, 0x00, 0x00, // ecall
      0x67, 0x80, 0x00, 0x00  // ret
  };

  memcpy(m, code, sizeof(code));
}

extern "C" void do_stuff() {
  for (int i = 0; i < 10; i++) {
    std::cout << i << "\n";
  }
}

void emit_call(uint8_t *m) {
  uint8_t code[] = {0x13, 0x01, 0x81, 0xff, 0x23, 0x30, 0x11, 0x00,
                    0xe7, 0x00, 0x05, 0x00, 0x83, 0x30, 0x01, 0x00,
                    0x13, 0x01, 0x81, 0x00, 0x67, 0x80, 0x00, 0x00};

  memcpy(m, code, sizeof(code));
}

void emit_test(uint8_t *m) {
  uint8_t code_64_ld[] = {0x13, 0x01, 0x81, 0xff, 0x23, 0x30, 0x11,
                          0x00, 0x83, 0x30, 0x01, 0x00, 0x13, 0x01,
                          0x81, 0x00, 0x67, 0x80, 0x00, 0x00};

  memcpy(m, code_64_ld, sizeof(code_64_ld));
}

size_t SIZE = 1024;

// Allocates RW memory, emits the code into it and sets it to RX before
// executing.
void alloc_and_run_call() {

  void *m = alloc_writable_memory(SIZE);

  emit_call((uint8_t *)m);
  make_memory_executable(m, SIZE);

  std::cout << "code written to memory" << std::endl;

  using FuncPtr = void (*)(void (*)());
  FuncPtr jitted_func = reinterpret_cast<FuncPtr>(m);
  jitted_func(&do_stuff);
}

void alloc_and_run_print() {
  void *m = alloc_writable_memory(SIZE);

  emit_print((uint8_t *)m);
  make_memory_executable(m, SIZE);

  using FuncPtr = void (*)(char *, size_t);
  FuncPtr jitted_func = reinterpret_cast<FuncPtr>(m);

  char sentence[] = "hello world\n";

  jitted_func(sentence, sizeof(sentence));
}

int main() { alloc_and_run_call(); }
