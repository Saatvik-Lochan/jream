// This code has heavily borrowed from:
// https://eli.thegreenplace.net/2013/11/05/how-to-jit-an-introduction
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <iostream>

// memory allocated on the page boundary
void *alloc_writable_memory(size_t size) {
  void *ptr =
      mmap(0, size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  if (ptr == (void *)-1) {
    perror("mmap");
    return NULL;
  }
  return (unsigned char*) ptr;
}

// sets a RX permission on the given memory, which must be page-aligned
int make_memory_executable(void *m, size_t size) {
  if (mprotect(m, size, PROT_READ | PROT_EXEC) == -1) {
    perror("mprotect");
    return -1;
  }
  return 0;
}

void emit_add(unsigned char* m) {
  unsigned char code[] = {
    0xb3, 0x00, 
    0xb5, 0x00, 
    0x67, 0x80, 
    0x00, 0x00
  };

  memcpy(m, code, sizeof(code));
}

void emit_code_into_memory(unsigned char* m) {

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

const size_t SIZE = 1024;
typedef long int (*JittedFunc)(long int, long int);

// Allocates RW memory, emits the code into it and sets it to RX before
// executing.
void emit_to_rw_run_from_rx() {

  void *m = alloc_writable_memory(SIZE);

  std::cout << "i was here before emit add" << std::endl;

  emit_add((unsigned char*) m);
  make_memory_executable(m, SIZE);

  std::cout << "i was here before" << std::endl;

  JittedFunc func = (JittedFunc) m;
  long int b = func(1, 2);
  std::cout << b;
}

int main() {
  emit_to_rw_run_from_rx();
}
