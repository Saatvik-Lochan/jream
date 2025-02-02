.section .text
.globl _start
_start:
  # save ra
  addi sp, sp, -8
  sd ra, 0(sp)

  # call
  jalr a0

  # restore ra
  ld ra, 0(sp)
  addi sp, sp, 8

  ret
