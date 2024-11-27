.section .text
.globl _start
_start:
    mv a2, a1  # lengh of message
    mv a1, a0  # address of message 
    li a0, 1   # File descriptor (stdout is 1)
    li a7, 64  # Load syscall number 64 (write) into a7
    ecall      # Make the syscall
    ret

