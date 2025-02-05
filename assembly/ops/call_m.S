# save ra
addi sp, sp, -8
sd ra, 0(sp)

# call
load_arg t0, 1
load_shared t0, ARENA_SIZE
call_cxx_fun PRINT_INT

# restore ra
ld ra, 0(sp)
addi sp, sp, 8

ret
