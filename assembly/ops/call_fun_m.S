# save ra
addi sp, sp, -8
sd ra, 0(sp)

# call
load_arg t0, 1
jalr t0

# restore ra
ld ra, 0(sp)
addi sp, sp, 8

ret
