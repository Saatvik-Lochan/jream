# THIS IS A FILE GENERATED BY meta_assembly_compile.py APPLIED ON /home/saatvikl/Documents/studies/part-ii/project/main/shared/jream/assembly/ops/call_m.S
# ASSUME s0 holds a pointer to the pcb
# ASSUME s1 holds a pointer to argument pointer array
# ASSUME s2 holds a pointer to the argument array for this instr

# save ra
addi sp, sp, -8
sd ra, 0(sp)

# call
ld t0, 8(s3) # generated from 'load_arg t0, 1'
ld t0, 32(s1) # generated from 'load_shared t0, ARENA_SIZE'
jalr ra, s4, 0 # generated from 'call_cxx_fun PRINT_INT'

# restore ra
ld ra, 0(sp)
addi sp, sp, 8

ret
