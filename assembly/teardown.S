# THIS IS A FILE GENERATED BY meta_assembly_compile.py APPLIED ON teardown_m.S
# ASSUME s1 holds a pointer to the pcb
# ASSUME s2 holds a pointer to argument pointer array for all instr
# ASSUME s3 holds a pointer to the argument array for this instr
# ASSUME s4 holds a pointer to the array of all callable cxx funs
# ASSUME s5 holds a pointer to the X register array 

# let s8 be the value register
mv a0, s8

ld s1, 0(sp)
ld s2, 8(sp)
ld s3, 16(sp)
ld s4, 24(sp)
ld s5, 32(sp)
ld s6, 40(sp)
ld s7, 48(sp)
ld s8, 56(sp)
ld ra, 64(sp)
addi sp, sp, 72

ret
