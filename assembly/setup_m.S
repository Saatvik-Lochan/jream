addi sp, sp, -56
sd ra, 48(sp)
sd s6, 40(sp)
sd s5, 32(sp)
sd s4, 24(sp)
sd s3, 16(sp)
sd s2, 8(sp)
sd s1, 0(sp)

# These should ideally all stay at the same place
mv s1, a0  # pcb pointer
mv s2, a1  # compacted arg array
# s3 used for local arg array
mv s4, a2  # all function array
load_shared s5, XREG_ARRAY
mv s6, a3  # compiled function pointers
