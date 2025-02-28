addi sp, sp, -48
sd ra, 40(sp)
sd s5, 32(sp)
sd s4, 24(sp)
sd s3, 16(sp)
sd s2, 8(sp)
sd s1, 0(sp)

# These should ideally all stay at the same place
mv s1, a0  # pcb pointer
mv s2, a1
mv s4, a2
mv s5, a3  # x reg array 
