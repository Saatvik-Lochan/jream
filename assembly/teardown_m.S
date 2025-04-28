# let s8 be the value register
mv a0, s8

# restore x0 to array
sd s2, 0(s5) 

ld ra, 0(sp)
ld s1, 8(sp)
ld s2, 16(sp)
ld s3, 24(sp)
ld s4, 32(sp)
ld s5, 40(sp)
ld s6, 48(sp)
ld s7, 56(sp)
ld s8, 64(sp)
ld s9, 72(sp)
ld s10, 80(sp)
addi sp, sp, 96

ret
