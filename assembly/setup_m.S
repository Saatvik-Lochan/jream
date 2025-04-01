addi sp, sp, -80 # remember must be 16-byte aligned
sd s9, 72(sp)
sd s8, 64(sp)
sd s7, 56(sp)
sd s6, 48(sp)
sd s5, 40(sp)
sd s4, 32(sp)
sd s3, 24(sp)
sd s2, 16(sp)
sd s1, 8(sp)
sd ra, 0(sp)

# These should ideally all stay at the same place
mv s1, a0  # pcb pointer
mv s2, a1  # compacted arg array
# s3 used for local arg array
mv s4, a2  # all function array
load_shared s5, XREG_ARRAY
mv s6, a3  # compiled function pointers
mv s7, a5 # pointer to teardown function
mv s8, zero # the exit code value
# s9 is a multi purpose saved register, so we can make ext calls

# go to teardown if trying to return back here
store_shared s7, CODE_POINTER

# a4 has function index
slli t0, a4, 3 
add s9, s6, t0
ld t0, 0(s9)

jr t0
