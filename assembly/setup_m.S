addi sp, sp, -80 # remember must be 16-byte aligned
sd ra, 64(sp)
sd s8, 56(sp)
sd s7, 48(sp)
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
mv s7, a5 # pointer to teardown function
mv s8, zero # the exit code value

# go to teardown if trying to return back here
store_shared s7, CODE_POINTER

# a4 has function index
slli t0, a4, 3 
add a2, s6, t0
ld t0, 0(a2)

jr t0
