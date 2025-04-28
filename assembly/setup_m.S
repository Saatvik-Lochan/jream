addi sp, sp, -96 # remember must be 16-byte aligned
sd s10, 80(sp)
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

# typedef ErlReturnCode (*goto_asm_label_p)(CodeChunk *code_chunk (a0),
#                                           ProcessControlBlock *pcb (a1),
#                                           std::uintptr_t func_array[] (a2),
#                                           const uint8_t *teardown_code (a3),
#                                           uint64_t label_num (a4));
# 

mv s1, a1 # pcb pointer (necessary for load_shared/store_shared)
# s2 caches the register X0 (see below)
# s3 used for local arg array (see add_setup_args in translate_code_section)
mv s4, a2 # all function array (necessary for call_cxx_fun)
load_shared s5, XREG_ARRAY 
ld s2, 0(s5) 
# s6 is the label loc pointer (assigned by setuping up code_chunk_p)
mv s7, a3 # pointer to teardown code
mv s8, zero # the exit code value
# s9 is a multipurpose register we pre-save
# s10 is the external jump pointer (assigned by setuping up code_chunk_p)

mv s9, a4 # temp to save it

# a0, s1 are implicit arguments
call_cxx_fun SET_NEW_CODE_CHUNK

# move back as argument to compile stub
mv a1, s9

# a4 has label index
slli t0, s9, 3 
add s9, s6, t0  
ld t0, 0(s9)

jr t0
