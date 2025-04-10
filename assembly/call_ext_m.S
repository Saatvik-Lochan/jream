  # load index in table
  load_arg t0, 1
  slli t3, t0, 3
  add t0, t0, s10

  # CHANGE CODE POINTER HERE
  # mv new code_chunk_p into a0 here! 
call_cxx_fun SET_NEW_CODE_CHUNK

  la t0, 3f # load pc relative address at the end
  store_shared t0, CODE_POINTER

# check reductions - maybe yields
  load_shared t0, REDUCTIONS
  bnez t0, 1f

  store_shared a4, RESUME_LABEL  
  exit_because YIELD
1:
  addi t0, t0, -1
  store_shared t0, REDUCTIONS

  slli a2, a4, 3
  add s9, a2, s6
  ld t1, 0(s9)
  jr t1

2: 
  nop  # we will never reach here so we can store the code pointer here
  nop  # 2 ops because 64 = 32 * 2
3:

  la t0, 2b
  ld a0, 0(t0)

  # original code chunk pointer over here
call_cxx_fun SET_NEW_CODE_CHUNK
  # RESTORE CODE POINTER HERE
