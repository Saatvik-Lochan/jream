  # load index in table
  load_arg t0, 1
  add t0, t0, s10

  # CHANGE CODE POINTER HERE

# check reductions - maybe yields
  load_shared t0, REDUCTIONS
  bnez t0, 1f

  store_shared a4, RESUME_LABEL  
  exit_because YIELD
1:
  addi t0, t0, -1
  store_shared t0, REDUCTIONS

  slli a2, a4, 3
  add a2, a2, s6
  ld t1, 0(a2)

  la t0, 2f # load pc relative address at the end
  store_shared t0, CODE_POINTER

  jr t1
2:

  # RESTORE CODE POINTER HERE
