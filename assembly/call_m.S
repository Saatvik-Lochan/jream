# load label we are going to
# must be a1 since it is implicitly used as an argument when going to compile
# stub
  # a1 already has the label

  la t0, 2f # load pc relative address at the end
  store_shared t0, CODE_POINTER

# check reductions - maybe yields
  load_shared t0, REDUCTIONS
  bnez t0, 1f

  store_shared a1, RESUME_LABEL  
  exit_because YIELD
1:
  addi t0, t0, -1
  store_shared t0, REDUCTIONS

  slli a2, a1, 3

  # must be s9 so we can save it here
  add s9, a2, s6
  ld t1, 0(s9)

  jr t1
2:
