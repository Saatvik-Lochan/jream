# deallocate
  load_arg t0, 1
  addi t0, t0, 1
  slli t1, t0, 3

  load_shared t2, STOP
  ld t3, 0(t2)
  store_shared t3, CODE_POINTER

  add t0, t1, t2
  store_shared t0, STOP

# load label we are going to
# must be a1 since it is implicitly used as an argument when going to compile
# stub
  load_arg a1, 0

# check reductions - maybe yields
  load_shared t0, REDUCTIONS
  bnez t0, 1f

  store_shared a1, RESUME_LABEL  
  exit_because YIELD
1:
  addi t0, t0, -1
  store_shared t0, REDUCTIONS

  slli a2, a1, 3

  # must be s9, implicit argument to compile stub
  add s9, a2, s6
  ld t1, 0(s9)

  # don't save code pointer

  jr t1
2:
