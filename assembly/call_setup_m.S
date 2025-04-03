# DEPRECATED
# check reductions - maybe yields
  load_shared t0, REDUCTIONS
  bnez t0, 1f

  # a4 has label where we were going
  store_shared a4, RESUME_LABEL  
  exit_because YIELD
1:
  addi t0, t0, -1
  store_shared t0, REDUCTIONS
