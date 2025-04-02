# check reductions - maybe yields
  load_shared t0, REDUCTIONS
  bnez t0, 1f
  exit_because YIELD
1:
  addi t0, t0, -1
  store_shared t0, REDUCTIONS
