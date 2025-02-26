# Deallocate
  load_shared t1, STOP
  load_arg t0, 0

  # width of ErlTerm is 8 bytes
  slli t0, t0, 3
  sub t1, t1, t0

  store_shared t1, STOP
