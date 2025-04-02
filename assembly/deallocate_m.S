# Deallocate
  load_shared t1, STOP
  load_arg t0, 0

# pop and restore code pointer
  ld t2, 0(t1)
  store_shared t2, CODE_POINTER
  addi t1, t1, 8

# width of ErlTerm is 8 bytes
  slli t0, t0, 3
  add t1, t1, t0

  store_shared t1, STOP
