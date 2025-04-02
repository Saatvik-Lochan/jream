# Allocate
  load_arg t0, 0
  load_shared t1, STOP
  
# mul 8 (width of an ErlTerm) (width of an ErlTerm)
  slli t0, t0, 3
  sub t1, t1, t0  # stack grows down

# save code pointer 
  addi t1, t1, -8 
  load_shared t2, CODE_POINTER
  sd t2, 0(t1)

  store_shared t1, STOP
