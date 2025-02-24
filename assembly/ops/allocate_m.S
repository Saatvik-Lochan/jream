# this is allocate
  load_arg t0, 0
  load_shared t1, STOP

# mul 8

  li a0, 5
  call_cxx_fun print_int
  
  sll t0, t0, 3
  ld t2, 0(t1)
  add t2, t2, t0

  sd t2, 0(t1)
