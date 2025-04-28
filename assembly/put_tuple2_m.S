  # i.e amount to allocate (including header)
  # amount to allocate is in t0
  load_shared t1, HTOP

  # create and store header
  addi t4, t0, -1  # subtract header word size
  slli t3, t4, 6   # no need for tag since tuple
  sd t3, 0(t1) 

  slli t0, t0, 3
  add t5, t1, t0

  store_shared t5, HTOP

  # t1 holds old HTOP at this point 
