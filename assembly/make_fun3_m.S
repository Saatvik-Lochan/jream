  # load index
  # index is in t2
  load_shared t1, HTOP

# num words to allocate is loaded into t0 before this snippet

  # create and store header
  addi t4, t0, -1  # subtract header word size
  slli t3, t4, 6
  addi t3, t3, 0b010100  # add this tag at the end
  sd t3, 0(t1) 

  # store index 
  sd t2, 8(t1)

  slli t0, t0, 3
  add t5, t1, t0

  store_shared t5, HTOP
