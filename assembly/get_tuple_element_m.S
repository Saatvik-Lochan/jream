  # get_tuple_element Source Element Destination

  # assume Source is in t0
  untag t0, t0

  # load Element num
  # element number is in t1
  slli t2, t1, 3

  # skip the header - we don't do an arity check!
  addi t3, t2, 8  
  add t3, t3, t0

  ld t0, 0(t3)

  # Store t0 in Destination after this
