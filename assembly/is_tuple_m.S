  # check last two bits
  andi t1, t0, 0b11
  li t2, 0b10

  # branch not equal t1, t2
  split

  # check that it points to a tuple
  untag t0, t0
  ld t1, 0(t0)

  andi t1, t1, 0b111111
  li t2, 0b000000

  # branch not equal t1, t2
