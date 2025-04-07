# assume arg is loaded in t0
  andi t1, t0, 0b111111
  li t2, 0b111011 # nil code

  # bne t1, t2
