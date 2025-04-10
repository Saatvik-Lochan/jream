# Arity in arg 0
# Atoms in arg 1

# Register loaded into t0

  andi t1, t0, 0b11
  li t2, 0b10

  split
  # branch if t1 != t2

  untag t0, t0
  ld t3, 0(t0)  # header

  andi t1, t3, 0b111111
  li t2, 0b000000

  split
  # branch if t1 != t2

  srli t1, t3, 6
  load_arg t2, 0

  split
  # branch if t1 != t2

  ld t1, 8(t0)  # first element
  load_arg t2, 1

  # branch if t1 != t2
