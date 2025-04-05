# What to load if call_ext is actually to a bif
  load_arg t0, 1

  slli t1, t0, 3
  add t2, t1, s10

  ld t0, 0(t2)
  jalr t0
