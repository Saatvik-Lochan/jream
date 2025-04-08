# What to load if call_ext is actually to a bif
  load_arg t0, 1

  slli t1, t0, 3
  add t2, t1, s10

  load_shared a1, CODE_CHUNK_P

  ld t0, 0(t2)
  jalr t0

  # result is in a0, success/failure in a1 though we don't check it here
  # we move it to x0
  store_xreg a0, 0
