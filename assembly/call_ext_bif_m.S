# What to load if call_ext is actually to a bif
  # t0 has the bif pointer to jump to
  jalr t0

  # result is in a0, success/failure in a1 though we don't check it here
  # we move it to x0
  store_xreg a0, 0
