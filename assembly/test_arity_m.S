# load arity to check
  # t0 has the arity to check

# assume that Arg1 was loaded into t1
  untag t1, t1
  ld t2, 0(t1)

  srli t1, t2, 6  # get arity

# bneq t0, t1 Label
