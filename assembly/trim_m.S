# @spec trim N Remaining
# @doc Reduce the stack usage by N words,
#      keeping the CP on the top of the stack.

  load_shared t0, STOP
  ld t1, 0(t0)  # load CP

  # load N
  load_arg t2, 0
  slli t2, t2, 3
  add t0, t0, t2

  sd t1, 0(t0) # save CP
  store_shared t0, STOP
