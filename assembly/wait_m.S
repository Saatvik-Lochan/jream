  # load label arg
  load_arg t0, 0

  store_shared t0, RESUME_LABEL
  exit_because WAIT
