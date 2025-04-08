# What to load if call_ext_only is actually to a bif
  load_arg t0, 1

  slli t1, t0, 3
  add t2, t1, s10

  mv a0, s5
  load_shared a1, CODE_CHUNK_P
  
# return (this is a tail_recursive call)
  load_shared ra, CODE_POINTER

  ld t0, 0(t2)
  jr t0
