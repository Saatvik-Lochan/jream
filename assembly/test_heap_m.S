# 
  load_shared t0, STOP
  load_shared t1, HTOP

  # space left
  sub t0, t0, t1  

  # space needed
  # a0 has the number of words
  slli t1, a0, 3

  bge t0, t1, 1f

  # a1 has the live registers
  call_cxx_fun EXECUTE_GC
1:
