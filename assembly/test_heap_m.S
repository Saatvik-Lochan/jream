# 
  load_shared t0, STOP
  load_shared t1, HTOP

  # space left
  sub t0, t0, t1  

  # space needed
  load_arg a0, 0 
  slli t1, a0, 3

  bge t0, t1, 1f

  load_arg a1, 1
  call_cxx_fun EXECUTE_GC
1:
