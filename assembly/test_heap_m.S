# 
  load_shared t0, STOP
  load_shared t1, HTOP

  # space left
  sub t0, t0, t1  

  # space needed
  load_arg t1, 0 

  bge t0, t1, 1f
  call_cxx_fun EXECUTE_GC
1:
