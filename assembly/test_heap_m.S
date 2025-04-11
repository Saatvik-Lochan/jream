# 
  load_shared t0, STOP
  load_shared t1, HTOP

  # space left
  sub t0, t0, t1  

  # space needed
  load_arg t1, 0 

  bge t0, t1, 1f
  exit_because HEAP_SPACE
1:
