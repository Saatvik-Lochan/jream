# 
  load_shared t0, STOP
  load_shared t1, HTOP

  # space left
  sub t0, t0, t1  

  # space needed
  # a0 has the number of words
  slli t1, a0, 3

  bge t0, t1, 1f

  # restore x0 to be included in the liveset
  sd s2, 0(s5)

  # a1 has the live registers
  call_cxx_fun EXECUTE_GC

  # restore any new value of x0
  ld s2, 0(s5)
1:
