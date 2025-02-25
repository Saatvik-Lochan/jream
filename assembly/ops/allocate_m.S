# Allocate
load_arg t0, 0
load_shared t1, STOP
  
# mul 8 (width of an ErlTerm) (width of an ErlTerm)
slli t0, t0, 3
add t1, t1, t0

store_shared t1, STOP
