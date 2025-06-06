# Allocate
  load_shared t1, STOP
  load_shared t2, HTOP
  
# mul 8 (width of an ErlTerm) (width of an ErlTerm)
  # t0 has the amount to allocate
  addi t0, t0, 1  # add 1 for code pointer
  slli s9, t0, 3  # size in s9 so it survives the gc if necessary
  sub t1, t1, s9  # stack grows down

# do gc if necessary
  bge t1, t2, 1f

  # load size and live regs
  mv a0, t0

  # restore x0 to be included in the liveset
  sd s2, 0(s5)

  # a1 has the number of live registers already
  call_cxx_fun EXECUTE_GC

  ld s2, 0(s5)

  # redo calculation
  load_shared t1, STOP
  sub t1, t1, s9
1:

# save code pointer 
  load_shared t2, CODE_POINTER
  sd t2, 0(t1)

  store_shared t1, STOP
