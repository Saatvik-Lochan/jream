# send
  mv a0, s2    # put x0 in a0
  ld a1, 8(s5) # put x1 in a1
  call_cxx_fun SEND_MSG
