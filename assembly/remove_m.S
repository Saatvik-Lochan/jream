# Remove message
  load_shared t0, MBOX_SAVE

  ld t1, 0(t0)  # *save (Message *)
  ld t2, 8(t1)  # (*save)->next (Message *)

  # if we are unlinking the last element, we have to fix the tail
  bnez t2, 1f
  store_shared t0, MBOX_TAIL

1:
  mv a0, t2
  sd t2, 0(t0)  # *save = (*save)->next 

  call_cxx_fun FREE_MSG

  # save = &head
  load_addr t3, MBOX_HEAD
  store_shared t3, MBOX_SAVE
