# Advance save pointer
  load_shared t0, MBOX_SAVE

  ld t1, 0(t0)  # *save (Message *)
  ld t2, 8(t1)  # (*save)->next (Message *)

  store_shared t2, MBOX_SAVE

