# check if save == tail
load_shared t0, MBOX_SAVE
load_shared t1, MBOX_TAIL

# if yes jump to label
split

# otherwise
ld t1, 0(t0) # *save
ld t2, 0(t1) # (*save)->value (value is the front of the struct)

# load into save register
