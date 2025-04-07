# check if save == tail
load_shared t0, MBOX_SAVE
load_shared t1, MBOX_TAIL

# if yes jump to label
split

load_shared t2, MBOX_HEAD

split

li t0, 4
