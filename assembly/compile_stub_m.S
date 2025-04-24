# Assume:
# - s9 has the pointer to the jump ptr
# (the above must be computed if you want to even jump here)
# - a1 has the label number
load_shared a0, CODE_CHUNK_P

call_cxx_fun COMPILE_LABEL

# set value in table to point to label loc
sd a0, 0(s9)
mv s2, a1  # update the new argument pointer

# then jump 
jr a0
