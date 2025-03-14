# THIS IS A FILE GENERATED BY meta_assembly_compile.py APPLIED ON get_list_m.S
# ASSUME s1 holds a pointer to the pcb
# ASSUME s2 holds a pointer to argument pointer array for all instr
# ASSUME s3 holds a pointer to the argument array for this instr
# ASSUME s4 holds a pointer to the array of all callable cxx funs
# ASSUME s5 holds a pointer to the X register array 

# From genop.tab
## @spec get_list  Source Head Tail
## @doc  Get the head and tail (or car and cdr) parts of a list
##       (a cons cell) from Source and put them into the registers
##       Head and Tail.
##

# assume that the pointer in Source is in t0

# untag the pointer
andi t0, t0, 0xfffffffffffffffc# generated from 'untag t0'
ld t1, 0(t0)
ld t2, 8(t0)
