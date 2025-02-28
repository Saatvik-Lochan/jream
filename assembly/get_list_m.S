# From genop.tab
## @spec get_list  Source Head Tail
## @doc  Get the head and tail (or car and cdr) parts of a list
##       (a cons cell) from Source and put them into the registers
##       Head and Tail.
##

# assume that the pointer in Source is in t0

  # untag the pointer
  untag t0
  ld t1, 0(t0)
  ld t2, 8(t0)
