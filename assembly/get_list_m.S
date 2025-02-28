# From genop.tab
## @spec get_list  Source Head Tail
## @doc  Get the head and tail (or car and cdr) parts of a list
##       (a cons cell) from Source and put them into the registers
##       Head and Tail.
##

  load_arg t0, 0
  ld t2, 0(t1)
