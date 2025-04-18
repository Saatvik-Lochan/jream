## From genop tab
## @spec get_tl  Source Tail
## @doc  Get the tail (or cdr) part of a list (a cons cell) from Source and
##       put it into the register Tail.

  # untag the pointer
  untag t0, t0

  # load into t1
  ld t1, 8(t0)

  # t1 then stored in Tail register
