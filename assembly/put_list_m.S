  # put_list Head Tail Destination

  # assume Head in t0
  # assume Tail in t1

  load_shared t2, HTOP

  sd t0, 0(t2)  # place head
  sd t1, 8(t2)  # place tail

  ori t0, t2, 0b01  # tag as list

  addi t2, t2, 16  # allocate 2 words
  store_shared t2, HTOP

  # store t0 into destination
