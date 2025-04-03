# DEPRECATED
# assume label index at a4
  slli a2, a4, 3
  add a2, a2, s6
  ld t1, 0(a2)

  la t0, 1f # load pc relative address at the end
  store_shared t0, CODE_POINTER

  jr t1
1:
# afterwards:
# - save code pointer
# load function pointer and call
