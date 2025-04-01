# just testing some stuff
  ld s3, 16(s2)
  ld t0, 0(s3)
  ld t1, 8(s1)
  slli t0,t0,0x3
  add t1, t1, t0
  sd t1, 8(s1)
  ld t0, 40(s1)
  jr t0
