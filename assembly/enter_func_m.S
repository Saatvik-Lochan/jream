# a4 has function index
slli t0, a4, 3 
add s9, s6, t0  # s9 is address of pointer in compiled func array
ld t0, 0(s9)

jr t0
