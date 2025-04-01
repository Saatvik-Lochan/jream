# Assume:
# - s9 has the pointer to the func_ptr location 
# (the above must be computed if you want to even jump here)
# - a4 has the function index
load_shared a0, CODE_CHUNK_P
mv a1, a4

call_cxx_fun COMPILE_FUNC

# set value in table to point to compiled funct
sd a0, 0(s9)

# then jump to the compiled func
jr a0
