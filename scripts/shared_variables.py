shared_var_names = [
    "HTOP",
    "STOP",
    "XREG_ARRAY",
    "CODE_CHUNK_P",
    "REDUCTIONS",
    "CODE_POINTER",
    "RESUME_LABEL",
    "MBOX_HEAD",
    "MBOX_TAIL",
]

shared_variables = {
    name: index for index, name in enumerate(shared_var_names)
}

# assert that we can actually index into them with the syntax
# ld reg, num(reg2)
# i.e we are checking that we never have a num that is out of bounds
assert (len(shared_variables) * 8 < 2048)
