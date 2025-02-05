shared_var_names = {
    "HTOP",
    "STOP",
    "ARENA",
    "ARENA_SIZE",
    "NEW_ARENA",
    "STUFFF",
}

shared_variables = {
    name: index for index, name in enumerate(shared_var_names)
}

# assert that we can actually index into them with the syntax
# ld reg, num(reg2)
# i.e we are checking that we never have a num that is out of bounds
assert (len(shared_variables) * 8 < 2048)
