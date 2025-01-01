

with open("reference/genop.tab") as genops:
    ops = {}

    largest_op = 0

    for line in genops:
        if line.startswith(("#", "BEAM_FORMAT_NUMBER")) or not line.strip():
            continue

        op_num, op_info = line.split(":")
        name, arity = op_info.strip().split("/")

        op_num = int(op_num)
        largest_op = max(largest_op, op_num)

        ops[op_num] = (name, arity)

    op_array = [
        ops[op_num] for op_num in range(1, largest_op+1)
    ]

    op_string = "".join([f"\n    {arity}, // {name}" for name, arity in op_array])
    out_string = f"int op_arrities[] = {{ {op_string} \n}};"
    
    print(out_string)
