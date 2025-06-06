import os
import re
from shared_variables import shared_variables


def get_tokens(line):
    tokens = list(map(lambda x: x.rstrip(", "), line.strip().split(" ")))
    return tokens


def transform_if_necessary(line: str):
    if line.startswith("load_shared"):
        tokens = get_tokens(line)

        assert (len(tokens) == 3)
        assert (tokens[0] == "load_shared")
        register = tokens[1]
        index = shared_variables[tokens[2]]

        # s1 is where we save the pointer to the shared variables
        # we initially pass it in as an argument then save it
        return f"ld {register}, {index * 8}(s1) " + \
            f"# generated from '{line.strip()}'\n"

    if line.startswith("load_xreg"):
        tokens = get_tokens(line)

        assert (len(tokens) == 3)
        assert (tokens[0] == "load_xreg")
        register = tokens[1]
        index = int(tokens[2])

        return f"ld {register}, {index * 8}(s5) " + \
            f"# generated from '{line.strip()}'\n"

    if line.startswith("store_xreg"):
        tokens = get_tokens(line)

        assert (len(tokens) == 3)
        assert (tokens[0] == "store_xreg")
        register = tokens[1]
        index = int(tokens[2])

        return f"sd {register}, {index * 8}(s5) " + \
            f"# generated from '{line.strip()}'\n"

    if line.startswith("load_addr"):
        tokens = get_tokens(line)

        assert (len(tokens) == 3)
        assert (tokens[0] == "load_addr")
        register = tokens[1]
        index = shared_variables[tokens[2]]

        return f"addi {register}, s1, {index * 8} " + \
            f"# generated from '{line.strip()}'\n"

    if line.startswith("store_shared"):
        tokens = get_tokens(line)

        assert (len(tokens) == 3)
        assert (tokens[0] == "store_shared")
        register = tokens[1]
        index = shared_variables[tokens[2]]

        # s1 is where we save the pointer to the shared variables
        # we initially pass it in as an argument then save it
        return f"sd {register}, {index * 8}(s1) " + \
            f"# generated from '{line.strip()}'\n"

    if line.startswith("load_arg"):
        tokens = get_tokens(line)

        raise Exception("load arg deprecated")

        assert (len(tokens) == 3)
        assert (tokens[0] == "load_arg")

        register = tokens[1]
        arg_index = int(tokens[2])

        # s3 is where we save the pointer to the argument array
        return f"ld {register}, {arg_index * 8}(s3) " + \
            f"# generated from '{line.strip()}'\n"

    if line.startswith("call_cxx_fun"):
        tokens = get_tokens(line)

        assert (len(tokens) == 2)
        assert (tokens[0] == "call_cxx_fun")

        fun_name = tokens[1].upper()
        fun_index = get_fun_index(fun_name)

        # check that we aren't giving a too large immediate
        # (12 bits signed max)
        assert (fun_index * 8 < 2048)

        # s4 is where we save the pointer to the function pointer array
        # we can use t0 since it is call-clobbered anyway
        instructions = (
            f"ld t0, {fun_index * 8}(s4) "
            f"# generated from '{line.strip()}'\n"
            f"jalr t0 "
            f"# cont...\n"
        )

        return instructions

    if line.startswith("untag"):
        tokens = get_tokens(line)

        assert (len(tokens) == 3)
        assert (tokens[0] == "untag")

        register1 = tokens[1]
        register2 = tokens[1]

        instructions = (
            f"andi {register1}, {register2}, 0xfffffffffffffffc"
            f"# generated from '{line.strip()}'\n"
        )
        return instructions

    if line.startswith("exit_because"):
        tokens = get_tokens(line)

        assert (len(tokens) == 2)
        assert (tokens[0] == "exit_because")

        reason = tokens[1]

        reason_code = {
            "ERROR": -1,
            "FINISH": 0,
            "YIELD": 1,
            "WAIT": 2,
            "BADMATCH": 3,
            "HEAP_SPACE": 4
        }

        # s8 is the return value register because it doesnt' change
        instructions = (
            f"li s8, {reason_code[reason]}"  # set exit code
            f"# generated from '{line.strip()}'\n"
            f"jr s7"  # jump to teardown
            f"# cont...\n"
        )

        return instructions

    else:
        return line


def get_fun_index(fun_name):
    return get_fun_index.mapping[fun_name]


def do_compile(filename, callable_fun_file):
    base_dir = os.path.dirname(filename)
    name = os.path.basename(filename)

    assert (name.endswith("_m.S"))

    new_name = name[:-4]
    raw_asm_filename = os.path.join(base_dir, "build", new_name)

    get_fun_index.mapping = {}
    get_fun_index.count = 0
    with open(callable_fun_file) as f:
        for line in f:
            match = re.search("// m_asm: (.*)", line)

            if match:
                fun_name = match.group(1).strip().upper()
                get_fun_index.mapping[fun_name] = get_fun_index.count
                get_fun_index.count += 1

    disclaimer_lines = [
        f"# THIS IS A FILE GENERATED BY {os.path.basename(__file__)}" +
        f" APPLIED ON {os.path.basename(filename)}\n",
        "# ASSUME s1 holds a pointer to the pcb\n",
        "# ASSUME s2 holds a pointer to argument pointer array for all instr\n",
        "# ASSUME s3 holds a pointer to the argument array for this instr\n",
        "# ASSUME s4 holds a pointer to the array of all callable cxx funs\n",
        "# ASSUME s5 holds a pointer to the X register array \n\n",
    ]

    count = 1
    name_to_write = raw_asm_filename + ".S"
    with open(filename) as meta_asm_file:
        while True:
            did_break = write_til_break(
                meta_asm_file, name_to_write, disclaimer_lines)

            if not did_break:
                return

            if count == 1:
                os.rename(name_to_write, raw_asm_filename + "_1.S")

            count += 1
            name_to_write = f"{raw_asm_filename}_{count}.S"


def write_til_break(read_file, name, disclaimer_lines):
    with open(name, "w") as raw_asm_file:
        raw_asm_file.writelines(disclaimer_lines)

        while True:
            line = read_file.readline()

            if line == "":
                return False

            cleaned_line = line.strip(" ")

            if cleaned_line == "split\n":
                return True

            raw_asm_file.write(transform_if_necessary(cleaned_line))
