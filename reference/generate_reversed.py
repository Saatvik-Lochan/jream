import sys

filename = sys.argv[1]
list_len = int(sys.argv[2])

with open(filename, "w") as file:
    out_list = [list_len - i for i in range(list_len)]

    file.write("[ ")

    for item in out_list[:-1]:
        file.write(f"{item}, ")

    file.write(f"{out_list[-1]} ].")
