import sys
import random

filename = sys.argv[1]
list_len = int(sys.argv[2])


with open(filename, "w") as file:
    rand_list = [random.randint(0, 2 << 10) for i in range(list_len)]

    file.write("[ ")

    for item in rand_list[:-1]:
        file.write(f"{item}, ")

    file.write(f"{rand_list[-1]} ].")
