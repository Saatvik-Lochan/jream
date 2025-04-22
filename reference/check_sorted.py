import sys

filename = sys.argv[1]
size = int(sys.argv[2])

with open(filename) as file:
    list_line = list(map(int, file.readline().strip()[1:-1].split(", ")))

is_sorted = all(a <= b for a, b in zip(list_line, list_line[1:]))
is_correct_size = len(list_line) == size

print(f"{is_sorted=}, {is_correct_size=}")
