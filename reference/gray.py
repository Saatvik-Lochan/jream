num = 5000


def gray(a):
    return a ^ (a >> 1)


accumulator = 0

for i in range(num + 1):
    accumulator ^= gray(i)

print(accumulator)
