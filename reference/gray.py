def gray(a):
    return a ^ (a >> 1)


def gray_burn(num):
    accumulator = 0
    curr = 0

    while curr <= num:
        accumulator ^= gray(curr)
        curr += 1

    return accumulator


for i in range(5001):
    print(gray_burn(i))
