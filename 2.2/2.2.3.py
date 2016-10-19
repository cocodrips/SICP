def unique_pairs(n):
    def itr(base, i, stack):
        print base, i, n, stack

        if base >= n:
            return stack
        if i == n:
            stack.append((base, i))
            return itr(base + 1, base + 2, stack)
        stack.append((base, i))
        itr(base, i + 1, stack)

    a = []
    print itr(1, 2, a)

print unique_pairs(4)