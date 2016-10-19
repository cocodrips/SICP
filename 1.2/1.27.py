def expmod(base, exp, m):

    if exp == 0:
        return 1
    if exp % 2 == 0:
        return expmod(base,exp/2,m) % m
    return base * expmod(base,exp-1,m) % m

def fermat_all_test(n, m):
    tmp = expmod(m, n, n)
    print "fermet-test:{m}^{n}({mn})%{n} => {tmp} {b}".format(m=m, n=n, mn=m^n, tmp=tmp, b="o" if tmp==m else "x")
    return tmp == m

def fast_prime(n, times):
    print "(fast_prime?:", n, times, ")"
    if times == 0:
        return True
    if fermat_all_test(n, times):
        fast_prime(n, times - 1)
    return False

def fermat_prime(n):
    print "FERMAT_TEST:", n
    fast_prime(n, n-1)


fermat_prime(4)
fermat_prime(11)