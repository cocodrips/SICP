def f(n):
    array = [0] * (n + 1)
    for i in xrange(n + 1):
        if (i < 3):
            array[i] = i
        else:
            array[i] = array[i - 1] + (2 *  array[i - 2]) + (3 * array[i - 3])

    return array[n]

for i in xrange(10):
    print i, f(i)
    
    
    
        
