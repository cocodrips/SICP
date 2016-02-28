def p(x):
    return 3 * x - 4 * pow(x, 3)

def sine(angle):
    while True:
        if angle < 0.1:
            return angle
        print angle
        return p(sine(angle/3))

print sine(12.5)