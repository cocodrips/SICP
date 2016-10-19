def simpson(f, a, b, n):
    h = 1.0 * (b - a) / n

    def y(f, a, k, h):
        return f(a + (k*h))

    def iter(result, cur, end):
        if cur == end:
            return result + y(f, a, cur, h)
        a1 = 4 * y(f, a, cur + 1, h)
        a2 = 2 * y(f, a, cur + 2, h)
        print cur + 1, ":", a1
        print cur + 2, ":", a2
        



#     (define (cube n)
#   (* n n n))

# (define (simpson f a b n)
#   (define h
#     (/ (- b a) n))
#   (define (y k)
#     (f (+ a (* k h))))

#   (define (iter result cur end)
#     (if (>= cur end)
#     (+ result (y cur))
#     (+ result (* 4 (y (+ cur 1))) (* 2 (y (+ cur 2))) (iter result (+ cur 2) end))))
#   (trace iter)
#   (* (/ h 3) (iter (y 0) 0 n)))