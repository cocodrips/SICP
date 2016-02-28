(define (square x)
  (* x x))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001)

(define (improve guess x) (average guess (/ x guess)))


