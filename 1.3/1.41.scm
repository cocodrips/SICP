(define (inc x) (+ x 1))

(define (double f)
  (lambda (x) (f (f x))))

((double inc) 1) ; 3
(((double (double double)) inc) 5) ; 21 (+16)

