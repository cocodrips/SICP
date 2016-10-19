
(define counter 0)

(define (f x)
  (define counter (+ 1 counter))
  (print counter)
  x)

(f 2)
