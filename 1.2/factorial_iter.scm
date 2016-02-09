(define (factorial-iter product counter max-count)
  (if (> counter max-count) product
      (factorial-iter (* product counter) (+ counter 1) max-count)))

(define (factorial n)
  (factorial-iter 1 1 n))
  
(print (factorial 5))
