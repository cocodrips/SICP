(load "./sqrt.scm")

(print (sqrt 5)) ;; 元のsqrt

 (define (new-if predicate then-clause else-clause)
   (cond (predicate then-clause)
 	(else else-clause)))

(print (new-if (= 2 3) 0 5))
(print (new-if (= 1 1) 0 5))
(define (sqrt-iter guess x) (new-if (good-enough? guess x)
 				    guess
 				    (sqrt-iter (improve guess x) x)))

;; (print (sqrt 5))

(define (p) (p))

(cond (#t 0)
      (else (p)))
