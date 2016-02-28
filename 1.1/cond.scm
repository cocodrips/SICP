(define (p) (p))
(define (test a b)
  (cond ((= a 0) a)
	(else b)))

(test 3 (p))

