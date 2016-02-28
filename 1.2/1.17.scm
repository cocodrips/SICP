(define (double x) (* x 2))
(define (halve x) (/ x 2))

(double 4) ;8
(halve 14) ;7

(define (*-org a b)
  (*-iter 0 a b))

(define (*-iter a b n)
;  (print "*-iter: " a " " b " " n)
  (cond ((= n 0) a)
	((even? n) (*-iter a (double b) (halve n)))
	(else (*-iter (+ a b) b (- n 1)))
	))

(*-org 3 4) ;12
(*-org 12 35) ; 420

