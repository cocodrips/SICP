(use slib)(require 'trace) 
(define (factorial n)
	(if (= n 1) 1 (* n #?=(factorial (- n 1)))))


(trace factorial) 
(print (factorial 6))

(define (factorial n) (fact-iter 1 1 n))
	(define (fact-iter product counter max-count)
		(if 
			(> counter max-count)
			product
			(fact-iter (* counter product) (+ counter 1) max-count)))


(trace factorial) 
(print (factorial 6))