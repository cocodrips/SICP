(define (fib n)
  (cond ((= n 0) 0)
	((= n 1) 1)
	(else (+ (fib (- n 1)) (fib (- n 2)))) ))
	      
;; > (fib 0)
;; 0
;; > (fib 1)
;; 1
;; > (fib 2)
;; 1
;; > (fib 5)
;; 5
;; > (fib 6)
;; 8
