(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (+ result (term a)))))
  (iter a 0))

(define (next a)
  (+ a 1))
(define (term a) a)
(sum term 0 next 10) ;; 55
(sum term 0 next 100) ;; 5050
    
