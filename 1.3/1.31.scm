(define (product term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (* result (term a)))))
  (iter a 1))


(define (factorial n)
  (define (next a)
    (+ a 1))
  (define (term a) a)
  (product term 1 next n))

  
(factorial 5) ;; 120
(factorial 6) ;; 720

(define (pi n)
  (define (next i) (+ i 1))
  (define (term-numer i)
    (if (even? i)
	(+ i 2)
	(term-numer (- i 1))))
  (define (term-denom i)
    (if (even? i)
	(term-denom (- i 1))
	(+ i 2)))

  (* 4.0
     (/ (product term-numer 1 next n)
	(product term-denom 1 next n))))
(pi 100) ;; 3.1570301764551676
(pi 10000) ;; 3.1417497057380523


    
