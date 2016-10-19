
(define (iterative-improve improve enough?)
  (lambda (guess)
    (define (iter guess)
      (if (enough? guess)
	  guess
	  (iter (improve guess))))
    (iter guess)))

;; sqrt
(define (sqrt x)
  (define (enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))

  (define (average x y) (/ (+ x y) 2))
  ((iterative-improve improve enough?) 1.0))

(print "sqrt 2 = " (sqrt 2)) ;; 1.4142156862745097
(print "sqrt 4 = " (sqrt 4)) ;; 2.0000000929222947


;; fix-point
(define (fix-point f first-guess)
  (define (enough? guess)
    (< (abs (- guess (f guess))) 0.00001))
  (f ((iterative-improve f enough?) first-guess)))


;; golden ratio
(define (g x)
  (+ 1 (/ 1 x)))
(print "fix-point golden ratio = " (fix-point g 1.0)) ;;1.6180327868852458
