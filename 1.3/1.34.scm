((lambda (x) (+ x 4)) 2) ;; test

(define (f x y)
  (let ((a (+ 1 (* x y)))
	(b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))
(print (f 1 2))



;; 1.34
(define (f g) (g 2))
(print "1.34 sample:" 
       (f (lambda (x) (* x x)))) ;; 4

(f f) ;; *** ERROR: invalid application: (2 2)

