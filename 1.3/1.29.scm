;; Simpson's rule
(use slib)
(require 'trace)


(define (cube n)
  (* n n n))

(define (simpson f a b n)
  (define h
    (/ (- b a) n))
  (define (y k)
    (f (+ a (* k h))))
  
  (define (iter result cur end)
    (if (>= cur end)
	(+ result (y cur))
	(+ result (* 4 (y (+ cur 1))) (* 2 (y (+ cur 2))) (iter result (+ cur 2) end))))
  (trace iter)
  (* (/ h 3) (iter (y 0) 0 n)))

(simpson cube 0 1.0 100) ;; 0.2566666666666668
(simpson cube 0 1.0 1000) ;; 0.2506666666666667
(simpson cube 0 1.0 10000) ;; 0.2500666666666664
