;; 1.44
(load "./1.43.scm")

(define (smooth f)
  (define dx 0.0001)
  (lambda (x)
    (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3) ))
  
((smooth square) 2) ;; 4.000000006666666

(define (n-fold-smooth f n)
  (repeated (smooth f) n))

((n-fold-smooth square 4) 2) ;; 65536.00098646007 (2 ^ 16)




















