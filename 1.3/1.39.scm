(define (cont-frac n d k)
  (define (iter i)
    (if (= i k)
	(/ (n i) (d i))
	(/ (n i) (+ (d i) (iter (+ i 1))))))
  (iter 1))

(define (tan x)
  (cont-frac (lambda (i)
	       (if (= i 0) x
		   (* -1 (* x x))))
	     (lambda (i) (- (* 2 i) 1))
	     n))

(define pi 3.141592)
(tan (/ pi 4)) ;; 0.9999996732051569

