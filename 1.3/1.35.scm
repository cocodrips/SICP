;; Binary search
(define (search f neg-point pos-point)
  (define (close-enough? x y) (< (abs (- x y))))
  (let ((mid-point (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
	mid-point
	(let ((test-value (f mid-point)))
	  (cond ((positive? test-value)
		 (search neg-point mid-point))
		((negative? test-value)
		 (search mid-point pos-point))
		(else mid-point)))
	)))



;; 1.35
(define tolerance 0.00001)
(define (fixed-point f first-guess)

  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))

  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (try next)))) (try first-guess))

(fixed-point (lambda (x) (+ 1 (/ 1.0 x)) ) 1) ;;1.6180327868
