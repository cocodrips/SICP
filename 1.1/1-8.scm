(load "./sqrt.scm")

(print (sqrt 5))

;;
(define (square x) (* x x))
(print (square 5))

(define (cube x)
  (* x x x))

;;
(define (improve guess x)
  (/ (+ (/ x (square guess))
	(* 2 guess))
     3))

(define (good-enough? guess x)
      (< (abs (- (cube guess) x)) 0.001))

(print (sqrt 9))
