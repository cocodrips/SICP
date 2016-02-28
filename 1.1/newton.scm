
(define (square x)
  (* x x))
(print (square 3)) ;;9

(define (average x y)
  (/ (+ x y) 2))
(print (average 4 8)) ;;6

(define (improve guess x) (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt-iter guess x) (if (good-enough? guess x)
				guess
				(sqrt-iter (improve guess x) x)))


(define (sqrt x) (sqrt-iter 1.0 x))
(print "元のやつ")
(print (sqrt 5)) ;; 元のsqrt
(print (sqrt 9))
(print (sqrt 0.00005))

(define (improve-new guess x)
  (/ (+ (/ x (square guess))
	(* 2 guess)
	)
     3))

(define (sqrt-iter-new guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter-new (improve-new guess x) x)))

(define (sqrt-new x) (sqrt-iter-new 1.0 x))
(print "新しいやつ")
(print (sqrt-new 5)) ;; 元のsqrt
(print (sqrt-new 9))
(print (sqrt-new 0.00005))
