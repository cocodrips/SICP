(+ 5 3 4)
(- 9 1)
(/ 6 2)
(+ (* 2 4) (- 4 6))
(define a 3)
(define b (+ a 1))
(+ a b (* a b))
(= a b)
(if (and (> b a) (< b (* a b)))
    b
    a)
(cond ((= a 4) 6)
((= b 4) (+ 6 7 a))
(else 25))
(+ 2 (if (> b a) b a))
(* (cond ((> a b) a)
((< a b) b)
(else -1))
(+ a 1))


;; 1.3
(define (max x y) (if (> x y) x y))
(max 5 2) ;test 5

(define (square x) (* x x))
(square 5) ;test 25

(define (sum-of-squares x y) (+ (square x) (square y)))
(sum-of-squares 3 4) ;test 25

(define (max-sum-of-squares x y z)
  (max (max (sum-of-squares x y) (sum-of-squares y z))
	    (sum-of-squares z x))) ;problem 1.3
(max-sum-of-squares 3 9 5) ;test 106

