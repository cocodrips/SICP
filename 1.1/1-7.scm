(load "./sqrt.scm")

(print (sqrt 0.01)) ;; 0.1
(print (sqrt 0.0001)) ;; 0.001




;; 大きい数字
(define n 10000000000000000000000)
(define guess 1)

(define guess (improve guess n))
(print guess "\n")

(define guess (improve guess n))
(print guess "\n")

(define guess (improve guess n))
(print guess "\n")

