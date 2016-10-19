(load "./newtons-method.scm")
(define (cube x) (* x x x))

; 不動点プロセスで x^3 + a*x^2 + b*x + c = 0 の零点の近似値を求める
(define (cubic a b c)
  (lambda (x) (+ (cube x)
		 (* a (square x))
		 (* b x)
		 c)))

(newtons-method (cubic -3 3 -1) 1) ;1.0

