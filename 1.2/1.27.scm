;;(use srfi-27)

;; base ^ exp % m を求める関数
(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
	(else (remainder (* base (expmod base (- exp 1) m)) m)))) ; 次をmで割った余り

;; all
(define (fermat-test n m)
  (= (expmod m n n) m))
  

;; fermat-all-testを使う
(define (prime? n times)
  (cond ((= times 0)
	 (display "Passed test. Prime number ")
	 (display n))
	((fermat-test n times) (prime? n (- times 1)))
	(else
	 (display "Failed:")
	 (display n))))

(define (fermat-prime? n)
  (prime? n (- n 1))
  (newline))

;; test
(fermat-prime? 4) ; #f
(fermat-prime? 24) ; #f
(fermat-prime? 11) ; #t
(fermat-prime? 19) ; #t

;; カーマイケル数
(fermat-prime? 561) ; #t <- 通過してしまう





