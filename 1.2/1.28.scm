(load "./1.27.scm")
(print "== みらーラビン==")
(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp) (sq-check (expmod base (/ exp 2) m) m))
 	(else (remainder (* base (expmod base (- exp 1) m)) m))))

(define (sq-check n m)
  (define sq_mod (remainder (* n n) m))
  (if (= sq_mod 1)
      (cond ((= n 1) 1)
	    ((= n (- m 1)) 1)
	    (else 0))      
      sq_mod))


  
;; test
(fermat-prime? 4) ; #f
(fermat-prime? 24) ; #f
(fermat-prime? 11) ; #t
(fermat-prime? 19) ; #t

;; カーマイケル数
(fermat-prime? 561) ; #t <- 通過してしまう

;; Failed:4
;; Failed:24
;; Passed test. Prime number 11
;; Passed test. Prime number 19
;; Passed test. Prime number 561
;; == みらーラビン==
;; Failed:4
;; Failed:24
;; Passed test. Prime number 11
;; Passed test. Prime number 19
;; Failed:561
