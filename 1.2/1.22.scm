;; 1.22.scm
(load "./smallest-divisor.scm")

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (runtime) 
  (use srfi-11)
  (let-values (((a b) (sys-gettimeofday)))
    (+ (* a 1000000) b))) ;; gosh にruntime実装がない

(define (search-for-primes n end)
  (cond ((even? n) (search-for-primes (+ n 1) end))
	((> n end) (display "end"))
	(else (timed-prime-test n) (search-for-primes (+ n 2) end))))



(search-for-primes 1000 1050) ; 1009 1013 1019 6マイクロ秒
(search-for-primes 10000 10100) ; 10007 10009 10037 17マイクロ秒
(search-for-primes 100000 100100) ; 100003 100019 100043 55 マイクロ秒
(search-for-primes 1000000 1000030) ; 395マイクロ秒
(search-for-primes 10000000 10000030) ; 797マイクロ秒




