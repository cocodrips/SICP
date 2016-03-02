(load "./1.22.scm")

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor  n (+ test-divisor 1)))))

(search-for-primes 1000 1020) ; 1009 1013 1019 9ミリ秒
(search-for-primes 10000 10138) ; 10007 10009 10037 26ミリ秒
(search-for-primes 100000 100044) ; 100003 100019 100043 65 ミリ秒

(define (next x) (if (= x 2) 3 (+ x 2)))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor  n (next test-divisor)))))


(search-for-primes 1000 1020) ; 1009 1013 1019 4ミリ秒
(search-for-primes 10000 10138) ; 10007 10009 10037 11ミリ秒
(search-for-primes 100000 100044) ; 100003 100019 100043 36 ミリ秒

