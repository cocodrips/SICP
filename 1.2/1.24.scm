(use srfi-27) 
(load "./1.22.scm")

;;ばらつくので3回ずつ実行
(search-for-primes 1000 1020) ; 6, 8, 6
(search-for-primes 10000 10040) ; 15, 26, 30
(search-for-primes 100000 100043) ; 47, 65, 54
(search-for-primes 1000032 1000033) ; 395, 185, 268
(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
	(else (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n)
  (define (try-it a) (= (expmod a n n) a))
  (try-it (+ 1 (random-integer (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
	((fermat-test n) (fast-prime? n (- times 1))) (else #f)))

(define fermet-time 3)
(define (start-prime-test n start-time)
  (if (fast-prime? n fermet-time)
      (report-prime (- (runtime) start-time))))

(search-for-primes 1000 1020) ; 10, 18, 29 (ふえた！)
(search-for-primes 10000 10040) ; 23, 20, 40
(search-for-primes 100000 100043) ; 29, 27, 24

(define fermet-time 10)
(search-for-primes 1000 1020) ; 21, 15, 37
(search-for-primes 10000 10040) ; 19, 22, 21
(search-for-primes 100000 100043) ; 28, 29, 19
;; あんまり増えない
(search-for-primes 1000032 1000033) ; 74, 52, 37
;; 元のprime?よりは大きくした時にはやい

;;もっと大きくなきゃわからん
(define fermet-time 3)
(search-for-primes 1000000000 1000000033) ; 48, 32, 52

(define fermet-time 10)
(search-for-primes 1000000000 1000000033) ; 162, 111, 119
