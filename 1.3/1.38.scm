(define (cont-frac n d k)
  (define (iter i)
    (if (= i k)
	(/ (n i) (d i))
	(/ (n i) (+ (d i) (iter (+ i 1))))))
  (iter 1))

(define (n-cont-frac n)
  (define (n-iter j)
    (if (> j n)
	"end"
	(let ((ans
	       (cont-frac (lambda (i) 1.0)
			  (lambda (i)
			    (let ((div (quotient i 3))
				  (mod (modulo i 3)))
			      (if (= mod 2)
				  (- i div)
				  1.0)))
			  j)))
	  (print j "=>" (+ 2 ans))
	  (n-iter (+ j 1)))
	))
  (n-iter 1))

(n-cont-frac 10)
;; 1=>3.0
;; 2=>2.6666666666666665
;; 3=>2.75
;; 4=>2.7142857142857144
;; 5=>2.71875
;; 6=>2.717948717948718
;; 7=>2.7183098591549295
;; 8=>2.718279569892473
;; 9=>2.718283582089552
;; 10=>2.7182817182817183
;; "end"


;;反復プロセス
(define (cont-frac n d k)
  (define (iter i result)
    (if (= i 0)
	result
	(iter (- i 1)
	      (/ (n i)
		 (+ (d i) result)))))
    (iter k 0))
(n-cont-frac 10)

;; 1=>3.0
;; 2=>2.6666666666666665
;; 3=>2.75
;; 4=>2.7142857142857144
;; 5=>2.71875
;; 6=>2.717948717948718
;; 7=>2.7183098591549295
;; 8=>2.718279569892473
;; 9=>2.718283582089552
;; 10=>2.7182817182817183
		    
