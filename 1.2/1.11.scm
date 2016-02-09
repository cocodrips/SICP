;; 再帰
(define (f n)
  (if (< n 3) n
      (+ (f (- n 1))
	 (* 2 (f (- n 2)))
	 (* 3 (f (- n 3))) ) ))

;; gosh> (f 2)
;; 2
;; gosh> (f 8)
;; 335


;; 反復
(define (f-iter count end a1 a2 a3)
  (if (<= count end)
      (f-iter (+ count 1)
	      end
	      a2
	      a3
	      (+ a3 (* 2 a2) (* 3 a1)) )
      a3))


(define (f n)
  (if (< n 3)
      n
      (f-iter 3 n 0 1 2)))

;; gosh> (f 4)
;; 11
;; gosh> (f 8)
;; 335
