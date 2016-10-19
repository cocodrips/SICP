;; 1.43
(load "./1.42.scm")
(define (inc x) (+ x 1))

(define (repeated func n)
  (if (= n 1)
      func
      (compose func (repeated func (- n 1)))))


((repeated square 2) 5) ;; 625
(time ((repeated inc 10000000) 1))
; real  12.750
; user  11.760
; sys    0.910

;; hioさんの見習う

(define (repeated-2 func n)
  (cond ((= n 1)
	 func)
	((= (remainder n 2) 1)
	 (compose func (repeated-2 func (- n 1))))
	(else
	 (let ((f (repeated-2 func (/ n 2))))
	   (compose f f)))
	))

((repeated-2 square 2) 5) ;; 625
(time ((repeated-2 inc 10000000) 1))
; real   0.556
; user   0.550
; sys    0.000

;; hioさんの末尾再帰版
(define (repeated-tail-rec f n)
  (define (iter i val)
    (if (>= i n)
	val
	(iter (+ i 1) (f val))))
  (lambda (x) (iter 0 x)))
(time ((repeated-tail-rec inc 10000000) 1))
; real   0.550
; user   0.550
; sys    0.000
;; そんな時間かわらないな
