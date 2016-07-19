(define x (cons 1 2)) ;; pair

;; 有理数
(define (make-rat n d) 
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))
(define (numer x) (car x)) ;; 分子 
(define (denom x) (cdr x)) ;; 分母

;; car Content of Address part of Register
;; cdr Content of Decrement part of Register

(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))

(display "4/6 -> ")
(print-rat (make-rat 4 6)) ;; 2/3


;; 2.1 正と負の両方を引数にとれる改良版 make-rat
(define (make-signed-rat n d) 
  (let ((g (gcd n d)))
    (if (or (and (>= n 0) (>= d 0)) 
	    (and (< n 0) (< d 0))) 
	(cons (/ (abs n) g) (/ (abs d) g))
	(cons (* -1 (/ (abs n) g)) (/ (abs d) g)))))



(display "======Ex 2.1======")
(newline)
(print-rat (make-signed-rat -4 -6)) ; 2/3
(print-rat (make-signed-rat 4 -6))  ; -2/3
(print-rat (make-signed-rat -4 6))  ; -2/3
(print-rat (make-signed-rat 4 6))   ; 2/3



;; 2.2 始点と終点のペア
(define (make-point x y) (cons x y))  
(define (point-x point) (car point))
(define (point-y point) (cdr point))

(define (make-segment start end) (cons start end))
(define (start-segment segment) (car segment))
(define (end-segment segment) (cdr segment))

(define (average a b) (/ (+ a b) 2))
(define (midpoint-segment segment)
  (let ((x (average (point-x (start-segment segment))
		    (point-x (end-segment segment))))
	(y (average (point-y (start-segment segment))
		    (point-y (end-segment segment)))))
    (make-point x y)))

(define (print-point point)
  (display "(")
  (display (point-x point))
  (display ", ")
  (display (point-y point))
  (display ")")
  (newline))

(display "======Ex 2.2======")
(newline)

(define start (make-point 0 0))
(define end (make-point 10 10))

(display "start:")
(print-point start)
(display "end:")
(print-point end)
(display "midpoint:")
(print-point 
 (midpoint-segment
  (make-segment start end)))
;start:(0, 0)
;end:(10, 10)
;midpoint:(5, 5)

;; 2.3 長方形の表現

;; 傾きがない
(display "======Ex 2.3======")
(newline)
(define (make-rect left-top size) (cons left-top size))
(define (rect-left-top left-top) (car rect))
(define (rect-size rect) (cdr rect))
(define (rect-width rect) (car (rect-size rect)))
(define (rect-height rect) (cdr (rect-size rect)))

(define (edge-length rect) (* 2 (+ (rect-width rect)
				   (rect-height rect))))

(define (rect-area rect) ( * (rect-width rect)
			     (rect-height rect)))


(define rect (make-rect (make-point 0 2)
			(make-point 5 8)))
(display "length:")
(display (edge-length rect))
(display " area:")
(display (rect-area rect))
(newline)
;; length:26 area:40





