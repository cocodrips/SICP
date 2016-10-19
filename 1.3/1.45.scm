;; fixed-point
(define tolerance 0.0001)
(define max-recursion 1000)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))

  (define (try guess i)
    (print "guess:" i "回目 " guess)
    (let ((next (f guess)))
      (if (or (close-enough? guess next) (= i max-recursion))
	  next
	  (try next (+ i 1)))))
  (try first-guess 1))

(define (average-damp f)
  (lambda (x) (/ (+ (f x) x) 2.0)))


;; 2, 3乗根
(define (nth-root x n)
  (fixed-point
   (average-damp (lambda (y) (/ x (expt y (- n 1))))) 1.0))

;;(print (nth-root 100 2)) ;;10.0
;;(print (nth-root 1000 3)) ;; 10.000002544054729
;; (nth-root 10000 4) ;;無限にfixせず、、、、


;; 4乗根以上
(define (repeated func n)
  (if (= n 1)
      func
      (compose func (repeated func (- n 1)))))

(define (repeated-nth-root x n m)
  (fixed-point
   (repeated (average-damp (lambda (y) (/ x (expt y (- n 1))))) m) 1.0))

(define (try-repeat m n root)
  (define (iter i)
    (print  m "** 1/" root " の" i "重平均緩和")
    (repeated-nth-root m i root)
    (if (= i n)
	0
	(iter (+ i 1))))
  (iter 1))

(print "4乗根")
(try-repeat 5 5 4)


(print "5乗根")
(try-repeat 5 5 5)
;; 5√5の1重平均緩和
;; guess:1回目 1.0
;; guess:2回目 4.875
;; guess:3回目 4.99609375
;; guess:4回目 4.9998779296875
;; guess:5回目 4.999996185302734
;; 5√5の2重平均緩和
;; guess:1回目 1.0
;; guess:2回目 2.236067977499978
;; 5√5の3重平均緩和
;; guess:1回目 1.0
;; guess:2回目 1.702269581480055
;; guess:3回目 1.7102189969426256
;; guess:4回目 1.7099683535828758
;; 5√5の4重平均緩和
;; guess:1回目 1.0
;; guess:2回目 1.4171889796143518
;; guess:3回目 1.5821346323216259
;; guess:4回目 1.4252217637682096
;; guess:5回目 1.5728118601733
;; guess:6回目 1.4311331958359594
;; guess:7回目 1.5659473501126113
;; guess:8回目 1.435727464855355
;; guess:9回目 1.5606221660053525
;; guess:10回目 1.4394355421637766
;; 5√5の5重平均緩和
;; guess:1回目 1.0
;; guess:2回目 1.1431908109098403
;; guess:3回目 2.0160342462038265
;; guess:4回目 1.1571279730879978
;; guess:5回目 1.9786755156104392
;; guess:6回目 1.1530091446573496
;; guess:7回目 1.989481486368132
;; guess:8回目 1.1541542434522962
;; guess:9回目 1.9864340527430726
;; guess:10回目 1.1538265524300433
