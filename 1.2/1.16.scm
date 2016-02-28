;; a: いままでのをかけたやつ。答えになるやつ
;; b: 現時点でのn^?の値
(define (fast-ext a b n)
  (cond ((= n 0) a)
	((even? n) (fast-ext a (* b b) (/ n 2)) )  ;;奇数
	(else (fast-ext (* a b) b (- n 1)) )
	))

;; 2 ^ 3
(fast-ext 1 2 3) ;8
(fast-ext 1 2 10) ;1024
