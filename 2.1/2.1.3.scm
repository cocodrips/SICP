;; 2.1.3 データとは何か

;; cons の実装
(define (cons x y)
  (define (dispatch m)
    (cond 
        ((= m 0) x)
        ((= m 1) y)
    	(else (error "Argument not 0 or 1: CONS" m))))
  dispatch)

(define (car z) (z 0))
(define (cdr z) (z 1))


;; Ex 2.4
;; (car (cons x y))がxを返すことを確認、cdrの実装
(define (cons x y) (lambda (m) (m x y)))
(define (car z) (z (lambda (p q) p)))

(display "(car (cons x y)): ")
(define p (cons 1 2))
(display (car p)) ; 1
(newline)

(define (cdr z) (z (lambda (p q) q)))
(display "(cdr (cons x y)): ")
(display (cdr p)) ; 2
(newline)
; (display "cdr:")


;; Ex 2.5 2^a * 3^b を使ってa, bのペアを表現する
(display "==========Ex 2.5===========")
(newline)
(define (cons a b)
    (* (expt 2 a) (expt 3 b)))


(define (expt-n num base)
    (define (itr n cnt)
        (print "n:" n   " counter:" cnt "\n")
        (if (= (remainder n base) 0)
            (itr (/ n base) (+ cnt 1))
            cnt))
    (itr num 0))

(define (car pair)
    (expt-n pair 2))

(define (cdr pair)
    (expt-n pair 3))


(define p (cons 2 5))
(display "(cons 2 5): ")
(display p)
(newline)

(display "(car (cons 2 5)): ")
(newline)
(display (car p))
(newline)
(newline)

; (cons 2 5): 972
; (car (cons 2 5)): 
; n:972 counter:0
; n:486 counter:1
; n:243 counter:2
; 2

(display "(cdr (cons 2 5)): ")
(newline)
(display (cdr p))
(newline)

; (cdr (cons 2 5)): 
; n:972 counter:0
; n:324 counter:1
; n:108 counter:2
; n:36 counter:3
; n:12 counter:4
; n:4 counter:5
; 5

;; Ex 2.6 1, 2の実装
(display "==========Ex 2.6===========")
(newline)

(define zero 
    (lambda (f) (lambda (x) x)))
(define (add-1 n)
    (lambda (f) (lambda (x) (f ((n f) x)))))

(define (inc n)
  (+ n 1))
(define (to-s z)
  ((z inc) 0)) ;http://d.hatena.ne.jp/tanakaBox/20070723/1186253252

(print "to-s test:" (to-s zero) " " (to-s (add-1 zero)) "\n")

; (define one 
;     (lambda (f) (lambda (x) )))

; zero(x) = x
; add-1(n) = f( n(f)(x))
; n = zeroなら
; add-1(n) = f( zero(f)(x) )
; add-1(n) = f( f(x) )


;; repeatedもってくる
(define (compose f g)
  (lambda (x) (f (g x)) ))

(define (repeated func n)
  (if (= n 1)
      func
      (compose func (repeated func (- n 1)))))

(define one 
    (lambda (f) (repeated f 1)))
(define two 
    (lambda (f) (repeated f 2)))

(display "one: ")
(display ((one inc) 0))
(newline)

(display "two: ")
(display ((two inc) 0))
(newline)

(define (add a b)
    (lambda (f)
        (lambda (x) ((a f) ((b f) x)))
        ))

(display "one + two = ")
(display (((add one two) inc) 0))
(newline)

; one: 1
; two: 2
; one + two = 3