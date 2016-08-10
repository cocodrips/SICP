;; list
(define l (list 1 2 3 4))

(display l) ;(1 2 3 4)
(newline)

(display (cdr l)) ;(2 3 4)
(newline)

;; n番目の要素を返す
(display "=====list-ref n番目の要素=====")
(newline)
(define (list-ref items n)
    (if (= n 0)
        (car items)
        (list-ref (cdr items) (- n 1))))
(define squares (list 1 4 9 16 25))
(display squares)
(newline)
(display "list-ref 3: ")
(display (list-ref squares 3))
(newline)
;(1 4 9 16 25)
;list-ref 3: 16


;; 長さ
(display "=====length=====")
(newline)
(define (len items)
    (if (null? items)
        0
        (+ 1 (len (cdr items)))))
(display "Length: ")
(display (len squares))
(newline)
;; lengthは定義済みだったので lenを定義


;; リストの合成
(display "=====append リストの合成=====")
(newline)
(define (append list1 list2)
    (if (null? list1)
        list2
        (cons (car list1) (append (cdr list1) list2)) ) )

(define l2 (list 1 2 3))
(display l2)
(display " + ")
(display squares)
(display " = ")
(display (append l2 squares))
(newline)
;(1 2 3) + (1 4 9 16 25) = (1 2 3 1 4 9 16 25)


(display "=====Ex 2.17=====")
(newline)
;; 最後の要素を返すlast-pairを定義

(define (last-pair items)
    (if (null? (cdr items))
        items
        (last-pair (cdr items))))

(display (last-pair squares))
(newline)
;(25)


(display "=====Ex 2.18=====")
(newline)
;; リストを逆順に返す

(define (reverse items)
    (define (itr src dist)
        (if (null? src)
            dist
            (itr (cdr src) (cons (car src) dist)))) ;; car でつなぐとlist表記にならなかったので・・・。効率悪そう。
    (itr items (list)))

(display (reverse squares))
;(25 16 9 4 1)
(newline)


(display "=====Ex 2.19=====")
(newline)
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))


(define (cc amount coin-values)
    (cond 
        ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
            (+ 
                (cc amount (except-first-denomination coin-values))
                (cc (- amount (first-denomination coin-values)) coin-values ))
        )))
(define (no-more? l) (null? l))
(define (except-first-denomination l) (cdr l))
(define (first-denomination l) (car l))

(display (cc 100 us-coins)) ;292
(newline)


(display "=====Ex 2.20=====")
(newline)

(define (same-parity . l)
    (define (itr base src dist)
        (if (null? src)
            dist
            (if (= (remainder (car src) 2) base)
                (itr base (cdr src) (append dist (list (car src))))
                (itr base (cdr src) dist))))
    (itr (remainder (car l) 2) l (list)))

(display (same-parity 1 2 3 4 5)) 
(newline)
;(1 3 5)

(display (same-parity 2 3 4 5 6 10))
(newline)
;(2 4 6 10)


;; Filter実装すればいいっぽい！
(define (filter l f)
    (cond 
        ((null? l) (list))
        ((f (car l)) (cons (car l) (filter (cdr l) f)))
    (else (filter (cdr l) f))))

(define (same-parity . l)
    (filter l (lambda (x) (= (remainder x 2) (remainder (car l) 2)))))


(display "=====Ex 2.20 filter=====")
(newline)

(display (same-parity 1 2 3 4 5)) 
(newline)
;(1 3 5)

(display (same-parity 2 3 4 5 6 10))
(newline)
;(2 4 6 10)

(display "=====Ex 2.20 reverse=====")
(newline)

;; reverse後からすることで、スタックが消費しないようにできる？
(define (filter l f)
    (define (filter-itr l f dist)
        (cond 
            ((null? l) dist)
            ((f (car l)) (filter-itr (cdr l) f (cons (car l) dist)))
        (else (filter-itr (cdr l) f dist))))
    (reverse (filter-itr l f (list))))

(display (same-parity 1 2 3 4 5)) 
(newline)
;(1 3 5)

(display (same-parity 2 3 4 5 6 10))
(newline)
;(2 4 6 10)


(define (map proc items)
    (if (null? items)
        (list)
        (cons (proc (car items)) (map proc (cdr items)))))

(display (map abs (list -10 2.5 -11.6 17)))
(newline)
;(10 2.5 11.6 17)

(display (map (lambda (x) (* x x)) (list 1 2 3 4)))
(newline)
;(10 2.5 11.6 17)


(display "==========Ex 2.21===========")
(newline)

(define (square-list items)
    (if (null? items)
        (list)
        (cons (* (car items) (car items)) (square-list (cdr items)))))

(display (square-list (list 1 2 3 4)))
(newline)
;(1 4 9 16)


(define (square-list items)
    (map (lambda (x) (* x x)) items))

(display (square-list (list 5 6 7 8)))
(newline)
;(25 36 49 64)

(display "==========Ex 2.22===========")
(newline)

(define (square x) (* x x))

(define (square-list items)
    (define (iter things answer)
        (if (null? things)
            answer
            (iter (cdr things)
                    (cons (square (car things))
                    answer))))
    (iter items (list)))
(display (square-list (list 5 6 7 8)))
(newline)
;(64 49 36 25)

(define (square-list items)
    (define (iter things answer)
        (if (null? things)
            answer
            (iter (cdr things)
                (cons answer
                (square (car things))))))
    (iter items (list)))
(display (square-list (list 5 6 7 8)))
(newline)
;((((() . 25) . 36) . 49) . 64)

; 前回も話した

(display "==========Ex 2.23===========")
(newline)


(define (for-each-orig f items)
    (cond 
        ((null? items) #t)
        (else 
            (f (car items))
            (for-each-orig f (cdr items)))))
        

;(define (for-each-orig f items)
;   (if
;       (null? items) 
;       #t
;       ((f (car items))
;        (for-each-orig f (cdr items)))))
     
; *** ERROR: invalid application: (#<undef> #t) なぜ～

(for-each-orig 
    (lambda (x)
            (newline)
            (display x))
    (list 57 321 88))

;57
;321
;88



        
