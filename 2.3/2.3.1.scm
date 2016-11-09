(print "===2.3記号データ===")
(print "===2.3.1クォート===")

(define a 1)
(define b 2)

(print (list a b))
(print (list 'a 'b))

(newline)

(print "--'(a b c)")
(define quote-list '(a b c))
(print (car quote-list))
(print (cdr quote-list))
;a  

(print "--空リスト")
(print '())

(print "--memq")
(define (memq item x)
    (cond 
        ((null? x) #f)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))
    ))

(print (memq 'apple '(pear banana prune)))
(print (memq 'apple '(x (apple sauce) y apple pear)))



(print "===Ex 2.53===")
(print (list 'a 'b 'c))                     ;(a b c)
(print (list (list 'george)))               ;((george))
(print (cdr '((x1 x2) (y1 y2))))            ;((y1 y2))
(print (cadr '((x1 x2) (y1 y2))))           ;(y1 y2)
(print (pair? (car '(a short list))))       ;#f
(print (memq 'red '((red shoes) (blue socks)))) ;#f
(print (memq 'red '(red shoes blue socks))) ;(red shoes blue socks)


(print "===Ex 2.54===")
; 再帰的なequal?を定義する
(define _equal? equal?)

(print "もとからあるequal?")
(print (equal? '(this is a list) '(this (is a) list)))

(print "自作のequal?")
(define (equal? a b)
    (cond 
        ((and (null? a) (null? b)) #t)
        ((or (null? a) (null? b)) #f)
        ((and (pair? (car a)) (pair? (car b)))
            (if 
                (equal? (car a) (car b))
                (equal? (cdr a) (cdr b))
                #f
            ))
        ((or (pair? (car a)) (pair? (car b))) #f)
        (else 
            (if 
                (eq? (car a) (car b))
                (equal? (cdr a) (cdr b))
                #f
            )
        )
    )
)

; これだと(equal? 'a 'a) でだめだった


(print "hioさんのみて書き直し")
(define (equal? a b)
    (cond 
        ((eq? a b) #t) ; 追加
        ((and (null? a) (null? b)) #t)
        ((or (null? a) (null? b)) #f)
        ((and (pair? (car a)) (pair? (car b)))
            (and (equal? (car a) (car b)) (equal? (cdr a) (cdr b))))
        ;((or (pair? (car a)) (pair? (car b))) #f) <= これはいらない。下のelseで吸収可能
        (else 
            (and (eq? (car a) (car b)) (equal? (cdr a) (cdr b))))
    )
)

(print (equal? '(this is a list) '(this (is a) list)))  ;#f
(print (equal? '(this is a list) '(this is a list)))    ;#t
(print (equal? (list (list 2 3) 'a) (list (list 2 3) 'b))) ;#f
(print (equal? 'a 'a))

(print "===Ex 2.55===")
(print (car ''abracadabra)) ; quote
; 'a => (quote a)

(print (quote a))                   ;a
(print (quote (quote a)))           ;'a
(print (car (quote (quote a))))     ;quote





