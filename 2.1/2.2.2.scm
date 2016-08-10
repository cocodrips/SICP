(display "*********2.2.2**********")
(newline)

(define linked-2-list (cons (list 1 2) (list 3 4)))
(display linked-2-list) 
;((1 2) 3 4)
(newline)

(define (count-leaves x)
    (cond 
        ((null? x) 0) 
        ((not (pair? x)) 1)
    (else 
        (+ 
            (count-leaves (car x))
            (count-leaves (cdr x))))))

(display (count-leaves linked-2-list))
(newline)
;4

(display "==========Ex 2.24===========")
(newline)

(display (list 1 (list 2 (list 3 4))))
(newline)
(display " 1") (newline)
(display " |") (newline)
(display " 2") (newline)
(display "/\\") (newline)
(display "3 4") (newline)

;(1 (2 (3 4)))
; 1
; |
; 2
;/\
;3 4

(display "==========Ex 2.25===========")
(newline)
; 7を取り出す
(define l (list 1 3 (list 5 7) 9))
(display l) (newline)
(display (cdr (car (cdr (cdr l))))) (newline)
;(1 3 (5 7) 9)
;(7)
(define l (list (list 7)))
(display l) (newline)
(display (car l)) (newline)
;((7))
;(7)

(define l (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
(display l) (newline)
(display (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr l))))))))))))
(newline)
;(1 (2 (3 (4 (5 (6 7))))))
;(7)