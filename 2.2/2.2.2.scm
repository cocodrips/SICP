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
;  *   
; / \
;1   *
;   / \
;   2  *
;     / \
;    3   4
(newline)

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


(display "==========Ex 2.26===========")
(newline)

(define x (list 1 2 3))
(define y (list 4 5 6))

(display (append x y)) (newline)    ;(1 2 3 4 5 6)      引数がlist
(display (cons x y)) (newline)      ;((1 2 3) 4 5 6)    引数なんでもいい
(display (list x y)) (newline)      ;((1 2 3) (4 5 6))　 引数なんでもいい



(display "==========Ex 2.27===========")
(newline)
(define x (list 1 2))
(define y (list 3 4))
(define xylist (list x y))

(display xylist)
(newline)

(define (deep-reverse items)
    (define (itr src dist)
        (display src)
        (display "|")
        (display dist)
        (newline)
        (cond 
            ((null? src) dist)
            ((pair? (car src)) (itr (cdr src) (cons (deep-reverse (car src)) dist)))
        (else   
            (itr (cdr src) (cons (car src) dist)))))
    (itr items (list)))

(display (deep-reverse xylist))
(newline)

;((1 2) (3 4))
;((1 2) (3 4))|()
;(1 2)|()
;(2)|(1)
;()|(2 1)
;((3 4))|((2 1))
;(3 4)|()
;(4)|(3)
;()|(4 3)
;()|((4 3) (2 1))
;((4 3) (2 1))

(display "==========Ex 2.28===========")
(newline)

(define (fringe items)
    (define (itr src dist)
        (display src)
        (display "|")
        (display dist)
        (newline)
        (cond 
            ((null? src) dist)
            ((list? (car src)) 
                (itr (cdr src) (itr (car src) dist)))
        (else
            (itr (cdr src) (cons (car src) dist)))))
    (reverse (itr items (list))))

(define x (list (list 1 2) (list 3 4)))
(display (fringe x))
(newline)

(display (fringe (list x x)))
(newline)

;((1 2) (3 4))|()
;(1 2)|()
;(2)|(1)
;()|(2 1)
;((3 4))|(2 1)
;(3 4)|(2 1)
;(4)|(3 2 1)
;()|(4 3 2 1)
;()|(4 3 2 1)
;(1 2 3 4)
;(((1 2) (3 4)) ((1 2) (3 4)))|()
;((1 2) (3 4))|()
;(1 2)|()
;(2)|(1)
;()|(2 1)
;((3 4))|(2 1)
;(3 4)|(2 1)
;(4)|(3 2 1)
;()|(4 3 2 1)
;()|(4 3 2 1)
;(((1 2) (3 4)))|(4 3 2 1)
;((1 2) (3 4))|(4 3 2 1)
;(1 2)|(4 3 2 1)
;(2)|(1 4 3 2 1)
;()|(2 1 4 3 2 1)
;((3 4))|(2 1 4 3 2 1)
;(3 4)|(2 1 4 3 2 1)
;(4)|(3 2 1 4 3 2 1)
;()|(4 3 2 1 4 3 2 1)
;()|(4 3 2 1 4 3 2 1)
;()|(4 3 2 1 4 3 2 1)
;(1 2 3 4 1 2 3 4)


(display "==========Ex 2.29===========")
(newline)

; 二枝モビール
; a.
(define (make-mobile left right)
    (list left right))

(define (left-branch mobile)
    (car mobile))

(define (right-branch mobile)
    (car (cdr mobile)))

(define (make-branch length structure)
    (list length structure))

(display "a.")
(newline)

(define mobile2 (make-mobile (make-branch 1 1) (make-branch 2 2)))

(define m (make-mobile (make-branch 1 3) (make-branch 3 mobile2)))
(display "Base:")
(display m)
(newline)

(display "left: ")
(display (left-branch m))
(newline)
(define left (left-branch m))

(display "right: ")
(display (right-branch m))
(newline)

(define (branch-length branch)
    (car branch))

(define (branch-structure branch)
    (cdr branch))

(display "length: ")
(display (branch-length m))
(newline)

(display "structure: ")
(display (branch-structure m))
(newline)


; b.
(display "b.")
(newline)

(define (total-weight mobile)
    (cond (list? mobile)
        (+ (total-weight (branch-structure (left-branch mobile))) (branch-structure (total-weight (right-branch mobile))))
        (branch-structure mobile)))

(define m (make-mobile (make-branch 3 5) (make-branch 1 mobile2)))
(display "Base:")
(display m)
(newline)
(display "Weight:")
(display (total-weight m))
(newline)

;b.
;Base:((3 5) 2)
;Weight:10

(display "c.")
(newline)

(define (balance? mobile)
    (= (total-weight (left-branch mobile)) (total-weight (right-branch mobile))))

(display m)
(display ":")
(display (balance? m)) 
(newline)

(define m (make-mobile (make-mobile 3 5) (make-mobile 2 6)))
(display m)
(display ":")
(display (balance? m)) 
(newline)
;c.
;((3 5) 2):#f
;((3 5) (2 6)):#t

(display "d.")
(newline)

(define (make-mobile left right) (cons left right))
(define (make-branch length structure)
    (cons length structure))

(define m (make-mobile 1 (make-mobile 2 3)))
(display m)
(newline)

;; 途中



;(display (pair? (cons 1 2))) (newline) ;#t
;(display (list? (cons 1 2))) (newline) ;#f




;(display "==========Scele tree===========")
;(newline)
;(define (scale-tree tree factor)
;    (cond 
;        ((null? tree) ())
;        ((not (pair? tree)) (* tree factor))
;        (else (cons (scale-tree (car tree) factor)
;                    (scale-tree (cdr tree) factor))))
;)

;(display (scale-tree (list 1 (list 2) 3) 10))
;(newline)
;(define (scale-tree tree factor)
;    (map (lambda (sub-tree)
;        (if (pair? sub-tree)
;            (scale-tree sub-tree factor)
;            (* sub-tree factor)))
;    tree)
;)



;;; ex 2.30
;(display "==========Ex 2.30===========")
;(newline)

;;; 普通に
;(define (square-tree tree)
;    (cond 
;        ((null? tree) ())
;        ((not (pair? tree)) (* tree tree))
;        (else (cons (square-tree (car tree)) (square-tree (cdr tree)))))
;)

;(display (square-tree (list 1 (list 2) 3)))
;(newline)
;;(1 (4) 9)

;;; map
;(define (square-tree tree)
;    (map (lambda (sub-tree)
;        (if (pair? sub-tree)
;            (square-tree sub-tree)
;            (* sub-tree sub-tree)))
;    tree)
;)
;(display (square-tree (list 1 (list 2) 3)))
;(newline)
;;(1 (4) 9)



;(display "==========Ex 2.31===========")
;(newline)
;(define (tree-map tree f)
;    (map (lambda (sub-tree)
;        (if (pair? sub-tree)
;            (tree-map sub-tree f)
;            (f sub-tree)))
;    tree)
;)
;(display (tree-map (list 1 (list 5) 7) (lambda (x) (* x x))))
;(newline)
;;(1 (25) 49)


;(display "==========Ex 2.32===========")
;(newline)
;(define (subsets s)
;    (if (null? s)
;        (list ())
;        (let 
;            ((rest (subsets (cdr s))))
;            (append rest (map (lambda (x) (cons x (car s))) rest)))))
;(display (subsets (list 1 4 7)))
;(newline)


;(1) -> () (1)
;(1 2 3)         : ()
;        (2 3)   : () (1)
;        (3)     : () (1) (2) (1 2)
;        ()      : () (1) (2) (1 2) (3) (1 3) (2 3) (1 2 3)
