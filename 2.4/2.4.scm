
;実部と虚部
(make-from-real-imag (real-part z) (imag-part z))

; 絶対値と偏角
(make-from-mag-ang (magnitude z) (angle z)) 

(define (add-complex z1 z2)
    (make-from-real-imag 
        (+ (real-part z1) (real-part z2))
        (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
    (make-from-real-imag 
        (- (real-part z1) (real-part z2))
        (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
    (make-from-mag-ang 
        (* (magnitude z1) (magnitude z2))
        (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
    (make-from-mag-ang 
        (/ (magnitude z1) (magnitude z2))
        (- (angle z1) (angle z2))))


; 実部と虚部で実装
(define (real-part z) (car z))
(define (imag-part z) (cdr z))
(define (magnitude z)
    (sqrt (+ (square (real-part z))
    (square (imag-part z)))))

(define (angle z)
    (atan (imag-part z) (real-part z)))

(define (make-from-real-imag x y) (cons x y))
(define (make-from-mag-ang r a) (cons (* r (cos a)) (* r (sin a))))

; 絶対値と偏角で実装¬
(define (real-part z) (* (magnitude z) (cos (angle z))))
(define (imag-part z) (* (magnitude z) (sin (angle z))))
(define (magnitude z) (car z))
(define (angle z) (cdr z))
(define (make-from-real-imag x y)
(cons (sqrt (+ (square x) (square y)))
(atan y x)))
(define (make-from-mag-ang r a) (cons r a))


(print "===2.4.2===")
; タグ付きデータ

; Type tag とContents手続き
(define (attach-tag type-tag contents)
    (cons type-tag contents))

(define (type-tag datum)
    (if (pair? datum)
        (car datum)
        (error "Bad tagged datum: TYPE-TAG" datum)))

(define (contents datum)
    (if (pair? datum)
        (cdr datum)
        (error "Bad tagged datum: CONTENTS" datum))


; 直行形式と極形式を識別するタグ
(define (rectangular? z)
    (eq? (type-tag z) 'rectangular))

(define (polar? z) 
    (eq? (type-tag z) 'polar))


; 直行形式
(define (real-part-rectangular z) (car z))
(define (imag-part-rectangular z) (cdr z))
(define (magnitude-rectangular z)
    (sqrt (+ 
        (square (real-part-rectangular z))
        (square (imag-part-rectangular z)))))

(define (angle-rectangular z)
    (atan 
        (imag-part-rectangular z)
        (real-part-rectangular z)))

(define (make-from-real-imag-rectangular x y)
    (attach-tag 'rectangular (cons x y)))
(define (make-from-mag-ang-rectangular r a)
    (attach-tag 'rectangular (cons (* r (cos a)) (* r (sin a)))))

; 極形式
(define (real-part-polar z)
    (* (magnitude-polar z) (cos (angle-polar z))))
(define (imag-part-polar z)
    (* (magnitude-polar z) (sin (angle-polar z))))
(define (magnitude-polar z) (car z))
(define (angle-polar z) (cdr z))
(define (make-from-real-imag-polar x y)
    (attach-tag 'polar
        (cons (sqrt (+ (square x) (square y)))
        (atan y x))))
(define (make-from-mag-ang-polar r a)
    (attach-tag 'polar (cons r a)))

(define (make-from-real-imag x y)
    (make-from-real-imag-rectangular x y))


(print "===2.4.3===")
;このままだと、種類が増える度にそれに対応する関数分だけ関数&&名前が増える


(define (install-rectangular-package)
    ;; 内部手続き
    (define (real-part z) (car z))
    (define (imag-part z) (cdr z))
    (define (make-from-real-imag x y) (cons x y))
    (define (magnitude z)
        (sqrt (+ 
            (square (real-part z))
            (square (imag-part z)))))
    (define (angle z)
        (atan (imag-part z) (real-part z)))
    (define (make-from-mag-ang r a)
        (cons (* r (cos a)) (* r (sin a))))
    ;; システムのほかの部分とのインターフェイス
    (define (tag x) (attach-tag 'rectangular x))
    (put 'real-part '(rectangular) real-part)
    (put 'imag-part '(rectangular) imag-part)
    (put 'magnitude '(rectangular) magnitude)
    (put 'angle '(rectangular) angle)
    (put 'make-from-real-imag 'rectangular
    (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'rectangular
    (lambda (r a) (tag (make-from-mag-ang r a))))
'done)














