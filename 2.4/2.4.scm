
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





















