(load "./../2.4/2.4.scm")

;(load "./get-put.scm")
;(load "./tag.scm")
;; 2.5.1

(define registory '())

(define (put op type item)
    (let ((key (cons op type)))
        (let ((pair (cons key item)))
            (set! registory (cons pair registory)))))

(define (get op type)
    (define (iter key rest)
        (cond
            ((null? rest)
                (error "no such object: GET:" key))
            ((equal? key (car (car rest)))
                (cdr (car rest)))
            (else
                (iter key (cdr rest)))))
    (iter (cons op type) registory))

;; 演算手続き from 2.4
(define (apply-generic op . args)
    (let 
        ((type-tags (map type-tag args)))
            (let ((proc (get op type-tags)))
                (if proc
                    (apply proc (map contents args))
                (error
                    "No method for these types: APPLY-GENERIC"
                    (list op type-tags ))))))



(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))


;; はじめに整数・浮動小数の計算を定義
(define (install-scheme-number-package)
    (define (tag x) (attach-tag 'scheme-number x))
    (put 'add '(scheme-number scheme-number)
        (lambda (x y) (tag (+ x y))))
    (put 'sub '(scheme-number scheme-number)
        (lambda (x y) (tag (- x y))))
    (put 'mul '(scheme-number scheme-number)
        (lambda (x y) (tag (* x y))))
    (put 'div '(scheme-number scheme-number)
        (lambda (x y) (tag (/ x y))))
    (put 'epq? '(scheme-number scheme-number) =)
    (put 'make 'scheme-number (lambda (x) (tag x)))
'done)

(define (make-scheme-number n)
    ((get 'make 'scheme-number) n))


(define (install-rational-package)
    ;; 内部⼿続き
    (define (numer x) (car x))
    (define (denom x) (cdr x))
    (define (make-rat n d)
        (let ((g (gcd n d)))
            (cons (/ n g) (/ d g))))
    (define (add-rat x y)
        (make-rat 
            (+ 
                (* (numer x) (denom y))
                (* (numer y) (denom x))
            )
            (* (denom x) (denom y))))
    (define (sub-rat x y)
        (make-rat 
            (- (* (numer x) (denom y))
            (* (numer y) (denom x)))
            (* (denom x) (denom y))))

    (define (mul-rat x y)
        (make-rat 
            (* (numer x) (numer y))
            (* (denom x) (denom y))))
    (define (div-rat x y)
        (make-rat 
            (* (numer x) (denom y))
            (* (denom x) (numer y))))
    ;; インタフェース
    (define (tag x) (attach-tag 'rational x))
    (put 'add '(rational rational)
        (lambda (x y) (tag (add-rat x y))))
    (put 'sub '(rational rational)
        (lambda (x y) (tag (sub-rat x y))))
    (put 'mul '(rational rational)
        (lambda (x y) (tag (mul-rat x y))))
    (put 'div '(rational rational)
        (lambda (x y) (tag (div-rat x y))))
    (put 'epq? '(rational rational)
        (lambda (x y) 
            (= 
                (* (numer x) (denom y))
                (* (denom x) (numer y)))))
    (put 'make 'rational
        (lambda (n d) (tag (make-rat n d))))
'done)
(define (make-rational n d)
    ((get 'make 'rational) n d))


;; 2.4.3より 複素数の各パッケージをinstall
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

(define (install-polar-package)
;; 内部手続き
    (define (magnitude z) (car z))
    (define (angle z) (cdr z))
    (define (make-from-mag-ang r a) (cons r a))
    (define (real-part z) (* (magnitude z) (cos (angle z))))
    (define (imag-part z) (* (magnitude z) (sin (angle z))))
    (define (make-from-real-imag x y)
        (cons 
            (sqrt (+ (square x) (square y)))
            (atan y x)))

    ;; システムのほかの部分とのインターフェイス
    (define (tag x) (attach-tag 'polar x))
    (put 'real-part '(polar) real-part)
    (put 'imag-part '(polar) imag-part)
    (put 'magnitude '(polar) magnitude)
    (put 'angle '(polar) angle)
    (put 'make-from-real-imag 'polar
    (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'polar
    (lambda (r a) (tag (make-from-mag-ang r a))))
'done)


;; 複素数パッケージ
(define (install-complex-package)
    ;; 直交形式パッケージと極形式パッケージからインポートした⼿続き
    (define (make-from-real-imag x y)
        ((get 'make-from-real-imag 'rectangular) x y))
    (define (make-from-mag-ang r a)
        ((get 'make-from-mag-ang 'polar) r a))
    
    ;; 内部⼿続き
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
    ;; システムのほかの部分とのインターフェイス
    (define (tag z) (attach-tag 'complex z))
    (put 'add '(complex complex)
        (lambda (z1 z2) (tag (add-complex z1 z2))))
    (put 'sub '(complex complex)
        (lambda (z1 z2) (tag (sub-complex z1 z2))))
    (put 'mul '(complex complex)
        (lambda (z1 z2) (tag (mul-complex z1 z2))))
    (put 'div '(complex complex)
        (lambda (z1 z2) (tag (div-complex z1 z2))))

    ; Ex.2.77 
    ; polar , rectanglarをマップしてcomplexという型にしている
    ; そのため型に対するwrapperを準備してあげる必要がある
    (put 'real-part '(complex) real-part)
    (put 'imag-part '(complex) imag-part)
    (put 'magnitude '(complex) magnitude)
    (put 'angle '(complex) angle)

    (put 'epq? '(complex complex) ; Ex.2.79
        (lambda (z1 z2) 
            (and 
                (= (real-part z1) (real-part z2))
                (= (imag-part z1) (imag-part z2))
        )))
    (put 'make-from-real-imag 'complex
        (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'complex
        (lambda (r a) (tag (make-from-mag-ang r a))))
'done)

(define (make-complex-from-real-imag x y)
    ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
    ((get 'make-from-mag-ang 'complex) r a))


(install-scheme-number-package)
(install-rational-package)
(install-polar-package)
(install-rectangular-package)
(install-complex-package)

(define num (make-scheme-number 5))
(print num)

(define rat (make-rational 1 2))
(print rat)

(define com (make-complex-from-mag-ang 3 4))
(print com)

(print "===Ex2.77===")

; これはいける
(define com-mag (make-from-mag-ang 3 4))
(print com-mag)

;; 複素数パッケージ
(define (install-complex-package)
    ;; 直交形式パッケージと極形式パッケージからインポートした⼿続き
    (define (make-from-real-imag x y)
        ((get 'make-from-real-imag 'rectangular) x y))
    (define (make-from-mag-ang r a)
        ((get 'make-from-mag-ang 'polar) r a))
    
    ;; 内部⼿続き
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
    ;; システムのほかの部分とのインターフェイス
    (define (tag z) (attach-tag 'complex z))
    (put 'add '(complex complex)
        (lambda (z1 z2) (tag (add-complex z1 z2))))
    (put 'sub '(complex complex)
        (lambda (z1 z2) (tag (sub-complex z1 z2))))
    (put 'mul '(complex complex)
        (lambda (z1 z2) (tag (mul-complex z1 z2))))
    (put 'div '(complex complex)        
        (lambda (z1 z2) (tag (div-complex z1 z2))))

    (put 'real-part '(complex) real-part)
    (put 'imag-part '(complex) imag-part)
    (put 'magnitude '(complex) magnitude)
    (put 'angle '(complex) angle)

    (put 'make-from-real-imag 'complex
        (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'complex
        (lambda (r a) (tag (make-from-mag-ang r a))))
'done)

;(print (magnitude com))

(print "===Ex.2.78===")

(define (attach-tag type-tag contents)
    (if 
        (number? contents)
        contents
        (cons type-tag contents)))

(define (type-tag datum)
    (cond 
        ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
    (else 
        (error "Bad tagged datum: TYPE-TAG" datum))))

(define (contents datum)
    (cond 
        ((number? datum) datum)
        ((pair? datum) (cdr datum))
    (else
        (error "Bad tagged datum: CONTENTS" datum))))



(print "===Ex.2.79===")
(define (epq? x y) (apply-generic 'epq? x y))

(print "--number epq?")
(define num (make-scheme-number 5))
(define num2 (make-scheme-number 5))
(print (epq? num num2))

(print "--rational epq?")
(define rat (make-rational 1 2))
(define rat2 (make-rational 1 2))
(define rat3 (make-rational 2 4))
(print (epq? rat rat2))
(print (epq? rat rat3))

(print "--complex epq?")
(define com (make-complex-from-mag-ang 3 4))
(define com2 (make-complex-from-mag-ang 3 4))
(print (epq? com com2))
;できれば直行形式でも検証

; どちらでも整数値になるような組み合わせがほしい・・・　TODO
(define (real-part com) (apply-generic 'real-part com))
(define (imag-part com) (apply-generic 'imag-part com))
(print (real-part com) (imag-part com))


(print "===Ex.2.80===")
; =zero?











