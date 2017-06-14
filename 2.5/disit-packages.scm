
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

    ;;;;;;;;;;;;; Ex 2.79
    (put 'equ? '(scheme-number scheme-number) =)

    ;;;;;;;;;;;;; Ex 2.80
    (put '=zero? '(scheme-number)
        (lambda (x) (= x 0)))
    
    ;;;;;;;;;;;;; Ex 2.81
    (put 'exp '(scheme-number scheme-number)
        (lambda (x y) (tag (expt x y)))) 

    ;;;;;;;;;;;;; Ex 2.83
    (put 'raise '(scheme-number)
        (lambda (x) (make-rational x 1))
    )

    ;;;;;;;;;;;;; Ex 2.84
    (put 'raise '(scheme-number)
        (lambda (x) (make-rational x 1))
    )    

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

    ;;;;;;;;;;;;; Ex 2.79 
    (put 'equ? '(rational rational) 
        (lambda (x y) 
            (= 
                (* (numer x) (denom y))
                (* (denom x) (numer y)))))
    ;;;;;;;;;;;;; Ex 2.80
    (put '=zero? '(rational)
        (lambda (x) (= (numer x) 0)))

    ;;;;;;;;;;;;; Ex 2.83
    (put 'raise '(rational)
        (lambda (x) (make-real-number (/ (numer x) (denom x))))
    )

    (put 'make 'rational
        (lambda (n d) (tag (make-rat n d))))
'done)
(define (make-rational n d)
    ((get 'make 'rational) n d))


;; 実数 2.83
(define (install-real-number-package)
    (define (tag x) (attach-tag 'real-number x))
    (put 'add '(real-number real-number)
        (lambda (x y) (tag (+ x y))))
    (put 'sub '(real-number real-number)
        (lambda (x y) (tag (- x y))))
    (put 'mul '(real-number real-number)
        (lambda (x y) (tag (* x y))))
    (put 'div '(real-number real-number)
        (lambda (x y) (tag (/ x y))))

    ;;;;;;;;;;;;; Ex 2.79
    (put 'equ? '(real-number real-number) =)

    ;;;;;;;;;;;;; Ex 2.80
    (put '=zero? '(real-number)
        (lambda (x) (= x 0)))
    
    ;;;;;;;;;;;;; Ex 2.81
    (put 'exp '(real-number real-number)
        (lambda (x y) (tag (expt x y)))) 

    ;;;;;;;;;;;;; Ex 2.83
    (put 'raise '(real-number)
        (lambda (x) (make-complex-from-real-imag x 0))
    )

    (put 'make 'real-number (lambda (x) (tag x)))

'done)

(define (make-real-number n)
    ((get 'make 'real-number) n))


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

    ;;;;;;;;;;;;; Ex 2.77
    ; polar , rectanglarをマップしてcomplexという型にしている
    ; そのため型に対するwrapperを準備してあげる必要がある
    (put 'real-part '(complex) real-part)
    (put 'imag-part '(complex) imag-part)
    (put 'magnitude '(complex) magnitude)
    (put 'angle '(complex) angle)
    
    ;;;;;;;;;;;;; Ex 2.79
    (put 'equ? '(complex complex) 
        (lambda (z1 z2) 
            (and 
                (= (real-part z1) (real-part z2))
                (= (imag-part z1) (imag-part z2))
        )))

    ;;;;;;;;;;;;; Ex 2.80
    (put '=zero? '(complex)
        (lambda (x) 
            (and 
                (= (real-part x) 0) 
                (= (imag-part x) 0)) ))

    ;; こっちでもいい
    ;(put '=zero? '(complex)
    ;(lambda (x) 
    ;    (= (magnitude x) 0)

    ;;;;;;;;;;;;; Ex 2.83
    (put 'raise '(complex) #f)

    (put 'make-from-real-imag 'complex
        (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'complex
        (lambda (r a) (tag (make-from-mag-ang r a))))
'done)

(define (make-complex-from-real-imag x y)
    ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
    ((get 'make-from-mag-ang 'complex) r a))









