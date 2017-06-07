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

(load "./disit-packages.scm")

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

; polar , rectanglarをマップしてcomplexという型にしている
; そのため型に対するwrapperを準備してあげる必要がある
(magnitude com) 




(print "===Ex.2.78===")

(define (attach-tag type-tag contents)
    (if 
        (eq? type-tag 'scheme-number)
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
(define (equ? x y) (apply-generic 'equ? x y))

(print "--number equ?") 
(define num (make-scheme-number 5))
(define num2 (make-scheme-number 5))
(print (equ? num num2))

(print "--rational equ?")
(define rat (make-rational 1 2))
(define rat2 (make-rational 1 2))
(define rat3 (make-rational 2 4))

(print rat3)
(print (equ? rat rat2))
(print (equ? rat rat3))

(print "--complex equ?")
(define com (make-complex-from-mag-ang 3 4))
(define com2 (make-complex-from-mag-ang 3 4))
(print (equ? com com2))
;できれば直行形式でも検証

; どちらでも整数値になるような組み合わせがほしい・・・　TODO
(define (real-part com) (apply-generic 'real-part com))
(define (imag-part com) (apply-generic 'imag-part com))
(print (real-part com) (imag-part com))


(print "===Ex.2.80===")
(define (=zero? x) (apply-generic '=zero? x))
(define num0 (make-scheme-number 0))
(define rat0 (make-rational 0 5))
(define com0 (make-complex-from-mag-ang 0 0))

(print "zero")
(print (=zero? num0))
(print (=zero? rat0))
(print (=zero? com0))

(define com_non0 (make-complex-from-real-imag 0 10))
(print "not zero")
(print (=zero? num))
(print (=zero? rat))
(print (=zero? com))
(print (=zero? com_non0))


(print "===2.5.2===")

(load "./../coercion.scm")
(define (scheme-number->complex n)
    (make-complex-from-real-imag (contents n) 0))

(put-coercion 'scheme-number 'complex scheme-number->complex)

(define (apply-generic op . args)
    (print "@@@@@@@@@@@@use apply-generic")
    (let ((type-tags (map type-tag args)))
        (let ((proc (get op type-tags)))
            (if proc
                (apply proc (map contents args))

                #?=(if (= (length args) 2)
                    (let 
                        (
                            (type1 (car type-tags))
                            (type2 (cadr type-tags))
                            (a1 (car args))
                            (a2 (cadr args))
                        )
                        (let 
                            (
                                #?=(t1->t2 (get-coercion type1 type2))
                                #?=(t2->t1 (get-coercion type2 type1))
                            )
                            (cond 
                                (t1->t2 (apply-generic op (t1->t2 a1) a2))
                                (t2->t1 (apply-generic op a1 (t2->t1 a2)))
                            (else (error "No method for these types"
                                (list op type-tags ))))))
                            (error "No method for these types"
                                        (list op type-tags )))))))


;apply-generic ⼿続きは、次のように設計し直すことができます。
;それぞ れの型に対して、その型のオブジェクトをタワーの⼀階上に “上げる”raise という⼿続きを⽤意します。
;こうすると、システムが異なる型のオブジェクトの演算を⾏うよう求められたとき、
;すべてのオブジェクトが塔の同じ階に揃うようになるまで低い型を連続して上げていくということができるようになります


(print "===Ex2.81===")
;Louis Reasoner は、引数の型がすでに同じであっても、
;apply-generic は引数をお互いの型に強制型変換しようとしてもいいのではないかと気がついた。
;そのため、それぞれの型の引数をそれ⾃⾝の型に強制型変換 (coerce) する⼿続きを強制型変換テーブルに⼊れる必要があると彼は考えた。
;例えば、上に⽰した scheme-number->complex という強制型変換に加え、彼は次のことを⾏う。

(define (scheme-number->scheme-number n) n)
(define (complex->complex z) z)
(put-coercion 
    'scheme-number
    'scheme-number
    scheme-number->scheme-number)
(put-coercion 'complex 'complex complex->complex)


(define (exp x y) (apply-generic 'exp x y))

(print "---a")
(define s1 (make-scheme-number 5))
(define s2 (make-scheme-number 2))
(print (exp s1 s2)) ;25


(define c1 (make-complex-from-real-imag 5 1))
(define c2 (make-complex-from-real-imag 2 1))
;(print (exp c1 c2)) 
;*** ERROR: no such object: GET: (exp complex complex)
; うーーーん


(print "---b")
(define r1 (make-rational 2 3))
(print (add s1 c1))
;*** ERROR: no such object: GET: (add scheme-number complex)
; それはそう。
; なぜうごかない？？？



(define (apply-generic op . args)
    (print "@@@@@@@@@@@@use apply-generic")
    (let ((type-tags (map type-tag args)))
        (let ((proc (get op type-tags)))
            (if proc
                (apply proc (map contents args))

                #?=(if (= (length args) 2)
                    (let 
                        (
                            (type1 (car type-tags))
                            (type2 (cadr type-tags))
                            (a1 (car args))
                            (a2 (cadr args))
                        )
                        (let 
                            (
                                #?=(t1->t2 (get-coercion type1 type2))
                                #?=(t2->t1 (get-coercion type2 type1))
                            )
                            (cond 
                                (t1->t2 (apply-generic op (t1->t2 a1) a2))
                                (t2->t1 (apply-generic op a1 (t2->t1 a2)))
                            (else (error "No method for these types"
                                (list op type-tags ))))))
                            (error "No method for these types"
                                        (list op type-tags )))))))





