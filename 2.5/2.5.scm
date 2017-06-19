(load "./../2.4/2.4.scm")

;(load "./get-put.scm")
;(load "./tag.scm")
;; 2.5.1
(load "./../get-put.scm")

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
    (let ((type-tags (map type-tag args)))
        (let ((proc (get op type-tags)))
            ;(print "type: " type-tags)
            (if proc
                (apply proc (map contents args))
                ;; procがなくて　lengthが2のとき 
                (if (= (length args) 2)
                    (let 
                        (
                            (type1 (car type-tags))
                            (type2 (cadr type-tags))
                            (a1 (car args))
                            (a2 (cadr args))
                        )
                        (let 
                            (
                                (t1->t2 (get-coercion type1 type2))
                                (t2->t1 (get-coercion type2 type1))
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

;※使っていたget/putの実装が今回の問題を想定していなかったので、
;実装を修正


(print "---a")
(define s1 (make-scheme-number 5))
(define s2 (make-scheme-number 2))
(print (exp s1 s2)) ;25


(define c1 (make-complex-from-real-imag 5 1))
(define c2 (make-complex-from-real-imag 2 1))
;(print (exp c1 c2)) 
;(complex complex)に揃ったあと無限ループ。


(print "---b")
(define r1 (make-rational 2 3))
(print (add s1 c1)) ;○ できる
(print (add c1 c2)) ;○ できる

(print "---c")
(define (apply-generic op . args)
    (define (all el l)
        (cond
            ((null? l) #t)
            ((not (pair? l)) #f)
            ((eq? el (car l)) (all el (cdr l)))
        (else #f)))

    (let ((type-tags (map type-tag args)))
        (let ((proc (get op type-tags)))
            (print "type: " type-tags (all (car type-tags) type-tags))
            (cond 
                (proc
                    (apply proc (map contents args)))
                ((all (car type-tags) type-tags)
                    (error "No method for these types" (list op type-tags)))
                (if (= (length args) 2)
                    (let 
                        (
                            (type1 (car type-tags))
                            (type2 (cadr type-tags))
                            (a1 (car args))
                            (a2 (cadr args))
                        )
                        (let 
                            (
                                (t1->t2 (get-coercion type1 type2))
                                (t2->t1 (get-coercion type2 type1))
                            )

                            (cond 
                                (t1->t2 (apply-generic op (t1->t2 a1) a2))
                                (t2->t1 (apply-generic op a1 (t2->t1 a2)))
                            (else (error "No method for these types"
                                (list op type-tags ))))
                        )))
                (else
                    (error "No method for these types"
                                (list op type-tags ))))
        )))
                                

(define c1 (make-complex-from-real-imag 5 1))
(define c2 (make-complex-from-real-imag 2 1))
;(print (exp c1 c2)) 
;*** ERROR: No method for these types (exp (complex complex))

(print "===Ex2.82===")
(define (raise x) (apply-generic 'raise x))

(define (apply-generic op . args)
    (define (all el l)
        (cond
            ((null? l) #t)
            ((not (pair? l)) #f)
            ((eq? el (car l)) (all el (cdr l)))
        (else #f)))

    (define (any el l)
        (cond 
            ((null? l) #f)
            ((eq? el (car l)) #t)
        (else
            (any el (cdr l)))
        ))

    (define (_raise target)
        (lambda (x) 
            (if 
                (= (type-tag x) target)
                (target)
                (parent (raise target))
           )
        )
    )


    (define (apply-all list)
        (let
            ((f (_raise (car list))))
            (let 
                ((next (map f list)))
                    ;(print "next:" next)
                    (if
                        next
                        next
                        (apply-all (cdr list))
                    )
            )))

    (let ((type-tags (map type-tag args)))
        (let ((proc (get op type-tags)))
            ;(print "type: " type-tags " proc:" proc)
            (cond 
                (proc
                    (apply proc (map contents args)))
                ((all (car type-tags) type-tags) #f)
                ((any #f type-tags) #f)
                (else 
                    (apply-generic op (apply-all args))
                )
                

        )))
)



(print "===Ex2.83===")   
(install-real-number-package)
(define rr1 (make-real-number 1))
(print "real-number" rr1)

(print "scheme-raise:" (raise s1))
(print "rational-raise:" (raise r1))
(print "real-raise:" (raise rr1))
(print "complex-raise:" (raise c1))

;real-number(real-number . 1)
;scheme-raise:(rational 5 . 1)
;rational-raise:(real-number . 2/3)
;real-raise:(complex rectangular 1 . 0)
;complex-raise:#f

;; 2.82  
;(print (add s2 s1)) ;7
;(print (add s1 r1)) ;7


(print "===Ex2.84===")   
;練習問題 2.83の raise 演算を使って apply-generic⼿続きを修正して、
;この節で検討した通り、連続して “上げる” という⽅法によって
;引数が同じ型を持つよう強制型変換を⾏うようにせよ。
;⼆つの型のどちらがタワーの中で⾼い位置にあるかをテストする⽅法を考える必要がある。
;システムのほかの部分と “互換性がある” ようなやり⽅でこれを⾏い、
;タワーに新しい階を追加する際に問題を引き起こさないようにせよ。

(define (install-tree-depth-package)
    (put 'tree-depth '(scheme-number)
        (lambda (x) (+ 1 (tree-depth (raise x)))))

    (put 'tree-depth '(rational)
        (lambda (x) (+ 1 (tree-depth (raise x)))))

    (put 'tree-depth '(real-number)
        (lambda (x) (+ 1 (tree-depth (raise x)))))

    (put 'tree-depth '(complex)
        (lambda (x) 1))
'done)

; * apply-generic使うと、内部で再帰してしまう、、
(define (tree-depth x)
    ((get 'tree-depth (list (type-tag x))) x))

(install-tree-depth-package)
(print "complex tree-depth:" (tree-depth c1))
(print "real    tree-depth:" (tree-depth rr1))
(print "rational tree-depth:" (tree-depth r1))
(print "scheme  tree-depth:" (tree-depth s1))


(define (debug-raise . args)
    ;　全部リスト内がおんなじエレメント
    (define (all el l)
        (cond
            ((null? l) #t)
            ((not (pair? l)) #f)
            ((eq? el (car l)) (all el (cdr l)))
        (else #f)))

    ; リスト内でdepthの浅いtype
    (define (min-type args)
        (define (itr min-element lst)
            (if (null? lst)
                min-element
                (let 
                    (
                        (head (car lst))
                    )
                    (if (< (tree-depth head) (tree-depth min-element))
                        (itr (type-tag head) (cdr lst))
                        (itr min-element (cdr lst)))
                )))
        (itr (car args) (cdr args)))

    ; depthを1段階引き上げる
    (define (apply-raise list)
        (define (raise-to min-depth)
            (lambda (x)
                (if (= min-depth (tree-depth x))
                    x
                    (raise x)
                )))
        (let
            ((min-depth (tree-depth (car list))) )
            (map (raise-to min-depth) list)))

    (define (apply-raise-to-min args)
        (define (itr l)
            (if 
                (all (tree-depth (car l)) (map tree-depth l))
                l
                (itr (apply-raise l)))
            )
        (itr args)
    )

    (print "depth-map: " (map tree-depth args))
    (print "depth-disit-zip: " (map cons (map tree-depth args) args))
    (print "min-type: "(min-type args))
    (print "apply-raise: " (apply-raise args))
    (print "apply-raise-to-min: " (apply-raise-to-min args))
)

(debug-raise c1 rr1 r1 s1)
;depth-map: (1 2 3 4)
;depth-disit-zip: ((1 complex rectangular 5 . 1) (2 real-number . 1) (3 rational 2 . 3) (4 . 5))
;min-type: (complex rectangular 5 . 1)
;apply-raise: ((complex rectangular 5 . 1) (complex rectangular 1 . 0) (real-number . 2/3) (rational 5 . 1))
;apply-raise-to-min: ((complex rectangular 5 . 1) (complex rectangular 1 . 0) (complex rectangular 2/3 . 0) (complex rectangular 5 . 0))

(define (apply-generic op . args)
    (define (all el l)
        (cond
            ((null? l) #t)
            ((not (pair? l)) #f)
            ((eq? el (car l)) (all el (cdr l)))
        (else #f)))

    (define (min-type args)
        (define (itr min-element lst)
            (if (null? lst)
                min-element
                (let 
                    (
                        (head (car lst))
                    )
                    (if (< (tree-depth head) (tree-depth min-element))
                        (itr (type-tag head) (cdr lst))
                        (itr min-element (cdr lst)))
                )))
        (itr (car args) (cdr args)))

    (define (apply-raise list)
        (define (raise-to min-depth)
            (lambda (x)
                (if (= min-depth (tree-depth x))
                    x
                    (raise x)
                )))
        (let
            ((min-depth (tree-depth (car list))) )
            (map (raise-to min-depth) list)))

    (let ((type-tags (map type-tag args)))
        (let ((proc (get op type-tags)))
            (print "type: " type-tags (all (car type-tags) type-tags))
            (cond 
                ; procがある状態
                (proc
                    (apply proc (map contents args)))

                ; procが無いが、全ての型が揃っている
                ((all (car type-tags) type-tags)
                    (error "No method for these types" (list op type-tags)))



                (if (= (length args) 2)
                    (let 
                        (
                            (type1 (car type-tags))
                            (type2 (cadr type-tags))
                            (a1 (car args))
                            (a2 (cadr args))
                        )
                        (let 
                            (
                                (t1->t2 (get-coercion type1 type2))
                                (t2->t1 (get-coercion type2 type1))
                            )

                            (cond 
                                (t1->t2 (apply-generic op (t1->t2 a1) a2))
                                (t2->t1 (apply-generic op a1 (t2->t1 a2)))
                            (else (error "No method for these types"
                                (list op type-tags ))))
                        )))
                (else
                    (error "No method for these types"
                                (list op type-tags ))))
        )))
                                



