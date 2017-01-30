(define nil (list))
(define (variable? x) (symbol? x)) ;; 変数かどうか
(define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))



(define (make-sum a1 a2) (list '+ a1 a2))
(define (make-product m1 m2) (list '* m1 m2))

(print (make-sum 'a 'b))
;(+ a b)

;和は、最初の要素が記号 + であるリスト
(define (sum? x) (and (pair? x) (eq? (car x) '+)))

;加数は、和のリストの二つ目の項
(define (addend s) (cadr s))

;被加数は、和のリストの三つ目の項
(define (augend s) (caddr s)) ;; augentっては初耳

;積は、最初の要素が記号 * であるリスト
(define (product? x) (and (pair? x) (eq? (car x) '*)))

;乗数は、積のリストの二つ目の項
(define (multiplier p) (cadr p))

;被乗数は、積のリストの三つ目の項
(define (multiplicand p) (caddr p))

(define (deriv exp var)
    (cond 
        ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) 
            (make-sum (deriv (addend exp) var) (deriv (augend exp) var)))
        ((product? exp)
            (make-sum 
                (make-product (multiplier exp) (deriv (multiplicand exp) var))
                (make-product (deriv (multiplier exp) var)
            (multiplicand exp))))
    (else (error "unknown expression type: DERIV" exp))))



(print "===微分してみる===")
;;
(print (deriv '(+ x 3) 'x))
(print (deriv '(* x y) 'x))
(print (deriv '(* (* x y) (+ x 3)) 'x))
;(+ 1 0)
;(+ (* x 0) (* 1 y))
;(+ (* (* x y) (+ 1 0)) (* (+ (* x 0) (* 1 y)) (+ x 3)))


(print "===+0や *1をmerge===")
(define (=number? exp num) 
    (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
    (cond 
        ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
    (else (list '+ a1 a2))))

(define (make-product m1 m2)
    (cond 
        ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
    (else (list '* m1 m2))))

(print (deriv '(+ x 3) 'x))
(print (deriv '(* x y) 'x))
(print (deriv '(* (* x y) (+ x 3)) 'x))
;1
;y
;(+ (* x y) (* y (+ x 3)))

(print "===Ex 2.56===")
(define (base v) (cadr v))
(define (exponent v) (caddr v))

(define (make-exponentiation base exponent)
    (cond 
        ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        ((and (number? base) (number? exponent)) (expt base exponent))
        (else (list '** base exponent))))

(define (exponentiation? v)
    (and (pair? v) (eq? '** (car v))))

(print "make-exponentiation")
(print (make-exponentiation 3 0));1
(print (make-exponentiation 3 1));3
(print (make-exponentiation 3 2));9
(print (make-exponentiation 'x 2));(** x 2)

(print "===deriv===")
(define (deriv exp var)
    (cond 
        ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) 
            (make-sum (deriv (addend exp) var) (deriv (augend exp) var)))
        ((product? exp)
            (make-sum 
                (make-product (multiplier exp) (deriv (multiplicand exp) var))
                (make-product (deriv (multiplier exp) var) (multiplicand exp))))
        ((exponentiation? exp)
            (make-product
                (make-product 
                    (exponent exp) 
                    (make-exponentiation (base exp) (- (exponent exp) 1)))
                (deriv (base exp) var)
                ))
    (else (error "unknown expression type: DERIV" exp))))

(print "(deriv '(** x 2) 'x)")
(print (deriv '(** x 2) 'x))
;(* 2 x)

(print "===Ex 2.57===")

(print "cddrとかcaddrの復習")
(define sum3 '(+ 1 2 3))
(print (cddr sum3)) ;(2 3)
(print (cdddr sum3)) ;(3)

(print (car sum3)) ;+
(print (cadr sum3)) ;1
(print (caddr sum3)) ;2
 
(define (augend s)
    (if (null? (cdddr s)) 
        (caddr s) ; 項が1つしか無い時
        (cons '+ (cddr s)))) ; 項が２つ以上のとき

(define (multiplicand p)
  (if (null? (cdddr p))
      (caddr p) ; 項が1つしか無い時
      (cons '* (cddr p)))) ; 項が２つ以上のとき

(define (augend-test)
    (print "(augend '(+ 1 2 x)) = "
     (augend '(+ 1 2 x)))

    (print "(multiplicand '(* 1 2 x)) = "
     (multiplicand '(* 1 2 x)))
    #t
)
(augend-test)
;(augend '(+ 1 2 x)) = (+ 2 x)
;(multiplicand '(* 1 2 x)) = (* 2 x)


(print (deriv '(* (* x y) (+ x 3)) 'x))
; ((xy) * (x + 3))'
; = (xy * 1) + (y * (x + 3))
; = xy  + (y (x + 3))

;Result: (+ (* x y) (* y (+ x 3)))

; ================================================
(print "===2.58 - a===")
;(x + (3 * (x + (y + 2)))) のような中置記法で表された代
;数式を微分するにはどのようにするかを示せ。問題を簡単に
;するため、+ と * は常に二つの引数を取り、式は完全に括弧
;でくくられていると仮定せよ。



;和は、2つ目の要素が記号 + であるリスト
(define (sum? x) (and (pair? x) (eq? (cadr x) '+)))

;加数は、和のリストの1つ目の項
(define (addend s) (car s))

;(define (augend s) (caddr s)) 変更なし

;積は、2つ目の要素が記号 * であるリスト
(define (product? x) (and (pair? x) (eq? (cadr x) '*)))

;乗数は、積のリストの1つ目の項
(define (multiplier p) (car p))

;被乗数は、積のリストの3つ目の項
;(define (multiplicand p) (caddr p) 変更なし


(define (make-sum a1 a2)
    (cond 
        ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
    (else (list a1 '+ a2))))
(define (make-product m1 m2)
    (cond 
        ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
    (else (list m1 '* m2))))

(print "中置演算子")

(define s (make-sum 3 'x))
(print s)
(print "addend=" (addend s))
(print "augend=" (augend s))
;(3 + x)
;addend=3
;augend=x


(define p (make-product 3 'x))
(print p)
(print "multiplier=" (multiplier p))
(print "multiplicand=" (multiplicand p))
;(3 * x)
;multiplier=3
;multiplicand=x


(print "(deriv '(x + (3 * (x + (y + 2)))) 'x)=" 
    (deriv '(x + (3 * (x + (y + 2)))) 'x))

(print "(deriv '((x * y) * (x + 3)) 'x)=" (deriv '((x * y) * (x + 3)) 'x))
;(deriv '(x + (3 * (x + (y + 2)))) 'x)=4
;(deriv '((x * y) * (x + 3)) 'x)=((x * y) + (y * (x + 3)))

(print "===2.58 - b===")
;(x + 3 * (x + y + 2)) のような標準的な代数記法を認め
;ると、問題はずっと難しくなる。この記法では、必要のない
;括弧は省略し、乗算は加算より先に行われると仮定している。
;ここでの微分プログラムがそのような記法に対してもうまく
;動くように、適切な述語、セレクタ、コンストラクタを設計
;できるだろうか。

(define (make-sum . a)
    (define (itr a formula num)
        ;debug
        ;(print a formula num) 
        (cond 
            ((null? a) 
                (cond 
                    ((null? formula) num)
                    ((= num 0) formula)
                    (else (append formula (list '+ num)))
                ))
            ((number? (car a)) (itr (cdr a) formula (+ num (car a))))
            (else 
                (if 
                    (null? formula) 
                    (itr (cdr a) (list (car a)) num)
                    (itr (cdr a) (append formula (list '+ (car a))) num))
            )))
    (itr a nil 0))

(print (make-sum 1 3 4 'x 'y))
;(x + y + 8)

(define (make-product . a)
    (define (itr a formula num)
        ;debug
        ;(print a formula num) 
        (cond 
            ((null? a) 
                (cond 
                    ((null? formula) num)
                    ((= num 1) formula)
                    (else (append formula (list '* num)))
                ))
            ((number? (car a)) 
                (if (= (car a) 0)
                    0
                    (itr (cdr a) formula (* num (car a)))))
            (else 
                (if 
                    (null? formula) 
                    (itr (cdr a) (list (car a)) num)
                    (itr (cdr a) (append formula (list '* (car a))) num))
            )))
    (itr a nil 1))

(print (make-product 1 3 4 'x 'y))
;(x * y * 12)
(print (make-product 'x 'y 0))
;0

(print "セレクタの設定")
;和は、2つ目の要素が記号 + であるリスト
(define (sum? x) (and (pair? x) (eq? (cadr x) '+)))

;加数は、和のリストの1つ目の項
(define (addend s) (car s))

(define (augend s) 
    (if 
        (null? (cdddr s))
        (caddr s)
        (cddr s)))

;積は、2つ目の要素が記号 * であるリスト
(define (product? x) (and (pair? x) (eq? (cadr x) '*)))

;乗数は、積のリストの1つ目の項
(define (multiplier p) (car p))

;被乗数は、積のリストの3つ目の項
(define (multiplicand p) 
    (if 
        (null? (cdddr p))
        (caddr p)
        (cddr p)))


(define s (make-sum 3 'x 'y))
(print s)
(print "addend=" (addend s))
(print "augend=" (augend s))
(print "augendのaugend=" (augend (augend s)))
;(x + y + 3)
;addend=x
;augend=(y + 3)
;augendのaugend=3
(print "(make-sum 1 3 5)=" (make-sum 1 3 5))
(print "(make-sum 'x 'y)=" (make-sum 'x 'y))


(define p (make-product 3 'x 'y))
(print p)
(print "multiplier=" (multiplier p))
(print "multiplicand=" (multiplicand p))
(print "multiplicandのmultiprcand=" (multiplicand (multiplicand p)))
;(x * y * 3)
;multiplier=x
;multiplicand=(y * 3)
;multiplicandのmultiprcand=3
(print "(make-product 1 3 5)=" (make-product 1 3 5))
(print "(make-product 'x 'y)=" (make-product 'x 'y))
;(make-product 1 3 5)=15
;(make-product 'x 'y)=(x * y)

(print "---deriv: sum---")
(print (deriv (make-sum 'x 'y 3) 'x)) 
;1
(print (deriv (make-sum 'x 'x 'y 3) 'x)) 
;2

(print "---deriv: product---")
(print (deriv (make-product 'x 'y 3) 'x)) 
;(((y * 3)))

(print (deriv (make-product 'x 'x 'y 3) 'x)) 
;((x * (((y * 3)))) + ((x * y * 3)))


(print "=== 優先順序を考慮しよう")
;和は、どこかに＋があるやつ

(define test-mix '(a * b + c))
(define test-mix2 '(a + b * c))
(define test-sum '(a + b + c))
(define test-prod '(a * b * c))
(print test-mix)
(print test-mix2)
(print test-sum)
(print test-prod)

(define (sum? x) 
    (define (itr xx)
        ;(print xx (car xx))
        (cond 
            ((null? xx) #f)
            ((not (pair? xx)) #f)
            ((null? (cdr xx)) #f)
            ((eq? (cadr xx) '+) #t)
            (else (itr (cddr xx)))))
    (itr x))
(print "-sum?")
(print (sum? test-mix))
(print (sum? test-mix2))
(print (sum? test-sum))
(print (sum? test-prod))
;#t
;#t
;#t
;#f

;加数は、和のリストの1つ目の項
(define (addend s) 
    (define (itr ss formula)
        (cond
            ((eq? (car ss) '+) formula)
            (else 
                (itr (cdr ss) (append formula (list (car ss)))))
        ))
(let ((ans (itr s nil)))
    (if 
        (null? (cdr ans))
        (car ans)
    ans)))


(print "-addend")
(print (addend test-mix))
(print (addend test-mix2))
(print (addend test-sum))
;(a * b)
;a
;a

(define (augend s)
    (define (itr ss)
        (cond
            ((eq? (car ss) '+) (cdr ss))
            (else 
                (itr (cdr ss))
        )))
(let ((ans (itr s)))
    (if 
        (null? (cdr ans))
        (car ans)
    ans)))

(print "-augend")
(print (augend test-mix))
(print (augend test-mix2))
(print (augend test-sum))
;c
;(b * c)
;(b + c)

(print (product? test-mix))
(print (product? test-mix2))
(print (product? test-sum))
(print (product? test-prod))
;#t
;#f
;#f
;#t


(print "===deriv===")
(define (deriv exp var)
    ;(print exp "," var)
    (cond 
        ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) 
            (make-sum (deriv (addend exp) var) (deriv (augend exp) var)))
        ((product? exp)
            (make-sum 
                (make-product (multiplier exp) (deriv (multiplicand exp) var))
                (make-product (deriv (multiplier exp) var) (multiplicand exp))))
        ((exponentiation? exp)
            (make-product
                (make-product 
                    (exponent exp) 
                    (make-exponentiation (base exp) (- (exponent exp) 1)))
                (deriv (base exp) var)
                ))
    (else (error "unknown expression type: DERIV" exp))))

(print "---deriv: mix---")
(print (deriv '(x + y * 3 * x) 'x)) 
;(((y * 3)) + 1)

;冪乗 にはたいおうしてなひ