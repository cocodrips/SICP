
;実部と虚部
;(make-from-real-imag (real-part z) (imag-part z))

; 絶対値と偏角
;(make-from-mag-ang (magnitude z) (angle z)) 

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
        (error "Bad tagged datum: CONTENTS" datum)))


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


; それぞれのアクセッサーが、タグを確認する
(define (real-part z)
    (cond 
        ((rectangular? z) (real-part-rectangular (contents z)))
        ((polar? z) (real-part-polar (contents z)))
    (else (error "Unknown type: REAL-PART" z))))

(define (imag-part z)
    (cond 
        ((rectangular? z) (imag-part-rectangular (contents z)))
        ((polar? z) (imag-part-polar (contents z)))
    (else (error "Unknown type: IMAG-PART" z))))

(define (magnitude z)
    (cond 
        ((rectangular? z) (magnitude-rectangular (contents z)))
        ((polar? z) (magnitude-polar (contents z)))
    (else (error "Unknown type: MAGNITUDE" z))))

(define (angle z)
    (cond 
        ((rectangular? z) (angle-rectangular (contents z)))
        ((polar? z) (angle-polar (contents z)))
    (else (error "Unknown type: ANGLE" z))))


; addとかはそのまま使える
(define (add-complex z1 z2)
    (make-from-real-imag 
        (+ (real-part z1) (real-part z2))
        (+ (imag-part z1) (imag-part z2))))


(define (make-from-real-imag x y)
    (make-from-real-imag-rectangular x y))

(define (make-from-mag-ang r a)
    (make-from-mag-ang-polar r a))


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


;; 演算手続き
(define (apply-generic op . args)
    (let 
        ((type-tags (map type-tag args)))
            (let ((proc (get op type-tags)))
                (if proc
                    (apply proc (map contents args))
                (error
                    "No method for these types: APPLY-GENERIC"
                    (list op type-tags ))))))

; apply-generic を使うと、複素数演算のジェネリックセレクタは以下のように定義でき、
; 新しい表現が増えてもここがかわることはない
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))




(print "===Ex 2.73===")
; 必要そうなのもってきた
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (=number? exp num) 
    (and (number? exp) (= exp num)))


;問題文中
(define (deriv exp var)
    (cond 
        ((number? exp) 0)
        ((variable? exp) 
            (if (same-variable? exp var) 1 0))
        ((sum? exp)
            (make-sum 
                (deriv (addend exp) var)
                (deriv (augend exp) var)))
        ((product? exp)
            (make-sum 
                (make-product 
                    (multiplier exp)
                    (deriv (multiplicand exp) var))
                (make-product

                    (deriv (multiplier exp) var)
                    (multiplicand exp))))
        ;⟨more rules can be added here⟩
        (else (error "unknown expression type: DERIV" exp))))
;このプログラムは、微分する式の型によってディスパッチを実行
;していると捉えることもできる。この場合、データの “タイプタグ” は代数演算記号 (+ など) で、行う演算は deriv ということになる。
;基本的な微分を行う手続きを次のように書き直すと、プログラムをデータ主導スタイルに変形できる。


(define (deriv exp var)
    (cond 
        ((number? exp) 0)
        ((variable? exp) 
            (if (same-variable? exp var) 1 0))
    (else 
        ((get 'deriv (operator exp)) (operands exp) var))))

(define (operator exp) (car exp)) 
(define (operands exp) (cdr exp))

; put/get の実装 by hioさん
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

;a. 上で何をしているか説明せよ。手続き number? と variable?は、
;   なぜデータ主導ディスパッチとして取り込むことができないのだろうか。


;b.
;和の実装
;((sum? exp)
;    (make-sum 
;        (deriv (addend exp) var)
;        (deriv (augend exp) var)))
(define (install-sum-package)

    (define (addend s) (car s))
    (define (augend s) (cadr s))
    (define (make-sum a1 a2) ;;2.3.2あたり
        (cond 
            ((=number? a1 0) a2)
            ((=number? a2 0) a1)
            ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))
    (define (deriv-sum exp var)
        (make-sum 
        (deriv (addend exp) var)
        (deriv (augend exp) var)))

    ;; インタフェース
    (put 'make '+ make-sum)
    (put 'deriv '+ deriv-sum)
'done)

;((product? exp)
;    (make-sum 
;        (make-product 
;            (multiplier exp)
;            (deriv (multiplicand exp) var))
;        (make-product
;            (deriv (multiplier exp) var)
;            (multiplicand exp))))
(define (install-product-package)
    ; install-sum-packageに依存している
    (define (multiplier s) (car s))
    (define (multiplicand s) (cadr s))
    (define (make-product m1 m2)
        (cond 
            ((or (=number? m1 0) (=number? m2 0)) 0)
            ((=number? m1 1) m2)
            ((=number? m2 1) m1)
            ((and (number? m1) (number? m2)) (* m1 m2))
        (else 
            (list '* m1 m2))))
    (define (deriv-product exp var)
        (print "multiplier" (multiplier exp))
        (print "multiplicand" (multiplicand exp))
        ((get 'make '+)
            (make-product 
                (multiplier exp)
                (deriv (multiplicand exp) var))
            (make-product
                (deriv (multiplier exp) var)
                (multiplicand exp))))
    ; インタフェース
    (put 'make '* make-product)
    (put 'deriv '* deriv-product)
'done)


;; install
(install-sum-package)
(install-product-package)

;; test
(print ((get 'make '+) 1 3)) ; 4
(define prod-sample ((get 'make '*) 'x 'y))
(print (deriv prod-sample 'x)) ;y



;; c. 指数微分を組み込む
(define (install-exponentiation-package)
    ; install-sum-packageに依存している
    (define (base v) (car v))
    (define (exponent v) (cadr v))
    (define (make-exponentiation base exponent)
        (cond 
            ((=number? exponent 0) 1)
            ((=number? exponent 1) base)
            ((and (number? base) (number? exponent)) (expt base exponent))
        (else (list '** base exponent))))
    (define (exponentiation? v)
        (and (pair? v) (eq? '** (car v))))

    (define (deriv-exponentiation exp var)
        ((get 'make '*)
                ((get 'make '*) 
                    (exponent exp) 
                    (make-exponentiation (base exp) (- (exponent exp) 1)))
                (deriv (base exp) var)))
    ; インタフェース
    (put 'make '** make-exponentiation)
    (put 'deriv '** deriv-exponentiation)
'done)

(print "c: exponentiation")
(install-exponentiation-package)
(print (deriv '(** x 2) 'x))




;; 2.74
(print "==Ex 2.74==")
(define nil '())
;; サンプルデータ
;; A社
(define (make-employee-record-A name salary address)
    (list name salary address))
(define A-employees 
    (list 
        (make-employee-record-A "a1" 500 1)
        (make-employee-record-A "a2" 600 2)
    )
)

;;  B社
(define (make-employee-record-B name address salary)
    (list name address salary))
(define B-employees 
    (list 
        (make-employee-record-B "b1" 11 700)
        (make-employee-record-B "b2" 12 400)
    )
)

(print "A-employees: " A-employees)
(print "B-employees: " B-employees)

;; 1. 事業所タグの定義
(define offices (list 'A 'B))

;;  2.各要素のgetterを定義
(define (install-A-data)
    (define (get-name record)
        (car record))
    (define (get-salary record)
        (cadr record))

    (put 'get-name 'A get-name)
    (put 'get-salary 'A get-salary)
'done)

(define (install-B-data)
    (define (get-name record)
        (car record))
    (define (get-salary record)
        (caddr record))

    (put 'get-name 'B get-name)
    (put 'get-salary 'B get-salary)
'done)


;; a. 任意の事業所を指定する get-record 手続きを本部向けに実装せよ。
(print "--a--")
(install-A-data)
(install-B-data)
(define (get-record data tag name)
    (if 
        (null? data)
        nil
        (let 
            ((record (car data)))
            (if 
                (string=? ((get 'get-name tag) record) name)
                (attach-tag tag record)
                (get-record (cdr data) tag name))
        )
    )
)

(print "get-record: " (get-record A-employees 'A "a1"))
(print "get-record: " (get-record B-employees 'B "a1"))


;; b. 任意の事業所の人事ファイル内の与えられた職員のレコードから給与情報を返す get-salary 手続きを本部向けに実装せよ。
(print "--b--")
(define (get-salary record)
    ((get 'get-salary (type-tag record)) (contents record))
)

(define record-A (get-record A-employees 'A "a1"))
(print "get-salary A: " (get-salary record-A))

(define record-B (get-record B-employees 'B "b1"))
(print "get-salary B: " (get-salary record-B))


;; c. find-employee-record
;; 事業所リストを作る
(print "--c--")
(define office-employees
    (list
        (list 'A A-employees)
        (list 'B B-employees)
    )
)

(define (find-employee-record name office-employees-list)
    (if 
        (null? office-employees-list)
        nil
        (let 
            ((data (car office-employees-list)))
            (let 
                ((record (get-record (cadr data) (car data) name)))
                (if 
                    (null? record)
                    (find-employee-record name (cdr office-employees-list))
                    record
                )
            )
        )
    )
)
(print "find-employee-record: " (find-employee-record "b1" office-employees))

; メッセージパッシング
(define (make-from-real-imag x y)
    (define (dispatch op)
        (cond
            ((eq? op 'real-part) x)
            ((eq? op 'imag-part) y)
            ((eq? op 'magnitude) (sqrt (+ (square x) (square y))))
            ((eq? op 'angle) (atan y x))
        (else (error "Unknown op: MAKE-FROM-REAL-IMAG" op))))
dispatch)

(define (apply-generic op obj) (obj op))

(print "==Ex.2.75")
;コンストラクタ make-from-mag-ang をメッセージパッシングスタイルで実装せよ。
(define (make-from-mag-ang r a) 
    (define (dispatch op)
        (cond
            ((eq? op 'real-part) (* r (cos a)))
            ((eq? op 'imag-part) (* r (sin a)))
            ((eq? op 'magnitude) r)
            ((eq? op 'angle) a)
        (else (error "Unknown op: MAKE-FROM-REAL-IMAG" op)))
    )
dispatch)

(define mag-ang-obj1 (make-from-mag-ang 10 5))
(print (apply-generic 'real-part mag-ang-obj1))
(print (apply-generic 'magnitude mag-ang-obj1))

(define mag-ang-obj1 (make-from-mag-ang 1 2))
(print (apply-generic 'real-part mag-ang-obj1))
(print (apply-generic 'magnitude mag-ang-obj1))


