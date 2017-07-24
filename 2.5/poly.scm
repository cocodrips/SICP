
; 多項式
(define (install-sparse-polynomical-package)
;; 内部⼿続き
    ;; poly の表現
    (define (make-poly variable term-list) (cons variable term-list))

    ; 基本
    (define (variable p) (car p))
    (define (term-list p) (cdr p))

    ;⟨2.3.2 節の same-variable? と variable? ⼿続き⟩
    (define (variable? x) (symbol? x))
    (define (same-variable? v1 v2)
        (and 
            (variable? v1)
            (variable? v2)
            (eq? v1 v2)))

    ;; システムのほかの部分とのインターフェイス
    (define (tag p) (attach-tag 'sparse-polynomial p))

    (put 'variable '(sparse-polynomial) variable)
    (put 'term-list '(sparse-polynomial) term-list)
    (put 'varibale '(sparse-polynomial) varibale)
    (put 'same-variable? '(sparse-polynomial sparse-polynomial)
            same-variable?)
    (put 'term-list '(sparse-polynomial) term-list)

    (put 'make 'sparse-polynomial
        (lambda (var terms) (tag (make-poly var terms))))

)

(define (make-sparse-polynomial var terms)
    ((get 'make 'sparse-polynomial) var terms))


;; 多項式パッケージのwrapper
(define (install-polynomial-package)

    (define (adjoin-term term term-list)
        (if 
            (=zero? (coeff term))
            term-list
            (cons term term-list)))
    (define (the-empty-termlist) '())
    (define (first-term term-list) (car term-list))
    (define (rest-terms term-list) (cdr term-list))
    (define (empty-termlist? term-list) (null? term-list))
    (define (make-term order coeff) (list order coeff))
    (define (order term) (car term))
    (define (coeff term) (cadr term))

    (define (add-terms L1 L2)
        (cond 
            ((empty-termlist? L1) L2)
            ((empty-termlist? L2) L1)
        (else
            (let 
                (
                    (t1 (first-term L1))
                    (t2 (first-term L2))
                )
                (cond 
                    ((> (order t1) (order t2))
                        (adjoin-term　t1 (add-terms (rest-terms L1) L2)))
                    ((< (order t1) (order t2))
                        (adjoin-term　t2 (add-terms L1 (rest-terms L2))))
                (else
                    (adjoin-term　(make-term (order t1)
                    (add (coeff t1) (coeff t2)))
                    (add-terms (rest-terms L1)
                    (rest-terms L2))))))
        )))

    (define (mul-terms L1 L2)
        (if 
            (empty-termlist? L1)
            (the-empty-termlist)
            (add-terms (mul-term-by-all-terms (first-term L1) L2)
                (mul-terms (rest-terms L1) L2))))

    (define (mul-term-by-all-terms t1 L)
        (if 
            (empty-termlist? L)
            (the-empty-termlist)
            (let 
                ((t2 (first-term L)))
                (adjoin-term
                    (make-term (+ (order t1) (order t2))
                    (mul (coeff t1) (coeff t2)))
                    (mul-term-by-all-terms t1 (rest-terms L))))))


    (define (add-poly p1 p2) 
        (if 
            (same-variable? (variable p1) (variable p2))
            (make-poly (variable p1)
                (add-terms (term-list p1) (term-list p2)))
            (error "Polys not in same var: ADD-POLY" (list p1 p2))))

    (define (mul-poly p1 p2)
        (if 
            (same-variable? (variable p1) (variable p2))
            (make-poly (variable p1)
                (mul-terms (term-list p1) (term-list p2)))
            (error "Polys not in same var: MUL-POLY" (list p1 p2))))

    ; Ex 2.87
    (define (=zero-poly? p) 
        (= 0 (fold-left + 0 (map coeff (term-list p))))
    )

    (define (negative p)
        (make-poly 
            (variable p) 
            (map list
                (map order (term-list p))
                (map (lambda (x) (mul -1 (coeff x))) (term-list p))))
    )

    ;; システムのほかの部分とのインターフェイス
    (define (tag p) (attach-tag 'polynomial p))
    (put 'add '(polynomial polynomial)
        (lambda (p1 p2) (tag (add-poly p1 p2))))

    (put 'sub '(polynomial polynomial) 
        (lambda (p1 p2) (tag (add-poly p1 (negative p2)))))

    (put 'mul '(polynomial polynomial)
        (lambda (p1 p2) (tag (mul-poly p1 p2))))
    
    (put '=zero? '(polynomial) =zero-poly?)

    (put 'make 'polynomial
        (lambda (var terms) (tag (make-poly var terms))))


'done)










