;; 
(define (install-sparse-polynomial-package)
    ;; 内部⼿続き
    ;; poly の表現
    (define (make-poly variable term-list) (cons variable term-list))
    (define (variable p) (car p))
    (define (term-list p) (cdr p))

    ;⟨2.3.2 節の same-variable? と variable? ⼿続き⟩
    (define (variable? x) (symbol? x))
    (define (same-variable? v1 v2)
        (and 
            (variable? v1)
            (variable? v2)
            (eq? v1 v2)))
    ;; 項と項リストの表現
    ;⟨下記の adjoin-term . . . coeff ⼿続き
    ;(define (adjoin-term term term-list)
    ;    (if 
    ;        (=zero? (coeff term))
    ;        term-list
    ;        (cons term term-list)))
    ;(define (the-empty-termlist) '())
    ;(define (first-term term-list) (car term-list))
    ;(define (rest-terms term-list) (cdr term-list))

    ;(define (make-term order coeff) (list order coeff))
    ;(define (order term) (car term))
    ;(define (coeff term) (cadr term))

    ;(define (add-terms L1 L2)
    ;    (cond 
    ;        ((empty-termlist? L1) L2)
    ;        ((empty-termlist? L2) L1)
    ;    (else
    ;        (let 
    ;            (
    ;                (t1 (first-term L1))
    ;                (t2 (first-term L2))
    ;            )
    ;            (cond 
    ;                ((> (order t1) (order t2))
    ;                    (adjoin-term　t1 (add-terms (rest-terms L1) L2)))
    ;                ((< (order t1) (order t2))
    ;                    (adjoin-term　t2 (add-terms L1 (rest-terms L2))))
    ;            (else
    ;                (adjoin-term　(make-term (order t1)
    ;                (add (coeff t1) (coeff t2)))
    ;                (add-terms (rest-terms L1)
    ;                (rest-terms L2))))))
    ;    )))

    ;(define (mul-terms L1 L2)
    ;    (if 
    ;        (empty-termlist? L1)
    ;        (the-empty-termlist)
    ;        (add-terms (mul-term-by-all-terms (first-term L1) L2)
    ;            (mul-terms (rest-terms L1) L2))))

    ;(define (mul-term-by-all-terms t1 L)
    ;    (if 
    ;        (empty-termlist? L)
    ;        (the-empty-termlist)
    ;        (let 
    ;            ((t2 (first-term L)))
    ;            (adjoin-term
    ;                (make-term (+ (order t1) (order t2))
    ;                (mul (coeff t1) (coeff t2)))
    ;                (mul-term-by-all-terms t1 (rest-terms L))))))


    ;(define (add-poly p1 p2) 
    ;    (if 
    ;        (same-variable? (variable p1) (variable p2))
    ;        (make-poly (variable p1)
    ;            (add-terms (term-list p1) (term-list p2)))
    ;        (error "Polys not in same var: ADD-POLY" (list p1 p2))))

    ;(define (mul-poly p1 p2)
    ;    (if 
    ;        (same-variable? (variable p1) (variable p2))
    ;        (make-poly (variable p1)
    ;            (mul-terms (term-list p1) (term-list p2)))
    ;        (error "Polys not in same var: MUL-POLY" (list p1 p2))))

    ;; Ex 2.87
    ;(define (=zero-poly? p) 
    ;    (= 0 (fold-left + 0 (map coeff (term-list p))))
    ;)

    ;(define (negative p)
    ;    (make-poly 
    ;        (variable p) 
    ;        (map list
    ;            (map order (term-list p))
    ;            (map (lambda (x) (mul -1 (coeff x))) (term-list p))))
    ;)

    ;;; システムのほかの部分とのインターフェイス
    (define (tag p) (attach-tag 'polynomial-sparse p))
    (put 'term-list 'polynomial-sparse 
        (lambda (p) 
            (print "sparse-term-list")
            (term-list p)))

    ;(put 'add '(polynomial-sparse polynomial-sparse)
    ;    (lambda (p1 p2) (tag (add-poly p1 p2))))

    ;(put 'sub '(polynomial-sparse polynomial-sparse) 
    ;    (lambda (p1 p2) (tag (add-poly p1 (negative p2)))))

    ;(put 'mul '(polynomial-sparse polynomial-sparse)
    ;    (lambda (p1 p2) (tag (mul-poly p1 p2))))
    
    ;(put '=zero? '(polynomial-sparse) =zero-poly?)

    (put 'make 'polynomial-sparse
        (lambda (var terms) (tag (make-poly var terms))))

    ;(put 'variable '(polynomial-sparse) variable)
    (put 'term-list '(polynomial-sparse) term-list)
    (put 'make-poly '(polynomial-sparse) make-poly)
    ;(put 'variavle? '(polynomial-sparse) variable?)
    ;(put 'same-variavle? '(polynomial-sparse polynomial-sparse) same-variable?)

'done)


(define (install-dense-polynomial-package)
    ;; 内部⼿続き
    (define (coeff-to-term coeff-list)
        (define (itr i dst lst)
            (if (null? lst)
                dst
            (itr (+ i 1) (cons (list i (car lst)) dst) (cdr lst))))
        (itr 0 '() coeff-list))

    ;; poly の表現
    (define (make-poly variable coeff-list) 
        (cons variable coeff-list))


    ;(define (variable p) (car p))
    ;(define (coeff-list p) (cdr p))
    (define (term-list p) 
        (define (expantion i lst dst)
            (if 
                (null? lst)
                dst
                (expantion (+ i 1) (cdr lst) (cons (list i (car lst)) dst))
            ))
        (expantion 0 (cdr p) '())
    )
    (define (empty-termlist? term-list) (null? term-list))

    ;;⟨2.3.2 節の same-variable? と variable? ⼿続き⟩
    ;(define (variable? x) (symbol? x))
    ;(define (same-variable? v1 v2)
    ;    (and 
    ;        (variable? v1)
    ;        (variable? v2)
    ;        (eq? v1 v2)))
    ;;;; 項と項リストの表現
    ;;⟨下記の adjoin-term . . . coeff ⼿続き
    ;(define (the-empty-termlist) '())
    ;(define (first-term term-list) (car term-list))
    ;(define (rest-terms term-list) (cdr term-list))
    ;(define (empty-termlist? term-list) (null? term-list))
    ;(define (make-term order coeff) (list order coeff))
    ;(define (order term) (car term))
    ;(define (coeff term) (cadr term))

    ;(define (add-terms L1 L2)
    ;    (cond 
    ;        ((empty-termlist? L1) L2)
    ;        ((empty-termlist? L2) L1)
    ;    (else
    ;        (let 
    ;            (
    ;                (t1 (first-term L1))
    ;                (t2 (first-term L2))
    ;            )
    ;            (cond 
    ;                ((> (order t1) (order t2))
    ;                    (adjoin-term　t1 (add-terms (rest-terms L1) L2)))
    ;                ((< (order t1) (order t2))
    ;                    (adjoin-term　t2 (add-terms L1 (rest-terms L2))))
    ;            (else
    ;                (adjoin-term　(make-term (order t1)
    ;                (add (coeff t1) (coeff t2)))
    ;                (add-terms (rest-terms L1)
    ;                (rest-terms L2))))))
    ;    )))

    ;(define (mul-terms L1 L2)
    ;    (if 
    ;        (empty-termlist? L1)
    ;        (the-empty-termlist)
    ;        (add-terms (mul-term-by-all-terms (first-term L1) L2)
    ;            (mul-terms (rest-terms L1) L2))))

    ;(define (mul-term-by-all-terms t1 L)
    ;    (if 
    ;        (empty-termlist? L)
    ;        (the-empty-termlist)
    ;        (let 
    ;            ((t2 (first-term L)))
    ;            (adjoin-term
    ;                (make-term (+ (order t1) (order t2))
    ;                (mul (coeff t1) (coeff t2)))
    ;                (mul-term-by-all-terms t1 (rest-terms L))))))


    ;(define (add-poly p1 p2) 
    ;    (if 
    ;        (same-variable? (variable p1) (variable p2))
    ;        (let 
    ;            (
    ;                (max-len (- (max (length p1) (length p2)) 1))
    ;            )
    ;            (make-poly (variable p1)
    ;                (map 
    ;                    + 
    ;                    (take* (coeff-list p1) max-len #t 0)
    ;                    (take* (coeff-list p2) max-len #t 0))) ;<== mapの長さ揃うやつさがす

    ;        )
    ;        (error "Polys not in same var: ADD-POLY" (list p1 p2))))

    ;(define (mul-poly p1 p2)
    ;    (if 
    ;        (same-variable? (variable p1) (variable p2))
    ;        (make-poly (variable p1)
    ;            (mul-terms (term-list p1) (term-list p2)))
    ;        (error "Polys not in same var: MUL-POLY" (list p1 p2))))

    ; Ex 2.87
    ;(define (=zero-poly? p) 
    ;    (= 0 (fold-left + 0 (map coeff (term-list p))))
    ;)

    ;(define (negative p)
    ;    (make-poly 
    ;        (variable p) 
    ;        (map (lambda (x) (* -1 x)) (coeff-list p)))
    ;)

    ;;; システムのほかの部分とのインターフェイス
    (define (tag p) (attach-tag 'polynomial-dense p))

    ;(put 'add '(polynomial polynomial)
    ;    (lambda (p1 p2) (tag (add-poly p1 p2))))

    ;(put 'sub '(polynomial polynomial) 
    ;    (lambda (p1 p2) (tag (add-poly p1 (negative p2)))))


    ;(put 'mul '(polynomial polynomial)
    ;    (lambda (p1 p2) (tag (mul-poly p1 p2))))
    
    ;(put '=zero? '(polynomial) =zero-poly?)

    (put 'make 'polynomial-dense
        (lambda (var terms) (tag (make-poly var terms))))
    ;(put 'variable '(polynomial-dense) variable)
    ;(put 'coeff-list '(polynomial-dense) coeff-list)
    (put 'term-list '(polynomial-dense) term-list)
    (put 'make-poly '(polynomial-dense) make-poly)
    ;(put 'variavle? 'polynomial-dense variable?)
    ;(put 'same-variavle? '(polynomial-dense polynomial-dense) same-variable?)

'done)


(define (install-polynomial-package)
    (define (make-sparse-poly variable term-list) 
    	((get 'make 'polynomial-sparse) variable term-list))
    (define (make-dense-poly variable coeff-list)
    	((get 'make 'polynomial-dense) variable coeff-list))

	(define (tag z) (attach-tag 'polynomial z))

    (define (variable p)
        (lambda (p) ((get 'variable (type-tag p)) p)))
    (define (variable? x) (symbol? x))

    (define (same-variable? v1 v2)
        (and 
            (variable? v1)
            (variable? v2)
            (eq? v1 v2)))


	; 依存関係をインストール
	;(install-sparse-polynomial-package)
	;(install-dense-polynomial-package)
    ;(print "::variable" variable)
    (define (order term) (car term))
    (define (coeff term) (cadr term))

    (define (first-term term-list) (car term-list))
    (define (rest-terms term-list) (cdr term-list))
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

    (define (add-poly p1 p2) 
        (if 
            (same-variable? (variable p1) (variable p2))
            (make-poly (variable p1)
                (add-terms (term-list p1) (term-list p2)))

            (error "Polys not in same var: ADD-POLY" (list p1 p2))))

    (put 'term-list '(polynomial) term-list)

    (put 'make-sparse-poly 'polynomial
    	(lambda (variable term-list) 
    		(tag (make-sparse-poly variable term-list))))

    (put 'make-dense-poly 'polynomial
    	(lambda (variable coeff-list) 
    		(tag (make-dense-poly variable coeff-list))))

    (print "--\tinstalled polynomial package")

'done)

(define (apply-generic op . args)
    (let 
        ((type-tags (map type-tag args)))
            (let ((proc (get op type-tags)))
                (if proc
                    (apply proc (map contents args))
                (error
                    "No method for these types: APPLY-GENERIC"
                    (list op type-tags ))))))


(define (make-sparse-poly variable term-list) 
	((get 'make-sparse-poly 'polynomial) variable term-list))

(define (make-dense-poly variable coeff-list) 
	((get 'make-dense-poly 'polynomial) variable coeff-list))

(define (term-list p)
    (apply-generic 'term-list p))

(define (make-poly p)
    (apply-generic 'make-poly p))

(define (test p)
    ((get 'test 'polynomial) p))

(print "--\tloaded poly2 package!")
