; 2.3.3 集合を表現する

; 順序なしリストとしての集合

; 集合にxが含まれているか？
(define (element-of-set? x set)
    (cond 
        ((null? set) #f)
        ((equal? x (car set)) #t)
    (else (element-of-set? x (cdr set)))))


; 集合の積 intersection-set 
(define (intersection-set set1 set2)
    (cond 
        ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
            (cons (car set1) (intersection-set (cdr set1) set2)))
    (else (intersection-set (cdr set1) set2))))


; Test
(define s1 (list 1 2 3))
(define s2 (list 1 3 5))

(print "s1:" s1)
(print "s2:" s2)

(print "要素が含まれているか")
(print "(element-of-set? 1 s1):" (element-of-set? 1 s1))
(print "(element-of-set? 10 s1):" (element-of-set? 10 s1))


; 実行結果

;```
;s1:(1 2 3)
;s2:(1 3 5)
;要素が含まれているか
;(element-of-set? 1 s1):#t
;(element-of-set? 10 s1):#f
;```

(print "===Ex 2.59===")
;練習問題 2.59: 順序なしリストとして表現した集合に対する
;union-set 演算を実装せよ。

(define (union-set base target)
    ;(print "base: " base "target: " target)
    (cond 
        ((null? target) base)
        ((not (element-of-set? (car target) base)) 
            (union-set (cons (car target) base) (cdr target)))
    (else (union-set base (cdr target))))
)

(print "(union-set s1 s2):" (union-set s1 s2))
; 実行結果

;```
;base: (1 2 3)target: (1 3 5)
;base: (1 2 3)target: (3 5)
;base: (1 2 3)target: (5)
;base: ((1 2 3) . 5)target: ()
;(union-set s1 s2):(5 1 2 3)
;```

(print "===Ex 2.60===")
;上の例では、集合は重複のないリストとして表現す
;るよう規定した。ここで、重複を許す場合について考えてみよう。そ
;の場合、例えば {1, 2, 3} という集合は (2 3 2 1 3 2 2) というリス
;トとして表現することもできる。この表現に対して演算を行う手続
;き element-of-set?, adjoin-set, union-set, intersection-set
;を設計せよ。それぞれの効率は、重複なし表現に対する手続きで
;それに対応するものと比べてどうだろうか。重複なしの表現より
;もこの表現のほうが向いているような応用はあるだろうか。

;adjoin-set: 要素の追加
;intersection: 積
;union-set: 和

;element-of-set 特に変更なし
(define (element-of-set? x set)
    (cond 
        ((null? set) #f)
        ((equal? x (car set)) #t)
    (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set) (cons set x))

(define (union-set set1 set2)
    (append set1 set2)
)
;intersection-set 変更なし
; (2 2 3) と (1 2 3) の積は (2 3) ? (3)?
(print "(adjoin-set 5 s1): " (adjoin-set 5 s1))
(print "(adjoin-set s2 s1): " (adjoin-set s2 s1))

(print "順序有りリストとしての集合")

(define (element-of-set? x set)
    (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
    (else (element-of-set? x (cdr set)))))
;自分より大きいのにあった瞬間切れる
;O(n)にかわりはないが、実行時間は半分程度が期待できる

(define (intersection-set set1 set2)
    (if (or (null? set1) (null? set2))
        '()
        (let ((x1 (car set1)) (x2 (car set2)))
            (cond 
                ((= x1 x2) (cons x1 (intersection-set (cdr set1) (cdr set2))))
                ((< x1 x2) (intersection-set (cdr set1) set2))
                ((< x2 x1) (intersection-set set1 (cdr set2)))))
    )
)

(print "===Ex2.61===")
;順序つき表現を使った adjoin-set を実装せよ。
;element-of-set? から類推して、順序つきであることの利点を生
;かして、順序なしの表現に比べて平均的に半分のステップを必要
;とする手続きを作るやり方を示せ。

; そもそもリストの真ん中に挿入はO(1)で出来る？
; 0~N までのリスト と N~Mまでのリストに分けられるっけ・・・

(define (adjoin-set x set)

    (cond 
        ((null? set) (list x))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else 
            (cons (car set) (adjoin-set x (cdr set))))
    )
)


(define s1 (list 1 2 3 4 5))
(define s2 (list 1 3 5 7 9))
(print "s1:" s1)
(print "s2:" s2)
(print "(adjoin-set 5 s1):" (adjoin-set 5 s1))
(print "(adjoin-set 4 s2):" (adjoin-set 4 s2))

(define s1 (list 1 2 3 4 5))
(print (cons (car s1) (cons 100 (cdr s1))))

(print "===Ex2.62===")
; 順序付リストO(n)でunion-setを実装

(define (union-set base target)
    (define (itr result l1 l2)
        ;(print result " " l1 (null? l1) " " l2 (null? l2))
        (cond
            ((and (null? l1) (null? l2)) result)
            ((or 
                (null? l2)
                (and (not (null? l1)) (<= (car l1) (car l2))))
             (itr (cons (car l1) result) (cdr l1) l2))
            (else
             (itr (cons (car l2) result) l1 (cdr l2)))
        )
    )
(reverse (itr (list) base target)))

(define s1 (list 1 2 3 4 5))
(define s2 (list 1 3 5 7 9))
(print "s1:" s1)
(print "s2:" s2)
(print "(union-set s1 s2):" (union-set s1 s2))



(print "**二分木としての集合")

(define (entry tree) (car tree)) ; entry: そのノードのてっぺんの値
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
    (list entry left right))

(define (element-of-set? x set)
    (cond 
        ((null? set) #f)
        ((= x (entry set)) #t)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))
    ))

(define (adjoin-set x set)
    (cond 
        ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
            (make-tree (entry set)
            (adjoin-set x (left-branch set))
            (right-branch set)))
        ((> x (entry set))
            (make-tree (entry set) (left-branch set)
            (adjoin-set x (right-branch set))))
    ))

(print "===Ex2.63===")
(define (tree->list-1 tree)
    (if (null? tree)
        '()
        (append (tree->list-1 (left-branch tree))
            (cons (entry tree)
            (tree->list-1
            (right-branch tree))))))

(define (tree->list-2 tree)
    (define (copy-to-list tree result-list)
        (if (null? tree)
            result-list
            (copy-to-list (left-branch tree)
                (cons (entry tree)
                    (copy-to-list
                    (right-branch tree)
                result-list )))))
(copy-to-list tree '()))

(define s (adjoin-set 5 '()))
(define s2 (adjoin-set 10 s))
(define s3 (adjoin-set 3 s2))
(define s4 (adjoin-set 4 s3))   
(print s4)
;(5 (3 () (4 () ())) (10 () ()))

;     5
;    /\
;   3 10
;   \
;    4

(print (tree->list-1 s4))
(print (tree->list-2 s4))
;(3 4 5 10)
;(3 4 5 10)

(define s (adjoin-set 1 '()))  
(define s (adjoin-set 2 s))
(define s (adjoin-set 3 s))
(define s (adjoin-set 4 s))
(define s (adjoin-set 5 s))
(define s (adjoin-set 6 s))
(define s (adjoin-set 7 s))
(print s)

; こっちのほうがいいよ
(define s (fold adjoin-set '() (list 1 2 3 4 5 6 7)))

(print "tree->list-1: " (tree->list-1 s))
(print "tree->list-2: " (tree->list-2 s))
;tree->list-1: (1 2 3 4 5 6 7)
;tree->list-2: (1 2 3 4 5 6 7)


(print "===Ex2.64===")
(define (list->tree elements)
    (car (partial-tree elements (length elements))))


(define (partial-tree elts n)
    ;(print "partial-tree  elts:" elts " n:" n)
    (if (= n 0)
        (cons '() elts)
        (let
            (
                (left-size (quotient (- n 1) 2)); leftsize = (n - 1) / 2
            ) 
            (let
                (
                    (left-result (partial-tree elts left-size)) ;leftresult = elementのleft-size分
                )
                (let 
                    (
                        (left-tree (car left-result))           ; left-tree         = left-resultの1こめ
                        (non-left-elts (cdr left-result))       ; non-left-elts     = leftresultののこり
                        (right-size (- n (+ left-size 1)))      ; right-size        = (n - leftsize) + 1
                    )
                    (let 
                        (
                            (this-entry (car non-left-elts))    ; this entry = non-left-eltsの1こめ
                            (right-result                       ; right-result = left-resultの3こめ以降の木
                                (partial-tree
                                    (cdr non-left-elts)
                                    right-size))
                        )
                        (let 
                            (
                                (right-tree (car right-result))     ; right-tree: right-resultの１番最初
                                (remaining-elts (cdr right-result)) ; のこり: right resultの2こ目以降
                            )
                            
                            (cons
                                (make-tree
                                    this-entry
                                    left-tree
                                    right-tree)
                                remaining-elts )
                        )))))))

(define l (list 1 2 3 4 5 6 7))
(print (list->tree l))
;     4
;    / \
;  2     6
; /\     /\
;1  3   5  7


;  2
; /\
;1  3  (4 5 6 7)

;  2                 5
; /\                /\
;1  3  と (4 ) と  6  7
; 
;     4
;    /\
;  2    5
; /\   /\
;1  3  6  7


(define l (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14))
(print (list->tree l))


(print "===Ex2.65===")

;(論理和)
(define (union-set set1 set2)
    (define (union-set-i set l)
        ;(print set l)
        (if 
            (null? l)
            set
            (let 
                (
                    (first (car l))
                    (lst (cdr l))) 
                (union-set-i (adjoin-set first set) lst)
            )
        )
    )
    (union-set-i set1 (tree->list-1 set2))
)

(define l1 (list 1 2 3 4 5 6 7))
(define l2 (list 1 3 5 7 9))
(print (union-set (list->tree l1) (list->tree l2)))

; 1個の要素追加にかかるコストがlog_n
; なのでこれだと n * log_n かかってしまう

(print "-- O(n)")
;; ?
(define (union-set set1 set2)
    (define (union-list base target)
        ; 最後の要素が同じでなければ追加
        (define (add x l)
            (cond 
                ((null? l) (cons x l))
                ((= (car l) x) l)
                (else (cons x l))
            ))

        (define (itr result l1 l2)
            ;(print result " " l1 (null? l1) " " l2 (null? l2))
            (cond
                ((and (null? l1) (null? l2)) result)
                ;((null? l2) ())
                ((or 
                    (null? l2)
                    (and (not (null? l1)) (<= (car l1) (car l2))))
                 (itr (add (car l1) result) (cdr l1) l2))
                (else
                    (itr (add (car l2) result) l1 (cdr l2)))

            )
        )
    (reverse (itr (list) base target)))
    (list->tree (union-list (tree->list-1 set1) (tree->list-1 set2)))
)
(print (union-set (list->tree l1) (list->tree l2)))

;() (1 2 3 4 5 6 7)#f (1 3 5 7 9)#f
;(1) (2 3 4 5 6 7)#f (1 3 5 7 9)#f
;(1) (2 3 4 5 6 7)#f (3 5 7 9)#f
;(2 1) (3 4 5 6 7)#f (3 5 7 9)#f
;(3 2 1) (4 5 6 7)#f (3 5 7 9)#f
;(3 2 1) (4 5 6 7)#f (5 7 9)#f
;(4 3 2 1) (5 6 7)#f (5 7 9)#f
;(5 4 3 2 1) (6 7)#f (5 7 9)#f
;(5 4 3 2 1) (6 7)#f (7 9)#f
;(6 5 4 3 2 1) (7)#f (7 9)#f
;(7 6 5 4 3 2 1) ()#t (7 9)#f
;(7 6 5 4 3 2 1) ()#t (9)#f
;(9 7 6 5 4 3 2 1) ()#t ()#t
;(1 2 3 4 5 6 7 9)
;(4 (2 (1 () ()) (3 () ())) (6 (5 () ()) (7 () (9 () ()))))

; これだと2・nで実行できるので O(n)

(print "--intersection(積)")
(define (intersection-set set1 set2)
    (define (intersection-list base target)
        ;(print base target)
        (if (or (null? base) (null? target))
            '()
            (let ((x1 (car base)) (x2 (car target)))
                (cond 
                    ((= x1 x2) (cons x1 (intersection-list (cdr base) (cdr target))))
                    ((< x1 x2) (intersection-list (cdr base) target))
                    ((< x2 x1) (intersection-list base (cdr target)))))
        )
    )
    (list->tree  (intersection-list (tree->list-1 set1) (tree->list-1 set2)))
)
(print (intersection-set (list->tree l1) (list->tree l2)))
;(1 2 3 4 5 6 7)(1 3 5 7 9)
;(2 3 4 5 6 7)(3 5 7 9)
;(3 4 5 6 7)(3 5 7 9)
;(4 5 6 7)(5 7 9)
;(5 6 7)(5 7 9)
;(6 7)(7 9)
;(7)(7 9)
;()(9)
;(3 (1 () ()) (5 () (7 () ())))



(print "===Ex2.66===")

(define (make-record k v) (list k v))
(define (key record) (car record))
(define (value record) (cadr record))

(define (lookup k records)
    ;(print records)
    (if 
        (null? records) 
        #f
        (let 
            ((record (entry records)))
            (cond 
                ((= k (key record)) (value record))
                ((< k (key record)) (lookup k (left-branch records)))
                ((> k (key record)) (lookup k (right-branch records)))
            ))
    )
)


(define _records
    (list->tree
    (list (make-record 1 "a") (make-record 2 "b") (make-record 3 "ab")))
)

(print (lookup 1 _records))
(print (lookup 3 _records))



(define (lookup-record k records)
    (if 
        (null? records) 
        #f
        (let 
            ((record (entry records)))
            (print record)
            (cond 
                ((= k (key record)) record)
                ((< k (key record)) (lookup k (left-branch records)))
                ((> k (key record)) (lookup k (right-branch records)))
            ))
    )
)

(print (lookup-record 1 _records))
