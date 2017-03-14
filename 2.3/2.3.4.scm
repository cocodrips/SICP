;; ハフマン符号木

;一般的に、符号化するメッセージの相対頻度を利用した可変長接頭符号を
;使えば、かなりの節約ができます。これを行う戦略のひとつにハフマン符号化
;法というものがあります。 p174

;左の枝を下りるたびに符号に 0 を追加し、右の枝を下りるたびに 1 を追加します p175
;まず、符号構築対象の初期データによって決まる、記号と頻度
;を持つ葉ノードの集合から始めます。ここで、重みが小さいほうから二つの葉
;を選び、二つをくっつけて新しいノードを作り、新しいノードの左と右の枝が
;その二つのノードになるようにします。

(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

;listの頭に'leaf つけて leaf オブジェクトであることを伝えてる

(define (make-code-tree left right)
    (list 
        left
        right
        (append (symbols left) (symbols right)) ; シンボルの集合
        (+ (weight left) (weight right))        ; シンボルの合計weight
    )
)

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
    (if 
        (leaf? tree)
        (list (symbol-leaf tree))
        (caddr tree)
    )
)

(define (weight tree)
    (if 
        (leaf? tree)
        (weight-leaf tree)
        (cadddr tree)
    )
)


; 復号化手続き
(define (decode bits tree)
    (define (decode-1 bits current-branch)
        (if 
            (null? bits)
            '()
            (let 
                (
                    (next-branch
                    (choose-branch (car bits) current-branch))
                )
                (if 
                    (leaf? next-branch)
                    (cons (symbol-leaf next-branch) (decode-1 (cdr bits) tree))
                    (decode-1 (cdr bits) next-branch )
                )
            )))
    (decode-1 bits tree)
)

(define (choose-branch bit branch)
    (cond 
        ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
    (else (error "bad bit: CHOOSE-BRANCH" bit))))

; treeのmergeの手続き
(define (adjoin-set x set)
    (cond 
        ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
    (else (cons (car set)
(adjoin-set x (cdr set))))))


; リストをleftのリストに変換
(define (make-leaf-set pairs)
    (if 
        (null? pairs)
        '()
        (let 
            ((pair (car pairs)))
            (adjoin-set 
                (make-leaf (car pair) (cadr pair))
                (make-leaf-set (cdr pairs)))
        )
    )
)

(define _pairs '((A 4) (B 2) (C 1) (D 1)) )
(print "pairs:" _pairs)
(print "  => make-leaf-set:" (make-leaf-set _pairs))




(print "===Ex.2.67===")

(define sample-tree
    (make-code-tree 
        (make-leaf 'A 4)
        (make-code-tree
            (make-leaf 'B 2)
            (make-code-tree (make-leaf 'D 1) (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(print (decode sample-message sample-tree)) ;(A D A B B C A)

(print "===Ex 2.68===")
;encode-symbolを設計する

(define (encode message tree)
    (if (null? message)
        '()
        (append (encode-symbol (car message) tree) (encode (cdr message) tree))))

(define (encode-symbol char tree)
    (define (itr char tree code)
        ;(print "left: " (left-branch tree) " right: " (right-branch tree))
        (cond 
            ((leaf? tree) code)
            ((contains-symbol? char (left-branch tree)) (itr char (left-branch tree) (cons 0 code)))
            ((contains-symbol? char (right-branch tree)) (itr char (right-branch tree) (cons 1 code)))
            (else #f)
        )
    )
    (reverse (itr char tree '()))
)

(define (contains-symbol? char tree)
    (memq char (symbols tree))
)

(define sample-message '(A D A B B C A))
(print (encode sample-message sample-tree))
; 正解：(0 1 1 0 0 1 0 1 0 1 1 1 0)
; 結果：(0 1 1 0 0 1 0 1 0 1 1 1 0)


; hioさんの 再帰の形にすれば、consでつなげる〜〜〜
(define (encode-symbol char tree)
    (define (iter current-tree)
        (cond
            ((leaf? current-tree)
                '())
            ((memq char (symbols (left-branch current-tree)))
                (cons 0 (iter (left-branch current-tree))))
            ((memq char (symbols (right-branch current-tree)))
                (cons 1 (iter (right-branch current-tree))))
            (else
                (error "NEVER REACH HERE"))))

    (if
        (memq char (symbols tree))
        (iter tree)
        (error "bad char:" char "not in tree" tree)))


(print "===Ex 2.69===")
; ハフマン符号木野実装

(define (generate-huffman-tree pairs)
    (successive-merge (make-leaf-set pairs)))

; 最小のもの同士をmerge
(define (successive-merge leafs)
    (define (join x lst) 
        (if 
            (null? x)
            lst
            (cons x lst)))
    (define (create-min-tree leafs lst l1 l2)
        (print "leafs:" leafs " lst:" lst " l1:" l1 " l2:" l2)
        (cond
            ((null? leafs) 
                (if
                    (null? lst)
                    (make-code-tree l1 l2)
                    (create-min-tree (join (make-code-tree l1 l2) lst) '() '() '())))
            ((null? l1) (create-min-tree (cdr leafs) lst (car leafs) l2))
            ((null? l2) (create-min-tree (cdr leafs) lst l1 (car leafs)))
            ((> (weight l1) (weight (car leafs)))
                (create-min-tree (cdr leafs) (join l1 lst) (car leafs) l2))
            ((> (weight l2) (weight (car leafs))) 
                (create-min-tree (cdr leafs) (join l2 lst) l1 (car leafs)))
        (else (create-min-tree (cdr leafs) (join (car leafs) lst) l1 l2))))
    (create-min-tree leafs '() '() '())
)

(define _pairs '((A 4) (B 2) (C 1) (D 1) (E 1)) )
(print (make-leaf-set _pairs))
(print (generate-huffman-tree _pairs))

(define _pairs '((A 4) (B 2) (C 1) (D 1)))
(print (generate-huffman-tree _pairs))


;hioさんの。これでさらっと！
(define (successive-merge leaves)
    (cond
        ; 要素数 == 0:
        ((null? leaves)
            (error "leaves must have at least one element"))
        ; 要素数 == 1:
        ((null? (cdr leaves))
            (car leaves))
        ; 要素数 >= 2:
        (else
            (successive-merge
                (adjoin-set
                    (make-code-tree
                        (car leaves)
                        (cadr leaves))
                    (cddr leaves))))))


(print "===Ex 2.70===")
(define _pairs '((na 16) (yip 9) (sha 3) (a 2) (get 2) (job 1) (wah 1) (boom 1)))
(define tree (generate-huffman-tree _pairs))
(print tree)

(define sample-message '(get a job 
    sha na na na na na na na na
    get a job
    sha na na na na na na na na
    wah yip yip yip yip yip yip yip yip yip
    sha boom))
(print sample-message)
(define encoded (encode sample-message tree))
(print encoded)
;(1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0
; 0 0 0 1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 
; 0 0 0 0 0 0 1 1 0 1 1 1 0 1 0 1 0 1 0 1 0 1 0 
; 1 0 1 0 1 0 1 1 1 0 1 1 0 1 0)
(print (length encoded)) ;84
; 元の文章だと124字


(print "===Ex 2.71===")
;n 記号のアルファベットに対するハフマン木があ
;り、記号の相対頻度は 1, 2, 4, . . . , 2^n−1 であるとする。n = 5、n = 10
;の場合の木をスケッチせよ。そのような木では、(一般の n につい
;て) 最も頻度の高い記号を符号化するのに何ビット必要になるだ
;ろうか。最も頻度の低い記号はどうだろうか。

(define n_3_sample '((A 1) (B 2) (C 4)))
(define tree (generate-huffman-tree n_3_sample))
(print (encode '(A) tree)) ;(0 0 0 0)


(define n_5_sample '((A 1) (B 2) (C 4) (D 8) (E 16)))
(define tree (generate-huffman-tree n_5_sample))
(print (encode '(A) tree)) ;(0 0 0 0)

(define n_10_sample '((A 1) (B 2) (C 4) (D 8) (E 16) (F 32) (G 64) (H 128) (I 256) (J 512)))
(define tree (generate-huffman-tree n_10_sample))
(print (encode '(A) tree)) ;(0 0 0 0 0 0 0 0 0) 長さ9
;最も頻度が高い 1bit
;最も頻度が低い n-1 bit

(print "===Ex 2.72===")
;練習問題 2.68で設計した符号化手続きについて考
;える。ひとつの記号を符号化するのに必要なステップ数の増加オ
;ーダーはどのようになるだろうか。各ノードに着くたびに記号リ
;ストを検索するのに必要なステップ数を含めることを忘れないよ
;うに。この問題の一般の場合について答えることは難しい。ここ
;では、n 記号の相対頻度が 練習問題 2.71のようになっている特別
;な場合について考えよう。アルファベット中で頻度が最大の記号
;と最小の記号を符号化するのにかかるステップ数の増加オーダー
;を (n の関数として) 答えよ。


 ;頻度細大の記号の符号化
 ;encode-symbol 1回
 ;ontains-symbol? 1回
 ;O(1)

 ;頻度最小の記号の符号化 
 ;encode-symbol n回
 ;ontains-symbol? n回
 ;O(n ^ 2)


(define shuffle_sample '((A 1) (C 4) (D 8) (J 512) (E 16) (B 2)))
(define tree (generate-huffman-tree shuffle_sample))
(print shuffle_sample)
(print tree)

(define shuffle_sample '((A 1) (B 2) (C 3) (D 4) (E 5) (F 6)))
(define tree (generate-huffman-tree shuffle_sample))
(print shuffle_sample)
(print tree)
