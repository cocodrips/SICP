(display "*********2.2.3**********")
(newline)
(define nil ())

(define _map map)

;; 標準インタフェース
;; 奇数の葉の二乗の合計を計算
(define (sum-odd-squares tree)
    (cond 
        ((null? tree) 0)
        ((not (pair? tree))　(if (odd? tree) (square tree) 0))
    (else 
        (+ (sum-odd-squares (car tree))
    (sum-odd-squares (cdr tree))))))

;;by @hioさん
; p.40, 1.2.2 木の再帰
(define (fib n)
    (define (iter a b count)
        (if
            (= count 0)
            b
            (iter (+ a b) a (- count 1))))
    (iter 1 0 n))

;; フィボナッチ数のうち、偶数のものをかえすリストを構築する
(define (even-fibs n)
    (define (next k)
        (if (> k n)
            ()
            (let ((f (fib k)))
                (if 
                    (even? f)
                    (cons f (next (+ k 1)))
                    (next (+ k 1))))))
(next 0))
(display (even-fibs 10))
(newline)
;(0 2 8 34)

;; Filter
(define (filter predicate sequence)
    (cond 
        ((null? sequence) ())
        ((predicate (car sequence)) 
            (cons (car sequence)
            (filter predicate (cdr sequence))))
    (else (filter predicate (cdr sequence)))))

(display (filter odd? (list 1 2 3 4 5)))
(newline)

;; 集積　accumulate (reduceのこと！)
(define (accumulate op initial sequence)
    (if 
        (null? sequence)
        initial
        (op (car sequence) (accumulate op initial (cdr sequence)))))

; ****(op elem acc-in)
; 第二引数に、結果を入れてく
; 1つめのはどんな op を使ってもsequenceの要素だが、
; acc-in の方は使う op 次第でいろんなものになる

(display (accumulate + 0 (list 1 2 3 4 5)))(newline)
;15
(display (accumulate * 1 (list 1 2 3 4 5)))(newline)
;120
(display (accumulate cons () (list 1 2 3 4 5)))(newline)

;(1 2 3 4 5)


(display "======Ex.2.33======")
(newline)
;練習問題 2.33: 基本的なリスト操作のいくつかを集積として定義
;したものを以下に示す。欠けている式を埋めて、完成させよ。

(define (map p sequence)
    (accumulate (lambda (x y) (cons (p x) y)) () sequence))

(define (double x)
   (* 2 x))
(display (map double (list 1 2 3 4 5))) 
(newline)
;(2 4 6 8 10)

(define (append seq1 seq2)
    (accumulate cons seq1 seq2))
(display (append (list 1 2 3) (list 4 5 8))) 
(newline)
;(4 5 8 1 2 3)
(define (length sequence)
    (accumulate (lambda (_ x) (+ x 1)) 0 sequence))
(display (length (list 1 2 3)) )
(newline)
;3

;木の葉の列挙は
(define (enumerate-tree tree)
    (cond 
        ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else 
            (append (enumerate-tree (car tree))
            (enumerate-tree (cdr tree))))))

(display "enumerate-tree\n")
(display (enumerate-tree (list 1 (list 2 (list 3 4)) 5)))
;(5 4 3 2 1)

(display "======Ex.2.34======")
(newline)

(define (horner-eval x coefficient-sequence)
    (accumulate
        (lambda (this-coeff higher-terms) (+ (* higher-terms x) this-coeff))
        0
        coefficient-sequence))

(display (horner-eval 2 (list 1 3 0 5 0 1)))
;; 79
(newline)



(display "======Ex.2.35======")
(newline)

(define (count-leaves t)
    (accumulate + 0 (map (lambda (x) 1) (enumerate-tree t))))

(define tree (cons (list 1 2) (list 3 4)))
(display (count-leaves tree))
(newline)
; 4

(define tree (cons (list 1 2) (list 3 4 5)))
(display (count-leaves tree))
(newline)
;5


;hio
(define (count-leaves t)
    (accumulate
        +
        0
        (map
            (lambda (x)
                (if
                    (pair? x)
                    (count-leaves x)
                    1))
            t)))


(display "======Ex.2.36======")
(newline)

(define (accumulate-n op init seqs)
(if (null? (car seqs))
    nil
    (cons (accumulate op init (map car seqs)) (accumulate-n op init (map cdr seqs)))))


(define s (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
(display s)
(newline)

(display (accumulate-n + 0 s))
(newline)

(display "======Ex.2.37======")
(newline)

;(define (map proc items)
;    (if (null? items)
;    nil
;    (cons (proc (car items)) (map proc (cdr items)))))
(define (dot-product v w)
    (accumulate + 0 (_map * v w)))

(define m1 (list (list 1 2 3 4) (list 4 5 6 7)))
(define v1 (list 1 2))
(define v2 (list 10 20))


(display (dot-product v1 v2)) 
(newline)
;50

(define (matrix-*-vector m v)
    (_map (lambda (m_i) (dot-product m_i v)) m)) ;; By @hioさん

(display (matrix-*-vector m1 v2))
(newline)
;(50 140)


(define (transpose mat)
    (accumulate-n cons nil mat))
(display (transpose m1))
(newline)
;((1 4) (2 5) (3 6) (4 7))


(define (matrix-*-matrix m n)
    (let ((cols (transpose n)))
        (map (lambda (m_i) (matrix-*-vector cols m_i)) m)))
(display (matrix-*-matrix m1 m1))
(newline)
;((9 12 15 18) 
; (24 33 42 51))
;cols???

(display "======Ex.2.38======")
(newline)

;練習問題 2.38: accumulate 手続きは、列の最初の要素と、右の
;すべての要素を組み合わせた結果とを組み合わせるため、foldright
;としても知られている。fold-left というものもあり、これ
;は fold-right に似ているが、要素の組み合わせを逆方向に行うと
;いう点が違う。
(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence) (accumulate op initial (cdr sequence)))))

(define fold-right accumulate)

(define (fold-left op initial sequence)
    (define (iter result rest)
        (if (null? rest)
            result
        (iter (op result (car rest))
            (cdr rest))))
(iter initial sequence))

(display (fold-right / 1 (list 1 2 3)))     (newline) 
; (/ 1 (/ 2 (/ 3 1)))
; 2 / 3
(display (fold-left / 1 (list 1 2 3)))      (newline)
; (/ (/ (/ 1 1) 2 ) 3)

(display (fold-right list nil (list 1 2 3)))(newline)
;(1 (2 (3 ())))
(display (fold-left list nil (list 1 2 3))) (newline)
;(((() 1) 2) 3)

;; 可換な演算ならright/fold-leftで結果がおなじになる

(display "======Ex.2.39======")
(newline)
;練習問題 2.39: 以下の reverse(練習問題 2.18) 手続きの定義を、練
;習問題 2.38の fold-right と fold-left によって完成させよ。
(define (reverse sequence)
    (fold-right (lambda (x y) (append (list x) y)) nil sequence))

(define l (list 1 3 5))
(display (reverse l))
(newline)

(define (reverse sequence)
    (fold-left (lambda (x y) (cons y x)) nil sequence))
(display (reverse l))
(newline)



;; マップのネスト
(define (enumerate-interval low high)
    (if 
        (> low high)
        nil
    (cons low (enumerate-interval (+ low 1) high))))
(display (enumerate-interval 2 7))
(newline)


(define n 3)
(display (accumulate
    append nil (map (lambda (i)
        (map (lambda (j) (list i j))
            (enumerate-interval 1 (- i 1))))
        (enumerate-interval 1 n))))
(newline)
;((3 1) (3 2) (2 1))
(load "../1.2/smallest-divisor.scm")
;(use math.prime)
(define (prime-sum? pair)
    (prime? (+ (car pair) (cadr pair))))

(define (flatmap proc seq)
    (accumulate append nil (map proc seq)))
;; [proc(seq[0]), proc(seq[1]) .....] みたいなのが出来る？ 
    
(define (make-pair-sum pair)
    (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
    (map make-pair-sum
        (filter prime-sum? (flatmap
            (lambda (i)
            (map (lambda (j) (list i j))
            (enumerate-interval 1 (- i 1))))
        (enumerate-interval 1 n)))))

(display (prime-sum-pairs 3))
(newline)
;((3 2 5) (2 1 3))


(display "======Ex.2.40======")
(newline)
;整数 n に対し、1 ≤ j < i ≤ n となるペア (i, j) の
;列を生成する手続き unique-pairs を定義せよ。unique-pairs を
;使って上の prime-sum-pairs の定義を簡単にせよ。

;; 自分の
(define (unique-pairs n)
    (define (itr base i stack)
        ;(print base ":" i "\n")
        (cond 
            ((>= base n) stack)
            ((> i n) (itr (+ base 1) (+ base 2) stack))
            (else (itr base (+ i 1) (cons (list base i) stack)))))
(itr 1 2 nil))

(display (unique-pairs 4))
(newline)
;((3 4) (2 4) (2 3) (1 4) (1 3) (1 2))

;(define (accumulate op initial sequence)
(define (prime-sum-pairs n)
    (accumulate 
        (lambda (i acc) 
            (if (prime? (+ (car i) (car (cdr i))))
                (cons i acc)
            acc))
        nil
    (unique-pairs n)))

(display (prime-sum-pairs 3))
(newline)
;((2 3) (1 2))

;; hioさんの
;(define (unique-pairs n)
;    (flatmap 
;        (lambda (i) 
;                (map (lambda (j) (list i j)) (enumerate-interval 1 (- i 1))))
;        (enumerate-interval 1 n)
;    )
;)
;(display (unique-pairs 4))
;(newline)
;((4 1) (4 2) (4 3) (3 1) (3 2) (2 1))
;これつかうと、2.41が小さい順にならんでいるというのが条件として満たさないと
;正しい挙動をしない実装をしてしまったため、コメントアウト

(display "======Ex.2.41======")
(newline)
;ある整数 n 以下の異なる正の整数が大小順に並ん
;だ三つ組 i, j, k の中で、合計がある整数 s となるものすべてを見
;つける手続きを書け。


(define (first p) (car p))
(define (second p) (car (cdr p)))
(define (unique-tri n s)
    (define (add-pattern p stack)
        (define (itr p i stack)
            ;(print p i stack)
            (cond 
                ((> i n) stack)
                ((>= (second p) i) (itr p (+ i 1) stack))
                ((> (second p) n) stack)
                ((not (= (+ (first p) (second p) i) s)) stack)
                (else 
                    (itr p (+ i 1) (cons (list (first p) (second p) i) stack)))
                        
            )
        )
        (itr p 1 stack)
    )


    (accumulate
        add-pattern
        nil
        (unique-pairs n))

)
(display "わたしの:")
(display (unique-tri 5 10))(newline)
;わたしの:((1 4 5))
;全ペア((3 4 5) (2 4 5) (2 3 5) (2 3 4) (1 4 5) (1 3 5) (1 3 4) (1 2 5) (1 2 4) (1 2 3))

(display "hioさんの:")
(define (unique-triples n)
    (flatmap
        (lambda (i)
            (flatmap
                (lambda (j)
                    (map
                        (lambda (k)
                            (list i j k))
                        (enumerate-interval 1 (- j 1))))
                (enumerate-interval 1 (- i 1))))
        (enumerate-interval 1 n)))
(display (unique-triples 5))
(newline)
;hioさんの:((5 4 1) (5 4 2) (5 4 3) (5 3 1) (5 3 2) (5 2 1) (4 3 1) (4 3 2) (4 2 1) (3 2 1))
; なるほど、こういうことなのね；； 納得；；

(display "======Ex.2.42======")
(newline)

(define (make-position row col)
   (cons row col))

(define (position-row position)
   (car position))

(define (position-col position)
   (cdr position))

(define empty-board nil)

(define (queens board-size)
    (define (queen-cols col)
        ;(print col)
        (if (= col 0)
            (list empty-board)
            (filter
                (lambda (positions) (safe? k positions))
                (flatmap
                    (lambda (rest-of-queens)
                        (print rest-of-queens)
                        (map 
                            (lambda (new-row) (adjoin-position new-row col rest-of-queens))
                            (enumerate-interval 1 board-size) ; [1...k]
                        ))
                    (queen-cols (- col 1))))))
(queen-cols board-size))

(define (safe? col positions) 
    (let ((kth-queen (list-ref positions (- col 1)))
     (other-queens (filter (lambda (q)
                             (not (= col (position-col q))))
                           positions)))
    (define (valid-position? p1 p2)
        ;(print "valid-position:" p1 p2)
        (not (or 
            (= (position-row p1) (position-row p2))
            (= (position-col p1) (position-col p2))
            (= 
                (abs (- (position-row p1) (position-col p1)))
                (abs (- (position-row p2) (position-col p2)))))))

    (define (iter queen-position board)
     (or (null? board)
         (and (valid-position? queen-position (car board))
              (iter queen-position (cdr board)))))
   (iter kth-queen other-queens))
)

(define (adjoin-position row col positions)
    (append (list (make-position row col)) positions))

;(display (queens 0))
;(newline)

;(display (queens 4))
;(newline)
;;(((4 . 1) (1 . 2) (3 . 3) (2 . 4)) ((4 . 1) (2 . 2) (1 . 3) (3 . 4)) ((3 . 1) (2 . 2) (4 . 3) (1 . 4)) ((2 . 1) (4 . 2) (3 . 3) (1 . 4)))

;(display (queens 5))
;(newline)
;(((3 . 1) (5 . 2) (2 . 3) (4 . 4) (1 . 5)) ((4 . 1) (2 . 2) (5 . 3) (3 . 4) (1 . 5)) ((5 . 1) (3 . 2) (1 . 3) (4 . 4) (2 . 5)) ((5 . 1) (2 . 2) (4 . 3) (1 . 4) (3 . 5)))

;(display (for-each (lambda (n) (print (list n (length (queens n))))) (enumerate-interval 1 5))) ;; 時間かかりすぎる




