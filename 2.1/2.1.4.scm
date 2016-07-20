;; 2.1.4 区間演算
(define (add-interval x y)
    (make-interval 
        (+ (lower-bound x) (lower-bound y))
        (+ (upper-bound x) (upper-bound y))))


(define (mul-interval x y)
    (let 
        (
            (p1 (* (lower-bound x) (lower-bound y)))
            (p2 (* (lower-bound x) (upper-bound y)))
            (p3 (* (upper-bound x) (lower-bound y)))
            (p4 (* (upper-bound x) (upper-bound y)))
        )
        (make-interval 
            (min p1 p2 p3 p4)
            (max p1 p2 p3 p4)
        )))

(define (div-interval x y)
    (mul-interval x (make-interval 
        (/ 1.0 (upper-bound y))
        (/ 1.0 (lower-bound y)))))

;; Ex 2.7 make-intervalが動くようにする
(display "==========Ex 2.7===========")
(newline)
(define (make-interval a b) (cons a b))
(define (lower-bound pair) (car pair))
(define (upper-bound pair) (cdr pair))
(define (print-interval pair) 
    (print 
        (lower-bound pair)
        " ~ "
        (upper-bound pair)
        "\n"))

(define i1 (make-interval 1 5))
(define i2 (make-interval 2 3))


(print "lower-bound:" (lower-bound i1) "\n")
(print "upper-bound:" (upper-bound i1) "\n")
(print "print-sample:")
(print-interval i1)

(define add-sample (add-interval i1 i2))
(display "add-interval:")
(print-interval add-sample)

; lower-bound:1
; upper-bound:5
; print-sample:1 ~ 5
; add-interval:3 ~ 8

;; Ex 2.8 差の区間
(display "==========Ex 2.8===========")
(newline)
(define (sub-interval x y)
    (let (
            (m1 (- (upper-bound x) (lower-bound y)))
            (m2 (- (upper-bound x) (upper-bound y)))
            (m3 (- (lower-bound x) (upper-bound y)))
            (m4 (- (lower-bound x) (lower-bound y)))
            (m5 (- (upper-bound y) (lower-bound x)))
            (m6 (- (upper-bound y) (upper-bound x)))
            (m7 (- (lower-bound y) (upper-bound x)))
            (m8 (- (lower-bound y) (lower-bound x)))
            )
            (make-interval 
                (min m1 m2 m3 m4 m5 m6 m7 m8)
                (max m1 m2 m3 m4 m5 m6 m7 m8))))

(print "sub-interval 1 :\n")
(define i1 (make-interval 1 5))
(define i2 (make-interval 2 3))

(define sub-sample (sub-interval i1 i2))
(print-interval i1)
(print-interval i2)
(print-interval sub-sample) 

(print "sub-interval 2 :\n")
(define i1 (make-interval 1 5))
(define i2 (make-interval 2 7))

(define sub-sample (sub-interval i1 i2))
(print-interval i1)
(print-interval i2)
(print-interval sub-sample) 

(print "sub-interval 3 :\n")
(define i1 (make-interval 1 5))
(define i2 (make-interval 2 7))

(define sub-sample (sub-interval i1 i2))
(print-interval i1)
(print-interval i2)
(print-interval sub-sample) 

;==========Ex 2.8===========
;sub-interval 1 :
;1 ~ 5
;2 ~ 3
;-3 ~ 3
;sub-interval 2 :
;1 ~ 5
;2 ~ 7
;-6 ~ 6
;sub-interval 3 :
;1 ~ 5
;2 ~ 7
;-6 ~ 6

;;;;;;;;;;;;;;;; これ間違ってた


;; hioさんのやつやってみる
(print "hioさんの")
(define (sub-interval x y)
    (make-interval
        (- (lower-bound x) (upper-bound y))
        (- (upper-bound x) (lower-bound y))))

(print "sub-interval 1 :\n")
(define i1 (make-interval 1 5))
(define i2 (make-interval 2 3))

(define sub-sample (sub-interval i1 i2))
(print-interval i1)
(print-interval i2)
(print-interval sub-sample) 

(print "sub-interval 2 :\n")
(define i1 (make-interval 1 5))
(define i2 (make-interval 2 7))

(define sub-sample (sub-interval i1 i2))
(print-interval i1)
(print-interval i2)
(print-interval sub-sample) 

(print "sub-interval 3 :\n")
(define i1 (make-interval 1 5))
(define i2 (make-interval 2 7))

(define sub-sample (sub-interval i1 i2))
(print-interval i1)
(print-interval i2)
(print-interval sub-sample) 

;hioさんの
;sub-interval 1 :
;1 ~ 5
;2 ~ 3
;-2 ~ 3
;sub-interval 2 :
;1 ~ 5
;2 ~ 7
;-6 ~ 3
;sub-interval 3 :
;1 ~ 5
;2 ~ 7
;-6 ~ 3



(newline)
(newline)

;; 商の区間
(define i1 (make-interval 1 5))
(define i2 (make-interval 2 7))

(display "商の区間")
(newline)
(define div-sample (div-interval i1 i2))
(print-interval i1)
(print-interval i2)
(print-interval div-sample) 

;; Ex 2.9 わからなかったのでhioさんの参考にする
;; 区間の幅

(display "==========Ex 2.9===========")
(newline)
(define i1 (make-interval 1 3))
(define i2 (make-interval 0 5))
(define (width-interval x)
    (/ (- (upper-bound x) (lower-bound x)) 2))

;(define width-add-interval 
;    (width-interval (add-interval i1 i2)))
;(print-interval width-add-interval)

;; Ex 2.10 ゼロ除算対応
(display "商の区間、0対応")
(define i1 (make-interval 1 3))
(define i2 (make-interval -5 5))

(define (div-interval x y)
    (if 
        ( < (* (upper-bound y) (lower-bound y)) 0)
        (print "err!!!\n")
        (mul-interval x (make-interval 
            (/ 1.0 (upper-bound y))
            (/ 1.0 (lower-bound y))))))

;商の区間
;1 ~ 5
;2 ~ 7
;.14285714285714285 ~ 2.5

(newline)
(print-interval i1)
(print-interval i2)
(define div-sample (div-interval i1 i2))
(print div-sample)

;商の区間、0対応
;1 ~ 3
;0 ~ 5
;err!!!
;; Ex 2.11



;; Ex 2.11


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (make-center-width c w)
    (make-interval (- c w) (+ c w)))

(define (center i)
    (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
    (/ (- (upper-bound i) (lower-bound i)) 2))

;; SAMPLE
(define interval-pp (make-interval 9.0 11.0))
(define interval-mp (make-interval -2.0 2.0))
(define interval-mm (make-interval -10.0 -2.0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ex 2.12
;; 中央値と％の許容誤差を取る
(newline)
(display "=========Ex 2.12==========")
(newline)

(define (percent i)
    (* (/ (width i) (center i)) 100))

(define (make-center-percent i)
    (cons (center i) (percent i)))
;; なぜかintervalをcenter-percent表記にかえる関数つくってしまった
;;;;中央値とパーセント許容誤差を取り、求める範囲を
;;;;返すコンストラクタ make-center-percent を定義せよ。
;; ってあったのに・・・。
;; いつか直す

(print-interval interval-pp)
(print-center-interval (make-center-percent interval-pp))
;9. ~ 11.
;[ 10.±10.% ]

(print-interval interval-mp)
(print-center-interval (make-center-percent interval-mp))
;-2. ~ 2.
;[ 0.±+inf.0% ]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.13 
; パーセント許容誤差が小さいという前提のもとで
;は、二つの区間の積のパーセント許容誤差を因数の許容誤差の積
;によって近似する簡単な式が存在することを示せ。すべての数値
;は正であると仮定して問題を単純化してもよい。
(newline)
(display "=========Ex 2.13==========")
(newline)

; 因数: 6 = 2 x 3 <-の2,3のこと（素数じゃなくておｋ）



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ２つの並列抵抗の式
(define (par1 r1 r2)
    (div-interval 
        (mul-interval r1 r2)
        (add-interval r1 r2)))

(define (par2 r1 r2)
    (let 
        (
            (one (make-interval 1 1))
        )
            (div-interval
                one (add-interval (div-interval one r1)
                    (div-interval one r2)))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.14
(newline)
(display "=========Ex 2.14==========")
(newline)

(define A (make-interval 6 10))
(define B (make-interval 19.0 21.0))
(print "A:")
(print-center-interval (make-center-percent A))

(print "B:")
(print-center-interval (make-center-percent B))

(print "par1: ")
(print-interval (par1 A B))

(print "par2: ")
(print-interval (par2 A B))

;par1: 3.6774193548387095 ~ 8.4
;par2: 4.5600000000000005 ~ 6.774193548387097

(print "A/A: ")
(define AA (div-interval A A))
(print-interval AA)
;; これが1にならない

(print "A/B: ")
(define AB (div-interval A B))
(print-interval (make-center-percent AB))

;A/A: 1.1333333333333333 ~ 47.058823529411754
;A/B: .40601503759398494 ~ 29.629629629629626



; 2.15
;「変数が繰り返し出てこないように書くほどより厳密な誤差限界を返すようにできる」は真か
;区間に対して四則演算をすればするほど差は解の区間は大きくなる
;widthが演算後に小さくなることはない

(newline)
(display "=========Ex 2.15==========")
(newline)

(newline)
(display "========= diff > 1 ==========")
(newline)
(define i1 (make-interval 5.0 7.0)) ;width 1
(define i2 (make-interval -5 -2)) 
(print (width (add-interval i1 i2)) "\n")
(print (width (mul-interval i1 i2)) "\n")
(print (width (sub-interval i1 i2)) "\n")
(print (width (div-interval i1 i2)) "\n")


(newline)
(display "========= diff < 1 ==========")
(newline)

(define i1 (make-interval 1 5)) ;width2
(define i2 (make-interval 1 1.2)) 
(print (width (add-interval i1 i2)) "\n")
(print (width (mul-interval i1 i2)) "\n")
(print (width (sub-interval i1 i2)) "\n")
(print (width (div-interval i1 i2)) "\n")

;========= diff > 1 ==========
;2.5
;12.5
;2.5
;1.25

;========= diff < 1 ==========
;2.1
;2.5
;2.1
;2.0833333333333335


;2.16
;群環体の体あたりの性質を満たしてなさそう（テキトウ）
