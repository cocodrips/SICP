(use srfi-27)
; このモジュールはメルセンヌツイスタアルゴリズム (Mersenne Twister乱数発生器 参照) を基礎に用いた
; SRFI-27疑似乱数発生器インタフェースを提供します。



;モンテカルロ積分を、⼿続き estimate-integral として実装せよ。
;引数として、述語 P、⻑⽅形の上下界 x1, x2, y1, y2、推定値を⽣
;成するための試⾏回数を取る。この⼿続きは、上で π を推定する
;ために使ったものと同じ monte-carlo ⼿続きを使わなければなら
;ない。この estimate-integral を使って、単位円の⾯積を測るこ
;とによって π の推定値を求めよ。
(print "==ex3.5==")

(define (random-in-range low high)
  (let ((range (- high low)))
  (+ low (* range (random-real))))) ; gaucheのrandom-integerを使う

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond 
      ((= trials-remaining 0)
        (/ trials-passed trials))
      ((experiment)
        (iter (- trials-remaining 1) (+ trials-passed 1)))
    (else
      (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0.0)
)

(define (estimate-integral P x1 x2 y1 y2 trials)
  (* 
    (* (- x2 x1) (- y2 y1))
    (monte-carlo trials (lambda () (P (random-in-range x1 x2) (random-in-range y1 y2)))))
)

;; 中心(5, 7) 半径3 の円の場合
;テスト手続き
(define (ex-3.5)
  (define (p-test x y)
    (< 
      (+ (expt (- x 5) 2) (expt (- y 7) 2)) 
      (expt 3 2)))
  (define (pi-test x y)
    (< 
      (+ (expt x 2) (expt y 2)) 
      (expt 1 2)))
  (print "==== answer " (* 3.1415 (* 3 3)))
  (print "100回: " (estimate-integral p-test 2 8 4 10 100))
  (print "10000回: " (estimate-integral p-test 2 8 4 10 10000))
  (print "100000回: " (estimate-integral p-test 2 8 4 10 100000))
  (print "==== answer " 3.1415)
  (print "100回: " (estimate-integral pi-test -1 1 -1 1 100))
  (print "10000回: " (estimate-integral pi-test -1 1 -1 1 10000))
  (print "100000回: " (estimate-integral pi-test -1 1 -1 1 100000))
  (print "1000000回: " (estimate-integral pi-test -1 1 -1 1 1000000))
)



(ex-3.5)