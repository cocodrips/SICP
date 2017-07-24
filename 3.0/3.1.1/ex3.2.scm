;練習問題 3.2: ソフトウェアをテストするアプリケーションでは、
;計算の過程で、ある⼿続きが何回呼ばれたかを数えられると便利
;だ。⼀引数の⼿続き f を⼊⼒として取る⼿続き make-monitored
;を書け。make-monitored が返すのは第三の⼿続き (仮に mf とす
;る) で、この⼿続きは内部カウンタを保持することによって呼び
;出された回数を覚えておく。mf に対する⼊⼒が how-many-calls?
;という特別な記号である場合、mf はカウンタの値を返す。⼊⼒が
;reset-count という特別な記号である場合、mf はカウンタをゼロ
;にリセットする。それ以外の⼊⼒に対しては、mf はその⼊⼒によ
;って f を呼び出した結果を返し、カウンタを 1 増やす。例えば、監
;視をつけたバージョンの sqrt ⼿続きを作ることもできる。
;(define s (make-monitored sqrt))
;(s 100)
;10
;(s 'how-many-calls?)
;1


(define (make-monitored f)
  (define counter 0)
  (define (call args)
    (begin 
      (set! counter (+ counter 1))
      (apply f args)
    ))
  (define (how-many-calls?) counter)
  (define (dispatch . args)
    (print (car args))
    (cond 
      ((eq? (car args) 'how-many-calls?) how-many-calls?)
    (else 
      (call args)
  )))
dispatch)


(define (ex-3.2)
  (define m (make-monitored sqrt))
  (define m2 (make-monitored +))
  (print "== Ex 3.2")
  (print "sqrt 100 = " (m 100))
  (print "+ 100 10 = " (m2 100 10))
;== Ex 3.2
;100
;sqrt 100 = 10
;100
;+ 100 10 = 110
)
(ex-3.2)
