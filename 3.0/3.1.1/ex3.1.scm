;練習問題 3.1: 
;アキュムレータ (accumulator) は、ひとつの数値引数を伴って繰り返し呼ばれる⼿続きで、
;引数を合計に集積していくというものである。
;呼び出されるたびに現在までに集積された合計を返す。
;それぞれ独⽴した合計を持つアキュムレータを返す
;⼿続き make-accumulator を書け。make-accumulator への⼊⼒は、
;合計の初期値を指定する。例えば、以下のようになる。
;(define A (make-accumulator 5))
;(A 10)
;15
;(A 10)
;25


(define (make-accumulator init)
  (let ((current init))
    (lambda (value)
      (begin 
        (set! current (+ current value))
        current
      ))
  )
)

(define (ex-3.1)
  (define A (make-accumulator 5))
  (print "== Ex 3.1")
  (print (A 10))
  (print (A 10))
;== Ex 3.1
;15
;25
)

(ex-3.1)
