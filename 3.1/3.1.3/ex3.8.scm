; ひたすら一個分評価を遅延して返す関数作った
(define (queue-1)
  (define queue 0)
  (lambda (x) 
    (let ((top queue))
    (begin
      (set! queue x)
      top
    )))
)


;(define (f)
;  (define a ~)
;  (lambda (x) aがみえる, setできる)
;)

(define f (queue-1))
(+ (f 0) (f 1))
; 0 
(define g (queue-1))
(+ (g 1) (g 0))
; 1