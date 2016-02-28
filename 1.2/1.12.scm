(define (pascal top left)
  (cond
   ((= left 1) 1)
   ((= top 1) 0)
   (else (+ (pascal (- top 1) (- left 1)) (pascal (- top 1) left) ))
   
   ))

;; gosh> (pascal 1 1)
;; 1
;; gosh> (pascal 5 3)
;; 6
