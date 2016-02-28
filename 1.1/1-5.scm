(define (p) (p))
(define (test x y)
  (if (= x 0) 0 y))

;;(test 0 (p)) 無限ループ
;; (p) 無限ループ
(test 1 3 4)
