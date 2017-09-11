(define balance 100)
(define (withdraw amount)
  (if (>= balance amount)
  (begin (set! balance (- balance amount)) balance)
"Insufficient funds"))

;(begin ⟨exp1⟩ ⟨exp2⟩ . . . ⟨expk⟩)
;⟨exp1 ⟩ から ⟨expk ⟩ までの式が順番に評価され、最後の式 ⟨expk ⟩ の値が begin
;形式全体の値として返されます。
(print "withdraw 100 ~ ")
(print (withdraw 10))
(print (withdraw 10))


;　誰もbalanceにアクセスできない
(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
        (begin 
          (set! balance (- balance amount))
          balance)
      "Insufficient funds"))))

(print "new-withdraw")
(print (new-withdraw 20))
(print (new-withdraw 20))

; 引き出し器をつくる
(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
    (begin 
      (set! balance (- balance amount))
      balance)
    "Insufficient funds")))

(print "make-withdraw")
(define withdraw-func (make-withdraw 100))
(print (withdraw-func 30))
(print (withdraw-func 30))


; 引き出しも預入もできるようにする
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin 
        (set! balance (- balance amount))
        balance)
    "Insufficient funds"))

  (define (deposit amount)
    (set! balance (+ balance amount))　balance)
  (define (dispatch m)
    (cond 
      ((eq? m 'withdraw) withdraw)
      ((eq? m 'deposit) deposit)
    (else (error "Unknown request: MAKE-ACCOUNT" m))))
dispatch)

(print "==make-account")
(define account (make-account 0))
(print ((account 'withdraw) 10))
(print ((account 'deposit) 9))
(print ((account 'withdraw) 8))



