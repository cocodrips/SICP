;練習問題 3.3の make-account ⼿続きを変更して別
;の局所状態変数を追加し、間違ったパスワードで⼝座に 7 回連続
;アクセスされると call-the-cops(警察を呼ぶ) という⼿続きを呼
;ぶようにせよ。

(define (make-account password balance)
  (define correct-password password)
  (define incorrect-num 0)

  (define (withdraw amount)
    (if (>= balance amount)
      (begin 
        (set! balance (- balance amount))
        balance)
    "Insufficient funds"))

  (define (deposit amount)
    (set! balance (+ balance amount))　balance)

  (define (call-the-cops) "がおー！警察だぞー！")

  (define (dispatch password m)
    (if 
      (eq? password correct-password)
      (cond 
        ((eq? m 'withdraw) withdraw)
        ((eq? m 'deposit) deposit)
        (else (error "Unknown request: MAKE-ACCOUNT" m)))
      (lambda (args) 
        (begin 
          (set! incorrect-num (+ incorrect-num 1))　
          (if 
            (> incorrect-num 7) 
            (call-the-cops)
            "Incorrect password"
          ))))
  )
dispatch)

(define (ex-3.4)
  (define account (make-account 'nyan 0))
  (print ((account 'nyan 'withdraw) 10))
  (print ((account 'nyan 'deposit) 10))
  (print ((account 'nyan 'withdraw) 10))
  (print ((account 'nyanko 'withdraw) 10))
  (print ((account 'nyanko 'deposit) 10))
  (print ((account 'nyanko 'deposit) 10))
  (print ((account 'nyanko 'deposit) 10))
  (print ((account 'nyanko 'deposit) 10))
  (print ((account 'nyanko 'deposit) 10))
  (print ((account 'nyanko 'deposit) 10))
  (print ((account 'nyanko 'deposit) 10))
  (print ((account 'nyanko 'deposit) 10))  
;Insufficient funds
;10
;0
;Incorrect password
;Incorrect password
;Incorrect password
;Incorrect password
;Incorrect password
;Incorrect password
;Incorrect password
;がおー！警察だぞー！
;がおー！警察だぞー！
)

(ex-3.4)