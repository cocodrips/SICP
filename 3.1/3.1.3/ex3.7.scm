; 3.3より
(define (make-account password balance)
  (define correct-password (list password))
  ;; 引き出し
  (define (withdraw amount)
    (if (>= balance amount)
      (begin 
        (set! balance (- balance amount))
        balance)
    "Insufficient funds"))

  ;; 預金
  (define (deposit amount)
    (set! balance (+ balance amount))　balance)
  
  ;; パスワード設定
  (define (add-password new-password)
    (begin
        (set! correct-password (cons new-password correct-password))
    dispatch))
  
  ;; password check
  (define (is-correct-password? password)
    (define (itr corrects password)
      (cond
       ((null? corrects) #f)
       ((eq? (car corrects) password) #t)
       (else (itr (cdr corrects) password))))
    (itr correct-password password)
  )
  
  ;;
  (define (dispatch password m)
    (if 
      (is-correct-password? password)
      (cond 
        ((eq? m 'withdraw) withdraw)
        ((eq? m 'deposit) deposit)
        ((eq? m 'new-password) add-password)
        (else (error "Unknown request: MAKE-ACCOUNT" m)))
      (lambda (args) "Incorrect password"))
    )
dispatch)

;; 共有口座
(define (make-joint account password new-password)
  ((account password 'new-password) new-password))


(define account (make-account 'nyan 0))
(define new-account (make-joint account 'nyan 'nyanko))
(print ((account 'nyan 'withdraw) 10))
(print ((account 'nyan 'deposit) 10))
(print ((account 'nyan 'withdraw) 10))
(print ((new-account 'nyan 'deposit) 10))
(print ((new-account 'nyanko 'deposit) 10))


(define (make-joint account password new-password)
  (define (wrap pass arg)
    (if 
     (eq? pass new-password)
     (account password arg)
     (lambda (_) "invalid password")
     ))
wrap)