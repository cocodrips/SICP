; put/get の実装 by hioさん
(define registory '())

(define (put op type item)
    (let ((key (cons op type)))
        (let ((pair (cons key item)))
            (set! registory (cons pair registory)))))

(define (get op type)
    (define (iter key rest)
        (cond
            ((null? rest) #f)
            ((equal? key (car (car rest)))
                (cdr (car rest)))
            (else
                (iter key (cdr rest)))))
    (iter (cons op type) registory))