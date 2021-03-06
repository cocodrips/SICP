(print "=====2.4 図形言語=====")
(define nil ())


(define (enumerate-interval low high)
    (if 
        (> low high)
        nil
    (cons low (enumerate-interval (+ low 1) high))))

(define (accumulate op initial sequence)
    (if 
        (null? sequence)
        initial
        (op (car sequence) (accumulate op initial (cdr sequence)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define wave
    (accumulate 
        (lambda (i acc) (append acc (list (enumerate-interval 0 15))))
         nil 
        (enumerate-interval 0 15)))




(define wave2 (beside wave (flip-vert wave)))
(define wave4 (below wave2 wave2))

(define (flipped-pairs painter)
(let ((painter2 (beside painter (flip-vert painter))))
(below painter2 painter2)))

(define (right-split painter n)
    (if (= n 0)
        painter
        (let ((smaller (right-split painter (- n 1))))
            (beside painter (below smaller smaller)))))

(define (corner-split painter n)
    (if 
        (= n 0)
        painter
        (let 
            ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
            (let 
                (
                    (top-left (beside up up))
                    (bottom-right (below right right))
                    (corner (corner-split painter (- n 1)))
                )
                (beside (below painter top-left)
            (below bottom-right corner)))
        )
    )
)

(define (square-limit painter n)
    (let 
        ((quarter (corner-split painter n)))
        (let 
            ((half (beside (flip-horiz quarter) quarter)))
        (below (flip-vert half) half))))

(print "===Ex 2.44===")
(define (up-split painter n)
    (if (= n 0)
        painter
        (let ((smaller (up-split painter (- n 1))))
            (below painter (beside smaller smaller)))))
