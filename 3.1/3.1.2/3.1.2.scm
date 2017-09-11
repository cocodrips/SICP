;(define rand 
;	(let ((x random-init))
;	(lambda ()
;		(set! x (rand-update x))
;	x)))

; モンテカルロ法
; 例
; ランダムに選んだ⼆つの整数が共通因⼦を持たない
; つまり最⼤公約数が 1 であるという確率が 6/π^2 であるという事実を使って、πを近似できます。

(define (estimate-pi trials)
	(sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
	(= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
	(define (iter trials-remaining trials-passed)
		(cond 
			((= trials-remaining 0)
				(/ trials-passed trials))
			((experiment)
				(iter (- trials-remaining 1) (+ trials-passed 1)))
		(else
			(iter (- trials-remaining 1) trials-passed))))
	(iter trials 0)
)

; randのかわりにrand-updateを使うようにする

(define (estimate-pi trials)
	(sqrt (/ 6 (random-gcd-test trials random-init))))

(define (random-gcd-test trials initial-x)
	(define (iter trials-remaining trials-passed x)
		(let ((x1 (rand-update x)))
			(let ((x2 (rand-update x1)))
				(cond 
					((= trials-remaining 0)　(/ trials-passed trials))
					((= (gcd x1 x2) 1)
						(iter 
							(- trials-remaining 1)
							(+ trials-passed 1)
							x2
						)
					)
				(else
					(iter (- trials-remaining 1)　trials-passed　x2))))))
	(iter trials 0 initial-x)
)