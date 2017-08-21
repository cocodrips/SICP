;練習問題 3.6: 乱数⽣成器をリセットして、与えられた値から始まる数列を作れるようにできると便利だ。
;記号 generate またはreset のどちらかを引数として取り、次のようなふるまいをする新しい rand ⼿続きを設計せよ。
;(rand 'generate) は新しい乱数を⽣成する。
;((rand 'reset) ⟨new-value ⟩) は内部の状態変数を指定された ⟨new-value⟩ にリセットする。
;つまり、状態をリセットすることによって、再現可能な数列が⽣成できる。
;これは乱数を使うプログラムをテストしたりデバッグしたりするのにとても役に⽴つ。

;gaucheのランダム
;random-source-pseudo-randomize! s i j
;https://practical-scheme.net/gauche/man/gauche-refj/randamubitutonososu.html

(use srfi-27)
(define (rand arg)
	(cond 
		((eq? arg 'generate) (random-real))
		((eq? arg 'reset) 
			(lambda (i j) (random-source-pseudo-randomize! default-random-source i j)))
	(else "Failed"))
)


(define (ex-3.6)
	((rand 'reset) 1 2)
	(print (rand 'generate))
	((rand 'reset) 3 4)
	(print (rand 'generate))
	((rand 'reset) 1 2)
	(print (rand 'generate))
	;0.6871964570424902
	;0.726033823048644
	;0.6871964570424902
)
(ex-3.6)
