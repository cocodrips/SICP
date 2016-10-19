;; 再帰プロセス
(define (cont-frac n d k)
  (define (iter i)
    (if (= i k)
	(/ (n i) (d i))
	#?= (/ (n i) (+ (d i) (iter (+ i 1))))))
  (iter 1))

(cont-frac (lambda (i) 1.0)
	   (lambda (i) 1.0)
	   20)

;; #?-    0.5
;; #?-    0.6666666666666666
;; #?-    0.6000000000000001
;; #?-    0.625
;; #?-    0.6153846153846154
;; #?-    0.6190476190476191
;; #?-    0.6176470588235294
;; #?-    0.6181818181818182
;; #?-    0.6179775280898876
;; #?-    0.6180555555555556
;; #?-    0.6180257510729613
;; #?-    0.6180371352785146
;; #?-    0.6180327868852459
;; #?-    0.6180344478216819
;; #?-    0.6180338134001252
;; #?-    0.6180340557275542
;; #?-    0.6180339631667064
;; #?-    0.6180339985218034
;; #?-    0.6180339850173578

;; 反復プロセス(末尾再帰)
(define (cont-frac n d k)
  (define (iter i result)
    (print i "->" result)
    (if (= i 0)
	result
	(iter (- i 1)
	      (/ (n i)
		 (+ (d i) result)))))
  (iter k 0))
	
(cont-frac (lambda (i) 1.0)
	   (lambda (i) 1.0)
	   20)


;; 20->0
;; 19->1.0
;; 18->0.5
;; 17->0.6666666666666666
;; 16->0.6000000000000001
;; 15->0.625
;; 14->0.6153846153846154
;; 13->0.6190476190476191
;; 12->0.6176470588235294
;; 11->0.6181818181818182
;; 10->0.6179775280898876
;; 9->0.6180555555555556
;; 8->0.6180257510729613
;; 7->0.6180371352785146
;; 6->0.6180327868852459
;; 5->0.6180344478216819
;; 4->0.6180338134001252
;; 3->0.6180340557275542
;; 2->0.6180339631667064
;; 1->0.6180339985218034
;; 0->0.6180339850173578
;; 0.6180339850173578
