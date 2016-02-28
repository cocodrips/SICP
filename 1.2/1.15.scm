(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))

(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      #?=(p (sine (/ angle 3.0)))))

(sine 12.5)
;; 5回
;; #?="./1.15.scm":7:(p (sine (/ angle 3.0)))
;; #?="./1.15.scm":7:(p (sine (/ angle 3.0)))
;; #?="./1.15.scm":7:(p (sine (/ angle 3.0)))
;; #?="./1.15.scm":7:(p (sine (/ angle 3.0)))
;; #?="./1.15.scm":7:(p (sine (/ angle 3.0)))
;; #?-    0.153776521096694
;; #?-    0.4467840153484446
;; #?-    0.9836111719853284
;; #?-    -0.8557060643295382
;; #?-    -0.060813768577286265

