#lang planet neil/sicp

;; 3.5

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (estimate-integral predicate x1 y1 x2 y2 number-of-trials)
  (define (iter trials-remaining trials-passed)
    (let ((x (random-in-range x1 x2))
          (y (random-in-range y1 y2)))
      (cond ((= trials-remaining 0)
             trials-passed)
            ((predicate x y)
             (iter (- trials-remaining 1) (+ trials-passed 1)))
            (else
             (iter (- trials-remaining 1) trials-passed)))))
  (* (/(iter number-of-trials 0) number-of-trials) 
     (* (- x2 x1) (- y2 y1))))

(define (in-circle a b)
  (if (< (+ (* a a) (* b b)) 9)
      #T
      #F))
(estimate-integral in-circle -4 -4 4 4 100000)

(define (in-unit-circle a b)
  (if (< (+ (* a a) (* b b)) 1)
      #T
      #F))
(/ (estimate-integral in-unit-circle -4 -4 4 4 100000) 1.0)
