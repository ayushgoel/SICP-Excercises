#lang racket
(define (square x) (* x x))
(define (cube x) (* x x x))
(define (abs x) (if (< x 0) 
                    (- x)
                    x))
(define (good-enough? guess x) (< (abs (- (abs (cube guess)) (abs x))) 0.0001))
(define (improve guess x) (/ (+ (/ x (square guess)) (* 2 guess)) 3))
(define (cube-iter guess x) 
  (if (good-enough? guess x)
      guess
      (cube-iter (improve guess x) x)))
(define (cubert x) (cube-iter 1.0 x))

(cubert -8)
(cubert 8)
(cubert 6)
(cubert 0.008)