#lang racket
;(require math/base)
(require "stream-memoized.rkt")

(define (random-number a b)
  (+ a (* (random) (- b a))))

(define (random-stream a b)
  (cons-stream (random-number a b)
               (random-stream a b)))
;(stream-take (random-stream 2 20) 10)

(define xs (random-stream 0 2))
(define ys (random-stream 0 2))
(define (in-unit-circle x y)
  (<= (+ (- x 1) (- y 1)) 1))
(define ins (stream-map in-unit-circle xs ys))
(define (f x y)
  (if x
      (/ (+ 1 (numerator y)) (+ 1 (denominator y)))
      (/ (numerator y) (+ 1 (denominator y)))))
(define outs
  (cons-stream 1
               (stream-map f ins outs)))
(define pi (scale-stream outs 4.0))
(stream-take ins 10)
(stream-take pi 1000)