#lang racket
(require "stream-memoized.rkt")

(define random-init 11)
(define (random-update x)
  (remainder (+ (* 13 x) 5) 24))

(define (random-number-generator req)
  (define (f x y)
    (if (= x 0)
        (random-update y)
        (random-update random-init)))
  (define seq 
    (cons-stream random-init
                 (stream-map f 
                             req
                             seq)))
  seq)

(define ones (cons-stream 1 ones))
(define zeroes (cons-stream 0 zeroes))
(define (interleave s1 s2)(if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))
(define in (interleave (interleave ones zeroes) zeroes))
(define out (random-number-generator in))
(stream-take out 10)