#lang racket

(require "stream-memoized.rkt")

(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

(define integers
  (cons-stream 1
               (stream-map (lambda (x) (+ x 1))
                           integers)))

(stream-take (integral integers 0 1) 10)

(define (RC R C dt)
  (define (consts v)
    (cons-stream v (consts v)))
  (define (volt i v0)
    (add-streams (consts v0)
                 (add-streams (scale-stream (integral i 0 dt)
                                            (/ 1 C))
                              (scale-stream i R))))
  volt)

(define RC1 (RC 1 1 1))
(stream-take (RC1 integers 0) 10)