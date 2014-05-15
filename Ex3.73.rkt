#lang racket

(require "stream-memoized.rkt")

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

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
  (define (volt i v0)
    (cons-stream v0
                 (add-streams (scale-stream (integral i 0 dt)
                                            (/ 1 C))
                              (scale-stream i R))))
  volt)

(define RC1 (RC 1 1 1))
(stream-take (RC1 integers 0) 10)