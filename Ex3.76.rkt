#lang racket

(require "stream-memoized.rkt")

(define (sign-change-detector x1 x2)
  (cond ((or (and (< x1 0) (< x2 0))
            (and (> x1 0) (> x2 0))) 0)
        ((and (< x1 0) (> x2 0)) 1)
        ((and (> x1 0) (< x2 0)) -1)
        (else 0)))

(define (smooth s)
  (stream-map (lambda (x1 x2) (/ (+ x1 x2) 2))
              (cons-stream 0 s)
              s))

(define integers
  (cons-stream -1
               (stream-map (lambda (x) (+ x 1))
                           integers)))

(define (zero-crossings sense-data)
  (stream-map sign-change-detector 
              sense-data 
              (stream-cdr sense-data)))

(define (smooth-zero-crossings sense-data)
  (zero-crossings (smooth sense-data)))

(stream-take integers 10)
(stream-take (smooth integers) 10)
(stream-take (smooth-zero-crossings integers) 10)
