#lang racket

(define (curry f x)
  (lambda args
    (apply f (cons x args))
    ))

(define (new-curry f x)
  (define (y . args)
    (apply f (cons x args))
    )
  y)

(define (sum a b)
  (+ a b))

(sum 1 2)

((curry sum 9) 7)