#lang racket
(require "stream-memoized.rkt")

(define (integral delayed-integrand initial-value dt)
  (cons-stream initial-value
               (let ((integrand (force delayed-integrand)))
                 (if (stream-null? integrand)
                     the-empty-stream
                     (integral (delay (stream-cdr integrand))
                               (+ (* dt (stream-car integrand))
                                  initial-value) 
                               dt)))))

(define (join-streams s1 s2)
  (cons-stream (cons (stream-car s1) (stream-car s2))
               (join-streams (stream-cdr s1)
                             (stream-cdr s2))))

(define (RLC R L C dt)
  (define (VI vC0 iL0)
    (define vC (integral (delay dvC) vC0 dt))
    (define iL (integral (delay diL) iL0 dt))
    (define dvC (scale-stream iL (/ -1 C)))
    (define diL (add-streams (scale-stream vC (/ 1 L))
                             (scale-stream iL (/ (- R) L))))
    (join-streams vC iL))
  VI)

(define x ((RLC 1 1 1 1) 0 1))
(stream-take x 10)