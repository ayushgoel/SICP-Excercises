#lang racket

(require "stream-memoized.rkt")

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (partial-sums s)
  (cons-stream (stream-car s)
               (add-streams (partial-sums s) 
                            (stream-cdr s))))


(define (ln-series n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln-series (+ n 1)))))

(define ln2-series (ln-series 1))

(define ln2 (partial-sums ln2-series))

(define (square x) (* x x))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

;(display-stream ln2-series)
(define euler-ln2 (euler-transform ln2))
;(display-stream (euler-transform ln2))
(stream-ref euler-ln2 10)

(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform
                             (transform s))))


(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))
