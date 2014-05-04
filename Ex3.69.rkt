#lang racket

(require "stream-memoized.rkt")

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define (triples s t u)
  (cons-stream 
   (list (stream-car s) (stream-car t) (stream-car u))
   (interleave 
    (stream-map (lambda (x) (list (stream-car s) (car x) (cadr x)))
                ;stream-cdr to get non-repeating answer
                (stream-cdr (pairs t u)))
    (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

(define integers
  (cons-stream 1
               (stream-map (lambda (x) (+ x 1))
                           integers)))

(define ints (triples integers integers integers))

(define (pythagoras-triplet? l)
  (let ((x (car l))
        (y (cadr l))
        (z (caddr l)))
    (= (+ (* x x) (* y y)) (* z z))))

(define pythagoras-triplets 
  (stream-filter (lambda (x) (pythagoras-triplet? x))
                 ints))

(stream-take ints 20)
(stream-take pythagoras-triplets 3)