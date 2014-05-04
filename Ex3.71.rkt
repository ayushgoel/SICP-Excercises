#lang racket

(require "stream-memoized.rkt")

(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2))
               (s1weight (weight (stream-car s1)))
               (s2weight (weight (stream-car s2))))
           (if (<= s1weight s2weight)
               (cons-stream s1car (merge-weighted (stream-cdr s1) 
                                                  s2 
                                                  weight))
               (cons-stream s2car (merge-weighted s1 
                                                  (stream-cdr s2)
                                                  weight)))))))

(define (pairs-weighted s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs-weighted (stream-cdr s) 
                    (stream-cdr t)
                    weight)
    weight)))

(define integers
  (cons-stream 1
               (stream-map (lambda (x) (+ x 1))
                           integers)))

(define (cube i) (* i i i))

(define (sum-weight p)
  (let ((i (car p))
        (j (cadr p)))
    (+ (cube i) (cube j))))

(define ints (pairs-weighted integers integers sum-weight))

(define (is-ramanujam-number? i j)
  (= (sum-weight i) (sum-weight j)))

(define (ramanujam-numbers s)
  (let ((i (stream-car s))
        (j (stream-car (stream-cdr s)))
        (k (stream-cdr s)))
    (if (is-ramanujam-number? i j)
        (cons-stream (list i j (sum-weight i))
                     (ramanujam-numbers k))
        (ramanujam-numbers (stream-cdr s)))))

(stream-take ints 20)
(stream-take (ramanujam-numbers ints) 3)