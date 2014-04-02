#lang racket

;(define-syntax (cons-stream x y)
 ; (cons x (delay y)))

(define (integers-starting-from n)
  (display " ")
  (display n)
  (stream-cons n (integers-starting-from (+ n 1))))

(define 5s (integers-starting-from 5))

(stream-ref 5s 5)

(define 1s (integers-starting-from 1))
(define div7? (lambda (x) (= (remainder x 7) 0)))
(define div7 (stream-filter div7? 1s))
(stream-ref div7 2)

(define (fibgen a b)
  (display "  ")
  (display a)
  (display " ")
  (display b)
  (stream-cons a (fibgen b (+ a b))))
(stream-ref (fibgen 1 1) 5)


(define (sieves stream)
  (stream-cons (stream-first stream)
               (sieves (stream-filter (lambda (x) (not (= (remainder x (stream-first stream)) 0)))
                              stream))))
(define primes (sieves (integers-starting-from 2)))
(stream-ref primes 1)
(stream-ref primes 2)
(stream-ref primes 3)
(stream-ref primes 4)

(define (stream-map proc . argstreams)
  (if (stream-empty? (car argstreams)) empty-stream
      (stream-cons
       (apply proc (map stream-first argstreams))
       (apply stream-map
              (cons proc (map stream-rest argstreams))))))


(define ones (stream-cons 1 ones))
(define (add-streams s1 s2)
  (stream-map + s1 s2))
(define integers (stream-cons 1 (add-streams ones integers)))
(stream-ref integers 1)
(stream-ref integers 2)
(stream-ref integers 3)
(stream-ref integers 4)

(define s (stream-cons 1 (add-streams s s)))
(stream-ref s 1)
(stream-ref s 2)
(stream-ref s 3)
(stream-ref s 4)
