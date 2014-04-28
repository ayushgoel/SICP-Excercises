#lang racket

;; Memoized implementations of Stream operation

(provide (all-defined-out))

(define-syntax cons-stream
  (syntax-rules ()
    ((- x y)
     (cons x (memo-proc (lambda () y))))))

(define (stream-car stream) (car stream))

(define (stream-cdr stream)
  ((cdr stream)))

(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))

(define the-empty-stream '())
(define stream-null? null?)

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream (apply proc (map stream-car argstreams))
                   (apply stream-map
                          (cons proc (map stream-cdr argstreams))))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (define (display-line x)
    (newline)
    (display x))
  (stream-for-each display-line s))

(define (stream-take s n)
  (if (= n 0)
      the-empty-stream
      (cons (stream-car s)
            (stream-take (stream-cdr s) (- n 1)))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred (stream-cdr stream))))
        (else
         (stream-filter pred (stream-cdr stream)))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low
                   (stream-enumerate-interval (+ low 1) high))))
