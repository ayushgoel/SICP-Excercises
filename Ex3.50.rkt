#lang planet neil/sicp

;(define (delay x)
;  (lambda() x))
(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream x y)
     (cons x (lambda () y)))))

(define (force delayed-object)
  (delayed-object))

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))
;(define (cons-stream x y) (cons x (delay y)))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
(stream-for-each proc (stream-cdr s)))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

;(define (stream-map proc . argstreams)
;  (if (stream-null? (car argstreams)) the-empty-stream
;      (cons-stream
;       (apply proc (map stream-car argstreams))
;       (apply stream-map
;              (cons proc (map stream-cdr argstreams))))))


(define (display-line x)
  (newline)
  (display x))


(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(stream-map display-line (stream-enumerate-interval 1 10))

(define (show x)
  (display-line x)
  x)

(define x (stream-map show (stream-enumerate-interval 0 10)))
(stream-ref x 5)
(stream-ref x 7)



(define (display-stream s)
  (stream-for-each display-line s))

(display "New")
(define sum 0)
(define (accum x)
  (display "X")
  (display x)
  (set! sum (+ x sum))
  sum)
(define seq (stream-map accum (stream-enumerate-interval 1 20)))
(display "New")
(define y (stream-filter even? seq))
(display sum)
(display "New")
(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
seq))
(display sum)
(display "New")
(stream-ref y 7)
(display sum)
(display "New")
(display-stream z)