#lang racket

(define (stream-map proc . argstreams)
  (if (stream-empty? (car argstreams)) empty-stream
      (stream-cons
       (apply proc (map stream-first argstreams))
       (apply stream-map
              (cons proc (map stream-rest argstreams))))))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define (partial-sums s)
  (stream-cons (stream-first s)
               (add-streams (partial-sums s) 
                            (stream-rest s))))


(define ones (stream-cons 1 ones))
(define integers (stream-cons 1 (add-streams ones integers)))
(stream-ref integers 1)
(stream-ref integers 2)
(stream-ref integers 3)

(define x (partial-sums integers))
(stream-ref x 1)
(stream-ref x 2)
(stream-ref x 3)
(stream-ref x 4)
(stream-ref x 5)
