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

(define ones (stream-cons 1 ones))
(define integers (stream-cons 1 (add-streams ones integers)))

;(stream-ref integers 5 )

(define factorials (stream-cons 1 (mul-streams integers factorials)))
(stream-ref factorials 2)
(stream-ref factorials 3)
(stream-ref factorials 5)

;this does't stop running
;(define factorials (stream-tail (stream-cons 1 (mul-streams integers factorials)) 1))
(stream-ref factorials 2)
(stream-ref factorials 3)
(stream-ref factorials 5)
;
;(define ans (stream-tail factorials 1))
;(stream-ref ans 2)