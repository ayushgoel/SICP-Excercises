#lang racket

(require racket/trace)


(define (show-10-elements s)
  (define (show-element s i)
    (display " ")
    (display i)
    (display " ")
    (display (stream-ref s i))
    (display "\n"))
  (define (show-n-elements s n)
    (if (= n 0) 
        (show-element s n)
        (begin (show-element s n)
               (show-n-elements s (- n 1)))))
  (show-n-elements s 10))

(define (stream-map proc . argstreams)
  (if (stream-empty? (car argstreams)) empty-stream
      (stream-cons
       (apply proc (map stream-first argstreams))
       (apply stream-map
              (cons proc (map stream-rest argstreams))))))

(define (add-streams s1 s2)
  (stream-map + s1 s2))
(trace add-streams)

(define (mul-series a b)
  (stream-cons (* (stream-first a) (stream-first b))
               (add-streams (scale-stream (stream-rest b) (stream-first a))
                            (mul-series (stream-rest a) b))))


(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (invert-unit-series s)
  (stream-cons 1 
               (scale-stream (mul-series (stream-rest s) 
                                          (invert-unit-series s)) 
                             -1)))


(define integers
  (stream-cons 1
               (stream-map add1 integers)))
(define invert-integers (invert-unit-series integers))
(show-10-elements integers)
(show-10-elements invert-integers)

(show-10-elements (mul-series integers invert-integers))