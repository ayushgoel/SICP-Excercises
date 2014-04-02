#lang racket

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

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define integers
  (stream-cons 1
               (stream-map add1 integers)))

(show-10-elements integers)

(define inverse-integers
  (stream-cons (/ 1 2)
               (stream-map (lambda (x) (/ 1 (+ (/ 1 x) 1))) 
                           inverse-integers)))
(show-10-elements inverse-integers)


(define (integrate-series s)
  (stream-cons (stream-first s) (mul-streams inverse-integers (stream-rest s))))

(define in (integrate-series integers))
(show-10-elements in)

(define ones (stream-cons 1 ones))
(show-10-elements ones)
(define integral-ones (integrate-series ones))
(show-10-elements integral-ones)
(display "asdasd")


(define exp-series
  (stream-cons 1 (integrate-series exp-series)))
(show-10-elements exp-series)