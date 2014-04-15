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

(define (add-streams s1 s2)
  (stream-map + s1 s2))
(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (mul-series a b)
  (stream-cons (* (stream-first a) (stream-first b))
               (add-streams (scale-stream (stream-rest b) (stream-first a))
                            (mul-series (stream-rest a) b))))

(define (invert-unit-series s)
  (stream-cons 1 
               (scale-stream (mul-series (stream-rest s) 
                                          (invert-unit-series s)) 
                             -1)))

(define (stream-map proc . argstreams)
  (if (stream-empty? (car argstreams)) empty-stream
      (stream-cons
       (apply proc (map stream-first argstreams))
       (apply stream-map
              (cons proc (map stream-rest argstreams))))))

(define (mul-streams s1 s2)
  (stream-map * s1 s2))


(define (div-series n d)
  (let ((denom-const (stream-first d)))
    (if (= denom-const 0)
        (error "Zero denom")
        (mul-series n (invert-unit-series (scale-stream d (/ 1 denom-const)))))))

(define integers
  (stream-cons 1
               (stream-map add1 integers)))

(define inverse-integers (stream-map (lambda (x) (/ 1 x)) integers))

(define (integrate-series s)
  (mul-streams inverse-integers
               s))


(define cosine-series
  (stream-cons 1 (stream-map - (integrate-series sine-series)))) 
(define sine-series
  (stream-cons 0 (integrate-series cosine-series)))

(define tan-series (div-series sine-series cosine-series))