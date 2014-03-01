#lang racket
(require (planet dyoo/sicp-concurrency:1:2/sicp-concurrency))

(define x 10)
(parallel-execute (lambda () (set! x (* x x)))
                  (lambda () (set! x (+ x 1))))
(print x)

(define y 10)
(define s (make-serializer))
(parallel-execute (s (lambda () (set! y (* y y))))
                  (s (lambda () (set! y (+ y 1)))))
(print y)


(define x 10)
(define s (make-serializer))
(parallel-execute (lambda () (set! x ((s (lambda () (* x x))))))
                  (s (lambda () (set! x (+ x 1)))))

;; 3.39
101
121
100

;; 3.40
100
1000
10000
100000
1000000
