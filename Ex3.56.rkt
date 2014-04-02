#lang racket

(define (merge s1 s2)
  (cond ((stream-empty? s1) s2)
        ((stream-empty? s2) s1)
        (else
         (let ((s1car (stream-first s1))(s2car (stream-first s2)))
           (cond ((< s1car s2car)
                  (stream-cons s1car (merge (stream-rest s1) s2)))
                 ((> s1car s2car)
                  (stream-cons s2car (merge s1 (stream-rest s2))))
                 (else
                  (stream-cons s1car
                               (merge (stream-rest s1)
                                      (stream-rest s2)))))))))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

;(define S2 (stream-cons 2 ))
(define S (stream-cons 1 (merge (scale-stream S 2) 
                                (merge (scale-stream S 3)
                                       (scale-stream S 5)))))


(define (show-10-elements s)
  (define (show-element s i)
    (display " ")
    (display i)
    (display " ")
    (display (stream-ref s i))
    (display "\n"))
  (define (show-n-elements s n)
    (if (= n 1) 
        (show-element s n)
        (begin (show-element s n)
               (show-n-elements s (- n 1)))))
  (show-n-elements s 10))

(show-10-elements S)
