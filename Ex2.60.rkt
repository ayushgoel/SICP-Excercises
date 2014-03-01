#lang planet neil/sicp

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cons x set))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (adjoin-set (car set1)
                     (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))
  
(define union-set append)

(define x (list 1 2 3))
(define y (list 2 3 4))
(define z (list 5 6 7))

(element-of-set? 2 x)
(element-of-set? 6 x)

(adjoin-set 2 x)
(adjoin-set nil x)
(adjoin-set 5 x)

(intersection-set x y)
(intersection-set x z)
(intersection-set '() y)
(intersection-set '() '())

(union-set x y)
(union-set y z)
(union-set '() '())
