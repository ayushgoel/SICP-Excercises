#lang planet neil/sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (square c) (* c c))
(define (enumerate-interval low high)
  (if (> low high)
nil
      (cons low (enumerate-interval (+ low 1) high))))

;(flatmap (lambda (i) (list (square i))) (list 1 2 3))


;; 2.40

(define (unique-pairs n)
  (flatmap (lambda (i) 
             (map (lambda (j) (list j i))
                  (enumerate-interval 1 i)))
           (enumerate-interval 1 n)))
(unique-pairs 5)

;; 2.41
(define (triplets-sum-to-s n s)
  (define (sum-to-s l)
    (= (+ (car l)
          (car (cdr l))
          (car (cdr (cdr l)))) s))
  (filter sum-to-s (flatmap (lambda (i) (map (lambda (j) (cons j i))
                                             (enumerate-interval 1 (car i))))
                            (unique-pairs 3))))
(triplets-sum-to-s 3 6)

;; 2.43
;; interchange is calulating positions of k-1 queens again and again 
;; for a suggested position for new queen