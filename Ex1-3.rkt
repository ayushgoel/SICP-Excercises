#lang planet neil/sicp

(define (square x) (* x x))
(define (divides? a b)
  (= (remainder b a) 0))
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (prime? n)
  (= n (smallest-divisor n)))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f 
          (+ a (/ dx 2.0)) 
          add-dx 
          b)
     dx))

;(integral (lambda (x) (* x x)) 1 2 0.5)

;; 1.29
(define (simpson-integral f a b n)
  (define (h) (/ (- b a) n))
  (define (y k) (f (+ a (* k (h)))))
  (define (sum i n crntsum)
    (cond ((= i 0) (sum 1 n (y i)))
          ((= i n) (+ crntsum (y i)))
          ((even? i) (sum (+ i 1) n (+ crntsum (* 2 (y i)))))
          (else (sum (+ i 1) n (+ crntsum (* 4 (y i)))))))
  (* (/ (h) 3)
     (sum 0 n 0))
  )
(define (cube x) (* x x x))
(simpson-integral cube 0 1 10)

;; 1.31
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))
(define (identity x) x)
(define (next-int x) (+ 1 x))
(product identity 2 next-int 4)

(define (factorial n)
  (if (< n 2)
      1
      (product identity 2 next-int n)))
(factorial 5)
;(product square 4 (lambda (x) (+ x 2)) 6)
(* 8.0 802.0 (/ (product square 4 (lambda (x) (+ x 2)) 800)
   (product square 3 (lambda (x) (+ x 2)) 801)))

;; 1.32
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))
(define (sumn term a next b) (accumulate + 0 term a next b))
(sumn identity 2 next-int 5)

;; 1.33
(define (filter-accumulate filter combiner null-value term a next b)
  (if (> a b)
      null-value
      (if (filter a)
          (combiner (term a)
                (filter-accumulate filter combiner null-value term (next a) next b))
          (filter-accumulate filter combiner null-value term (next a) next b))))
;; a
(filter-accumulate prime? + 0 square 6 next-int 8)
