;#lang racket
#lang planet neil/sicp
;; 1.11

;; recursive
(define (recursiveF n)
  (if (< n 3)
      n
      (+ (recursiveF (- n 1)) (* 2 (recursiveF (- n 2))) (* 3 (recursiveF (- n 3))))))
(recursiveF 2)
(recursiveF 10)

;; iterative
(define (iterF n) 
  (if (< n 3)
      n
      (iterateF 4 2 1 n)))
(define (iterateF res fn1 fn2 n) 
  (if (= n 3)
      res
      (iterateF (+ res (* 2 fn1) (* 3 fn2)) res fn1 (- n 1))))
(iterF 2)
(iterF 10)

;; 1.12
(define (pascal-triangle row col) 
  (if (or 
       (= col 0) 
       (= row col))
      1
      (+ 
       (pascal-triangle (- row 1) col) 
       (pascal-triangle (- row 1) (- col 1)))))
(pascal-triangle 2 2)
(pascal-triangle 4 2)

;; 1.16
(define (square x) (* x x))
(define (even? x) (= (remainder x 2) 0))
(define (pow b n) 
  (if (= n 0)
      1
      (pow-iter 1 b n)))
(define (pow-iter prod b n) 
  (cond ((= n 1) (* prod b))
        ((even? n) (pow-iter prod 
                             (square b) 
                             (/ n 2)))
        (else (pow-iter (* prod b) 
                        b 
                        (- n 1)))))
(pow 2 5)
(pow 2 2)
(pow 2 2)
(pow 2 5)
(pow 2 2)
(pow 2 2)

;; 1.17
(define (double x) (* x 2))
(define (fast-mult a b) 
  (cond ((= b 0) 0)
        ((= b 1) a)
        ((even? b) (double (fast-mult a (/ b 2))))
        (else (+ a (fast-mult a (- b 1))))))
(fast-mult 2 3)
(fast-mult 4 4)


;; 1.21
(define (divides? a b)
  (= (remainder b a) 0))
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)

;; 1.22
(define (prime? n)
  (= n (smallest-divisor n)))
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))
      (timed-prime-test (+ n 2))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time)
  (newline))

(define (get-first-odd n) 
  (if (even? n)
      (+ n 1)
      n))
(define (search-prime n) 
  (if (timed-prime-test n) 
      n
      (search-prime (+ n 2))))
(define (search-for-prime n) (search-prime (get-first-odd n)))
(search-for-prime 100)
;(search-for-prime 1000)
;(search-for-prime 10000)
;(search-for-prime 100000)
;(search-for-prime 1000000)
;(search-for-prime 10000000)

;; 1.23
(define (next n) 
  (if (= n 2)
      3
      (+ n 2)))
(define (new-smallest-divisor n)
  (new-find-divisor n 2))
(define (new-find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))
;(new-smallest-divisor 23)
(define (new-prime? n)
  (= n (new-smallest-divisor n)))
(define (new-timed-prime-test n)
  (newline)
  (display n)
  (new-start-prime-test n (runtime)))
(define (new-start-prime-test n start-time)
  (if (new-prime? n)
      (report-prime (- (runtime) start-time))
      (timed-prime-test (+ n 2))))
(new-timed-prime-test 1009)
(timed-prime-test 1009)
