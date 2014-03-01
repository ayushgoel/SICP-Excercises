#lang planet neil/sicp

;; 1.35

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(fixed-point (lambda (x) (+ 1.0 (/ 1.0 x))) 1.0)
;; won't work as doesn't converge
; (fixed-point (lambda (x) (- (* x x) 1.0)) 0.1)

;; 1.36
(define (verbose-fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display guess)
      (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))
(verbose-fixed-point (lambda (x) (/ (log 1000) (log x))) 3.0)
  ; with damping
(verbose-fixed-point (lambda (x) (/ (+ x (/ (log 1000) (log x))) 2.0)) 3.0)

;; 1.37
(define (cont-frac n d k) 
  (define (cont n d k i) 
    (if (= i k)
        0
        (/ (n i) (+ (d i) (cont n d k (+ i 1))))))
  (cont n d k 1))

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           100)
;; 1.38
(cont-frac (lambda (i) 1.0)
           (lambda (i) 
             (if (= 0 (remainder (- i 2) 3))
                 (* 2 (+ 1 (/ (- i 2) 3)))
                 1))
           100)

;; 1.39
(define (tan-cf x k)
  (cont-frac (lambda (i)
               (if (= i 1)
                   x
                   (- 0 (* x x))))
             (lambda (i) 
               (- (* i 2) 1))
             k))
(tan-cf 0.785 100)