#lang planet neil/sicp

(define square (lambda (x) (* x x)))
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
(define average (lambda (a b) (/ (+ a b) 2)))
(define (average-damp f)
  (lambda (x) (average x (f x))))
;(define (sqrt x)
;  (fixed-point (average-damp (lambda (y) (/ x y)))
;               1.0))
;(sqrt 2)
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))
(define dx 0.00001)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))
(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (/ x y))
                            average-damp
                            1.0))
(define (sqrt1 x)
  (fixed-point-of-transform (lambda (y) (- (square y) x))
                            newton-transform
                            1.0))

;; 1.40
(define cubic
  (lambda (a b c)
    (lambda (x)
      (+ (* x x x)
         (* a x x)
         (* b x)
         c))))
(newtons-method (cubic -4 5 -2) 2.5)
;; all zeroes?

;; 1.41
(define double
  (lambda (f)
    (lambda (x) 
      (f (f x)))))
(define (inc x) (+ x 1))
(((double (double double)) inc) 1)

;; 1.42
(define compose
  (lambda (f g)
    (lambda (x) (f (g x)))))
((compose square inc) 6)

;; 1.43
(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))
((repeated square 2) 5)

;; 1.44
(define smooth
  (lambda (f)
    (lambda (x) 
      (/ (+ (f x)
            (f (+ x dx))
            (f (- x dx)))
         3))))
(define n-smooth
  (lambda (f n)
    ((repeated smooth n) f)))
((n-smooth square 10) 2)

;; 1.45
(define (pow x n)
  (if (= n 1)
      x
      (* x (pow x (- n 1)))))
(define (nth-root x n)
  (fixed-point-of-transform (lambda (y) 
                              (/ x 
                                 (pow y (- n 1))))
                            (repeated average-damp 5)
                            3.0))
(nth-root 8 3)

;; 1.46
(define (iterative-improve good-enough? improve)
  (define (f guess)
    (if (good-enough? guess)
        guess
        (f (improve guess))))
  f)
(define (isqrt x)
  ((iterative-improve (lambda (y) (< (abs (- x (* y y))) 0.0001))
                     (lambda (y) (average y (/ x y)))) 2))
(isqrt 9.0)

(define (iterative-improvement good-enough? improve)
  (lambda (guess)
    (if (good-enough? guess)
        guess
        ((iterative-improvement good-enough? improve) (improve guess)))))

(define (sqrt2 x)
  (* 1.0 ((iterative-improvement (lambda (y) (< (abs (- x (* y y))) 0.0001))
                     (lambda (y) (average y (/ x y)))) x)))

(sqrt1 10)

 (define (iterative-improvment good-enough? improve)
  (lambda (guess)
    ((lambda (f guess)
       (if (good-enough? guess)
           guess
           (f (improve guess))))
     (lambda (f guess)
       (if (good-enough? guess)
           guess
           (f (improve guess))))
     guess)))
 
(define (sqrt3 x) 
  ((iterative-improvement (lambda (y) (< (abs (- x (* y y))) 0.0001))
                       (lambda (y) (average y (/ x y)))) x))
;((iterative-improvement (lambda (y) (< (abs (- 10 (* y y))) 0.0001))
 ;                      (lambda (y) (average y (/ 10 y)))) 2)
(sqrt3 10.0)