#lang planet neil/sicp
;sqrt
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
(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))
(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (/ x y))
                            average-damp
                            1.0))

;

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

;; 2.1
(define (make-rat n d)
  (let ((g (gcd n d))
        (neg-n (- n))
        (neg-d (- d)))
    (if (> d 0)
        (cons (/ n g) (/ d g))
        (cons (/ neg-n g) (/ neg-d g)))))
      
(print-rat (make-rat 2 3))
(print-rat (make-rat -2 3))
(print-rat (make-rat 2 -3))
(print-rat (make-rat -2 -3))

;; 2.2
(define (avg a b) (/ (+ a b) 2))

(define (make-point x y) (cons x y))
(define (x-point pt) (car pt))
(define (y-point pt) (cdr pt))

(define (make-segment start end) (cons start end))
(define (start-segment seg) (car seg))
(define (end-segment seg) (cdr seg))

(define (midpoint-segment line)
  (make-point (avg (x-point (start-segment line)) 
                   (x-point (end-segment line)))
              (avg (y-point (start-segment line)) 
                   (y-point (end-segment line)))))
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;(print-point (midpoint-segment (make-segment (make-point 0 0) (make-point 4 4))))

(define (length-of-segment line)
  (sqrt (+ (square (- (x-point (end-segment line)) 
                      (x-point (start-segment line)))) 
           (square (- (y-point (start-segment line)) 
                      (y-point (end-segment line)))))))

;; 2.3
;;
(define (perimeter rect)
  (* 2 (+ (length-rectangle rect)
          (breadth-rectangle rect))))
(define (area rect)
  (* (length-rectangle rect)
     (breadth-rectangle rect)))

; implement as four sides
(define (make-rectangle pt1 pt2 pt3 pt4)
  (cons (cons (make-segment pt1 pt2)
              (make-segment pt2 pt3))
        (cons (make-segment pt3 pt4)
              (make-segment pt4 pt1))))
(define (line1 rect)
  (car (car rect)))
(define (line2 rect)
  (car (cdr rect)))
(define (length-rectangle1 rect)
  (length-of-segment (line1 rect)))
(define (breadth-rectangle1 rect)
  (length-of-segment (line2 rect)))

;(perimeter (make-rectangle (make-point 0 0) (make-point 2 0) (make-point 2 2) (make-point 2 0)))
;(area (make-rectangle (make-point 0 0) (make-point 2 0) (make-point 2 2) (make-point 2 0)))

;(define (length-points rect)
 ; (car rect))
;(define (breadth-points rect)
 ; (cons (car (cdr rect))
  ;      (cdr (car rect))))
; implement as four points
(define (make-rectangle-from-points pt1 pt2 pt3 pt4)
  (cons (cons pt1 pt2)
        (cons pt3 pt4)))
(define (length-rectangle rect)
  (length-of-segment (make-segment (car (car rect)) (cdr (car rect)))))
(define (breadth-rectangle rect)
  (length-of-segment (make-segment (cdr (car rect)) (car (cdr rect)))))

(perimeter (make-rectangle-from-points (make-point 0 0) (make-point 2 0) (make-point 2 2) (make-point 2 0)))
(area (make-rectangle-from-points (make-point 0 0) (make-point 2 0) (make-point 2 2) (make-point 2 0)))


;; 2.4
(define (acdr z)
  (z (lambda (p q) q)))

;; 2.5
(define (logn a b)
  (/ (log a) (log b)))
(define (remove-component x a)
  (if (= (remainder x a) 0)
      (remove-component (/ x a) a)
      x))

(define (ncons a b)
  (* (expt 2 a) (expt 3 b)))
(define (ncar z)
  (logn (remove-component z 3) 2))
(define (ncdr z)
  (logn (remove-component z 2) 3))
(ncar (ncons 2 3))
(ncdr (ncons 2 3))

;; 2.6
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
;; ???

;; 2.7
(define (make-interval a b) (cons a b))
(define upper-bound (lambda (x) (car x)))
(define lower-bound (lambda (x) (cdr x)))

;; 2.8
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

;; 2.10
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
(define (div-interval x y)
  (if (or (not (= upper-bound 0)) (not (= upper-bound 0)))
      (mul-interval x(make-interval (/ 1.0 (upper-bound y))
                                    (/ 1.0 (lower-bound y))))
      (display "ZERO-DIVISION-ERROR!!")))

;; 2.12
(define (make-center-percent center percent) 
  (make-interval (* center (+ 1 (/ percent 100.0)))
                 (* center (- 1 (/ percent 100.0)))))
(define (center p-interval) 
  (/ (+ (upper-bound p-interval)
        (lower-bound p-interval))
     2))
(define (percent p-interval) 
  (* 100.0 
     (/ (- (upper-bound p-interval)
           (lower-bound p-interval)) 
        (* 2 
           (center p-interval)))))

(center (make-center-percent 5 10))
(percent (make-center-percent 5 10))