;#lang planet neil/sicp
#lang racket

(beside einstein (flip-vert einstein))
(define (square-limit painter n)
  (let ((quarter (flip-vert painter)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))
(paint (square-limit einstein 3))

(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))
(define wave
    (segments->painter
     (list (make-segment (make-vect 0.0 0.65) (make-vect 0.15 0.40))
           (make-segment (make-vect 0.15 0.40) (make-vect 0.25 0.55))
           (make-segment (make-vect 0.25 0.55) (make-vect 0.35 0.45))
           (make-segment (make-vect 0.35 0.45) (make-vect 0.25 0.0))
           (make-segment (make-vect 0.4 0.0) (make-vect 0.5 0.35))
           (make-segment (make-vect 0.5 0.35) (make-vect 0.6 0.0))
           (make-segment (make-vect 0.75 0.0) (make-vect 0.62 0.45))
           (make-segment (make-vect 0.62 0.45) (make-vect 1.0 0.15))
           (make-segment (make-vect 1.0 0.35) (make-vect 0.75 0.65))
           (make-segment (make-vect 0.75 0.65) (make-vect 0.62 0.65))
           (make-segment (make-vect 0.62 0.65) (make-vect 0.68 0.81))
           (make-segment (make-vect 0.68 0.81) (make-vect 0.62 1.0))
           (make-segment (make-vect 0.48 1.0) (make-vect 0.42 0.81))
           (make-segment (make-vect 0.42 0.81) (make-vect 0.48 0.65))
           (make-segment (make-vect 0.48 0.65) (make-vect 0.3 0.65))
           (make-segment (make-vect 0.3 0.65) (make-vect 0.18 0.58))
           (make-segment (make-vect 0.18 0.58) (make-vect 0.0 0.8)))))

(paint wave)

;; 2.44
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))
(paint (up-split einstein 2))

;; 2.45
(define (split trans1 trans2)
  (define (tmp painter n)
    (if (= n 0)
        painter
        (let ((smaller (tmp painter (- n 1))))
           (trans1 painter (trans2 smaller smaller)))))
  tmp)
;; right-split
;(paint ((split beside below) einstein 3))
;; up-split
;(paint ((split below beside) einstein 12))

;; 2.46
(define (make-vect-1 xcor ycor)
  (cons xcor ycor))
(define (xcor-vect v)
  (car v))
(define (ycor-vect v)
  (cdr v))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))
(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))
(define (scale-vect s v)
  (make-vect (* (xcor-vect v) s)
             (* (ycor-vect v) s)))

(define v1 (make-vect 2 3))
(define v2 (make-vect 7 8))
(add-vect v1 v2)
(sub-vect v1 v2)
(scale-vect 3 v1)

;; 2.47
(define (make-frame-1 origin edge1 edge2)
  (list origin edge1 edge2))
(define (origin-frame frame)
  (car frame))
(define (edge1-frame frame)
  (cadr frame))
(define (edge2-frame frame)
  (caddr frame))

;(define (make-frame origin edge1 edge2)
;  (cons origin (cons edge1 edge2)))
;(define (origin-frame frame)
;  (car frame))
;(define (edge1-frame frame)
;  (cadr frame))
;(define (edge2-frame frame)
;  (cddr frame))

;; 2.48
(define (make-segment-1 start-segment end-segment)
  (cons start-segment end-segment))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cdr segment))








; test
(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))
(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))
(paint (corner-split einstein 2))

