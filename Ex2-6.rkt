;#lang planet neil/sicp
#lang racket
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

;;
;(define (segments->painter segment-list)
;  (lambda (frame)
;    (for-each
;     (lambda (segment)
;       (draw-line
;        ((frame-coord-map frame) (start-segment segment))
;        ((frame-coord-map frame) (end-segment segment))))
;     segment-list)))
; 2.49
; a
(segments->painter (list (make-segment 
                         (make-vect 0 0) 
                         (make-vect 0 0.99))
                        (make-segment 
                         (make-vect 0 0.99) 
                         (make-vect 0.99 0.99))
                        (make-segment 
                         (make-vect 0.99 0.99) 
                         (make-vect 0.99 0))
                        (make-segment 
                         (make-vect 0.99 0) 
                         (make-vect 0 0))))


;; 2.50
(define (flip-horiz-1 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

;; rotate counter clockwise 180 is same as flip-vert

(define (rotate-cc-270 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))


;; 2.51
(define (below-1 painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-bottom
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              (make-vect 0.0 1.0)
                              split-point))
          (paint-top
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.5)
                              (make-vect 0.0 1.0))))
      (lambda (frame)
        (paint-bottom frame)
        (paint-top frame)))))

;(define (below-as-beside painter1 painter2)
;  (lambda (frame)
;    (let ((p1-ac-90 (rotate-ac-90 painter1))
;          (p2-ac-90 (rotate-ac-90 painter2)))
;      (let ((aside-p1-p2 (beside p1-ac-90 p2-ac-90)))
;        (rotate-ac-270 aside-p1-p2)))))


;; 2.52

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

;; a
;; add more segments
(define list-of-lines (list (make-segment 
                             (make-vect 0 0) 
                             (make-vect 0 0.99)))) 
(define (wave)
  (lambda (frame)
    (segments-painter (list-of-lines))))

