#lang racket
(require graphics/graphics)
(open-graphics)

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
(edge2-frame frame))))))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))
;; 2.49
; a
(segments-painter (list (make-segment 
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
