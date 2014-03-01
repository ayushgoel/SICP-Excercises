#lang planet neil/sicp

;; 2.73
; a
; because they won't respond to operator operation

; b
(define (deriv-sum operands var)
  (make-sum (deriv (addend operands) var)
            (deriv (augend operands) var)))

(put 'deriv '+ deriv-sum)


;; 2.74 TBD

;; 2.75
(define (make-from-mag-ang m a)
  (define (dispatch op)
    (cond ((eq? op 'magnitude) m)
          ((eq? op 'angle) a)
          ((eq? op 'real-part)
           (* m (cos a)))
          ((eq? op 'imag-part) (* m (sin a)))
          (else
           (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)
