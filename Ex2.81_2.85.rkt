#lang planet neil/sicp

;; 2.81

; a It goes on a loop trying to raise the numbers again 
; and again and try to find a matching procedure to call.
; Or calls the function as retrieved from the table

; 2.82
; Iterate over keeping a max type-tag, coerce all args to that


; 2.83
;(put-coercion 'scheme-number 'rational integer->rational)
(define (integer->rational i)
  (make-rational (contents i) 1))
(put-raise 'scheme-number integer->rational)
(define (rational->real r)
  (make-real (exact->inexact (/ (numerator r) (denominator r)))))
(put-raise 'rational rational->real)
(define (real->complex r)
  (make-complex (contents r) 0))
(put-raise 'real real->complex)
(define (raise x)
  (let ((proc (get-raise (type-tag x))))
    (if proc
        (proc x)
        (error "Couldn't raise since procedure not found."))))
