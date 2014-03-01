#lang planet neil/sicp

(define (wire-or-gate a1 a2 output)
  ((define c (make-wire))
   (define d (make-wire))
   (define e (make-wire))
   (inverter a1 c)
   (inverter a2 d)
   (and-gate c d e)
   (inverter e output)))
