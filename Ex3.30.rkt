#lang planet neil/sicp

(define (ripple-carry-adder A B S C)
  (let ((c-in (make-wire)))
    (if (null? (cdr A))
        (set-signal! c-in 0)
        (ripple-carry-adder (cdr A) (cdr B) (cdr S) c-in))
    (full-adder (car A) (car B) c-in (car S) C)))


(define (ripple-carry-adder as bs ss c)
  (if (null? as)
      (begin
        (set-signal! c 0)
        'ok)
      (let ((ai (car as))
            (bi (car bs))
            (si (car ss))
            (ci (make-wire)))
        (full-adder ai bi c si ci)
        (ripple-carry-adder (cdr as) (cdr bs) (cdr ss) ci))))
