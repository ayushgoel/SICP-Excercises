#lang planet neil/sicp

;; Ex 3.1

(define (make-accumulator amount)
  (define (add x)
    (begin (set! amount (+ amount x))
           amount))
  (define (dispatch m)
    (cond ((eq? m 'add) add)
          (else (error "Unknown Request -- accumulator"
                       m))))
  dispatch)
(define A1 (make-accumulator 0))
((A1 'add) 10)
((A1 'add) 10)
((A1 'add) 10)
((A1 'add) 10)

;; 3.2

(define (make-monitored f)
  (let ((counter 0))
    (define (how-many-calls?) counter)
    (define (reset-count) (set! counter 0))
    (define (dispatch m)
      (cond ((eq? m 'how-many-calls?) (how-many-calls?))
            ((eq? m 'reset-count) (reset-count))
            (else (begin (set! counter (+ counter 1))
                         (f m)))))
    dispatch))

(define M1 (make-monitored sqrt))
(M1 'how-many-calls?)
(M1 16)
(M1 16)
(M1 16)
(M1 'how-many-calls?)
(M1 'reset-count)
(M1 'how-many-calls?)

;; 3.3

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch pass m)
    (if (eq? pass password)
         (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m)))
        (error "Incorrect Password")))
  dispatch)

(define AC1 (make-account 100 'a))
((AC1 'a 'withdraw) 20)
((AC1 'a 'deposit) 20)
((AC1 'b 'withdraw) 20)
