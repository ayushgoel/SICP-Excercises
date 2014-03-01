#lang planet neil/sicp

(define (clear! cell)
  (set-car! cell false))

(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true)
             false)))

;; 3.47 b Semaphore
(define (make-semaphore count)
  (let ((cell (list false))
        (acquirers 0))
    (define available? (< acquirers count))
    (define (acquire)
      (begin (test-and-set! cell)
             (if (available?)
                 (begin (set! acquirers (+ acquirers 1))
                        (clear! cell))
                 (begin (clear! cell)
                        (acquire)))))
    (define (release)
      (begin (test-and-set! cell)
             (if (> count 0)
                 (set! acquirers (- acquirers 1)))
             (clear! cell)))
    (define (the-semaphore s)
      (cond ((eq? s 'acquire) acquire)
            ((eq? s 'release) release)))
    the-semaphore))