#lang planet neil/sicp

;; Mutex
(define (make-mutex)
  (let ((cell (list false)))
    
    (define (clear! cell)
      (set-car! cell false))
    
    (define (test-and-set! cell)
      (if (car cell)
          true
          (begin (set-car! cell true)
                 false)))
    
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire))) ; retry 
            ((eq? m 'release) (clear! cell))))
    the-mutex))

;; 3.47 a Semaphore
(define (make-semaphore count)
  (let ((mutex (make-mutex))
        (acquirers 0))
    (define available? (< acquirers count))
    (define (acquire)
      (begin (mutex 'acquire)
             (if (available?)
                 (begin (set! acquirers (+ acquirers 1))
                        (mutex 'release))
                 (begin (mutex 'release)
                        (acquire)))))
    (define (release)
      (begin (mutex 'acquire)
             (if (> count 0)
                 (set! acquirers (- acquirers 1)))
             (mutex 'release)))
    (define (the-semaphore s)
      (cond ((eq? s 'acquire) acquire)
            ((eq? s 'release) release)))
    the-semaphore))