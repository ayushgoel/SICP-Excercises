#lang planet neil/sicp

;(define (assoc key records)
;  (cond ((null? records) false)
;        ((equal? key (caar records)) (car records))
;        (else (assoc key (cdr records)))))

; 3.25
(define (make-table)
  (let ((local-table (list '*table*)))
    
    (define (lookup keys)
      (define (lookforkeys keys table)
        (let ((key (car keys)))
          (if (null? key)
              false
              (let ((subtable (assoc key (cdr table))))
                (if subtable
                    (if (pair? (car subtable))
                        (cadr subtable)
                        (lookup (cdr keys)))
                    false)))))
      (lookforkeys keys local-table))
    
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define tb1 (make-table))
((tb1 'insert-proc!) 'a 'b 12)
((tb1 'lookup-proc) (list 'a 'b))