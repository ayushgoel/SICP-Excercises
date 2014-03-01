#lang planet neil/sicp

(define (assoc same-key? key records)
  (cond ((null? records) false)
        (( same-key? key (caar records)) (car records))
        (else (assoc same-key? key (cdr records)))))

(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc same-key? key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc same-key? key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc same-key? key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc same-key? key-2 (cdr subtable))))
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

(define tb1 (make-table eq?))
((tb1 'insert-proc!) 'a 'b 23)
((tb1 'lookup-proc) 'a 'b)
((tb1 'lookup-proc) 'a 'c)
((tb1 'insert-proc!) 'a 'c 12)
((tb1 'lookup-proc) 'a 'c)
