#lang planet neil/sicp
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

;; 2.33
(define (square x) (* x x))
(define (nmap p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence)) 
(nmap square (list 2 3 4))
(define (append seq1 seq2)
  (accumulate cons seq2 seq1)) 
(append (list 1 2 3 4) (list 1 2 3))
(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))
(length (list 1 2 3 5 2 3 4))

;; 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) 
                (+ this-coeff (* x higher-terms))) 
              0
              coefficient-sequence))

;; 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))
(define s (list (list 1 2 3) (list 6 4 2) (list 4 3 2)))
(accumulate-n + 0 s)

;; 2.37
(define (matrix-*-vector m v)
  (map 
   (lambda (x) (accumulate +
                      0
                      (map (lambda (a b) (* a b)) 
                           x 
                           v)))
   m))
(matrix-*-vector (list (list 1 2) (list 1 2))
                 (list 3 4))

(define (transpose mat)
  (accumulate-n cons nil mat))
(transpose (list (list 1 2) (list 3 4)))

;(define (matrix-*-matrix m n)
;  (let ((cols (transpose n)))))