#lang sicp
(define (square x) (* x x))

(define (sum-of-squares-larger n1 n2 n3)
  (cond ((and (<= n1 n2)
              (<= n1 n3))
         (+ (square n2)
            (square n3)))
        ((and (<= n2 n1)
              (<= n2 n3))
         (+ (square n1)
            (square n3)))
        (else
         (+ (square n1)
            (square n2)))))


(sum-of-squares-larger 1 2 3)   ;Value: 13 
(sum-of-squares-larger 1 1 1)   ;Value: 2 
(sum-of-squares-larger 1 2 2)   ;Value: 8 
(sum-of-squares-larger 1 1 2)   ;Value: 5
