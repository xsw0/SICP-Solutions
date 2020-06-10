#lang sicp
(define x 0)
(define (f n)
  (let((old 0))
    (begin (set! old x)
           (set! x (+ x n))
           old)))
(+ (f 0) (f 1))