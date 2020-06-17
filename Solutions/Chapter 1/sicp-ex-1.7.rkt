#lang sicp
(define (square x) (* x x))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess next-guess)
  (< (/ (abs (- next-guess guess)) guess) 0.1))

(define (square-root x)
  (sqrt-iter x 1.0 (improve 1.0 x)))

(define (sqrt-iter x guess next-guess)
  (cond ((= x 0) 0)
        ((good-enough? guess next-guess) guess)
        (else (sqrt-iter x
                         next-guess
                         (improve next-guess x)))))

(square-root 9)
(square-root 0.0001)
(square-root 10000000000000.0001)
(square-root 100000000000000000000)
(square-root 100000000000000000000000000)
(square-root 0.0000000000001)
(square-root 0)
