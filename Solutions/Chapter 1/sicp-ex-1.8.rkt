#lang sicp
(define (square x) (* x x))
(define (cube x) (* x x x))

(define (improve guess x)
  (/ (+ (/ x (square guess))
        (* 2 guess))
     3))

(define (good-enough? guess x)
  (< (/ (abs (- x (cube guess))) x) 0.1))

(define (cube-root x)
  (cube-iter 1.0 x))

(define (cube-iter guess x)
  (cond ((= x 0) 0)
        ((good-enough? guess x) guess)
        (else (cube-iter (improve guess x)
                         x))))

(cube-root 9)
(cube-root 0.0001)
(cube-root 10000000000000.0001)
(cube-root 100000000000000000000)
(cube-root 100000000000000000000000000)
(cube-root 0.0000000000001)
(cube-root 0)
