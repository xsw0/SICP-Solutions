#lang sicp
(define (add-recursive a b)
  (if (= a 0)
      b
      (inc (add-recursive (dec a) b))))

(define (add-iterative a b)
  (if (= a 0)
      b
      (add-iterative (dec a) (inc b))))

(add-recursive 4 5)
(add-iterative 4 5)
