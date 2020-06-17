#lang sicp
(define (f-recursive n) ; n is integer
  (if (< n 3)
      n
      (+ (f-recursive (- n 1))
         (* 2 (f-recursive (- n 2)))
         (* 3 (f-recursive (- n 3))))))

(define (f-iterative n)
  (define (iter n-3 n-2 n-1 x)
    (let ((next (+ n-1
                   (* 2 n-2)
                   (* 3 n-3))))
      (if (= x n)
          next
          (iter n-2 n-1 next (+ x 1)))))
  (if (< n 3)
      n
      (iter 0 1 2 3)))

(f-recursive 0)
(f-recursive 1)
(f-recursive 2)
(f-recursive 3)
(f-recursive 4)
(f-recursive 5)

(f-iterative 0)
(f-iterative 1)
(f-iterative 2)
(f-iterative 3)
(f-iterative 4)
(f-iterative 5)